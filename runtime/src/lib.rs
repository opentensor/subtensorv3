#![cfg_attr(not(feature = "std"), no_std)]
// `construct_runtime!` does a lot of recursion and requires us to increase the limit to 256.
#![recursion_limit = "256"]
// Some arithmetic operations can't use the saturating equivalent, such as the PerThing types
#![allow(clippy::arithmetic_side_effects)]

// Make the WASM binary available.
#[cfg(feature = "std")]
include!(concat!(env!("OUT_DIR"), "/wasm_binary.rs"));

mod bag_thresholds;
pub mod check_nonce;
mod migrations;

use codec::{Decode, Encode, MaxEncodedLen};
use frame_election_provider_support::bounds::ElectionBoundsBuilder;
use frame_election_provider_support::{generate_solution_type, onchain, SequentialPhragmen};
use frame_support::pallet_prelude::DispatchClass;
use frame_support::traits::Imbalance;
use frame_support::PalletId;
use frame_support::{
    dispatch::DispatchResultWithPostInfo,
    genesis_builder_helper::{build_state, get_preset},
    pallet_prelude::Get,
    traits::{
        fungible::{
            DecreaseIssuance, HoldConsideration, Imbalance as FungibleImbalance, IncreaseIssuance,
        },
        Contains, LinearStoragePrice, OnUnbalanced,
    },
};
use frame_system::{EnsureNever, EnsureRoot, EnsureRootWithSuccess, RawOrigin};
use pallet_commitments::CanCommit;
use pallet_election_provider_multi_phase::GeometricDepositBase;
use pallet_grandpa::{
    fg_primitives, AuthorityId as GrandpaId, AuthorityList as GrandpaAuthorityList,
};
use pallet_registry::CanRegisterIdentity;
use pallet_session::historical as session_historical;
use pallet_staking::UseValidatorsMap;
use polkadot_core_primitives::Moment;
use runtime_common::prod_or_fast;
use scale_info::TypeInfo;
use smallvec::smallvec;
use sp_api::impl_runtime_apis;
use sp_consensus_aura::sr25519::AuthorityId as AuraId;
use sp_core::{crypto::ByteArray, H160, H256, U256};
use sp_core::{crypto::KeyTypeId, OpaqueMetadata};
use sp_runtime::curve::PiecewiseLinear;
use sp_runtime::generic::Era;
use sp_runtime::traits::OpaqueKeys;
use sp_runtime::transaction_validity::TransactionPriority;
use sp_runtime::Percent;
use sp_runtime::SaturatedConversion;
use sp_runtime::{
    create_runtime_str, generic, impl_opaque_keys,
    traits::{
        AccountIdLookup, BlakeTwo256, Block as BlockT, DispatchInfoOf, Dispatchable,
        IdentifyAccount, NumberFor, One, PostDispatchInfoOf, UniqueSaturatedInto, Verify,
    },
    transaction_validity::{TransactionSource, TransactionValidity, TransactionValidityError},
    AccountId32, ApplyExtrinsicResult, ConsensusEngineId, MultiSignature,
};
use sp_staking::currency_to_vote::SaturatingCurrencyToVote;
use sp_staking::SessionIndex;
use sp_std::cmp::Ordering;
use sp_std::prelude::*;
#[cfg(feature = "std")]
use sp_version::NativeVersion;
use sp_version::RuntimeVersion;

// A few exports that help ease life for downstream crates.
pub use frame_support::{
    construct_runtime, parameter_types,
    traits::{
        ConstBool, ConstU128, ConstU32, ConstU64, ConstU8, FindAuthor, InstanceFilter,
        KeyOwnerProofSystem, OnFinalize, OnTimestampSet, PrivilegeCmp, Randomness, StorageInfo,
    },
    weights::{
        constants::{
            BlockExecutionWeight, ExtrinsicBaseWeight, RocksDbWeight, WEIGHT_REF_TIME_PER_SECOND,
        },
        IdentityFee, Weight, WeightToFeeCoefficient, WeightToFeeCoefficients,
        WeightToFeePolynomial,
    },
    StorageValue,
};
pub use frame_system::Call as SystemCall;
pub use pallet_balances::Call as BalancesCall;
pub use pallet_timestamp::Call as TimestampCall;
use pallet_transaction_payment::{ConstFeeMultiplier, FungibleAdapter, Multiplier};
#[cfg(any(feature = "std", test))]
pub use sp_runtime::BuildStorage;
pub use sp_runtime::{Perbill, Permill};

use core::marker::PhantomData;

mod precompiles;
use precompiles::FrontierPrecompiles;

// Frontier
use fp_rpc::TransactionStatus;
use pallet_ethereum::{Call::transact, PostLogContent, Transaction as EthereumTransaction};
use pallet_evm::{Account as EVMAccount, BalanceConverter, FeeCalculator, Runner};

// Drand
impl pallet_drand::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type WeightInfo = pallet_drand::weights::SubstrateWeight<Runtime>;
    type AuthorityId = pallet_drand::crypto::TestAuthId;
    type Verifier = pallet_drand::verifier::QuicknetVerifier;
    type UnsignedPriority = ConstU64<{ 1 << 20 }>;
    type HttpFetchTimeout = ConstU64<1_000>;
}

impl frame_system::offchain::SigningTypes for Runtime {
    type Public = <Signature as Verify>::Signer;
    type Signature = Signature;
}

impl<C> frame_system::offchain::SendTransactionTypes<C> for Runtime
where
    RuntimeCall: From<C>,
{
    type Extrinsic = UncheckedExtrinsic;
    type OverarchingCall = RuntimeCall;
}

impl frame_system::offchain::CreateSignedTransaction<pallet_drand::Call<Runtime>> for Runtime {
    fn create_transaction<S: frame_system::offchain::AppCrypto<Self::Public, Self::Signature>>(
        call: RuntimeCall,
        public: <Signature as Verify>::Signer,
        account: AccountId,
        index: Index,
    ) -> Option<(
        RuntimeCall,
        <UncheckedExtrinsic as sp_runtime::traits::Extrinsic>::SignaturePayload,
    )> {
        use sp_runtime::traits::StaticLookup;

        let address = <Runtime as frame_system::Config>::Lookup::unlookup(account.clone());
        let extra: SignedExtra = (
            frame_system::CheckNonZeroSender::<Runtime>::new(),
            frame_system::CheckSpecVersion::<Runtime>::new(),
            frame_system::CheckTxVersion::<Runtime>::new(),
            frame_system::CheckGenesis::<Runtime>::new(),
            frame_system::CheckEra::<Runtime>::from(Era::Immortal),
            check_nonce::CheckNonce::<Runtime>::from(index),
            frame_system::CheckWeight::<Runtime>::new(),
            pallet_transaction_payment::ChargeTransactionPayment::<Runtime>::from(0),
            pallet_subtensor::SubtensorSignedExtension::<Runtime>::new(),
            pallet_commitments::CommitmentsSignedExtension::<Runtime>::new(),
            frame_metadata_hash_extension::CheckMetadataHash::<Runtime>::new(true),
        );

        let raw_payload = SignedPayload::new(call.clone(), extra.clone()).ok()?;
        let signature = raw_payload.using_encoded(|payload| S::sign(payload, public))?;

        let signature_payload = (address, signature, extra);

        Some((call, signature_payload))
    }
}

// Subtensor module
pub use pallet_scheduler;
pub use pallet_subtensor;

// An index to a block.
pub type BlockNumber = u32;

// Alias to 512-bit hash when used in the context of a transaction signature on the chain.
pub type Signature = MultiSignature;

// Some way of identifying an account on the chain. We intentionally make it equivalent
// to the public key of our transaction signing scheme.
pub type AccountId = <<Signature as Verify>::Signer as IdentifyAccount>::AccountId;

// Balance of an account.
pub type Balance = u64;

// Index of a transaction in the chain.
pub type Index = u32;

// A hash of some data used by the chain.
pub type Hash = sp_core::H256;

// Member type for membership
type MemberCount = u32;

pub type Nonce = u32;

// Method used to calculate the fee of an extrinsic
pub const fn deposit(items: u32, bytes: u32) -> Balance {
    pub const ITEMS_FEE: Balance = 2_000 * 10_000;
    pub const BYTES_FEE: Balance = 100 * 10_000;
    (items as Balance)
        .saturating_mul(ITEMS_FEE)
        .saturating_add((bytes as Balance).saturating_mul(BYTES_FEE))
}

parameter_types! {
    /// A limit for off-chain phragmen unsigned solution submission.
    ///
    /// We want to keep it as high as possible, but can't risk having it reject,
    /// so we always subtract the base block execution weight.
    pub OffchainSolutionWeightLimit: Weight = BlockWeights::get()
        .get(DispatchClass::Normal)
        .max_extrinsic
        .expect("Normal extrinsics have weight limit configured by default; qed")
        .saturating_sub(BlockExecutionWeight::get());

    /// A limit for off-chain phragmen unsigned solution length.
    ///
    /// We allow up to 90% of the block's size to be consumed by the solution.
    pub OffchainSolutionLengthLimit: u32 = Perbill::from_rational(90_u32, 100) *
        *BlockLength::get()
        .max
        .get(DispatchClass::Normal);
}

/// 1 in 4 blocks (on average, not counting collisions) will be primary babe blocks.
/// The choice of is done in accordance to the slot duration and expected target
/// block time, for safely resisting network delays of maximum two seconds.
/// <https://research.web3.foundation/en/latest/polkadot/BABE/Babe/#6-practical-results>
pub const PRIMARY_PROBABILITY: (u64, u64) = (1, 4);

/// RAO per TAO
pub const UNITS: Balance = 1_000_000_000;

/// TODO: Check this
pub const EPOCH_DURATION_IN_SLOTS: BlockNumber = prod_or_fast!(4 * HOURS, 1 * MINUTES);

// Opaque types. These are used by the CLI to instantiate machinery that don't need to know
// the specifics of the runtime. They can then be made to be agnostic over specific formats
// of data like extrinsics, allowing for them to continue syncing the network through upgrades
// to even the core data structures.
pub mod opaque {
    use super::*;

    pub use sp_runtime::OpaqueExtrinsic as UncheckedExtrinsic;

    // Opaque block header type.
    pub type Header = generic::Header<BlockNumber, BlakeTwo256>;
    // Opaque block type.
    pub type Block = generic::Block<Header, UncheckedExtrinsic>;
    // Opaque block identifier type.
    pub type BlockId = generic::BlockId<Block>;

    impl_opaque_keys! {
        pub struct SessionKeys {
            pub grandpa: Grandpa,
            pub babe: Babe,
            pub authority_discovery: AuthorityDiscovery,
        }
    }
}

// To learn more about runtime versioning, see:
// https://docs.substrate.io/main-docs/build/upgrade#runtime-versioning
#[sp_version::runtime_version]
pub const VERSION: RuntimeVersion = RuntimeVersion {
    spec_name: create_runtime_str!("node-subtensor"),
    impl_name: create_runtime_str!("node-subtensor"),
    authoring_version: 1,
    // The version of the runtime specification. A full node will not attempt to use its native
    //   runtime in substitute for the on-chain Wasm runtime unless all of `spec_name`,
    //   `spec_version`, and `authoring_version` are the same between Wasm and native.
    // This value is set to 100 to notify Polkadot-JS App (https://polkadot.js.org/apps) to use
    //   the compatible custom types.
    spec_version: 218,
    impl_version: 1,
    apis: RUNTIME_API_VERSIONS,
    transaction_version: 1,
    state_version: 1,
};

/// The BABE epoch configuration at genesis.
pub const BABE_GENESIS_EPOCH_CONFIG: babe_primitives::BabeEpochConfiguration =
    babe_primitives::BabeEpochConfiguration {
        c: PRIMARY_PROBABILITY,
        allowed_slots: babe_primitives::AllowedSlots::PrimaryAndSecondaryVRFSlots,
    };

/// This determines the average expected block time that we are targeting.
/// Blocks will be produced at a minimum duration defined by `SLOT_DURATION`.
/// `SLOT_DURATION` is picked up by `pallet_timestamp` which is in turn picked
/// up by `pallet_aura` to implement `fn slot_duration()`.
///
/// Change this to adjust the block time.
pub const MILLISECS_PER_BLOCK: u64 = prod_or_fast!(12_000, 1000);

// NOTE: Currently it is not possible to change the slot duration after the chain has started.
//       Attempting to do so will brick block production.
pub const SLOT_DURATION: u64 = MILLISECS_PER_BLOCK;

// Time is measured by number of blocks.
pub const MINUTES: BlockNumber = 60_000 / (MILLISECS_PER_BLOCK as BlockNumber);
pub const HOURS: BlockNumber = MINUTES * 60;
pub const DAYS: BlockNumber = HOURS * 24;

pub const MAXIMUM_BLOCK_WEIGHT: Weight =
    Weight::from_parts(4u64 * WEIGHT_REF_TIME_PER_SECOND, u64::MAX);

// The version information used to identify this runtime when compiled natively.
#[cfg(feature = "std")]
pub fn native_version() -> NativeVersion {
    NativeVersion {
        runtime_version: VERSION,
        can_author_with: Default::default(),
    }
}

const NORMAL_DISPATCH_RATIO: Perbill = Perbill::from_percent(75);

parameter_types! {
    pub const BlockHashCount: BlockNumber = 2400;
    pub const Version: RuntimeVersion = VERSION;
    // We allow for 2 seconds of compute with a 6 second average block time.
    pub BlockWeights: frame_system::limits::BlockWeights =
        frame_system::limits::BlockWeights::with_sensible_defaults(
            MAXIMUM_BLOCK_WEIGHT,
            NORMAL_DISPATCH_RATIO,
        );
    pub BlockLength: frame_system::limits::BlockLength = frame_system::limits::BlockLength
        ::max_with_normal_ratio(10 * 1024 * 1024, NORMAL_DISPATCH_RATIO);
    pub const SS58Prefix: u8 = 42;
}

// Configure FRAME pallets to include in runtime.

impl frame_system::Config for Runtime {
    // The basic call filter to use in dispatchable.
    type BaseCallFilter = SafeMode;
    // Block & extrinsics weights: base values and limits.
    type BlockWeights = BlockWeights;
    // The maximum length of a block (in bytes).
    type BlockLength = BlockLength;
    // The identifier used to distinguish between accounts.
    type AccountId = AccountId;
    // The aggregated dispatch type that is available for extrinsics.
    type RuntimeCall = RuntimeCall;
    // The aggregated runtime tasks.
    type RuntimeTask = RuntimeTask;
    // The lookup mechanism to get account ID from whatever is passed in dispatchers.
    type Lookup = AccountIdLookup<AccountId, ()>;
    // The type for hashing blocks and tries.
    type Hash = Hash;
    // The hashing algorithm used.
    type Hashing = BlakeTwo256;
    // The ubiquitous event type.
    type RuntimeEvent = RuntimeEvent;
    // The ubiquitous origin type.
    type RuntimeOrigin = RuntimeOrigin;
    // Maximum number of block number to block hash mappings to keep (oldest pruned first).
    type BlockHashCount = BlockHashCount;
    // The weight of database operations that the runtime can invoke.
    type DbWeight = RocksDbWeight;
    // Version of the runtime.
    type Version = Version;
    // Converts a module to the index of the module in `construct_runtime!`.
    //
    // This type is being generated by `construct_runtime!`.
    type PalletInfo = PalletInfo;
    // What to do if a new account is created.
    type OnNewAccount = ();
    // What to do if an account is fully reaped from the system.
    type OnKilledAccount = ();
    // The data to be stored in an account.
    type AccountData = pallet_balances::AccountData<Balance>;
    // Weight information for the extrinsics of this pallet.
    type SystemWeightInfo = ();
    // This is used as an identifier of the chain. 42 is the generic substrate prefix.
    type SS58Prefix = SS58Prefix;
    // The set code logic, just the default since we're not a parachain.
    type OnSetCode = ();
    type MaxConsumers = frame_support::traits::ConstU32<16>;
    type Nonce = Nonce;
    type Block = Block;
    type SingleBlockMigrations = Migrations;
    type MultiBlockMigrator = ();
    type PreInherents = ();
    type PostInherents = ();
    type PostTransactions = ();
}

parameter_types! {
    pub EpochDuration: u64 = prod_or_fast!(
        EPOCH_DURATION_IN_SLOTS as u64,
        2 * MINUTES as u64,
        "TAO_EPOCH_DURATION"
    );
    pub const ExpectedBlockTime: Moment = MILLISECS_PER_BLOCK;
    pub ReportLongevity: u64 =
        BondingDuration::get() as u64 * SessionsPerEra::get() as u64 * EpochDuration::get();
}

impl pallet_babe::Config for Runtime {
    type EpochDuration = EpochDuration;
    type ExpectedBlockTime = ExpectedBlockTime;

    // session module is the trigger
    type EpochChangeTrigger = pallet_babe::ExternalTrigger;

    type DisabledValidators = Session;

    type WeightInfo = ();

    type MaxAuthorities = MaxAuthorities;
    type MaxNominators = MaxNominators;

    type KeyOwnerProof =
        <Historical as KeyOwnerProofSystem<(KeyTypeId, pallet_babe::AuthorityId)>>::Proof;

    type EquivocationReportSystem =
        pallet_babe::EquivocationReportSystem<Self, Offences, Historical, ReportLongevity>;
}

impl pallet_authorship::Config for Runtime {
    type FindAuthor = pallet_session::FindAccountFromAuthorIndex<Self, Babe>;
    type EventHandler = Staking;
}

impl pallet_session::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type ValidatorId = AccountId;
    type ValidatorIdOf = pallet_staking::StashOf<Self>;
    type ShouldEndSession = Babe;
    type NextSessionRotation = Babe;
    type SessionManager = pallet_session::historical::NoteHistoricalRoot<Self, Staking>;
    type SessionHandler = <opaque::SessionKeys as OpaqueKeys>::KeyTypeIdProviders;
    type Keys = opaque::SessionKeys;
    type WeightInfo = ();
}

impl pallet_session::historical::Config for Runtime {
    type FullIdentification = pallet_staking::Exposure<AccountId, Balance>;
    type FullIdentificationOf = pallet_staking::ExposureOf<Runtime>;
}

parameter_types! {
    // phase durations. 1/4 of the last session for each.
    // in testing: 1min or half of the session for each
    pub SignedPhase: u32 = prod_or_fast!(
        EPOCH_DURATION_IN_SLOTS / 4,
        (1 * MINUTES).min(EpochDuration::get().saturated_into::<u32>() / 2),
        "TAO_SIGNED_PHASE"
    );
    pub UnsignedPhase: u32 = prod_or_fast!(
        EPOCH_DURATION_IN_SLOTS / 4,
        (1 * MINUTES).min(EpochDuration::get().saturated_into::<u32>() / 2),
        "TAO_UNSIGNED_PHASE"
    );

    // signed config
    pub const SignedMaxSubmissions: u32 = 16;
    pub const SignedMaxRefunds: u32 = 16 / 4;
    pub const SignedFixedDeposit: Balance = deposit(2, 0);
    pub const SignedDepositIncreaseFactor: Percent = Percent::from_percent(10);
    // 0.01 TAO per KB of solution data.
    pub const SignedDepositByte: Balance = deposit(0, 10) / 1024;
    // Each good submission will get 1 TAO as reward
    pub SignedRewardBase: Balance = 1 * UNITS;

    // 4 hour session, 1 hour unsigned phase, 32 offchain executions.
    pub OffchainRepeat: BlockNumber = UnsignedPhase::get() / 32;

    pub const MaxElectingVoters: u32 = 22_500;
    /// We take the top 22500 nominators as electing voters and all of the validators as electable
    /// targets. Whilst this is the case, we cannot and shall not increase the size of the
    /// validator intentions.
    pub ElectionBounds: frame_election_provider_support::bounds::ElectionBounds =
        ElectionBoundsBuilder::default().voters_count(MaxElectingVoters::get().into()).build();
    /// Setup election pallet to support maximum winners upto 1200. This will mean Staking Pallet
    /// cannot have active validators higher than this count.
    pub const MaxActiveValidators: u32 = 1200;
}

generate_solution_type!(
    #[compact]
    pub struct NposCompactSolution16::<
        VoterIndex = u32,
        TargetIndex = u16,
        Accuracy = sp_runtime::PerU16,
        MaxVoters = MaxElectingVoters,
    >(16)
);

pub struct OnChainSeqPhragmen;
impl onchain::Config for OnChainSeqPhragmen {
    type System = Runtime;
    type Solver = SequentialPhragmen<AccountId, runtime_common::elections::OnChainAccuracy>;
    type DataProvider = Staking;
    type WeightInfo = ();
    type MaxWinners = MaxActiveValidators;
    type Bounds = ElectionBounds;
}

impl pallet_election_provider_multi_phase::MinerConfig for Runtime {
    type AccountId = AccountId;
    type MaxLength = OffchainSolutionLengthLimit;
    type MaxWeight = OffchainSolutionWeightLimit;
    type Solution = NposCompactSolution16;
    type MaxVotesPerVoter = <
		<Self as pallet_election_provider_multi_phase::Config>::DataProvider
		as
		frame_election_provider_support::ElectionDataProvider
	>::MaxVotesPerVoter;
    type MaxWinners = MaxActiveValidators;

    // The unsigned submissions have to respect the weight of the submit_unsigned call, thus their
    // weight estimate function is wired to this call's weight.
    fn solution_weight(v: u32, t: u32, a: u32, d: u32) -> Weight {
        <
			<Self as pallet_election_provider_multi_phase::Config>::WeightInfo
			as
			pallet_election_provider_multi_phase::WeightInfo
		>::submit_unsigned(v, t, a, d)
    }
}

impl pallet_election_provider_multi_phase::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type Currency = Balances;
    type EstimateCallFee = TransactionPayment;
    type SignedPhase = SignedPhase;
    type UnsignedPhase = UnsignedPhase;
    type SignedMaxSubmissions = SignedMaxSubmissions;
    type SignedMaxRefunds = SignedMaxRefunds;
    type SignedRewardBase = SignedRewardBase;
    type SignedDepositBase =
        GeometricDepositBase<Balance, SignedFixedDeposit, SignedDepositIncreaseFactor>;
    type SignedDepositByte = SignedDepositByte;
    type SignedDepositWeight = ();
    type SignedMaxWeight =
        <Self::MinerConfig as pallet_election_provider_multi_phase::MinerConfig>::MaxWeight;
    type MinerConfig = Self;
    type SlashHandler = (); // burn slashes
    type RewardHandler = (); // nothing to do upon rewards
    type BetterSignedThreshold = ();
    type OffchainRepeat = OffchainRepeat;
    type MinerTxPriority = NposSolutionPriority;
    type DataProvider = Staking;
    #[cfg(any(feature = "fast-runtime", feature = "runtime-benchmarks"))]
    type Fallback = onchain::OnChainExecution<OnChainSeqPhragmen>;
    #[cfg(not(any(feature = "fast-runtime", feature = "runtime-benchmarks")))]
    type Fallback = frame_election_provider_support::NoElection<(
        AccountId,
        BlockNumber,
        Staking,
        MaxActiveValidators,
    )>;
    type GovernanceFallback = onchain::OnChainExecution<OnChainSeqPhragmen>;
    type Solver = SequentialPhragmen<
        AccountId,
        pallet_election_provider_multi_phase::SolutionAccuracyOf<Self>,
        (),
    >;
    type BenchmarkingConfig = runtime_common::elections::BenchmarkConfig;
    type ForceOrigin = EnsureRoot<Self::AccountId>;
    type WeightInfo = ();
    type MaxWinners = MaxActiveValidators;
    type ElectionBounds = ElectionBounds;
}

parameter_types! {
    pub const BagThresholds: &'static [u64] = &bag_thresholds::THRESHOLDS;
}

type VoterBagsListInstance = pallet_bags_list::Instance1;
impl pallet_bags_list::Config<VoterBagsListInstance> for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type ScoreProvider = Staking;
    type WeightInfo = ();
    type BagThresholds = BagThresholds;
    type Score = sp_npos_elections::VoteWeight;
}

// TODO #6469: This shouldn't be static, but a lazily cached value, not built unless needed, and
// re-built in case input parameters have changed. The `ideal_stake` should be determined by the
// amount of parachain slots being bid on: this should be around `(75 - 25.min(slots / 4))%`.
pallet_staking_reward_curve::build! {
    const REWARD_CURVE: PiecewiseLinear<'static> = curve!(
        min_inflation: 0_025_000,
        max_inflation: 0_100_000,
        // 3:2:1 staked : parachains : float.
        // while there's no parachains, then this is 75% staked : 25% float.
        ideal_stake: 0_750_000,
        falloff: 0_050_000,
        max_piece_count: 40,
        test_precision: 0_005_000,
    );
}

parameter_types! {
    // Six sessions in an era (24 hours).
    pub const SessionsPerEra: SessionIndex = prod_or_fast!(6, 1);

    // 28 eras for unbonding (28 days).
    pub BondingDuration: sp_staking::EraIndex = prod_or_fast!(
        28,
        28,
        "DOT_BONDING_DURATION"
    );
    pub SlashDeferDuration: sp_staking::EraIndex = prod_or_fast!(
        27,
        27,
        "DOT_SLASH_DEFER_DURATION"
    );
    pub const RewardCurve: &'static PiecewiseLinear<'static> = &REWARD_CURVE;
    pub const MaxExposurePageSize: u32 = 512;
    // Note: this is not really correct as Max Nominators is (MaxExposurePageSize * page_count) but
    // this is an unbounded number. We just set it to a reasonably high value, 1 full page
    // of nominators.
    pub const MaxNominators: u32 = 512;
    // pub const OffendingValidatorsThreshold: Perbill = Perbill::from_percent(17);
    // 16
    pub const MaxNominations: u32 = <NposCompactSolution16 as frame_election_provider_support::NposSolution>::LIMIT as u32;
}

/// Custom version of `runtime_commong::era_payout` somewhat tailored for Polkadot's crowdloan
/// unlock history. The only tweak should be
///
/// ```diff
/// - let auction_proportion = Perquintill::from_rational(auctioned_slots.min(60), 200u64);
/// + let auction_proportion = Perquintill::from_rational(auctioned_slots.min(60), 300u64);
/// ```
///
/// See <https://forum.polkadot.network/t/adjusting-polkadots-ideal-staking-rate-calculation/3897>.
// fn polkadot_era_payout(
//     _total_staked: Balance,
//     _total_stakable: Balance,
//     _max_annual_inflation: Perquintill,
//     _period_fraction: Perquintill,
//     _auctioned_slots: u64,
// ) -> (Balance, Balance) {
//     todo!()

// let min_annual_inflation = Perquintill::from_rational(25u64, 1000u64);
// let delta_annual_inflation = max_annual_inflation.saturating_sub(min_annual_inflation);
//
// // 20% reserved for up to 60 slots.
// let auction_proportion = Perquintill::from_rational(auctioned_slots.min(60), 300u64);
//
// // Therefore the ideal amount at stake (as a percentage of total issuance) is 75% less the
// // amount that we expect to be taken up with auctions.
// let ideal_stake = Perquintill::from_percent(75).saturating_sub(auction_proportion);
//
// let stake = Perquintill::from_rational(total_staked, total_stakable);
// let falloff = Perquintill::from_percent(5);
// let adjustment = compute_inflation(stake, ideal_stake, falloff);
// let staking_inflation =
//     min_annual_inflation.saturating_add(delta_annual_inflation * adjustment);
//
// let max_payout = period_fraction * max_annual_inflation * total_stakable;
// let staking_payout = (period_fraction * staking_inflation) * total_stakable;
// let rest = max_payout.saturating_sub(staking_payout);
//
// let other_issuance = total_stakable.saturating_sub(total_staked);
// if total_staked > other_issuance {
//     let _cap_rest = Perquintill::from_rational(other_issuance, total_staked) * staking_payout;
//     // We don't do anything with this, but if we wanted to, we could introduce a cap on the
//     // treasury amount with: `rest = rest.min(cap_rest);`
// }
// (staking_payout, rest)
// }

pub struct EraPayout;
impl pallet_staking::EraPayout<Balance> for EraPayout {
    fn era_payout(
        _total_staked: Balance,
        _total_issuance: Balance,
        _era_duration_millis: u64,
    ) -> (Balance, Balance) {
        todo!()

        // all para-ids that are not active.
        // let auctioned_slots = Paras::parachains()
        //     .into_iter()
        //     // all active para-ids that do not belong to a system chain is the number
        //     // of parachains that we should take into account for inflation.
        //     .filter(|i| *i >= LOWEST_PUBLIC_ID)
        //     .count() as u64;
        //
        // const MAX_ANNUAL_INFLATION: Perquintill = Perquintill::from_percent(10);
        // const MILLISECONDS_PER_YEAR: u64 = 1000 * 3600 * 24 * 36525 / 100;
        //
        // polkadot_era_payout(
        //     total_staked,
        //     total_issuance,
        //     MAX_ANNUAL_INFLATION,
        //     Perquintill::from_rational(era_duration_millis, MILLISECONDS_PER_YEAR),
        //     auctioned_slots,
        // )
    }
}

impl pallet_staking::Config for Runtime {
    type Currency = Balances;
    type CurrencyBalance = Balance;
    type UnixTime = Timestamp;
    type CurrencyToVote = SaturatingCurrencyToVote;
    type RewardRemainder = ();
    // type RewardRemainder = Treasury;
    type RuntimeEvent = RuntimeEvent;
    type Slash = ();
    // type Slash = Treasury;
    type Reward = ();
    type SessionsPerEra = SessionsPerEra;
    type BondingDuration = BondingDuration;
    type SlashDeferDuration = SlashDeferDuration;
    type AdminOrigin = EnsureRoot<Self::AccountId>;
    type SessionInterface = Self;
    type EraPayout = EraPayout;
    type MaxExposurePageSize = MaxExposurePageSize;
    type NextNewSession = Session;
    type ElectionProvider = ElectionProviderMultiPhase;
    type GenesisElectionProvider = onchain::OnChainExecution<OnChainSeqPhragmen>;
    type VoterList = VoterList;
    type TargetList = UseValidatorsMap<Self>;
    type NominationsQuota = pallet_staking::FixedNominationsQuota<{ MaxNominations::get() }>;
    type MaxUnlockingChunks = frame_support::traits::ConstU32<32>;
    type HistoryDepth = frame_support::traits::ConstU32<84>;
    type MaxControllersInDeprecationBatch = ConstU32<5314>;
    type BenchmarkingConfig = runtime_common::StakingBenchmarkingConfig;
    type DisablingStrategy = pallet_staking::UpToLimitDisablingStrategy;
    // type EventListeners = NominationPools;
    type EventListeners = ();
    type WeightInfo = ();
}

impl pallet_fast_unstake::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type Currency = Balances;
    type BatchSize = frame_support::traits::ConstU32<16>;
    type Deposit = frame_support::traits::ConstU64<{ UNITS }>;
    type ControlOrigin = EnsureRoot<AccountId>;
    type Staking = Staking;
    type MaxErasToCheckPerBlock = ConstU32<1>;
    type WeightInfo = ();
}

impl pallet_offences::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type IdentificationTuple = pallet_session::historical::IdentificationTuple<Self>;
    type OnOffenceHandler = Staking;
}

parameter_types! {
    // TODO: Polkadot default 👇
    // pub const MaxAuthorities: u32 = 100_000;
    pub const MaxAuthorities: u32 = 32;
    pub NposSolutionPriority: TransactionPriority =
        Perbill::from_percent(90) * TransactionPriority::max_value();
}

impl pallet_authority_discovery::Config for Runtime {
    type MaxAuthorities = MaxAuthorities;
}

parameter_types! {
    pub const PoolsPalletId: PalletId = PalletId(*b"py/nopls");
    // Allow pools that got slashed up to 90% to remain operational.
    pub const MaxPointsToBalance: u8 = 10;
}

impl pallet_insecure_randomness_collective_flip::Config for Runtime {}

parameter_types! {
    pub MaxSetIdSessionEntries: u32 = BondingDuration::get() * SessionsPerEra::get();
}

impl pallet_grandpa::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;

    type KeyOwnerProof = <Historical as KeyOwnerProofSystem<(KeyTypeId, GrandpaId)>>::Proof;

    type WeightInfo = ();
    type MaxAuthorities = MaxAuthorities;
    type MaxSetIdSessionEntries = MaxSetIdSessionEntries;
    type MaxNominators = MaxNominators;

    type EquivocationReportSystem =
        pallet_grandpa::EquivocationReportSystem<Self, Offences, Historical, ReportLongevity>;
}

impl pallet_timestamp::Config for Runtime {
    // A timestamp: milliseconds since the unix epoch.
    type Moment = u64;
    type OnTimestampSet = Babe;
    type MinimumPeriod = ConstU64<{ SLOT_DURATION / 2 }>;
    type WeightInfo = ();
}

impl pallet_utility::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type RuntimeCall = RuntimeCall;
    type PalletsOrigin = OriginCaller;
    type WeightInfo = pallet_utility::weights::SubstrateWeight<Runtime>;
}

parameter_types! {
    pub const DisallowPermissionlessEnterDuration: BlockNumber = 0;
    pub const DisallowPermissionlessExtendDuration: BlockNumber = 0;

    pub const RootEnterDuration: BlockNumber = 5 * 60 * 24; // 24 hours

    pub const RootExtendDuration: BlockNumber = 5 * 60 * 12; // 12 hours

    pub const DisallowPermissionlessEntering: Option<Balance> = None;
    pub const DisallowPermissionlessExtending: Option<Balance> = None;
    pub const DisallowPermissionlessRelease: Option<BlockNumber> = None;
}

pub struct SafeModeWhitelistedCalls;
impl Contains<RuntimeCall> for SafeModeWhitelistedCalls {
    fn contains(call: &RuntimeCall) -> bool {
        matches!(
            call,
            RuntimeCall::Sudo(_)
                | RuntimeCall::Multisig(_)
                | RuntimeCall::System(_)
                | RuntimeCall::SafeMode(_)
                | RuntimeCall::Timestamp(_)
                | RuntimeCall::SubtensorModule(
                    pallet_subtensor::Call::set_weights { .. }
                        | pallet_subtensor::Call::set_root_weights { .. }
                        | pallet_subtensor::Call::serve_axon { .. }
                )
                | RuntimeCall::Commitments(pallet_commitments::Call::set_commitment { .. })
        )
    }
}

impl pallet_safe_mode::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type Currency = Balances;
    type RuntimeHoldReason = RuntimeHoldReason;
    type WhitelistedCalls = SafeModeWhitelistedCalls;
    type EnterDuration = DisallowPermissionlessEnterDuration;
    type ExtendDuration = DisallowPermissionlessExtendDuration;
    type EnterDepositAmount = DisallowPermissionlessEntering;
    type ExtendDepositAmount = DisallowPermissionlessExtending;
    type ForceEnterOrigin = EnsureRootWithSuccess<AccountId, RootEnterDuration>;
    type ForceExtendOrigin = EnsureRootWithSuccess<AccountId, RootExtendDuration>;
    type ForceExitOrigin = EnsureRoot<AccountId>;
    type ForceDepositOrigin = EnsureRoot<AccountId>;
    type Notify = ();
    type ReleaseDelay = DisallowPermissionlessRelease;
    type WeightInfo = pallet_safe_mode::weights::SubstrateWeight<Runtime>;
}

// Existential deposit.
pub const EXISTENTIAL_DEPOSIT: u64 = 500;

impl pallet_balances::Config for Runtime {
    type MaxLocks = ConstU32<50>;
    type MaxReserves = ConstU32<50>;
    type ReserveIdentifier = [u8; 8];
    // The type for recording an account's balance.
    type Balance = Balance;
    // The ubiquitous event type.
    type RuntimeEvent = RuntimeEvent;
    type DustRemoval = ();
    type ExistentialDeposit = ConstU64<EXISTENTIAL_DEPOSIT>;
    type AccountStore = System;
    type WeightInfo = pallet_balances::weights::SubstrateWeight<Runtime>;

    type RuntimeHoldReason = RuntimeHoldReason;
    type RuntimeFreezeReason = RuntimeFreezeReason;
    type FreezeIdentifier = RuntimeFreezeReason;
    type MaxFreezes = ConstU32<50>;
}

pub struct LinearWeightToFee;

impl WeightToFeePolynomial for LinearWeightToFee {
    type Balance = Balance;

    fn polynomial() -> WeightToFeeCoefficients<Self::Balance> {
        let coefficient = WeightToFeeCoefficient {
            coeff_integer: 0,
            coeff_frac: Perbill::from_parts(500_000),
            negative: false,
            degree: 1,
        };

        smallvec!(coefficient)
    }
}

parameter_types! {
    pub const OperationalFeeMultiplier: u8 = 5;
    pub FeeMultiplier: Multiplier = Multiplier::one();
}

/// Deduct the transaction fee from the Subtensor Pallet TotalIssuance when dropping the transaction
/// fee.
pub struct TransactionFeeHandler;
impl
    OnUnbalanced<
        FungibleImbalance<
            u64,
            DecreaseIssuance<AccountId32, pallet_balances::Pallet<Runtime>>,
            IncreaseIssuance<AccountId32, pallet_balances::Pallet<Runtime>>,
        >,
    > for TransactionFeeHandler
{
    fn on_nonzero_unbalanced(
        credit: FungibleImbalance<
            u64,
            DecreaseIssuance<AccountId32, pallet_balances::Pallet<Runtime>>,
            IncreaseIssuance<AccountId32, pallet_balances::Pallet<Runtime>>,
        >,
    ) {
        let ti_before = pallet_subtensor::TotalIssuance::<Runtime>::get();
        pallet_subtensor::TotalIssuance::<Runtime>::put(ti_before.saturating_sub(credit.peek()));
        drop(credit);
    }
}

impl pallet_transaction_payment::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type OnChargeTransaction = FungibleAdapter<Balances, TransactionFeeHandler>;
    // Convert dispatch weight to a chargeable fee.
    type WeightToFee = LinearWeightToFee;
    type OperationalFeeMultiplier = OperationalFeeMultiplier;
    type LengthToFee = IdentityFee<Balance>;
    type FeeMultiplierUpdate = ConstFeeMultiplier<FeeMultiplier>;
}

// Configure collective pallet for council
parameter_types! {
    pub const CouncilMotionDuration: BlockNumber = 12 * HOURS;
    pub const CouncilMaxProposals: u32 = 10;
    pub const CouncilMaxMembers: u32 = 3;
}

// Configure collective pallet for Senate
parameter_types! {
    pub const SenateMaxMembers: u32 = 12;
}

use pallet_collective::{CanPropose, CanVote, GetVotingMembers};
pub struct CanProposeToTriumvirate;
impl CanPropose<AccountId> for CanProposeToTriumvirate {
    fn can_propose(account: &AccountId) -> bool {
        Triumvirate::is_member(account)
    }
}

pub struct CanVoteToTriumvirate;
impl CanVote<AccountId> for CanVoteToTriumvirate {
    fn can_vote(_: &AccountId) -> bool {
        //Senate::is_member(account)
        false // Disable voting from pallet_collective::vote
    }
}

use pallet_subtensor::{CollectiveInterface, MemberManagement};
pub struct ManageSenateMembers;
impl MemberManagement<AccountId> for ManageSenateMembers {
    fn add_member(account: &AccountId) -> DispatchResultWithPostInfo {
        let who = Address::Id(account.clone());
        SenateMembers::add_member(RawOrigin::Root.into(), who)
    }

    fn remove_member(account: &AccountId) -> DispatchResultWithPostInfo {
        let who = Address::Id(account.clone());
        SenateMembers::remove_member(RawOrigin::Root.into(), who)
    }

    fn swap_member(rm: &AccountId, add: &AccountId) -> DispatchResultWithPostInfo {
        let remove = Address::Id(rm.clone());
        let add = Address::Id(add.clone());

        Triumvirate::remove_votes(rm)?;
        SenateMembers::swap_member(RawOrigin::Root.into(), remove, add)
    }

    fn is_member(account: &AccountId) -> bool {
        SenateMembers::members().contains(account)
    }

    fn members() -> Vec<AccountId> {
        SenateMembers::members().into()
    }

    fn max_members() -> u32 {
        SenateMaxMembers::get()
    }
}

pub struct GetSenateMemberCount;
impl GetVotingMembers<MemberCount> for GetSenateMemberCount {
    fn get_count() -> MemberCount {
        SenateMembers::members().len() as u32
    }
}
impl Get<MemberCount> for GetSenateMemberCount {
    fn get() -> MemberCount {
        SenateMaxMembers::get()
    }
}

pub struct TriumvirateVotes;
impl CollectiveInterface<AccountId, Hash, u32> for TriumvirateVotes {
    fn remove_votes(hotkey: &AccountId) -> Result<bool, sp_runtime::DispatchError> {
        Triumvirate::remove_votes(hotkey)
    }

    fn add_vote(
        hotkey: &AccountId,
        proposal: Hash,
        index: u32,
        approve: bool,
    ) -> Result<bool, sp_runtime::DispatchError> {
        Triumvirate::do_vote(hotkey.clone(), proposal, index, approve)
    }
}

type EnsureMajoritySenate =
    pallet_collective::EnsureProportionMoreThan<AccountId, TriumvirateCollective, 1, 2>;

// We call pallet_collective TriumvirateCollective
type TriumvirateCollective = pallet_collective::Instance1;
impl pallet_collective::Config<TriumvirateCollective> for Runtime {
    type RuntimeOrigin = RuntimeOrigin;
    type Proposal = RuntimeCall;
    type RuntimeEvent = RuntimeEvent;
    type MotionDuration = CouncilMotionDuration;
    type MaxProposals = CouncilMaxProposals;
    type MaxMembers = GetSenateMemberCount;
    type DefaultVote = pallet_collective::PrimeDefaultVote;
    type WeightInfo = pallet_collective::weights::SubstrateWeight<Runtime>;
    type SetMembersOrigin = EnsureNever<AccountId>;
    type CanPropose = CanProposeToTriumvirate;
    type CanVote = CanVoteToTriumvirate;
    type GetVotingMembers = GetSenateMemberCount;
}

// We call council members Triumvirate
#[allow(dead_code)]
type TriumvirateMembership = pallet_membership::Instance1;
impl pallet_membership::Config<TriumvirateMembership> for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type AddOrigin = EnsureRoot<AccountId>;
    type RemoveOrigin = EnsureRoot<AccountId>;
    type SwapOrigin = EnsureRoot<AccountId>;
    type ResetOrigin = EnsureRoot<AccountId>;
    type PrimeOrigin = EnsureRoot<AccountId>;
    type MembershipInitialized = Triumvirate;
    type MembershipChanged = Triumvirate;
    type MaxMembers = CouncilMaxMembers;
    type WeightInfo = pallet_membership::weights::SubstrateWeight<Runtime>;
}

// We call our top K delegates membership Senate
#[allow(dead_code)]
type SenateMembership = pallet_membership::Instance2;
impl pallet_membership::Config<SenateMembership> for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type AddOrigin = EnsureRoot<AccountId>;
    type RemoveOrigin = EnsureRoot<AccountId>;
    type SwapOrigin = EnsureRoot<AccountId>;
    type ResetOrigin = EnsureRoot<AccountId>;
    type PrimeOrigin = EnsureRoot<AccountId>;
    type MembershipInitialized = ();
    type MembershipChanged = ();
    type MaxMembers = SenateMaxMembers;
    type WeightInfo = pallet_membership::weights::SubstrateWeight<Runtime>;
}

impl pallet_sudo::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type RuntimeCall = RuntimeCall;

    type WeightInfo = pallet_sudo::weights::SubstrateWeight<Runtime>;
}

parameter_types! {
    // According to multisig pallet, key and value size be computed as follows:
    // value size is `4 + sizeof((BlockNumber, Balance, AccountId))` bytes
    // key size is `32 + sizeof(AccountId)` bytes.
    // For our case, One storage item; key size is 32+32=64 bytes; value is size 4+4+8+32 bytes = 48 bytes.
    pub const DepositBase: Balance = deposit(1, 112);
    // Additional storage item size of 32 bytes.
    pub const DepositFactor: Balance = deposit(0, 32);
    pub const MaxSignatories: u32 = 100;
}

impl pallet_multisig::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type RuntimeCall = RuntimeCall;
    type Currency = Balances;
    type DepositBase = DepositBase;
    type DepositFactor = DepositFactor;
    type MaxSignatories = MaxSignatories;
    type WeightInfo = pallet_multisig::weights::SubstrateWeight<Runtime>;
}

// Proxy Pallet config
parameter_types! {
    // One storage item; key size sizeof(AccountId) = 32, value sizeof(Balance) = 8; 40 total
    pub const ProxyDepositBase: Balance = deposit(1, 40);
    // Adding 32 bytes + sizeof(ProxyType) = 32 + 1
    pub const ProxyDepositFactor: Balance = deposit(0, 33);
    pub const MaxProxies: u32 = 20; // max num proxies per acct
    pub const MaxPending: u32 = 15 * 5; // max blocks pending ~15min
    // 16 bytes
    pub const AnnouncementDepositBase: Balance =  deposit(1, 16);
    // 68 bytes per announcement
    pub const AnnouncementDepositFactor: Balance = deposit(0, 68);
}

#[derive(
    Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Debug, MaxEncodedLen, TypeInfo,
)]
pub enum ProxyType {
    Any,
    Owner, // Subnet owner Calls
    NonCritical,
    NonTransfer,
    Senate,
    NonFungibile, // Nothing involving moving TAO
    Triumvirate,
    Governance, // Both above governance
    Staking,
    Registration,
    Transfer,
    SmallTransfer,
    RootWeights,
    ChildKeys,
    SudoUncheckedSetCode,
}
// Transfers below SMALL_TRANSFER_LIMIT are considered small transfers
pub const SMALL_TRANSFER_LIMIT: Balance = 500_000_000; // 0.5 TAO
impl Default for ProxyType {
    fn default() -> Self {
        Self::Any
    }
} // allow all Calls; required to be most permissive
impl InstanceFilter<RuntimeCall> for ProxyType {
    fn filter(&self, c: &RuntimeCall) -> bool {
        match self {
            ProxyType::Any => true,
            ProxyType::NonTransfer => !matches!(c, RuntimeCall::Balances(..)),
            ProxyType::NonFungibile => !matches!(
                c,
                RuntimeCall::Balances(..)
                    | RuntimeCall::SubtensorModule(pallet_subtensor::Call::add_stake { .. })
                    | RuntimeCall::SubtensorModule(pallet_subtensor::Call::remove_stake { .. })
                    | RuntimeCall::SubtensorModule(pallet_subtensor::Call::burned_register { .. })
                    | RuntimeCall::SubtensorModule(pallet_subtensor::Call::root_register { .. })
                    | RuntimeCall::SubtensorModule(
                        pallet_subtensor::Call::schedule_swap_coldkey { .. }
                    )
                    | RuntimeCall::SubtensorModule(pallet_subtensor::Call::swap_hotkey { .. })
            ),
            ProxyType::Transfer => matches!(
                c,
                RuntimeCall::Balances(pallet_balances::Call::transfer_keep_alive { .. })
                    | RuntimeCall::Balances(pallet_balances::Call::transfer_allow_death { .. })
                    | RuntimeCall::Balances(pallet_balances::Call::transfer_all { .. })
            ),
            ProxyType::SmallTransfer => match c {
                RuntimeCall::Balances(pallet_balances::Call::transfer_keep_alive {
                    value, ..
                }) => *value < SMALL_TRANSFER_LIMIT,
                RuntimeCall::Balances(pallet_balances::Call::transfer_allow_death {
                    value,
                    ..
                }) => *value < SMALL_TRANSFER_LIMIT,
                _ => false,
            },
            ProxyType::Owner => matches!(c, RuntimeCall::AdminUtils(..)),
            ProxyType::NonCritical => !matches!(
                c,
                RuntimeCall::SubtensorModule(pallet_subtensor::Call::dissolve_network { .. })
                    | RuntimeCall::SubtensorModule(pallet_subtensor::Call::root_register { .. })
                    | RuntimeCall::SubtensorModule(pallet_subtensor::Call::burned_register { .. })
                    | RuntimeCall::Triumvirate(..)
                    | RuntimeCall::SubtensorModule(pallet_subtensor::Call::set_root_weights { .. })
                    | RuntimeCall::Sudo(..)
            ),
            ProxyType::Triumvirate => matches!(
                c,
                RuntimeCall::Triumvirate(..) | RuntimeCall::TriumvirateMembers(..)
            ),
            ProxyType::Senate => matches!(c, RuntimeCall::SenateMembers(..)),
            ProxyType::Governance => matches!(
                c,
                RuntimeCall::SenateMembers(..)
                    | RuntimeCall::Triumvirate(..)
                    | RuntimeCall::TriumvirateMembers(..)
            ),
            ProxyType::Staking => matches!(
                c,
                RuntimeCall::SubtensorModule(pallet_subtensor::Call::add_stake { .. })
                    | RuntimeCall::SubtensorModule(pallet_subtensor::Call::remove_stake { .. })
            ),
            ProxyType::Registration => matches!(
                c,
                RuntimeCall::SubtensorModule(pallet_subtensor::Call::burned_register { .. })
                    | RuntimeCall::SubtensorModule(pallet_subtensor::Call::register { .. })
            ),
            ProxyType::RootWeights => matches!(
                c,
                RuntimeCall::SubtensorModule(pallet_subtensor::Call::set_root_weights { .. })
            ),
            ProxyType::ChildKeys => matches!(
                c,
                RuntimeCall::SubtensorModule(pallet_subtensor::Call::set_children { .. })
                    | RuntimeCall::SubtensorModule(
                        pallet_subtensor::Call::set_childkey_take { .. }
                    )
            ),
            ProxyType::SudoUncheckedSetCode => match c {
                RuntimeCall::Sudo(pallet_sudo::Call::sudo_unchecked_weight { call, weight: _ }) => {
                    let inner_call: RuntimeCall = *call.clone();

                    matches!(
                        inner_call,
                        RuntimeCall::System(frame_system::Call::set_code { .. })
                    )
                }
                _ => false,
            },
        }
    }
    fn is_superset(&self, o: &Self) -> bool {
        match (self, o) {
            (x, y) if x == y => true,
            (ProxyType::Any, _) => true,
            (_, ProxyType::Any) => false,
            (ProxyType::NonTransfer, _) => {
                // NonTransfer is NOT a superset of Transfer or SmallTransfer
                !matches!(o, ProxyType::Transfer | ProxyType::SmallTransfer)
            }
            (ProxyType::Governance, ProxyType::Triumvirate | ProxyType::Senate) => true,
            (ProxyType::Transfer, ProxyType::SmallTransfer) => true,
            _ => false,
        }
    }
}

impl pallet_proxy::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type RuntimeCall = RuntimeCall;
    type Currency = Balances;
    type ProxyType = ProxyType;
    type ProxyDepositBase = ProxyDepositBase;
    type ProxyDepositFactor = ProxyDepositFactor;
    type MaxProxies = MaxProxies;
    type WeightInfo = pallet_proxy::weights::SubstrateWeight<Runtime>;
    type MaxPending = MaxPending;
    type CallHasher = BlakeTwo256;
    type AnnouncementDepositBase = AnnouncementDepositBase;
    type AnnouncementDepositFactor = AnnouncementDepositFactor;
}

parameter_types! {
    pub MaximumSchedulerWeight: Weight = Perbill::from_percent(80) *
        BlockWeights::get().max_block;
    pub const MaxScheduledPerBlock: u32 = 50;
    pub const NoPreimagePostponement: Option<u32> = Some(10);
}

/// Used the compare the privilege of an origin inside the scheduler.
pub struct OriginPrivilegeCmp;

impl PrivilegeCmp<OriginCaller> for OriginPrivilegeCmp {
    fn cmp_privilege(left: &OriginCaller, right: &OriginCaller) -> Option<Ordering> {
        if left == right {
            return Some(Ordering::Equal);
        }

        match (left, right) {
            // Root is greater than anything.
            (OriginCaller::system(frame_system::RawOrigin::Root), _) => Some(Ordering::Greater),
            // Check which one has more yes votes.
            (
                OriginCaller::Triumvirate(pallet_collective::RawOrigin::Members(
                    l_yes_votes,
                    l_count,
                )),
                OriginCaller::Triumvirate(pallet_collective::RawOrigin::Members(
                    r_yes_votes,
                    r_count,
                )), // Equivalent to (l_yes_votes / l_count).cmp(&(r_yes_votes / r_count))
            ) => Some(
                l_yes_votes
                    .saturating_mul(*r_count)
                    .cmp(&r_yes_votes.saturating_mul(*l_count)),
            ),
            // For every other origin we don't care, as they are not used for `ScheduleOrigin`.
            _ => None,
        }
    }
}

impl pallet_scheduler::Config for Runtime {
    type RuntimeOrigin = RuntimeOrigin;
    type RuntimeEvent = RuntimeEvent;
    type PalletsOrigin = OriginCaller;
    type RuntimeCall = RuntimeCall;
    type MaximumWeight = MaximumSchedulerWeight;
    type ScheduleOrigin = EnsureRoot<AccountId>;
    type MaxScheduledPerBlock = MaxScheduledPerBlock;
    type WeightInfo = pallet_scheduler::weights::SubstrateWeight<Runtime>;
    type OriginPrivilegeCmp = OriginPrivilegeCmp;
    type Preimages = Preimage;
}

parameter_types! {
    pub const PreimageMaxSize: u32 = 4096 * 1024;
    pub const PreimageBaseDeposit: Balance = deposit(2, 64);
    pub const PreimageByteDeposit: Balance = deposit(0, 1);
    pub const PreimageHoldReason: RuntimeHoldReason =
        RuntimeHoldReason::Preimage(pallet_preimage::HoldReason::Preimage);
}

impl pallet_preimage::Config for Runtime {
    type WeightInfo = pallet_preimage::weights::SubstrateWeight<Runtime>;
    type RuntimeEvent = RuntimeEvent;
    type Currency = Balances;
    type ManagerOrigin = EnsureRoot<AccountId>;
    type Consideration = HoldConsideration<
        AccountId,
        Balances,
        PreimageHoldReason,
        LinearStoragePrice<PreimageBaseDeposit, PreimageByteDeposit, Balance>,
    >;
}

pub struct AllowIdentityReg;

impl CanRegisterIdentity<AccountId> for AllowIdentityReg {
    #[cfg(not(feature = "runtime-benchmarks"))]
    fn can_register(address: &AccountId, identified: &AccountId) -> bool {
        if address != identified {
            SubtensorModule::coldkey_owns_hotkey(address, identified)
                && SubtensorModule::is_hotkey_registered_on_network(0, identified)
        } else {
            SubtensorModule::is_subnet_owner(address)
        }
    }

    #[cfg(feature = "runtime-benchmarks")]
    fn can_register(_: &AccountId, _: &AccountId) -> bool {
        true
    }
}

// Configure registry pallet.
parameter_types! {
    pub const MaxAdditionalFields: u32 = 1;
    pub const InitialDeposit: Balance = 100_000_000; // 0.1 TAO
    pub const FieldDeposit: Balance = 100_000_000; // 0.1 TAO
}

impl pallet_registry::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type RuntimeHoldReason = RuntimeHoldReason;
    type Currency = Balances;
    type CanRegister = AllowIdentityReg;
    type WeightInfo = pallet_registry::weights::SubstrateWeight<Runtime>;

    type MaxAdditionalFields = MaxAdditionalFields;
    type InitialDeposit = InitialDeposit;
    type FieldDeposit = FieldDeposit;
}

parameter_types! {
    pub const MaxCommitFields: u32 = 1;
    pub const CommitmentInitialDeposit: Balance = 0; // Free
    pub const CommitmentFieldDeposit: Balance = 0; // Free
    pub const CommitmentRateLimit: BlockNumber = 100; // Allow commitment every 100 blocks
}

pub struct AllowCommitments;
impl CanCommit<AccountId> for AllowCommitments {
    #[cfg(not(feature = "runtime-benchmarks"))]
    fn can_commit(netuid: u16, address: &AccountId) -> bool {
        SubtensorModule::is_hotkey_registered_on_network(netuid, address)
    }

    #[cfg(feature = "runtime-benchmarks")]
    fn can_commit(_: u16, _: &AccountId) -> bool {
        true
    }
}

impl pallet_commitments::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type Currency = Balances;
    type WeightInfo = pallet_commitments::weights::SubstrateWeight<Runtime>;

    type CanCommit = AllowCommitments;

    type MaxFields = MaxCommitFields;
    type InitialDeposit = CommitmentInitialDeposit;
    type FieldDeposit = CommitmentFieldDeposit;
    type RateLimit = CommitmentRateLimit;
}

pub const INITIAL_SUBNET_TEMPO: u16 = prod_or_fast!(99, 10);

pub const INITIAL_CHILDKEY_TAKE_RATELIMIT: u64 = prod_or_fast!(216000, 5); // 30 days at 12 seconds per block

// Configure the pallet subtensor.
parameter_types! {
    pub const SubtensorInitialRho: u16 = 10;
    pub const SubtensorInitialKappa: u16 = 32_767; // 0.5 = 65535/2
    pub const SubtensorInitialMaxAllowedUids: u16 = 4096;
    pub const SubtensorInitialIssuance: u64 = 0;
    pub const SubtensorInitialMinAllowedWeights: u16 = 1024;
    pub const SubtensorInitialEmissionValue: u16 = 0;
    pub const SubtensorInitialMaxWeightsLimit: u16 = 1000; // 1000/2^16 = 0.015
    pub const SubtensorInitialValidatorPruneLen: u64 = 1;
    pub const SubtensorInitialScalingLawPower: u16 = 50; // 0.5
    pub const SubtensorInitialMaxAllowedValidators: u16 = 128;
    pub const SubtensorInitialTempo: u16 = INITIAL_SUBNET_TEMPO;
    pub const SubtensorInitialDifficulty: u64 = 10_000_000;
    pub const SubtensorInitialAdjustmentInterval: u16 = 100;
    pub const SubtensorInitialAdjustmentAlpha: u64 = 0; // no weight to previous value.
    pub const SubtensorInitialTargetRegistrationsPerInterval: u16 = 2;
    pub const SubtensorInitialImmunityPeriod: u16 = 4096;
    pub const SubtensorInitialActivityCutoff: u16 = 5000;
    pub const SubtensorInitialMaxRegistrationsPerBlock: u16 = 1;
    pub const SubtensorInitialPruningScore : u16 = u16::MAX;
    pub const SubtensorInitialBondsMovingAverage: u64 = 900_000;
    pub const SubtensorInitialDefaultTake: u16 = 11_796; // 18% honest number.
    pub const SubtensorInitialMinDelegateTake: u16 = 0; // Allow 0% delegate take
    pub const SubtensorInitialDefaultChildKeyTake: u16 = 0; // Allow 0% childkey take
    pub const SubtensorInitialMinChildKeyTake: u16 = 0; // 0 %
    pub const SubtensorInitialMaxChildKeyTake: u16 = 11_796; // 18 %
    pub const SubtensorInitialWeightsVersionKey: u64 = 0;
    pub const SubtensorInitialMinDifficulty: u64 = 10_000_000;
    pub const SubtensorInitialMaxDifficulty: u64 = u64::MAX / 4;
    pub const SubtensorInitialServingRateLimit: u64 = 50;
    pub const SubtensorInitialBurn: u64 = 1_000_000_000; // 1 tao
    pub const SubtensorInitialMinBurn: u64 = 1_000_000_000; // 1 tao
    pub const SubtensorInitialMaxBurn: u64 = 100_000_000_000; // 100 tao
    pub const SubtensorInitialTxRateLimit: u64 = 1000;
    pub const SubtensorInitialTxDelegateTakeRateLimit: u64 = 216000; // 30 days at 12 seconds per block
    pub const SubtensorInitialTxChildKeyTakeRateLimit: u64 = INITIAL_CHILDKEY_TAKE_RATELIMIT;
    pub const SubtensorInitialRAORecycledForRegistration: u64 = 0; // 0 rao
    pub const SubtensorInitialSenateRequiredStakePercentage: u64 = 1; // 1 percent of total stake
    pub const SubtensorInitialNetworkImmunity: u64 = 7 * 7200;
    pub const SubtensorInitialMinAllowedUids: u16 = 128;
    pub const SubtensorInitialMinLockCost: u64 = 1_000_000_000_000; // 1000 TAO
    pub const SubtensorInitialSubnetOwnerCut: u16 = 11_796; // 18 percent
    pub const SubtensorInitialSubnetLimit: u16 = 12;
    pub const SubtensorInitialNetworkLockReductionInterval: u64 = 14 * 7200;
    pub const SubtensorInitialNetworkRateLimit: u64 = 7200;
    pub const SubtensorInitialTargetStakesPerInterval: u16 = 1;
    pub const SubtensorInitialKeySwapCost: u64 = 100_000_000; // 0.1 TAO
    pub const InitialAlphaHigh: u16 = 58982; // Represents 0.9 as per the production default
    pub const InitialAlphaLow: u16 = 45875; // Represents 0.7 as per the production default
    pub const InitialLiquidAlphaOn: bool = false; // Default value for LiquidAlphaOn
    pub const SubtensorInitialHotkeyEmissionTempo: u64 = 7200; // Drain every day.
    pub const SubtensorInitialNetworkMaxStake: u64 = u64::MAX; // Maximum possible value for u64, this make the make stake infinity
    pub const  InitialColdkeySwapScheduleDuration: BlockNumber = 5 * 24 * 60 * 60 / 12; // 5 days
    pub const  InitialDissolveNetworkScheduleDuration: BlockNumber = 5 * 24 * 60 * 60 / 12; // 5 days

}

impl pallet_subtensor::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type RuntimeCall = RuntimeCall;
    type SudoRuntimeCall = RuntimeCall;
    type Currency = Balances;
    type CouncilOrigin = EnsureMajoritySenate;
    type SenateMembers = ManageSenateMembers;
    type TriumvirateInterface = TriumvirateVotes;
    type Scheduler = Scheduler;
    type InitialRho = SubtensorInitialRho;
    type InitialKappa = SubtensorInitialKappa;
    type InitialMaxAllowedUids = SubtensorInitialMaxAllowedUids;
    type InitialBondsMovingAverage = SubtensorInitialBondsMovingAverage;
    type InitialIssuance = SubtensorInitialIssuance;
    type InitialMinAllowedWeights = SubtensorInitialMinAllowedWeights;
    type InitialEmissionValue = SubtensorInitialEmissionValue;
    type InitialMaxWeightsLimit = SubtensorInitialMaxWeightsLimit;
    type InitialValidatorPruneLen = SubtensorInitialValidatorPruneLen;
    type InitialScalingLawPower = SubtensorInitialScalingLawPower;
    type InitialTempo = SubtensorInitialTempo;
    type InitialDifficulty = SubtensorInitialDifficulty;
    type InitialAdjustmentInterval = SubtensorInitialAdjustmentInterval;
    type InitialAdjustmentAlpha = SubtensorInitialAdjustmentAlpha;
    type InitialTargetRegistrationsPerInterval = SubtensorInitialTargetRegistrationsPerInterval;
    type InitialImmunityPeriod = SubtensorInitialImmunityPeriod;
    type InitialActivityCutoff = SubtensorInitialActivityCutoff;
    type InitialMaxRegistrationsPerBlock = SubtensorInitialMaxRegistrationsPerBlock;
    type InitialPruningScore = SubtensorInitialPruningScore;
    type InitialMaxAllowedValidators = SubtensorInitialMaxAllowedValidators;
    type InitialDefaultDelegateTake = SubtensorInitialDefaultTake;
    type InitialDefaultChildKeyTake = SubtensorInitialDefaultChildKeyTake;
    type InitialMinDelegateTake = SubtensorInitialMinDelegateTake;
    type InitialMinChildKeyTake = SubtensorInitialMinChildKeyTake;
    type InitialWeightsVersionKey = SubtensorInitialWeightsVersionKey;
    type InitialMaxDifficulty = SubtensorInitialMaxDifficulty;
    type InitialMinDifficulty = SubtensorInitialMinDifficulty;
    type InitialServingRateLimit = SubtensorInitialServingRateLimit;
    type InitialBurn = SubtensorInitialBurn;
    type InitialMaxBurn = SubtensorInitialMaxBurn;
    type InitialMinBurn = SubtensorInitialMinBurn;
    type InitialTxRateLimit = SubtensorInitialTxRateLimit;
    type InitialTxDelegateTakeRateLimit = SubtensorInitialTxDelegateTakeRateLimit;
    type InitialTxChildKeyTakeRateLimit = SubtensorInitialTxChildKeyTakeRateLimit;
    type InitialMaxChildKeyTake = SubtensorInitialMaxChildKeyTake;
    type InitialRAORecycledForRegistration = SubtensorInitialRAORecycledForRegistration;
    type InitialSenateRequiredStakePercentage = SubtensorInitialSenateRequiredStakePercentage;
    type InitialNetworkImmunityPeriod = SubtensorInitialNetworkImmunity;
    type InitialNetworkMinAllowedUids = SubtensorInitialMinAllowedUids;
    type InitialNetworkMinLockCost = SubtensorInitialMinLockCost;
    type InitialNetworkLockReductionInterval = SubtensorInitialNetworkLockReductionInterval;
    type InitialSubnetOwnerCut = SubtensorInitialSubnetOwnerCut;
    type InitialSubnetLimit = SubtensorInitialSubnetLimit;
    type InitialNetworkRateLimit = SubtensorInitialNetworkRateLimit;
    type InitialTargetStakesPerInterval = SubtensorInitialTargetStakesPerInterval;
    type KeySwapCost = SubtensorInitialKeySwapCost;
    type AlphaHigh = InitialAlphaHigh;
    type AlphaLow = InitialAlphaLow;
    type LiquidAlphaOn = InitialLiquidAlphaOn;
    type InitialHotkeyEmissionTempo = SubtensorInitialHotkeyEmissionTempo;
    type InitialNetworkMaxStake = SubtensorInitialNetworkMaxStake;
    type Preimages = Preimage;
    type InitialColdkeySwapScheduleDuration = InitialColdkeySwapScheduleDuration;
    type InitialDissolveNetworkScheduleDuration = InitialDissolveNetworkScheduleDuration;
}

use sp_runtime::BoundedVec;

pub struct GrandpaInterfaceImpl;
impl pallet_admin_utils::GrandpaInterface<Runtime> for GrandpaInterfaceImpl {
    fn schedule_change(
        next_authorities: Vec<(pallet_grandpa::AuthorityId, u64)>,
        in_blocks: BlockNumber,
        forced: Option<BlockNumber>,
    ) -> sp_runtime::DispatchResult {
        Grandpa::schedule_change(next_authorities, in_blocks, forced)
    }
}

impl pallet_admin_utils::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type Grandpa = GrandpaInterfaceImpl;
    type Balance = Balance;
    type WeightInfo = pallet_admin_utils::weights::SubstrateWeight<Runtime>;
}

/// Define the ChainId
/// EVM Chain ID will be set by sudo transaction for each chain
///     Mainnet Finney: 0x03C4 - Unicode for lowercase tau
///     TestNet Finney: 0x03B1 - Unicode for lowercase alpha
impl pallet_evm_chain_id::Config for Runtime {}

pub struct ConfigurableChainId;

impl Get<u64> for ConfigurableChainId {
    fn get() -> u64 {
        pallet_evm_chain_id::ChainId::<Runtime>::get()
    }
}

pub struct FindAuthorTruncated<F>(PhantomData<F>);
impl<F: FindAuthor<u32>> FindAuthor<H160> for FindAuthorTruncated<F> {
    fn find_author<'a, I>(digests: I) -> Option<H160>
    where
        I: 'a + IntoIterator<Item = (ConsensusEngineId, &'a [u8])>,
    {
        if let Some(author_index) = F::find_author(digests) {
            pallet_babe::Authorities::<Runtime>::get()
                .get(author_index as usize)
                .and_then(|authority_id| {
                    let raw_vec = authority_id.0.to_raw_vec();
                    raw_vec.get(4..24).map(H160::from_slice)
                })
        } else {
            None
        }
    }
}

const BLOCK_GAS_LIMIT: u64 = 75_000_000;

/// `WeightPerGas` is an approximate ratio of the amount of Weight per Gas.
///
fn weight_per_gas() -> Weight {
    (NORMAL_DISPATCH_RATIO * MAXIMUM_BLOCK_WEIGHT).saturating_div(BLOCK_GAS_LIMIT)
}

parameter_types! {
    pub BlockGasLimit: U256 = U256::from(BLOCK_GAS_LIMIT);
    pub const GasLimitPovSizeRatio: u64 = 0;
    pub PrecompilesValue: FrontierPrecompiles<Runtime> = FrontierPrecompiles::<_>::new();
    pub WeightPerGas: Weight = weight_per_gas();
    pub SuicideQuickClearLimit: u32 = 0;
}

/// The difference between EVM decimals and Substrate decimals.
/// Substrate balances has 9 decimals, while EVM has 18, so the
/// difference factor is 9 decimals, or 10^9
const EVM_DECIMALS_FACTOR: u64 = 1_000_000_000_u64;

pub struct SubtensorEvmBalanceConverter;

impl BalanceConverter for SubtensorEvmBalanceConverter {
    /// Convert from Substrate balance (u64) to EVM balance (U256)
    fn into_evm_balance(value: U256) -> Option<U256> {
        value
            .checked_mul(U256::from(EVM_DECIMALS_FACTOR))
            .and_then(|evm_value| {
                // Ensure the result fits within the maximum U256 value
                if evm_value <= U256::MAX {
                    Some(evm_value)
                } else {
                    None
                }
            })
    }

    /// Convert from EVM balance (U256) to Substrate balance (u64)
    fn into_substrate_balance(value: U256) -> Option<U256> {
        value
            .checked_div(U256::from(EVM_DECIMALS_FACTOR))
            .and_then(|substrate_value| {
                // Ensure the result fits within the TAO balance type (u64)
                if substrate_value <= U256::from(u64::MAX) {
                    Some(substrate_value)
                } else {
                    None
                }
            })
    }
}

impl pallet_evm::Config for Runtime {
    type FeeCalculator = BaseFee;
    type GasWeightMapping = pallet_evm::FixedGasWeightMapping<Self>;
    type WeightPerGas = WeightPerGas;
    type BlockHashMapping = pallet_ethereum::EthereumBlockHashMapping<Self>;
    type CallOrigin = pallet_evm::EnsureAddressTruncated;
    type WithdrawOrigin = pallet_evm::EnsureAddressTruncated;
    type AddressMapping = pallet_evm::HashedAddressMapping<BlakeTwo256>;
    type Currency = Balances;
    type RuntimeEvent = RuntimeEvent;
    type PrecompilesType = FrontierPrecompiles<Self>;
    type PrecompilesValue = PrecompilesValue;
    type ChainId = ConfigurableChainId;
    type BlockGasLimit = BlockGasLimit;
    type Runner = pallet_evm::runner::stack::Runner<Self>;
    type OnChargeTransaction = ();
    type OnCreate = ();
    type FindAuthor = FindAuthorTruncated<Babe>;
    type GasLimitPovSizeRatio = GasLimitPovSizeRatio;
    type SuicideQuickClearLimit = SuicideQuickClearLimit;
    type Timestamp = Timestamp;
    type WeightInfo = pallet_evm::weights::SubstrateWeight<Self>;
    type BalanceConverter = SubtensorEvmBalanceConverter;
}

parameter_types! {
    pub const PostBlockAndTxnHashes: PostLogContent = PostLogContent::BlockAndTxnHashes;
}

impl pallet_ethereum::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type StateRoot = pallet_ethereum::IntermediateStateRoot<Self>;
    type PostLogContent = PostBlockAndTxnHashes;
    type ExtraDataLength = ConstU32<30>;
}

parameter_types! {
    pub BoundDivision: U256 = U256::from(1024);
}

impl pallet_dynamic_fee::Config for Runtime {
    type MinGasPriceBoundDivisor = BoundDivision;
}

parameter_types! {
    pub DefaultBaseFeePerGas: U256 = U256::from(20_000_000_000_u128);
    pub DefaultElasticity: Permill = Permill::from_parts(125_000);
}
pub struct BaseFeeThreshold;
impl pallet_base_fee::BaseFeeThreshold for BaseFeeThreshold {
    fn lower() -> Permill {
        Permill::zero()
    }
    fn ideal() -> Permill {
        Permill::from_parts(500_000)
    }
    fn upper() -> Permill {
        Permill::from_parts(1_000_000)
    }
}
impl pallet_base_fee::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type Threshold = BaseFeeThreshold;
    type DefaultBaseFeePerGas = DefaultBaseFeePerGas;
    type DefaultElasticity = DefaultElasticity;
}

#[derive(Clone)]
pub struct TransactionConverter<B>(PhantomData<B>);

impl<B> Default for TransactionConverter<B> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<B: BlockT> fp_rpc::ConvertTransaction<<B as BlockT>::Extrinsic> for TransactionConverter<B> {
    fn convert_transaction(
        &self,
        transaction: pallet_ethereum::Transaction,
    ) -> <B as BlockT>::Extrinsic {
        let extrinsic = UncheckedExtrinsic::new_unsigned(
            pallet_ethereum::Call::<Runtime>::transact { transaction }.into(),
        );
        let encoded = extrinsic.encode();
        <B as BlockT>::Extrinsic::decode(&mut &encoded[..])
            .expect("Encoded extrinsic is always valid")
    }
}

impl fp_self_contained::SelfContainedCall for RuntimeCall {
    type SignedInfo = H160;

    fn is_self_contained(&self) -> bool {
        match self {
            RuntimeCall::Ethereum(call) => call.is_self_contained(),
            _ => false,
        }
    }

    fn check_self_contained(&self) -> Option<Result<Self::SignedInfo, TransactionValidityError>> {
        match self {
            RuntimeCall::Ethereum(call) => call.check_self_contained(),
            _ => None,
        }
    }

    fn validate_self_contained(
        &self,
        info: &Self::SignedInfo,
        dispatch_info: &DispatchInfoOf<RuntimeCall>,
        len: usize,
    ) -> Option<TransactionValidity> {
        match self {
            RuntimeCall::Ethereum(call) => call.validate_self_contained(info, dispatch_info, len),
            _ => None,
        }
    }

    fn pre_dispatch_self_contained(
        &self,
        info: &Self::SignedInfo,
        dispatch_info: &DispatchInfoOf<RuntimeCall>,
        len: usize,
    ) -> Option<Result<(), TransactionValidityError>> {
        match self {
            RuntimeCall::Ethereum(call) => {
                call.pre_dispatch_self_contained(info, dispatch_info, len)
            }
            _ => None,
        }
    }

    fn apply_self_contained(
        self,
        info: Self::SignedInfo,
    ) -> Option<sp_runtime::DispatchResultWithInfo<PostDispatchInfoOf<Self>>> {
        match self {
            call @ RuntimeCall::Ethereum(pallet_ethereum::Call::transact { .. }) => {
                Some(call.dispatch(RuntimeOrigin::from(
                    pallet_ethereum::RawOrigin::EthereumTransaction(info),
                )))
            }
            _ => None,
        }
    }
}

// Create the runtime by composing the FRAME pallets that were previously configured.
construct_runtime!(
    pub struct Runtime
    {
        System: frame_system = 0,
        RandomnessCollectiveFlip: pallet_insecure_randomness_collective_flip = 1,
        Timestamp: pallet_timestamp = 2,
        // Aura: pallet_aura = 3,
        Grandpa: pallet_grandpa = 4,
        Balances: pallet_balances = 5,
        TransactionPayment: pallet_transaction_payment = 6,
        SubtensorModule: pallet_subtensor = 7,
        Triumvirate: pallet_collective::<Instance1>::{Pallet, Call, Storage, Origin<T>, Event<T>, Config<T>} = 8,
        TriumvirateMembers: pallet_membership::<Instance1>::{Pallet, Call, Storage, Event<T>, Config<T>} = 9,
        SenateMembers: pallet_membership::<Instance2>::{Pallet, Call, Storage, Event<T>, Config<T>} = 10,
        Utility: pallet_utility = 11,
        Sudo: pallet_sudo = 12,
        Multisig: pallet_multisig = 13,
        Preimage: pallet_preimage = 14,
        Scheduler: pallet_scheduler = 15,
        Proxy: pallet_proxy = 16,
        Registry: pallet_registry = 17,
        Commitments: pallet_commitments = 18,
        AdminUtils: pallet_admin_utils = 19,
        SafeMode: pallet_safe_mode = 20,

        // Frontier
        Ethereum: pallet_ethereum = 21,
        EVM: pallet_evm = 22,
        EVMChainId: pallet_evm_chain_id = 23,
        DynamicFee: pallet_dynamic_fee = 24,
        BaseFee: pallet_base_fee = 25,

        Drand: pallet_drand = 26,

        // PoS Consensus.
        // Authorship must be before session in order to note author in the correct session and era
        // for staking.
        Authorship: pallet_authorship = 30,
        Staking: pallet_staking = 31,
        Offences: pallet_offences = 32,
        Historical: session_historical = 33,
        Session: pallet_session = 34,
        AuthorityDiscovery: pallet_authority_discovery = 35,
        Babe: pallet_babe = 36,
        ElectionProviderMultiPhase: pallet_election_provider_multi_phase = 37,
        VoterList: pallet_bags_list::<Instance1> = 38,
        FastUnstake: pallet_fast_unstake = 39,
        // TODO: Evaluate if we need nomination pools...
        // NominationPools: pallet_nomination_pools = 40,
    }
);

// The address format for describing accounts.
pub type Address = sp_runtime::MultiAddress<AccountId, ()>;
// Block header type as expected by this runtime.
pub type Header = generic::Header<BlockNumber, BlakeTwo256>;
// Block type as expected by this runtime.
pub type Block = generic::Block<Header, UncheckedExtrinsic>;
// The SignedExtension to the basic transaction logic.
pub type SignedExtra = (
    frame_system::CheckNonZeroSender<Runtime>,
    frame_system::CheckSpecVersion<Runtime>,
    frame_system::CheckTxVersion<Runtime>,
    frame_system::CheckGenesis<Runtime>,
    frame_system::CheckEra<Runtime>,
    check_nonce::CheckNonce<Runtime>,
    frame_system::CheckWeight<Runtime>,
    pallet_transaction_payment::ChargeTransactionPayment<Runtime>,
    pallet_subtensor::SubtensorSignedExtension<Runtime>,
    pallet_commitments::CommitmentsSignedExtension<Runtime>,
    frame_metadata_hash_extension::CheckMetadataHash<Runtime>,
);

type Migrations = (
    // Leave this migration in the runtime, so every runtime upgrade tiny rounding errors (fractions of fractions
    // of a cent) are cleaned up. These tiny rounding errors occur due to floating point coversion.
    pallet_subtensor::migrations::migrate_init_total_issuance::initialise_total_issuance::Migration<
        Runtime,
    >,
);

// Unchecked extrinsic type as expected by this runtime.
pub type UncheckedExtrinsic =
    fp_self_contained::UncheckedExtrinsic<Address, RuntimeCall, Signature, SignedExtra>;

/// Extrinsic type that has already been checked.
pub type CheckedExtrinsic =
    fp_self_contained::CheckedExtrinsic<AccountId, RuntimeCall, SignedExtra, H160>;

// The payload being signed in transactions.
pub type SignedPayload = generic::SignedPayload<RuntimeCall, SignedExtra>;
// Executive: handles dispatch to the various modules.
pub type Executive = frame_executive::Executive<
    Runtime,
    Block,
    frame_system::ChainContext<Runtime>,
    Runtime,
    AllPalletsWithSystem,
    Migrations,
>;

#[cfg(feature = "runtime-benchmarks")]
#[macro_use]
extern crate frame_benchmarking;

#[cfg(feature = "runtime-benchmarks")]
mod benches {
    define_benchmarks!(
        [frame_benchmarking, BaselineBench::<Runtime>]
        [frame_system, SystemBench::<Runtime>]
        [pallet_balances, Balances]
        [pallet_timestamp, Timestamp]
        [pallet_sudo, Sudo]
        [pallet_registry, Registry]
        [pallet_commitments, Commitments]
        [pallet_admin_utils, AdminUtils]
        [pallet_subtensor, SubtensorModule]
        [pallet_drand, Drand]
    );
}

impl_runtime_apis! {
    impl sp_api::Core<Block> for Runtime {
        fn version() -> RuntimeVersion {
            VERSION
        }

        fn execute_block(block: Block) {
            Executive::execute_block(block);
        }

        fn initialize_block(header: &<Block as BlockT>::Header) -> sp_runtime::ExtrinsicInclusionMode {
            Executive::initialize_block(header)
        }
    }

    impl sp_api::Metadata<Block> for Runtime {
        fn metadata() -> OpaqueMetadata {
            OpaqueMetadata::new(Runtime::metadata().into())
        }

        fn metadata_at_version(version: u32) -> Option<OpaqueMetadata> {
            Runtime::metadata_at_version(version)
        }

        fn metadata_versions() -> sp_std::vec::Vec<u32> {
            Runtime::metadata_versions()
        }
    }

    impl sp_block_builder::BlockBuilder<Block> for Runtime {
        fn apply_extrinsic(extrinsic: <Block as BlockT>::Extrinsic) -> ApplyExtrinsicResult {
            Executive::apply_extrinsic(extrinsic)
        }

        fn finalize_block() -> <Block as BlockT>::Header {
            Executive::finalize_block()
        }

        fn inherent_extrinsics(data: sp_inherents::InherentData) -> Vec<<Block as BlockT>::Extrinsic> {
            data.create_extrinsics()
        }

        fn check_inherents(
            block: Block,
            data: sp_inherents::InherentData,
        ) -> sp_inherents::CheckInherentsResult {
            data.check_extrinsics(&block)
        }
    }

    impl sp_genesis_builder::GenesisBuilder<Block> for Runtime {
        fn build_state(config: Vec<u8>) -> sp_genesis_builder::Result {
            build_state::<RuntimeGenesisConfig>(config)
        }

        fn get_preset(id: &Option<sp_genesis_builder::PresetId>) -> Option<Vec<u8>> {
            get_preset::<RuntimeGenesisConfig>(id, |_| None)
        }

        fn preset_names() -> Vec<sp_genesis_builder::PresetId> {
            vec![]
        }
    }

    impl sp_transaction_pool::runtime_api::TaggedTransactionQueue<Block> for Runtime {
        fn validate_transaction(
            source: TransactionSource,
            tx: <Block as BlockT>::Extrinsic,
            block_hash: <Block as BlockT>::Hash,
        ) -> TransactionValidity {
            Executive::validate_transaction(source, tx, block_hash)
        }
    }

    impl sp_offchain::OffchainWorkerApi<Block> for Runtime {
        fn offchain_worker(header: &<Block as BlockT>::Header) {
            Executive::offchain_worker(header)
        }
    }

    impl sp_session::SessionKeys<Block> for Runtime {
        fn generate_session_keys(seed: Option<Vec<u8>>) -> Vec<u8> {
            opaque::SessionKeys::generate(seed)
        }

        fn decode_session_keys(
            encoded: Vec<u8>,
        ) -> Option<Vec<(Vec<u8>, KeyTypeId)>> {
            opaque::SessionKeys::decode_into_raw_public_keys(&encoded)
        }
    }

    impl fg_primitives::GrandpaApi<Block> for Runtime {
        fn grandpa_authorities() -> GrandpaAuthorityList {
            Grandpa::grandpa_authorities()
        }

        fn current_set_id() -> fg_primitives::SetId {
            Grandpa::current_set_id()
        }

        fn submit_report_equivocation_unsigned_extrinsic(
            _equivocation_proof: fg_primitives::EquivocationProof<
                <Block as BlockT>::Hash,
                NumberFor<Block>,
            >,
            _key_owner_proof: fg_primitives::OpaqueKeyOwnershipProof,
        ) -> Option<()> {
            None
        }

        fn generate_key_ownership_proof(
            _set_id: fg_primitives::SetId,
            _authority_id: GrandpaId,
        ) -> Option<fg_primitives::OpaqueKeyOwnershipProof> {
            // NOTE: this is the only implementation possible since we've
            // defined our key owner proof type as a bottom type (i.e. a type
            // with no values).
            None
        }
    }

    impl frame_system_rpc_runtime_api::AccountNonceApi<Block, AccountId, Index> for Runtime {
        fn account_nonce(account: AccountId) -> Index {
            System::account_nonce(account)
        }
    }

    impl pallet_transaction_payment_rpc_runtime_api::TransactionPaymentApi<Block, Balance> for Runtime {
        fn query_info(
            uxt: <Block as BlockT>::Extrinsic,
            len: u32,
        ) -> pallet_transaction_payment_rpc_runtime_api::RuntimeDispatchInfo<Balance> {
            TransactionPayment::query_info(uxt, len)
        }
        fn query_fee_details(
            uxt: <Block as BlockT>::Extrinsic,
            len: u32,
        ) -> pallet_transaction_payment::FeeDetails<Balance> {
            TransactionPayment::query_fee_details(uxt, len)
        }
        fn query_weight_to_fee(weight: Weight) -> Balance {
            TransactionPayment::weight_to_fee(weight)
        }
        fn query_length_to_fee(length: u32) -> Balance {
            TransactionPayment::length_to_fee(length)
        }
    }

    impl pallet_transaction_payment_rpc_runtime_api::TransactionPaymentCallApi<Block, Balance, RuntimeCall>
        for Runtime
    {
        fn query_call_info(
            call: RuntimeCall,
            len: u32,
        ) -> pallet_transaction_payment::RuntimeDispatchInfo<Balance> {
            TransactionPayment::query_call_info(call, len)
        }
        fn query_call_fee_details(
            call: RuntimeCall,
            len: u32,
        ) -> pallet_transaction_payment::FeeDetails<Balance> {
            TransactionPayment::query_call_fee_details(call, len)
        }
        fn query_weight_to_fee(weight: Weight) -> Balance {
            TransactionPayment::weight_to_fee(weight)
        }
        fn query_length_to_fee(length: u32) -> Balance {
            TransactionPayment::length_to_fee(length)
        }
    }

    impl fp_rpc::EthereumRuntimeRPCApi<Block> for Runtime {
        fn chain_id() -> u64 {
            <Runtime as pallet_evm::Config>::ChainId::get()
        }

        fn account_basic(address: H160) -> EVMAccount {
            let (account, _) = pallet_evm::Pallet::<Runtime>::account_basic(&address);
            account
        }

        fn gas_price() -> U256 {
            let (gas_price, _) = <Runtime as pallet_evm::Config>::FeeCalculator::min_gas_price();
            gas_price
        }

        fn account_code_at(address: H160) -> Vec<u8> {
            pallet_evm::AccountCodes::<Runtime>::get(address)
        }

        fn author() -> H160 {
            <pallet_evm::Pallet<Runtime>>::find_author()
        }

        fn storage_at(address: H160, index: U256) -> H256 {
            let mut tmp = [0u8; 32];
            index.to_big_endian(&mut tmp);
            pallet_evm::AccountStorages::<Runtime>::get(address, H256::from_slice(&tmp[..]))
        }

        fn call(
            from: H160,
            to: H160,
            data: Vec<u8>,
            value: U256,
            gas_limit: U256,
            max_fee_per_gas: Option<U256>,
            max_priority_fee_per_gas: Option<U256>,
            nonce: Option<U256>,
            estimate: bool,
            access_list: Option<Vec<(H160, Vec<H256>)>>,
        ) -> Result<pallet_evm::CallInfo, sp_runtime::DispatchError> {
            use pallet_evm::GasWeightMapping as _;

            let config = if estimate {
                let mut config = <Runtime as pallet_evm::Config>::config().clone();
                config.estimate = true;
                Some(config)
            } else {
                None
            };

                    // Estimated encoded transaction size must be based on the heaviest transaction
                    // type (EIP1559Transaction) to be compatible with all transaction types.
                    let mut estimated_transaction_len = data.len() +
                        // pallet ethereum index: 1
                        // transact call index: 1
                        // Transaction enum variant: 1
                        // chain_id 8 bytes
                        // nonce: 32
                        // max_priority_fee_per_gas: 32
                        // max_fee_per_gas: 32
                        // gas_limit: 32
                        // action: 21 (enum varianrt + call address)
                        // value: 32
                        // access_list: 1 (empty vec size)
                        // 65 bytes signature
                        258;

                    if access_list.is_some() {
                        estimated_transaction_len += access_list.encoded_size();
                    }


                    let gas_limit = if gas_limit > U256::from(u64::MAX) {
                        u64::MAX
                    } else {
                        gas_limit.low_u64()
                    };
            let without_base_extrinsic_weight = true;

            let (weight_limit, proof_size_base_cost) =
                match <Runtime as pallet_evm::Config>::GasWeightMapping::gas_to_weight(
                    gas_limit,
                    without_base_extrinsic_weight
                ) {
                    weight_limit if weight_limit.proof_size() > 0 => {
                        (Some(weight_limit), Some(estimated_transaction_len as u64))
                    }
                    _ => (None, None),
                };

            <Runtime as pallet_evm::Config>::Runner::call(
                from,
                to,
                data,
                value,
                gas_limit.unique_saturated_into(),
                max_fee_per_gas,
                max_priority_fee_per_gas,
                nonce,
                access_list.unwrap_or_default(),
                false,
                true,
                weight_limit,
                proof_size_base_cost,
                config.as_ref().unwrap_or(<Runtime as pallet_evm::Config>::config()),
            ).map_err(|err| err.error.into())
        }

        fn create(
            from: H160,
            data: Vec<u8>,
            value: U256,
            gas_limit: U256,
            max_fee_per_gas: Option<U256>,
            max_priority_fee_per_gas: Option<U256>,
            nonce: Option<U256>,
            estimate: bool,
            access_list: Option<Vec<(H160, Vec<H256>)>>,
        ) -> Result<pallet_evm::CreateInfo, sp_runtime::DispatchError> {
            use pallet_evm::GasWeightMapping as _;

            let config = if estimate {
                let mut config = <Runtime as pallet_evm::Config>::config().clone();
                config.estimate = true;
                Some(config)
            } else {
                None
            };


            let mut estimated_transaction_len = data.len() +
                // from: 20
                // value: 32
                // gas_limit: 32
                // nonce: 32
                // 1 byte transaction action variant
                // chain id 8 bytes
                // 65 bytes signature
                190;

            if max_fee_per_gas.is_some() {
                estimated_transaction_len += 32;
            }
            if max_priority_fee_per_gas.is_some() {
                estimated_transaction_len += 32;
            }
            if access_list.is_some() {
                estimated_transaction_len += access_list.encoded_size();
            }


            let gas_limit = if gas_limit > U256::from(u64::MAX) {
                u64::MAX
            } else {
                gas_limit.low_u64()
            };
            let without_base_extrinsic_weight = true;

            let (weight_limit, proof_size_base_cost) =
                match <Runtime as pallet_evm::Config>::GasWeightMapping::gas_to_weight(
                    gas_limit,
                    without_base_extrinsic_weight
                ) {
                    weight_limit if weight_limit.proof_size() > 0 => {
                        (Some(weight_limit), Some(estimated_transaction_len as u64))
                    }
                    _ => (None, None),
                };

            let whitelist = pallet_evm::WhitelistedCreators::<Runtime>::get();
            let whitelist_disabled = pallet_evm::DisableWhitelistCheck::<Runtime>::get();
            <Runtime as pallet_evm::Config>::Runner::create(
                from,
                data,
                value,
                gas_limit.unique_saturated_into(),
                max_fee_per_gas,
                max_priority_fee_per_gas,
                nonce,
                access_list.unwrap_or_default(),
                whitelist,
                whitelist_disabled,
                false,
                true,
                weight_limit,
                proof_size_base_cost,
                config.as_ref().unwrap_or(<Runtime as pallet_evm::Config>::config()),
            ).map_err(|err| err.error.into())
        }

        fn current_transaction_statuses() -> Option<Vec<TransactionStatus>> {
            pallet_ethereum::CurrentTransactionStatuses::<Runtime>::get()
        }

        fn current_block() -> Option<pallet_ethereum::Block> {
            pallet_ethereum::CurrentBlock::<Runtime>::get()
        }

        fn current_receipts() -> Option<Vec<pallet_ethereum::Receipt>> {
            pallet_ethereum::CurrentReceipts::<Runtime>::get()
        }

        fn current_all() -> (
            Option<pallet_ethereum::Block>,
            Option<Vec<pallet_ethereum::Receipt>>,
            Option<Vec<TransactionStatus>>
        ) {
            (
                pallet_ethereum::CurrentBlock::<Runtime>::get(),
                pallet_ethereum::CurrentReceipts::<Runtime>::get(),
                pallet_ethereum::CurrentTransactionStatuses::<Runtime>::get()
            )
        }

        fn extrinsic_filter(
            xts: Vec<<Block as BlockT>::Extrinsic>,
        ) -> Vec<EthereumTransaction> {
            xts.into_iter().filter_map(|xt| match xt.0.function {
                RuntimeCall::Ethereum(transact { transaction }) => Some(transaction),
                _ => None
            }).collect::<Vec<EthereumTransaction>>()
        }

        fn elasticity() -> Option<Permill> {
            Some(pallet_base_fee::Elasticity::<Runtime>::get())
        }

        fn gas_limit_multiplier_support() {}

        fn pending_block(
            xts: Vec<<Block as BlockT>::Extrinsic>,
        ) -> (Option<pallet_ethereum::Block>, Option<Vec<TransactionStatus>>) {
            for ext in xts.into_iter() {
                let _ = Executive::apply_extrinsic(ext);
            }

            Ethereum::on_finalize(System::block_number() + 1);

            (
                pallet_ethereum::CurrentBlock::<Runtime>::get(),
                pallet_ethereum::CurrentTransactionStatuses::<Runtime>::get()
            )
        }

        fn initialize_pending_block(header: &<Block as BlockT>::Header) {
            Executive::initialize_block(header);
        }
    }

    impl fp_rpc::ConvertTransactionRuntimeApi<Block> for Runtime {
        fn convert_transaction(transaction: EthereumTransaction) -> <Block as BlockT>::Extrinsic {
            UncheckedExtrinsic::new_unsigned(
                pallet_ethereum::Call::<Runtime>::transact { transaction }.into(),
            )
        }
    }

    #[cfg(feature = "runtime-benchmarks")]
    impl frame_benchmarking::Benchmark<Block> for Runtime {
        fn benchmark_metadata(extra: bool) -> (
            Vec<frame_benchmarking::BenchmarkList>,
            Vec<frame_support::traits::StorageInfo>,
        ) {
            use frame_benchmarking::{baseline, Benchmarking, BenchmarkList};
            use frame_support::traits::StorageInfoTrait;
            use frame_system_benchmarking::Pallet as SystemBench;
            use baseline::Pallet as BaselineBench;

            let mut list = Vec::<BenchmarkList>::new();
            list_benchmarks!(list, extra);

            let storage_info = AllPalletsWithSystem::storage_info();

            (list, storage_info)
        }

        fn dispatch_benchmark(
            config: frame_benchmarking::BenchmarkConfig
        ) -> Result<Vec<frame_benchmarking::BenchmarkBatch>, sp_runtime::RuntimeString> {
            use frame_benchmarking::{baseline, Benchmarking, BenchmarkBatch};
            use sp_storage::TrackedStorageKey;

            use frame_system_benchmarking::Pallet as SystemBench;
            use baseline::Pallet as BaselineBench;

            #[allow(non_local_definitions)]
            impl frame_system_benchmarking::Config for Runtime {}

            #[allow(non_local_definitions)]
            impl baseline::Config for Runtime {}

            use frame_support::traits::WhitelistedStorageKeys;
            let whitelist: Vec<TrackedStorageKey> = AllPalletsWithSystem::whitelisted_storage_keys();

            let mut batches = Vec::<BenchmarkBatch>::new();
            let params = (&config, &whitelist);
            add_benchmarks!(params, batches);

            Ok(batches)
        }
    }

    #[cfg(feature = "try-runtime")]
    impl frame_try_runtime::TryRuntime<Block> for Runtime {
        #[allow(clippy::unwrap_used)]
        fn on_runtime_upgrade(checks: frame_try_runtime::UpgradeCheckSelect) -> (Weight, Weight) {
            // NOTE: intentional unwrap: we don't want to propagate the error backwards, and want to
            // have a backtrace here. If any of the pre/post migration checks fail, we shall stop
            // right here and right now.
            let weight = Executive::try_runtime_upgrade(checks).unwrap();
            (weight, BlockWeights::get().max_block)
        }

        fn execute_block(
            block: Block,
            state_root_check: bool,
            signature_check: bool,
            select: frame_try_runtime::TryStateSelect
        ) -> Weight {
            // NOTE: intentional unwrap: we don't want to propagate the error backwards, and want to
            // have a backtrace here.
            Executive::try_execute_block(block, state_root_check, signature_check, select).expect("execute-block failed")
        }
    }

    impl subtensor_custom_rpc_runtime_api::DelegateInfoRuntimeApi<Block> for Runtime {
        fn get_delegates() -> Vec<u8> {
            let result = SubtensorModule::get_delegates();
            result.encode()
        }

        fn get_delegate(delegate_account_vec: Vec<u8>) -> Vec<u8> {
            let _result = SubtensorModule::get_delegate(delegate_account_vec);
            if _result.is_some() {
                let result = _result.expect("Could not get DelegateInfo");
                result.encode()
            } else {
                vec![]
            }
        }

        fn get_delegated(delegatee_account_vec: Vec<u8>) -> Vec<u8> {
            let result = SubtensorModule::get_delegated(delegatee_account_vec);
            result.encode()
        }
    }

    impl subtensor_custom_rpc_runtime_api::NeuronInfoRuntimeApi<Block> for Runtime {
        fn get_neurons_lite(netuid: u16) -> Vec<u8> {
            let result = SubtensorModule::get_neurons_lite(netuid);
            result.encode()
        }

        fn get_neuron_lite(netuid: u16, uid: u16) -> Vec<u8> {
            let _result = SubtensorModule::get_neuron_lite(netuid, uid);
            if _result.is_some() {
                let result = _result.expect("Could not get NeuronInfoLite");
                result.encode()
            } else {
                vec![]
            }
        }

        fn get_neurons(netuid: u16) -> Vec<u8> {
            let result = SubtensorModule::get_neurons(netuid);
            result.encode()
        }

        fn get_neuron(netuid: u16, uid: u16) -> Vec<u8> {
            let _result = SubtensorModule::get_neuron(netuid, uid);
            if _result.is_some() {
                let result = _result.expect("Could not get NeuronInfo");
                result.encode()
            } else {
                vec![]
            }
        }
    }

    impl subtensor_custom_rpc_runtime_api::SubnetInfoRuntimeApi<Block> for Runtime {
        fn get_subnet_info(netuid: u16) -> Vec<u8> {
            let _result = SubtensorModule::get_subnet_info(netuid);
            if _result.is_some() {
                let result = _result.expect("Could not get SubnetInfo");
                result.encode()
            } else {
                vec![]
            }
        }

        fn get_subnets_info() -> Vec<u8> {
            let result = SubtensorModule::get_subnets_info();
            result.encode()
        }

        fn get_subnet_info_v2(netuid: u16) -> Vec<u8> {
            let _result = SubtensorModule::get_subnet_info_v2(netuid);
            if _result.is_some() {
                let result = _result.expect("Could not get SubnetInfo");
                result.encode()
            } else {
                vec![]
            }
        }

        fn get_subnets_info_v2() -> Vec<u8> {
            let result = SubtensorModule::get_subnets_info_v2();
            result.encode()
        }

        fn get_subnet_hyperparams(netuid: u16) -> Vec<u8> {
            let _result = SubtensorModule::get_subnet_hyperparams(netuid);
            if _result.is_some() {
                let result = _result.expect("Could not get SubnetHyperparams");
                result.encode()
            } else {
                vec![]
            }
        }
    }

    impl sp_consensus_aura::AuraApi<Block, AuraId> for Runtime {
        fn slot_duration() -> sp_consensus_aura::SlotDuration {
            unimplemented!()
            // sp_consensus_aura::SlotDuration::from_millis(Aura::slot_duration())
        }

        fn authorities() -> Vec<AuraId> {
            unimplemented!()
            // pallet_aura::Authorities::<Runtime>::get().into_inner()
        }
    }

    impl subtensor_custom_rpc_runtime_api::StakeInfoRuntimeApi<Block> for Runtime {
        fn get_stake_info_for_coldkey( coldkey_account_vec: Vec<u8> ) -> Vec<u8> {
            let result = SubtensorModule::get_stake_info_for_coldkey( coldkey_account_vec );
            result.encode()
        }

        fn get_stake_info_for_coldkeys( coldkey_account_vecs: Vec<Vec<u8>> ) -> Vec<u8> {
            let result = SubtensorModule::get_stake_info_for_coldkeys( coldkey_account_vecs );
            result.encode()
        }
    }

    impl subtensor_custom_rpc_runtime_api::SubnetRegistrationRuntimeApi<Block> for Runtime {
        fn get_network_registration_cost() -> u64 {
            SubtensorModule::get_network_lock_cost()
        }
    }

    impl pallet_staking_runtime_api::StakingApi<Block, Balance, AccountId> for Runtime {
        fn nominations_quota(balance: Balance) -> u32 {
            Staking::api_nominations_quota(balance)
        }

        fn eras_stakers_page_count(era: sp_staking::EraIndex, account: AccountId) -> sp_staking::Page {
            Staking::api_eras_stakers_page_count(era, account)
        }

        fn pending_rewards(era: sp_staking::EraIndex, account: AccountId) -> bool {
            Staking::api_pending_rewards(era, account)
        }
    }

    impl babe_primitives::BabeApi<Block> for Runtime {
        fn configuration() -> babe_primitives::BabeConfiguration {
            let epoch_config = Babe::epoch_config().unwrap_or(BABE_GENESIS_EPOCH_CONFIG);
            babe_primitives::BabeConfiguration {
                slot_duration: Babe::slot_duration(),
                epoch_length: EpochDuration::get(),
                c: epoch_config.c,
                authorities: Babe::authorities().to_vec(),
                randomness: Babe::randomness(),
                allowed_slots: epoch_config.allowed_slots,
            }
        }

        fn current_epoch_start() -> babe_primitives::Slot {
            Babe::current_epoch_start()
        }

        fn current_epoch() -> babe_primitives::Epoch {
            Babe::current_epoch()
        }

        fn next_epoch() -> babe_primitives::Epoch {
            Babe::next_epoch()
        }

        fn generate_key_ownership_proof(
            _slot: babe_primitives::Slot,
            authority_id: babe_primitives::AuthorityId,
        ) -> Option<babe_primitives::OpaqueKeyOwnershipProof> {
            use codec::Encode;

            Historical::prove((babe_primitives::KEY_TYPE, authority_id))
                .map(|p| p.encode())
                .map(babe_primitives::OpaqueKeyOwnershipProof::new)
        }

        fn submit_report_equivocation_unsigned_extrinsic(
            equivocation_proof: babe_primitives::EquivocationProof<<Block as BlockT>::Header>,
            key_owner_proof: babe_primitives::OpaqueKeyOwnershipProof,
        ) -> Option<()> {
            let key_owner_proof = key_owner_proof.decode()?;

            Babe::submit_unsigned_equivocation_report(
                equivocation_proof,
                key_owner_proof,
            )
        }
    }
}

#[test]
fn check_whitelist() {
    use crate::*;
    use frame_support::traits::WhitelistedStorageKeys;
    use sp_core::hexdisplay::HexDisplay;
    use std::collections::HashSet;
    let whitelist: HashSet<String> = AllPalletsWithSystem::whitelisted_storage_keys()
        .iter()
        .map(|e| HexDisplay::from(&e.key).to_string())
        .collect();

    // Block Number
    assert!(whitelist.contains("26aa394eea5630e07c48ae0c9558cef702a5c1b19ab7a04f536c519aca4983ac"));
    // Total Issuance
    assert!(whitelist.contains("c2261276cc9d1f8598ea4b6a74b15c2f57c875e4cff74148e4628f264b974c80"));
    // Execution Phase
    assert!(whitelist.contains("26aa394eea5630e07c48ae0c9558cef7ff553b5a9862a516939d82b3d3d8661a"));
    // Event Count
    assert!(whitelist.contains("26aa394eea5630e07c48ae0c9558cef70a98fdbe9ce6c55837576c60c7af3850"));
    // System Events
    assert!(whitelist.contains("26aa394eea5630e07c48ae0c9558cef780d41e5e16056765bc8461851072c9d7"));
}
