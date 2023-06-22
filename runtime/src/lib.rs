#![cfg_attr(not(feature = "std"), no_std)]
// `construct_runtime!` does a lot of recursion and requires us to increase the limit to 256.
#![recursion_limit = "256"]

// Make the WASM binary available.
#[cfg(feature = "std")]
include!(concat!(env!("OUT_DIR"), "/wasm_binary.rs"));

use codec::{Encode, Decode};
use pallet_collective::EnsureMember;
use pallet_grandpa::{
	fg_primitives, AuthorityId as GrandpaId, AuthorityList as GrandpaAuthorityList,
};

use frame_support::{pallet_prelude::{Get, TypeInfo, MaxEncodedLen, PhantomData, EnsureOrigin, DispatchResult}, traits::{EitherOfDiverse}, RuntimeDebug};
use frame_system::{EnsureRoot, Config, EnsureNever, RawOrigin};

use smallvec::smallvec;
use sp_api::impl_runtime_apis;
use sp_consensus_aura::sr25519::AuthorityId as AuraId;
use sp_core::{crypto::KeyTypeId, OpaqueMetadata};
use sp_runtime::{
	create_runtime_str, generic, impl_opaque_keys,
	traits::{
		AccountIdLookup, BlakeTwo256, Block as BlockT, IdentifyAccount, NumberFor, One, Verify,
	},
	transaction_validity::{TransactionSource, TransactionValidity},
	ApplyExtrinsicResult, MultiSignature,
};

use sp_std::prelude::*;
#[cfg(feature = "std")]
use sp_version::NativeVersion;
use sp_version::RuntimeVersion;

// A few exports that help ease life for downstream crates.
pub use frame_support::{
	construct_runtime, parameter_types,
	traits::{
		ConstU128, ConstU32, ConstU64, ConstU8, KeyOwnerProofSystem, Randomness, StorageInfo,
	},
	weights::{
		constants::{
			BlockExecutionWeight, ExtrinsicBaseWeight, RocksDbWeight, WEIGHT_REF_TIME_PER_SECOND, 
		},
		IdentityFee, Weight, WeightToFeeCoefficients, WeightToFeeCoefficient, WeightToFeePolynomial
	},
	StorageValue,
};
pub use frame_system::Call as SystemCall;
pub use pallet_balances::Call as BalancesCall;
pub use pallet_timestamp::Call as TimestampCall;
use pallet_transaction_payment::{CurrencyAdapter, Multiplier};
#[cfg(any(feature = "std", test))]
pub use sp_runtime::BuildStorage;
pub use sp_runtime::{Perbill, Permill};

// Subtensor module
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
			pub aura: Aura,
			pub grandpa: Grandpa,
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
	spec_version: 122,
	impl_version: 1,
	apis: RUNTIME_API_VERSIONS,
	transaction_version: 1,
	state_version: 1,
};

/// This determines the average expected block time that we are targeting.
/// Blocks will be produced at a minimum duration defined by `SLOT_DURATION`.
/// `SLOT_DURATION` is picked up by `pallet_timestamp` which is in turn picked
/// up by `pallet_aura` to implement `fn slot_duration()`.
///
/// Change this to adjust the block time.
pub const MILLISECS_PER_BLOCK: u64 = 12000;

// NOTE: Currently it is not possible to change the slot duration after the chain has started.
//       Attempting to do so will brick block production.
pub const SLOT_DURATION: u64 = MILLISECS_PER_BLOCK;

// Time is measured by number of blocks.
pub const MINUTES: BlockNumber = 60_000 / (MILLISECS_PER_BLOCK as BlockNumber);
pub const HOURS: BlockNumber = MINUTES * 60;
pub const DAYS: BlockNumber = HOURS * 24;

// The version information used to identify this runtime when compiled natively.
#[cfg(feature = "std")]
pub fn native_version() -> NativeVersion {
	NativeVersion { runtime_version: VERSION, can_author_with: Default::default() }
}

const NORMAL_DISPATCH_RATIO: Perbill = Perbill::from_percent(75);

parameter_types! {
	pub const BlockHashCount: BlockNumber = 2400;
	pub const Version: RuntimeVersion = VERSION;
	// We allow for 2 seconds of compute with a 6 second average block time.
	pub BlockWeights: frame_system::limits::BlockWeights =
		frame_system::limits::BlockWeights::with_sensible_defaults(
			Weight::from_parts(4u64 * WEIGHT_REF_TIME_PER_SECOND, u64::MAX),
			NORMAL_DISPATCH_RATIO,
		);
	pub BlockLength: frame_system::limits::BlockLength = frame_system::limits::BlockLength
		::max_with_normal_ratio(10 * 1024 * 1024, NORMAL_DISPATCH_RATIO);
	pub const SS58Prefix: u8 = 42;
}

// Configure FRAME pallets to include in runtime.

impl frame_system::Config for Runtime {
	// The basic call filter to use in dispatchable.
	type BaseCallFilter = frame_support::traits::Everything;
	// Block & extrinsics weights: base values and limits.
	type BlockWeights = BlockWeights;
	// The maximum length of a block (in bytes).
	type BlockLength = BlockLength;
	// The identifier used to distinguish between accounts.
	type AccountId = AccountId;
	// The aggregated dispatch type that is available for extrinsics.
	type RuntimeCall = RuntimeCall;
	// The lookup mechanism to get account ID from whatever is passed in dispatchers.
	type Lookup = AccountIdLookup<AccountId, ()>;
	// The index type for storing how many extrinsics an account has signed.
	type Index = Index;
	// The index type for blocks.
	type BlockNumber = BlockNumber;
	// The type for hashing blocks and tries.
	type Hash = Hash;
	// The hashing algorithm used.
	type Hashing = BlakeTwo256;
	// The header type.
	type Header = generic::Header<BlockNumber, BlakeTwo256>;
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
}

impl pallet_insecure_randomness_collective_flip::Config for Runtime {}

impl pallet_aura::Config for Runtime {
	type AuthorityId = AuraId;
	type DisabledValidators = ();
	type MaxAuthorities = ConstU32<32>;
}

impl pallet_grandpa::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;

	type KeyOwnerProofSystem = ();

	type KeyOwnerProof =
		<Self::KeyOwnerProofSystem as KeyOwnerProofSystem<(KeyTypeId, GrandpaId)>>::Proof;

	type KeyOwnerIdentification = <Self::KeyOwnerProofSystem as KeyOwnerProofSystem<(
		KeyTypeId,
		GrandpaId,
	)>>::IdentificationTuple;

	type HandleEquivocation = ();

	type WeightInfo = ();
	type MaxAuthorities = ConstU32<32>;
	type MaxSetIdSessionEntries = ConstU64<0>;
}

impl pallet_timestamp::Config for Runtime {
	// A timestamp: milliseconds since the unix epoch.
	type Moment = u64;
	type OnTimestampSet = Aura;
	type MinimumPeriod = ConstU64<{ SLOT_DURATION / 2 }>;
	type WeightInfo = ();
}

impl pallet_utility::Config for Runtime {
    type RuntimeEvent = RuntimeEvent;
    type RuntimeCall = RuntimeCall;
    type PalletsOrigin = OriginCaller;
    type WeightInfo = pallet_utility::weights::SubstrateWeight<Runtime>;
}

// Existential deposit.
pub const EXISTENTIAL_DEPOSIT: u64 = 500;

impl pallet_balances::Config for Runtime {
	type MaxLocks = ConstU32<50>;
	type MaxReserves = ();
	type ReserveIdentifier = [u8; 8];
	// The type for recording an account's balance.
	type Balance = Balance;
	// The ubiquitous event type.
	type RuntimeEvent = RuntimeEvent;
	type DustRemoval = ();
	type ExistentialDeposit = ConstU64<EXISTENTIAL_DEPOSIT>;
	type AccountStore = System;
	type WeightInfo = pallet_balances::weights::SubstrateWeight<Runtime>;
}

pub struct LinearWeightToFee<C>(sp_std::marker::PhantomData<C>);

impl<C> WeightToFeePolynomial for LinearWeightToFee<C>
where
	C: Get<Balance>,
{
	type Balance = Balance;

	fn polynomial() -> WeightToFeeCoefficients<Self::Balance> {
		let coefficient = WeightToFeeCoefficient {
			coeff_integer: 0,
			coeff_frac: Perbill::from_parts(1),
			negative: false,
			degree: 1,
		};

		smallvec!(coefficient)
	}
}

parameter_types! {
	// Used with LinearWeightToFee conversion.
	pub const FeeWeightRatio: u64 = 1;
	pub const TransactionByteFee: u128 = 1;
	pub FeeMultiplier: Multiplier = Multiplier::one();
}

impl pallet_transaction_payment::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;

	type OnChargeTransaction = CurrencyAdapter<Balances, ()>;
	//type TransactionByteFee = TransactionByteFee;

	// Convert dispatch weight to a chargeable fee.
	type WeightToFee = LinearWeightToFee<FeeWeightRatio>;

	type FeeMultiplierUpdate = ();

	type OperationalFeeMultiplier = ConstU8<1>;

	type LengthToFee = IdentityFee<Balance>;
	//type FeeMultiplierUpdate = ConstFeeMultiplier<FeeMultiplier>;
}

// Configure collective pallet for council
parameter_types! {
	pub const CouncilMotionDuration: BlockNumber = 100;
	pub const CouncilMaxProposals: u32 = 10;
	pub const CouncilMaxMembers: u32 = 3;
}

// Configure collective pallet for Senate
parameter_types! {
	pub const SenateMaxMembers: u32 = 10;
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
	fn can_vote(account: &AccountId) -> bool {
		//Senate::is_member(account)
		false // Disable voting from pallet_collective::vote
	}
}

use pallet_subtensor::{MemberManagement, CollectiveInterface};
pub struct ManageSenateMembers;
impl MemberManagement<AccountId> for ManageSenateMembers {
	fn add_member(account: &AccountId) -> DispatchResult {
		let who = Address::Id( account.clone() );
		SenateMembers::add_member(RawOrigin::Root.into(), who)
	}

	fn remove_member(account: &AccountId) -> DispatchResult {
		let who = Address::Id( account.clone() );
		SenateMembers::remove_member(RawOrigin::Root.into(), who)
	}

	fn swap_member(remove: &AccountId, add: &AccountId) -> DispatchResult {
		let remove = Address::Id( remove.clone() );
		let add = Address::Id( add.clone() );

		SenateMembers::swap_member(RawOrigin::Root.into(), remove, add)
	}

	fn is_member(account: &AccountId) -> bool {
		Senate::is_member(account)
	}

	fn members() -> Vec<AccountId> {
		Senate::members()
	}

	fn max_members() -> u32 {
		SenateMaxMembers::get()
	}
}

pub struct GetSenateMemberCount;
impl GetVotingMembers<MemberCount> for GetSenateMemberCount {
	fn get_count() -> MemberCount {Senate::members().len() as u32}
}
impl Get<MemberCount> for GetSenateMemberCount {
	fn get() -> MemberCount {SenateMaxMembers::get()}
}

pub struct TriumvirateVotes;
impl CollectiveInterface<AccountId, Hash, u32> for TriumvirateVotes {
	fn remove_votes(hotkey: &AccountId) -> Result<bool, sp_runtime::DispatchError> {
		Triumvirate::remove_votes(hotkey)
	}

	fn add_vote(hotkey: &AccountId, proposal: Hash, index: u32, approve: bool) -> Result<bool, sp_runtime::DispatchError> {
		Triumvirate::do_vote(hotkey.clone(), proposal, index, approve)
	}
}

type EnsureMajoritySenate = pallet_collective::EnsureProportionMoreThan<AccountId, TriumvirateCollective, 1, 2>;

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

// This is a dummy collective instance for managing senate members
// Probably not the best solution, but fastest implementation
type SenateCollective = pallet_collective::Instance2;
impl pallet_collective::Config<SenateCollective> for Runtime {
	type RuntimeOrigin = RuntimeOrigin;
	type Proposal = RuntimeCall; 
	type RuntimeEvent = RuntimeEvent;
	type MotionDuration = CouncilMotionDuration;
	type MaxProposals = CouncilMaxProposals;
	type MaxMembers = SenateMaxMembers;
	type DefaultVote = pallet_collective::PrimeDefaultVote;
	type WeightInfo = pallet_collective::weights::SubstrateWeight<Runtime>;
	type SetMembersOrigin = EnsureNever<AccountId>;
	type CanPropose = ();
	type CanVote = ();
	type GetVotingMembers = ();
}

// We call our top K delegates membership Senate
type SenateMembership = pallet_membership::Instance2;
impl pallet_membership::Config<SenateMembership> for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type AddOrigin = EnsureRoot<AccountId>;
	type RemoveOrigin = EnsureRoot<AccountId>;
	type SwapOrigin = EnsureRoot<AccountId>;
	type ResetOrigin = EnsureRoot<AccountId>;
	type PrimeOrigin = EnsureRoot<AccountId>;
	type MembershipInitialized = Senate;
	type MembershipChanged = Senate;
	type MaxMembers = SenateMaxMembers;
	type WeightInfo = pallet_membership::weights::SubstrateWeight<Runtime>;
}

impl pallet_sudo::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type RuntimeCall = RuntimeCall;
}

// Configure the pallet subtensor.
parameter_types! {
	pub const SubtensorInitialRho: u16 = 10;
    pub const SubtensorInitialKappa: u16 = 32_767; // 0.5 = 65535/2 
    pub const SubtensorInitialMaxAllowedUids: u16 = 4096;
    pub const SubtensorInitialIssuance: u64 = 0;
    pub const SubtensorInitialMinAllowedWeights: u16 = 1024;
    pub const SubtensorInitialEmissionValue: u16 = 0;
    pub const SubtensorInitialMaxWeightsLimit: u16 = 1000; // 1000/2^16 = 0.015
    pub const SubtensorInitialValidatorBatchSize: u16 = 32; // 32
    pub const SubtensorInitialValidatorSequenceLen: u16 = 256; // 256
    pub const SubtensorInitialValidatorEpochLen: u16 = 100;
    pub const SubtensorInitialValidatorEpochsPerReset: u16 = 60;
    pub const SubtensorInitialValidatorExcludeQuantile: u16 = 6554; // 10% of u16
    pub const SubtensorInitialValidatorPruneLen: u64 = 1;
    pub const SubtensorInitialValidatorLogitsDivergence: u16 = 1310; // 2% of u16
    pub const SubtensorInitialScalingLawPower: u16 = 50; // 0.5
    pub const SubtensorInitialSynergyScalingLawPower: u16 = 50; // 0.5
    pub const SubtensorInitialMaxAllowedValidators: u16 = 128;
    pub const SubtensorInitialTempo: u16 = 99;
    pub const SubtensorInitialDifficulty: u64 = 10_000_000;
    pub const SubtensorInitialAdjustmentInterval: u16 = 100;
    pub const SubtensorInitialTargetRegistrationsPerInterval: u16 = 2;
    pub const SubtensorInitialImmunityPeriod: u16 = 4096;
    pub const SubtensorInitialActivityCutoff: u16 = 5000;
    pub const SubtensorInitialMaxRegistrationsPerBlock: u16 = 1;
    pub const SubtensorInitialPruningScore : u16 = u16::MAX;
    pub const SubtensorInitialBondsMovingAverage: u64 = 900_000;
    pub const SubtensorInitialDefaultTake: u16 = 11_796; // 18% honest number.
    pub const SubtensorInitialWeightsVersionKey: u64 = 0;
    pub const SubtensorInitialMinDifficulty: u64 = 10_000_000;
    pub const SubtensorInitialMaxDifficulty: u64 = u64::MAX / 4;
    pub const SubtensorInitialServingRateLimit: u64 = 50; 
	pub const SubtensorInitialBurn: u64 = 1_000_000_000; // 1 tao
	pub const SubtensorInitialMinBurn: u64 = 1_000_000_000; // 1 tao
	pub const SubtensorInitialMaxBurn: u64 = 100_000_000_000; // 100 tao
	pub const SubtensorInitialTxRateLimit: u64 = 1000;
	pub const SubtensorInitialRAORecycledForRegistration: u64 = 0; // 0 rao
	pub const SubtensorInitialSenateRequiredStakePercentage: u64 = 2; // 2 percent of total stake
}

impl pallet_subtensor::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type SudoRuntimeCall = RuntimeCall;
	type Currency = Balances;
	type CouncilOrigin = EnsureMajoritySenate;
	type SenateMembers = ManageSenateMembers;
	type TriumvirateInterface = TriumvirateVotes;

	type InitialRho = SubtensorInitialRho;
	type InitialKappa = SubtensorInitialKappa;
	type InitialMaxAllowedUids = SubtensorInitialMaxAllowedUids;
	type InitialBondsMovingAverage = SubtensorInitialBondsMovingAverage;
	type InitialIssuance = SubtensorInitialIssuance;
	type InitialMinAllowedWeights = SubtensorInitialMinAllowedWeights;
	type InitialEmissionValue = SubtensorInitialEmissionValue;
	type InitialMaxWeightsLimit = SubtensorInitialMaxWeightsLimit;
	type InitialValidatorBatchSize = SubtensorInitialValidatorBatchSize;
	type InitialValidatorSequenceLen = SubtensorInitialValidatorSequenceLen;
	type InitialValidatorEpochLen = SubtensorInitialValidatorEpochLen;
	type InitialValidatorEpochsPerReset = SubtensorInitialValidatorEpochsPerReset;
	type InitialValidatorExcludeQuantile = SubtensorInitialValidatorExcludeQuantile;
	type InitialValidatorPruneLen = SubtensorInitialValidatorPruneLen;
	type InitialValidatorLogitsDivergence = SubtensorInitialValidatorLogitsDivergence;
	type InitialScalingLawPower = SubtensorInitialScalingLawPower;
	type InitialSynergyScalingLawPower = SubtensorInitialSynergyScalingLawPower;
	type InitialTempo = SubtensorInitialTempo;
	type InitialDifficulty = SubtensorInitialDifficulty;
	type InitialAdjustmentInterval = SubtensorInitialAdjustmentInterval;
	type InitialTargetRegistrationsPerInterval = SubtensorInitialTargetRegistrationsPerInterval;
	type InitialImmunityPeriod = SubtensorInitialImmunityPeriod;
	type InitialActivityCutoff = SubtensorInitialActivityCutoff;
	type InitialMaxRegistrationsPerBlock = SubtensorInitialMaxRegistrationsPerBlock;
	type InitialPruningScore = SubtensorInitialPruningScore;
	type InitialMaxAllowedValidators = SubtensorInitialMaxAllowedValidators;
	type InitialDefaultTake = SubtensorInitialDefaultTake;
	type InitialWeightsVersionKey = SubtensorInitialWeightsVersionKey;
	type InitialMaxDifficulty = SubtensorInitialMaxDifficulty;
	type InitialMinDifficulty = SubtensorInitialMinDifficulty;
	type InitialServingRateLimit = SubtensorInitialServingRateLimit;
	type InitialBurn = SubtensorInitialBurn;
	type InitialMaxBurn = SubtensorInitialMaxBurn;
	type InitialMinBurn = SubtensorInitialMinBurn;
	type InitialTxRateLimit = SubtensorInitialTxRateLimit;
	type InitialRAORecycledForRegistration = SubtensorInitialRAORecycledForRegistration;
	type InitialSenateRequiredStakePercentage = SubtensorInitialSenateRequiredStakePercentage;
}

// Create the runtime by composing the FRAME pallets that were previously configured.
construct_runtime!(
	pub struct Runtime
	where
		Block = Block,
		NodeBlock = opaque::Block,
		UncheckedExtrinsic = UncheckedExtrinsic,
	{
		System: frame_system,
		RandomnessCollectiveFlip: pallet_insecure_randomness_collective_flip,
		Timestamp: pallet_timestamp,
		Aura: pallet_aura,
		Grandpa: pallet_grandpa,
		Balances: pallet_balances,
		TransactionPayment: pallet_transaction_payment,
		SubtensorModule: pallet_subtensor,
		Triumvirate: pallet_collective::<Instance1>::{Pallet, Call, Storage, Origin<T>, Event<T>, Config<T>},
		TriumvirateMembers: pallet_membership::<Instance1>::{Pallet, Call, Storage, Event<T>, Config<T>},
		Senate: pallet_collective::<Instance2>::{Pallet, Call, Storage, Origin<T>, Event<T>, Config<T>},
		SenateMembers: pallet_membership::<Instance2>::{Pallet, Call, Storage, Event<T>, Config<T>},
		Utility: pallet_utility,
		Sudo: pallet_sudo,
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
	frame_system::CheckNonce<Runtime>,
	frame_system::CheckWeight<Runtime>,
	pallet_transaction_payment::ChargeTransactionPayment<Runtime>,
	pallet_subtensor::SubtensorSignedExtension<Runtime>
);

// Unchecked extrinsic type as expected by this runtime.
pub type UncheckedExtrinsic =
	generic::UncheckedExtrinsic<Address, RuntimeCall, Signature, SignedExtra>;
// The payload being signed in transactions.
pub type SignedPayload = generic::SignedPayload<RuntimeCall, SignedExtra>;
// Executive: handles dispatch to the various modules.
pub type Executive = frame_executive::Executive<
	Runtime,
	Block,
	frame_system::ChainContext<Runtime>,
	Runtime,
	AllPalletsWithSystem,
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
		[pallet_subtensor, SubtensorModule]
		[pallet_timestamp, Timestamp]
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

		fn initialize_block(header: &<Block as BlockT>::Header) {
			Executive::initialize_block(header)
		}
	}

	impl sp_api::Metadata<Block> for Runtime {
		fn metadata() -> OpaqueMetadata {
			OpaqueMetadata::new(Runtime::metadata().into())
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

	impl sp_consensus_aura::AuraApi<Block, AuraId> for Runtime {
		fn slot_duration() -> sp_consensus_aura::SlotDuration {
			sp_consensus_aura::SlotDuration::from_millis(Aura::slot_duration())
		}

		fn authorities() -> Vec<AuraId> {
			Aura::authorities().into_inner()
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
			use frame_benchmarking::{baseline, Benchmarking, BenchmarkBatch, TrackedStorageKey};

			use frame_system_benchmarking::Pallet as SystemBench;
			use baseline::Pallet as BaselineBench;

			impl frame_system_benchmarking::Config for Runtime {}
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
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use frame_support::traits::WhitelistedStorageKeys;
	use sp_core::hexdisplay::HexDisplay;
	use std::collections::HashSet;

	#[test]
	fn check_whitelist() {
		let whitelist: HashSet<String> = AllPalletsWithSystem::whitelisted_storage_keys()
			.iter()
			.map(|e| HexDisplay::from(&e.key).to_string())
			.collect();

		// Block Number
		assert!(
			whitelist.contains("26aa394eea5630e07c48ae0c9558cef702a5c1b19ab7a04f536c519aca4983ac")
		);
		// Total Issuance
		assert!(
			whitelist.contains("c2261276cc9d1f8598ea4b6a74b15c2f57c875e4cff74148e4628f264b974c80")
		);
		// Execution Phase
		assert!(
			whitelist.contains("26aa394eea5630e07c48ae0c9558cef7ff553b5a9862a516939d82b3d3d8661a")
		);
		// Event Count
		assert!(
			whitelist.contains("26aa394eea5630e07c48ae0c9558cef70a98fdbe9ce6c55837576c60c7af3850")
		);
		// System Events
		assert!(
			whitelist.contains("26aa394eea5630e07c48ae0c9558cef780d41e5e16056765bc8461851072c9d7")
		);
	}
}