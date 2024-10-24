#![cfg_attr(not(feature = "std"), no_std)]
#![recursion_limit = "512"]
#![allow(clippy::too_many_arguments)]
// Edit this file to define custom logic or remove it if it is not needed.
// Learn more about FRAME and the core library of Substrate FRAME pallets:
// <https://docs.substrate.io/reference/frame-pallets/>
pub use pallet::*;

use frame_system::{self as system, ensure_signed};

use frame_support::{
    dispatch::{self, DispatchInfo, DispatchResult, DispatchResultWithPostInfo, PostDispatchInfo},
    ensure,
    pallet_macros::import_section,
    traits::{tokens::fungible, IsSubType},
};

use codec::{Decode, Encode};
use frame_support::sp_runtime::transaction_validity::InvalidTransaction;
use frame_support::sp_runtime::transaction_validity::ValidTransaction;
use pallet_balances::Call as BalancesCall;
// use pallet_scheduler as Scheduler;
use scale_info::TypeInfo;
use sp_runtime::{
    traits::{DispatchInfoOf, Dispatchable, PostDispatchInfoOf, SignedExtension},
    transaction_validity::{TransactionValidity, TransactionValidityError},
    DispatchError,
};
use sp_std::marker::PhantomData;

// ============================
//	==== Benchmark Imports =====
// ============================
mod benchmarks;

// =========================
//	==== Pallet Imports =====
// =========================
pub mod coinbase;
pub mod epoch;
pub mod macros;
pub mod migrations;
pub mod rpc_info;
pub mod staking;
pub mod subnets;
pub mod swap;
pub mod utils;
use crate::utils::rate_limiting::TransactionType;
use macros::{config, dispatches, errors, events, genesis, hooks};

// apparently this is stabilized since rust 1.36
extern crate alloc;

#[deny(missing_docs)]
#[import_section(errors::errors)]
#[import_section(events::events)]
#[import_section(dispatches::dispatches)]
#[import_section(genesis::genesis)]
#[import_section(hooks::hooks)]
#[import_section(config::config)]
#[frame_support::pallet]
pub mod pallet {
    use crate::migrations;
    use frame_support::{
        dispatch::GetDispatchInfo,
        pallet_prelude::{DispatchResult, StorageMap, ValueQuery, *},
        traits::{
            tokens::fungible, OriginTrait, QueryPreimage, StorePreimage, UnfilteredDispatchable,
        },
        BoundedVec,
    };
    use frame_system::pallet_prelude::*;
    use sp_core::H256;
    use sp_runtime::traits::{Dispatchable, TrailingZeroInput};
    use sp_std::collections::vec_deque::VecDeque;
    use sp_std::vec;
    use sp_std::vec::Vec;
    use subtensor_macros::freeze_struct;

    #[cfg(not(feature = "std"))]
    use alloc::boxed::Box;
    #[cfg(feature = "std")]
    use sp_std::prelude::Box;

    /// Origin for the pallet
    pub type PalletsOriginOf<T> =
        <<T as frame_system::Config>::RuntimeOrigin as OriginTrait>::PalletsOrigin;

    /// Call type for the pallet
    pub type CallOf<T> = <T as frame_system::Config>::RuntimeCall;

    /// Tracks version for migrations. Should be monotonic with respect to the
    /// order of migrations. (i.e. always increasing)
    const STORAGE_VERSION: StorageVersion = StorageVersion::new(7);

    /// Minimum balance required to perform a coldkey swap
    pub const MIN_BALANCE_TO_PERFORM_COLDKEY_SWAP: u64 = 100_000_000; // 0.1 TAO in RAO

    #[pallet::pallet]
    #[pallet::without_storage_info]
    #[pallet::storage_version(STORAGE_VERSION)]
    pub struct Pallet<T>(_);

    /// Alias for the account ID.
    pub type AccountIdOf<T> = <T as frame_system::Config>::AccountId;

    /// Struct for Axon.
    pub type AxonInfoOf = AxonInfo;

    /// local one
    pub type LocalCallOf<T> = <T as Config>::RuntimeCall;

    /// Data structure for Axon information.
    #[crate::freeze_struct("3545cfb0cac4c1f5")]
    #[derive(Encode, Decode, Default, TypeInfo, Clone, PartialEq, Eq, Debug)]
    pub struct AxonInfo {
        ///  Axon serving block.
        pub block: u64,
        ///  Axon version
        pub version: u32,
        ///  Axon u128 encoded ip address of type v6 or v4.
        pub ip: u128,
        ///  Axon u16 encoded port.
        pub port: u16,
        ///  Axon ip type, 4 for ipv4 and 6 for ipv6.
        pub ip_type: u8,
        ///  Axon protocol. TCP, UDP, other.
        pub protocol: u8,
        ///  Axon proto placeholder 1.
        pub placeholder1: u8,
        ///  Axon proto placeholder 2.
        pub placeholder2: u8,
    }

    /// Struct for NeuronCertificate.
    pub type NeuronCertificateOf = NeuronCertificate;
    /// Data structure for NeuronCertificate information.
    #[freeze_struct("1c232be200d9ec6c")]
    #[derive(Decode, Encode, Default, TypeInfo, PartialEq, Eq, Clone, Debug)]
    pub struct NeuronCertificate {
        ///  The neuron TLS public key
        pub public_key: BoundedVec<u8, ConstU32<64>>,
        ///  The algorithm used to generate the public key
        pub algorithm: u8,
    }

    impl TryFrom<Vec<u8>> for NeuronCertificate {
        type Error = ();

        fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
            if value.len() > 65 {
                return Err(());
            }
            // take the first byte as the algorithm
            let algorithm = value.first().ok_or(())?;
            // and the rest as the public_key
            let certificate = value.get(1..).ok_or(())?.to_vec();
            Ok(Self {
                public_key: BoundedVec::try_from(certificate).map_err(|_| ())?,
                algorithm: *algorithm,
            })
        }
    }

    ///  Struct for Prometheus.
    pub type PrometheusInfoOf = PrometheusInfo;

    /// Data structure for Prometheus information.
    #[crate::freeze_struct("5dde687e63baf0cd")]
    #[derive(Encode, Decode, Default, TypeInfo, Clone, PartialEq, Eq, Debug)]
    pub struct PrometheusInfo {
        /// Prometheus serving block.
        pub block: u64,
        /// Prometheus version.
        pub version: u32,
        ///  Prometheus u128 encoded ip address of type v6 or v4.
        pub ip: u128,
        ///  Prometheus u16 encoded port.
        pub port: u16,
        /// Prometheus ip type, 4 for ipv4 and 6 for ipv6.
        pub ip_type: u8,
    }

    ///  Struct for ChainIdentities.
    pub type ChainIdentityOf = ChainIdentity;

    /// Data structure for Chain Identities.
    #[crate::freeze_struct("bbfd00438dbe2b58")]
    #[derive(Encode, Decode, Default, TypeInfo, Clone, PartialEq, Eq, Debug)]
    pub struct ChainIdentity {
        /// The name of the chain identity
        pub name: Vec<u8>,
        /// The URL associated with the chain identity
        pub url: Vec<u8>,
        /// The image representation of the chain identity
        pub image: Vec<u8>,
        /// The Discord information for the chain identity
        pub discord: Vec<u8>,
        /// A description of the chain identity
        pub description: Vec<u8>,
        /// Additional information about the chain identity
        pub additional: Vec<u8>,
    }

    ///  Struct for SubnetIdentities.
    pub type SubnetIdentityOf = SubnetIdentity;
    /// Data structure for Subnet Identities
    #[crate::freeze_struct("f448dc3dad763108")]
    #[derive(Encode, Decode, Default, TypeInfo, Clone, PartialEq, Eq, Debug)]
    pub struct SubnetIdentity {
        /// The name of the subnet
        pub subnet_name: Vec<u8>,
        /// The github repository associated with the chain identity
        pub github_repo: Vec<u8>,
        /// The subnet's contact
        pub subnet_contact: Vec<u8>,
    }
    /// ============================
    /// ==== Staking + Accounts ====
    /// ============================

    #[pallet::type_value]
    /// Total Rao in circulation.
    pub fn TotalSupply<T: Config>() -> u64 {
        21_000_000_000_000_000
    }
    #[pallet::type_value]
    /// Default Delegate Take.
    pub fn DefaultDelegateTake<T: Config>() -> u16 {
        T::InitialDefaultDelegateTake::get()
    }

    #[pallet::type_value]
    /// Default childkey take.
    pub fn DefaultChildKeyTake<T: Config>() -> u16 {
        T::InitialDefaultChildKeyTake::get()
    }
    #[pallet::type_value]
    /// Default minimum delegate take.
    pub fn DefaultMinDelegateTake<T: Config>() -> u16 {
        T::InitialMinDelegateTake::get()
    }

    #[pallet::type_value]
    /// Default minimum childkey take.
    pub fn DefaultMinChildKeyTake<T: Config>() -> u16 {
        T::InitialMinChildKeyTake::get()
    }

    #[pallet::type_value]
    /// Default maximum childkey take.
    pub fn DefaultMaxChildKeyTake<T: Config>() -> u16 {
        T::InitialMaxChildKeyTake::get()
    }

    #[pallet::type_value]
    /// Default account take.
    pub fn DefaultAccountTake<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default stakes per interval.
    pub fn DefaultStakesPerInterval<T: Config>() -> (u64, u64) {
        (0, 0)
    }
    #[pallet::type_value]
    /// Default emission per block.
    pub fn DefaultBlockEmission<T: Config>() -> u64 {
        1_000_000_000
    }
    #[pallet::type_value]
    /// Default allowed delegation.
    pub fn DefaultAllowsDelegation<T: Config>() -> bool {
        false
    }
    #[pallet::type_value]
    /// Default total issuance.
    pub fn DefaultTotalIssuance<T: Config>() -> u64 {
        T::InitialIssuance::get()
    }
    #[pallet::type_value]
    /// Default account, derived from zero trailing bytes.
    pub fn DefaultAccount<T: Config>() -> T::AccountId {
        T::AccountId::decode(&mut TrailingZeroInput::zeroes())
            .expect("trailing zeroes always produce a valid account ID; qed")
    }
    #[pallet::type_value]
    /// Default target stakes per interval.
    pub fn DefaultTargetStakesPerInterval<T: Config>() -> u64 {
        T::InitialTargetStakesPerInterval::get()
    }
    #[pallet::type_value]
    /// Default stake interval.
    pub fn DefaultStakeInterval<T: Config>() -> u64 {
        360
    }
    #[pallet::type_value]
    /// Default account linkage
    pub fn DefaultAccountLinkage<T: Config>() -> Vec<(u64, T::AccountId)> {
        vec![]
    }
    #[pallet::type_value]
    /// Default account linkage
    pub fn DefaultProportion<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default accumulated emission for a hotkey
    pub fn DefaultAccumulatedEmission<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default last adjustment block.
    pub fn DefaultLastAdjustmentBlock<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default last adjustment block.
    pub fn DefaultRegistrationsThisBlock<T: Config>() -> u16 {
        0
    }
    #[pallet::type_value]
    /// Default registrations this block.
    pub fn DefaultBurn<T: Config>() -> u64 {
        T::InitialBurn::get()
    }
    #[pallet::type_value]
    /// Default burn token.
    pub fn DefaultMinBurn<T: Config>() -> u64 {
        T::InitialMinBurn::get()
    }
    #[pallet::type_value]
    /// Default min burn token.
    pub fn DefaultMaxBurn<T: Config>() -> u64 {
        T::InitialMaxBurn::get()
    }
    #[pallet::type_value]
    /// Default max burn token.
    pub fn DefaultDifficulty<T: Config>() -> u64 {
        T::InitialDifficulty::get()
    }
    #[pallet::type_value]
    /// Default difficulty value.
    pub fn DefaultMinDifficulty<T: Config>() -> u64 {
        T::InitialMinDifficulty::get()
    }
    #[pallet::type_value]
    /// Default min difficulty value.
    pub fn DefaultMaxDifficulty<T: Config>() -> u64 {
        T::InitialMaxDifficulty::get()
    }
    #[pallet::type_value]
    /// Default max difficulty value.
    pub fn DefaultMaxRegistrationsPerBlock<T: Config>() -> u16 {
        T::InitialMaxRegistrationsPerBlock::get()
    }
    #[pallet::type_value]
    /// Default max registrations per block.
    pub fn DefaultRAORecycledForRegistration<T: Config>() -> u64 {
        T::InitialRAORecycledForRegistration::get()
    }
    #[pallet::type_value]
    /// Default number of networks.
    pub fn DefaultN<T: Config>() -> u16 {
        0
    }
    #[pallet::type_value]
    /// Default value for modality.
    pub fn DefaultModality<T: Config>() -> u16 {
        0
    }
    #[pallet::type_value]
    /// Default value for hotkeys.
    pub fn DefaultHotkeys<T: Config>() -> Vec<u16> {
        vec![]
    }
    #[pallet::type_value]
    /// Default value if network is added.
    pub fn DefaultNeworksAdded<T: Config>() -> bool {
        false
    }
    #[pallet::type_value]
    /// Default value for network member.
    pub fn DefaultIsNetworkMember<T: Config>() -> bool {
        false
    }
    #[pallet::type_value]
    /// Default value for registration allowed.
    pub fn DefaultRegistrationAllowed<T: Config>() -> bool {
        false
    }
    #[pallet::type_value]
    /// Default value for network registered at.
    pub fn DefaultNetworkRegisteredAt<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for network immunity period.
    pub fn DefaultNetworkImmunityPeriod<T: Config>() -> u64 {
        T::InitialNetworkImmunityPeriod::get()
    }
    #[pallet::type_value]
    /// Default value for network last registered.
    pub fn DefaultNetworkLastRegistered<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for nominator min required stake.
    pub fn DefaultNominatorMinRequiredStake<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for network min allowed UIDs.
    pub fn DefaultNetworkMinAllowedUids<T: Config>() -> u16 {
        T::InitialNetworkMinAllowedUids::get()
    }
    #[pallet::type_value]
    /// Default value for network min lock cost.
    pub fn DefaultNetworkMinLockCost<T: Config>() -> u64 {
        T::InitialNetworkMinLockCost::get()
    }
    #[pallet::type_value]
    /// Default value for network lock reduction interval.
    pub fn DefaultNetworkLockReductionInterval<T: Config>() -> u64 {
        T::InitialNetworkLockReductionInterval::get()
    }
    #[pallet::type_value]
    /// Default value for subnet owner cut.
    pub fn DefaultSubnetOwnerCut<T: Config>() -> u16 {
        T::InitialSubnetOwnerCut::get()
    }
    #[pallet::type_value]
    /// Default value for subnet limit.
    pub fn DefaultSubnetLimit<T: Config>() -> u16 {
        T::InitialSubnetLimit::get()
    }
    #[pallet::type_value]
    /// Default value for network rate limit.
    pub fn DefaultNetworkRateLimit<T: Config>() -> u64 {
        if cfg!(feature = "pow-faucet") {
            return 0;
        }
        T::InitialNetworkRateLimit::get()
    }
    #[pallet::type_value]
    /// Default value for emission values.
    pub fn DefaultEmissionValues<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for pending emission.
    pub fn DefaultPendingEmission<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for blocks since last step.
    pub fn DefaultBlocksSinceLastStep<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for last mechanism step block.
    pub fn DefaultLastMechanismStepBlock<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for subnet owner.
    pub fn DefaultSubnetOwner<T: Config>() -> T::AccountId {
        T::AccountId::decode(&mut sp_runtime::traits::TrailingZeroInput::zeroes())
            .expect("trailing zeroes always produce a valid account ID; qed")
    }
    #[pallet::type_value]
    /// Default value for subnet locked.
    pub fn DefaultSubnetLocked<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for network tempo
    pub fn DefaultTempo<T: Config>() -> u16 {
        T::InitialTempo::get()
    }
    #[pallet::type_value]
    /// Default value for weights set rate limit.
    pub fn DefaultWeightsSetRateLimit<T: Config>() -> u64 {
        100
    }
    #[pallet::type_value]
    /// Default block number at registration.
    pub fn DefaultBlockAtRegistration<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for rho parameter.
    pub fn DefaultRho<T: Config>() -> u16 {
        T::InitialRho::get()
    }
    #[pallet::type_value]
    /// Default value for kappa parameter.
    pub fn DefaultKappa<T: Config>() -> u16 {
        T::InitialKappa::get()
    }
    #[pallet::type_value]
    /// Default maximum allowed UIDs.
    pub fn DefaultMaxAllowedUids<T: Config>() -> u16 {
        T::InitialMaxAllowedUids::get()
    }
    #[pallet::type_value]
    /// Default immunity period.
    pub fn DefaultImmunityPeriod<T: Config>() -> u16 {
        T::InitialImmunityPeriod::get()
    }
    #[pallet::type_value]
    /// Default activity cutoff.
    pub fn DefaultActivityCutoff<T: Config>() -> u16 {
        T::InitialActivityCutoff::get()
    }
    #[pallet::type_value]
    /// Default maximum weights limit.
    pub fn DefaultMaxWeightsLimit<T: Config>() -> u16 {
        T::InitialMaxWeightsLimit::get()
    }
    #[pallet::type_value]
    /// Default weights version key.
    pub fn DefaultWeightsVersionKey<T: Config>() -> u64 {
        T::InitialWeightsVersionKey::get()
    }
    #[pallet::type_value]
    /// Default minimum allowed weights.
    pub fn DefaultMinAllowedWeights<T: Config>() -> u16 {
        T::InitialMinAllowedWeights::get()
    }
    #[pallet::type_value]
    /// Default maximum allowed validators.
    pub fn DefaultMaxAllowedValidators<T: Config>() -> u16 {
        T::InitialMaxAllowedValidators::get()
    }
    #[pallet::type_value]
    /// Default adjustment interval.
    pub fn DefaultAdjustmentInterval<T: Config>() -> u16 {
        T::InitialAdjustmentInterval::get()
    }
    #[pallet::type_value]
    /// Default bonds moving average.
    pub fn DefaultBondsMovingAverage<T: Config>() -> u64 {
        T::InitialBondsMovingAverage::get()
    }
    #[pallet::type_value]
    /// Default validator prune length.
    pub fn DefaultValidatorPruneLen<T: Config>() -> u64 {
        T::InitialValidatorPruneLen::get()
    }
    #[pallet::type_value]
    /// Default scaling law power.
    pub fn DefaultScalingLawPower<T: Config>() -> u16 {
        T::InitialScalingLawPower::get()
    }
    #[pallet::type_value]
    /// Default target registrations per interval.
    pub fn DefaultTargetRegistrationsPerInterval<T: Config>() -> u16 {
        T::InitialTargetRegistrationsPerInterval::get()
    }
    #[pallet::type_value]
    /// Default adjustment alpha.
    pub fn DefaultAdjustmentAlpha<T: Config>() -> u64 {
        T::InitialAdjustmentAlpha::get()
    }
    #[pallet::type_value]
    /// Default minimum stake for weights.
    pub fn DefaultWeightsMinStake<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default minimum stake for weights.
    pub fn DefaultRevealPeriodEpochs<T: Config>() -> u64 {
        1
    }
    #[pallet::type_value]
    /// Value definition for vector of u16.
    pub fn EmptyU16Vec<T: Config>() -> Vec<u16> {
        vec![]
    }
    #[pallet::type_value]
    /// Value definition for vector of u64.
    pub fn EmptyU64Vec<T: Config>() -> Vec<u64> {
        vec![]
    }
    #[pallet::type_value]
    /// Value definition for vector of bool.
    pub fn EmptyBoolVec<T: Config>() -> Vec<bool> {
        vec![]
    }
    #[pallet::type_value]
    /// Value definition for bonds with type vector of (u16, u16).
    pub fn DefaultBonds<T: Config>() -> Vec<(u16, u16)> {
        vec![]
    }
    #[pallet::type_value]
    /// Value definition for weights with vector of (u16, u16).
    pub fn DefaultWeights<T: Config>() -> Vec<(u16, u16)> {
        vec![]
    }
    #[pallet::type_value]
    /// Default value for key with type T::AccountId derived from trailing zeroes.
    pub fn DefaultKey<T: Config>() -> T::AccountId {
        T::AccountId::decode(&mut sp_runtime::traits::TrailingZeroInput::zeroes())
            .expect("trailing zeroes always produce a valid account ID; qed")
    }
    #[pallet::type_value]
    /// Default value for network immunity period.
    pub fn DefaultHotkeyEmissionTempo<T: Config>() -> u64 {
        T::InitialHotkeyEmissionTempo::get()
    }
    #[pallet::type_value]
    /// Default value for rate limiting
    pub fn DefaultTxRateLimit<T: Config>() -> u64 {
        T::InitialTxRateLimit::get()
    }
    #[pallet::type_value]
    /// Default value for delegate take rate limiting
    pub fn DefaultTxDelegateTakeRateLimit<T: Config>() -> u64 {
        T::InitialTxDelegateTakeRateLimit::get()
    }
    #[pallet::type_value]
    /// Default value for chidlkey take rate limiting
    pub fn DefaultTxChildKeyTakeRateLimit<T: Config>() -> u64 {
        T::InitialTxChildKeyTakeRateLimit::get()
    }
    #[pallet::type_value]
    /// Default value for last extrinsic block.
    pub fn DefaultLastTxBlock<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for serving rate limit.
    pub fn DefaultServingRateLimit<T: Config>() -> u64 {
        T::InitialServingRateLimit::get()
    }
    #[pallet::type_value]
    /// Default value for weight commit/reveal enabled.
    pub fn DefaultCommitRevealWeightsEnabled<T: Config>() -> bool {
        false
    }
    #[pallet::type_value]
    /// Senate requirements
    pub fn DefaultSenateRequiredStakePercentage<T: Config>() -> u64 {
        T::InitialSenateRequiredStakePercentage::get()
    }
    #[pallet::type_value]
    /// -- ITEM (switches liquid alpha on)
    pub fn DefaultLiquidAlpha<T: Config>() -> bool {
        false
    }
    #[pallet::type_value]
    /// (alpha_low: 0.7, alpha_high: 0.9)
    pub fn DefaultAlphaValues<T: Config>() -> (u16, u16) {
        (45875, 58982)
    }
    #[pallet::type_value]
    /// Default value for network max stake.
    pub fn DefaultNetworkMaxStake<T: Config>() -> u64 {
        T::InitialNetworkMaxStake::get()
    }

    #[pallet::type_value]
    /// Default value for coldkey swap schedule duration
    pub fn DefaultColdkeySwapScheduleDuration<T: Config>() -> BlockNumberFor<T> {
        T::InitialColdkeySwapScheduleDuration::get()
    }

    #[pallet::storage]
    pub type ColdkeySwapScheduleDuration<T: Config> =
        StorageValue<_, BlockNumberFor<T>, ValueQuery, DefaultColdkeySwapScheduleDuration<T>>;

    #[pallet::type_value]
    /// Default value for dissolve network schedule duration
    pub fn DefaultDissolveNetworkScheduleDuration<T: Config>() -> BlockNumberFor<T> {
        T::InitialDissolveNetworkScheduleDuration::get()
    }

    #[pallet::storage]
    pub type DissolveNetworkScheduleDuration<T: Config> =
        StorageValue<_, BlockNumberFor<T>, ValueQuery, DefaultDissolveNetworkScheduleDuration<T>>;

    #[pallet::storage]
    pub type SenateRequiredStakePercentage<T> =
        StorageValue<_, u64, ValueQuery, DefaultSenateRequiredStakePercentage<T>>;

    /// ============================
    /// ==== Staking Variables ====
    /// ============================
    /// The Subtensor [`TotalIssuance`] represents the total issuance of tokens on the Bittensor network.
    ///
    /// It is comprised of three parts:
    /// - The total amount of issued tokens, tracked in the TotalIssuance of the Balances pallet
    /// - The total amount of tokens staked in the system, tracked in [`TotalStake`]
    /// - The total amount of tokens locked up for subnet reg, tracked in [`TotalSubnetLocked`] attained by iterating over subnet lock.
    ///
    /// Eventually, Bittensor should migrate to using Holds afterwhich time we will not require this
    /// separate accounting.
    #[pallet::storage] // --- ITEM ( total_issuance )
    pub type TotalIssuance<T> = StorageValue<_, u64, ValueQuery, DefaultTotalIssuance<T>>;
    #[pallet::storage] // --- ITEM ( total_stake )
    pub type TotalStake<T> = StorageValue<_, u64, ValueQuery>;
    #[pallet::storage] // --- ITEM ( default_delegate_take )
    pub type MaxDelegateTake<T> = StorageValue<_, u16, ValueQuery, DefaultDelegateTake<T>>;
    #[pallet::storage] // --- ITEM ( min_delegate_take )
    pub type MinDelegateTake<T> = StorageValue<_, u16, ValueQuery, DefaultMinDelegateTake<T>>;
    #[pallet::storage] // --- ITEM ( default_childkey_take )
    pub type MaxChildkeyTake<T> = StorageValue<_, u16, ValueQuery, DefaultMaxChildKeyTake<T>>;
    #[pallet::storage] // --- ITEM ( min_childkey_take )
    pub type MinChildkeyTake<T> = StorageValue<_, u16, ValueQuery, DefaultMinChildKeyTake<T>>;

    #[pallet::storage] // --- ITEM ( global_block_emission )
    pub type BlockEmission<T> = StorageValue<_, u64, ValueQuery, DefaultBlockEmission<T>>;
    #[pallet::storage] // --- ITEM (target_stakes_per_interval)
    pub type TargetStakesPerInterval<T> =
        StorageValue<_, u64, ValueQuery, DefaultTargetStakesPerInterval<T>>;
    #[pallet::storage] // --- ITEM (default_stake_interval)
    pub type StakeInterval<T> = StorageValue<_, u64, ValueQuery, DefaultStakeInterval<T>>;
    #[pallet::storage] // --- MAP ( hot ) --> stake | Returns the total amount of stake under a hotkey.
    pub type TotalHotkeyStake<T: Config> =
        StorageMap<_, Identity, T::AccountId, u64, ValueQuery, DefaultAccountTake<T>>;
    #[pallet::storage] // --- MAP ( cold ) --> stake | Returns the total amount of stake under a coldkey.
    pub type TotalColdkeyStake<T: Config> =
        StorageMap<_, Identity, T::AccountId, u64, ValueQuery, DefaultAccountTake<T>>;
    #[pallet::storage]
    /// MAP (hot, cold) --> stake | Returns a tuple (u64: stakes, u64: block_number)
    pub type TotalHotkeyColdkeyStakesThisInterval<T: Config> = StorageDoubleMap<
        _,
        Identity,
        T::AccountId,
        Identity,
        T::AccountId,
        (u64, u64),
        ValueQuery,
        DefaultStakesPerInterval<T>,
    >;
    #[pallet::storage]
    /// MAP ( hot ) --> cold | Returns the controlling coldkey for a hotkey.
    pub type Owner<T: Config> =
        StorageMap<_, Blake2_128Concat, T::AccountId, T::AccountId, ValueQuery, DefaultAccount<T>>;
    #[pallet::storage]
    /// MAP ( hot ) --> take | Returns the hotkey delegation take. And signals that this key is open for delegation.
    pub type Delegates<T: Config> =
        StorageMap<_, Blake2_128Concat, T::AccountId, u16, ValueQuery, DefaultDelegateTake<T>>;
    #[pallet::storage]
    /// DMAP ( hot, netuid ) --> take | Returns the hotkey childkey take for a specific subnet
    pub type ChildkeyTake<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId, // First key: hotkey
        Identity,
        u16, // Second key: netuid
        u16, // Value: take
        ValueQuery,
    >;

    #[pallet::storage]
    /// DMAP ( hot, cold ) --> stake | Returns the stake under a coldkey prefixed by hotkey.
    pub type Stake<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        T::AccountId,
        u64,
        ValueQuery,
        DefaultAccountTake<T>,
    >;
    #[pallet::storage]
    /// Map ( hot ) --> last_hotkey_emission_drain | Last block we drained this hotkey's emission.
    pub type LastHotkeyEmissionDrain<T: Config> = StorageMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        u64,
        ValueQuery,
        DefaultAccumulatedEmission<T>,
    >;
    #[pallet::storage]
    /// ITEM ( hotkey_emission_tempo )
    pub type HotkeyEmissionTempo<T> =
        StorageValue<_, u64, ValueQuery, DefaultHotkeyEmissionTempo<T>>;
    #[pallet::storage]
    /// Map ( hot ) --> emission | Accumulated hotkey emission.
    pub type PendingdHotkeyEmission<T: Config> = StorageMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        u64,
        ValueQuery,
        DefaultAccumulatedEmission<T>,
    >;
    #[pallet::storage]
    /// Map ( hot, cold ) --> block_number | Last add stake increase.
    pub type LastAddStakeIncrease<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        T::AccountId,
        u64,
        ValueQuery,
        DefaultAccountTake<T>,
    >;
    #[pallet::storage]
    /// DMAP ( parent, netuid ) --> Vec<(proportion,child)>
    pub type ChildKeys<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        u16,
        Vec<(u64, T::AccountId)>,
        ValueQuery,
        DefaultAccountLinkage<T>,
    >;
    #[pallet::storage]
    /// DMAP ( child, netuid ) --> Vec<(proportion,parent)>
    pub type ParentKeys<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        u16,
        Vec<(u64, T::AccountId)>,
        ValueQuery,
        DefaultAccountLinkage<T>,
    >;
    #[pallet::storage] // --- DMAP ( cold ) --> Vec<hot> | Maps coldkey to hotkeys that stake to it
    pub type StakingHotkeys<T: Config> =
        StorageMap<_, Blake2_128Concat, T::AccountId, Vec<T::AccountId>, ValueQuery>;
    #[pallet::storage] // --- MAP ( cold ) --> Vec<hot> | Returns the vector of hotkeys controlled by this coldkey.
    pub type OwnedHotkeys<T: Config> =
        StorageMap<_, Blake2_128Concat, T::AccountId, Vec<T::AccountId>, ValueQuery>;

    #[pallet::storage] // --- DMAP ( cold ) --> () | Maps coldkey to if a coldkey swap is scheduled.
    pub type ColdkeySwapScheduled<T: Config> =
        StorageMap<_, Blake2_128Concat, T::AccountId, (), ValueQuery>;

    /// ============================
    /// ==== Global Parameters =====
    /// ============================
    #[pallet::storage]
    /// --- StorageItem Global Used Work.
    pub type UsedWork<T: Config> = StorageMap<_, Identity, Vec<u8>, u64, ValueQuery>;
    #[pallet::storage]
    /// --- ITEM( global_max_registrations_per_block )
    pub type MaxRegistrationsPerBlock<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxRegistrationsPerBlock<T>>;
    #[pallet::storage]
    /// --- ITEM( maximum_number_of_networks )
    pub type SubnetLimit<T> = StorageValue<_, u16, ValueQuery, DefaultSubnetLimit<T>>;
    #[pallet::storage]
    /// --- ITEM( total_number_of_existing_networks )
    pub type TotalNetworks<T> = StorageValue<_, u16, ValueQuery>;
    #[pallet::storage]
    /// ITEM( network_immunity_period )
    pub type NetworkImmunityPeriod<T> =
        StorageValue<_, u64, ValueQuery, DefaultNetworkImmunityPeriod<T>>;
    #[pallet::storage]
    /// ITEM( network_last_registered_block )
    pub type NetworkLastRegistered<T> =
        StorageValue<_, u64, ValueQuery, DefaultNetworkLastRegistered<T>>;
    #[pallet::storage]
    /// ITEM( network_min_allowed_uids )
    pub type NetworkMinAllowedUids<T> =
        StorageValue<_, u16, ValueQuery, DefaultNetworkMinAllowedUids<T>>;
    #[pallet::storage]
    /// ITEM( min_network_lock_cost )
    pub type NetworkMinLockCost<T> = StorageValue<_, u64, ValueQuery, DefaultNetworkMinLockCost<T>>;
    #[pallet::storage]
    /// ITEM( last_network_lock_cost )
    pub type NetworkLastLockCost<T> =
        StorageValue<_, u64, ValueQuery, DefaultNetworkMinLockCost<T>>;
    #[pallet::storage]
    /// ITEM( network_lock_reduction_interval )
    pub type NetworkLockReductionInterval<T> =
        StorageValue<_, u64, ValueQuery, DefaultNetworkLockReductionInterval<T>>;
    #[pallet::storage]
    /// ITEM( subnet_owner_cut )
    pub type SubnetOwnerCut<T> = StorageValue<_, u16, ValueQuery, DefaultSubnetOwnerCut<T>>;
    #[pallet::storage]
    /// ITEM( network_rate_limit )
    pub type NetworkRateLimit<T> = StorageValue<_, u64, ValueQuery, DefaultNetworkRateLimit<T>>;
    #[pallet::storage]
    /// ITEM( nominator_min_required_stake )
    pub type NominatorMinRequiredStake<T> =
        StorageValue<_, u64, ValueQuery, DefaultNominatorMinRequiredStake<T>>;

    /// ============================
    /// ==== Subnet Parameters =====
    /// ============================
    #[pallet::storage]
    /// --- MAP ( netuid ) --> subnetwork_n (Number of UIDs in the network).
    pub type SubnetworkN<T: Config> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultN<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> modality   TEXT: 0, IMAGE: 1, TENSOR: 2
    pub type NetworkModality<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultModality<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> network_is_added
    pub type NetworksAdded<T: Config> =
        StorageMap<_, Identity, u16, bool, ValueQuery, DefaultNeworksAdded<T>>;
    #[pallet::storage]
    /// --- DMAP ( hotkey, netuid ) --> bool
    pub type IsNetworkMember<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        u16,
        bool,
        ValueQuery,
        DefaultIsNetworkMember<T>,
    >;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> network_registration_allowed
    pub type NetworkRegistrationAllowed<T: Config> =
        StorageMap<_, Identity, u16, bool, ValueQuery, DefaultRegistrationAllowed<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> network_pow_allowed
    pub type NetworkPowRegistrationAllowed<T: Config> =
        StorageMap<_, Identity, u16, bool, ValueQuery, DefaultRegistrationAllowed<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> block_created
    pub type NetworkRegisteredAt<T: Config> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultNetworkRegisteredAt<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> tempo
    pub type Tempo<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultTempo<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> emission_values
    pub type EmissionValues<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultEmissionValues<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> pending_emission
    pub type PendingEmission<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultPendingEmission<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> blocks_since_last_step
    pub type BlocksSinceLastStep<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultBlocksSinceLastStep<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> last_mechanism_step_block
    pub type LastMechansimStepBlock<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultLastMechanismStepBlock<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> subnet_owner
    pub type SubnetOwner<T: Config> =
        StorageMap<_, Identity, u16, T::AccountId, ValueQuery, DefaultSubnetOwner<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> subnet_locked
    pub type SubnetLocked<T: Config> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultSubnetLocked<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> serving_rate_limit
    pub type ServingRateLimit<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultServingRateLimit<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> Rho
    pub type Rho<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultRho<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> Kappa
    pub type Kappa<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultKappa<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> uid, we use to record uids to prune at next epoch.
    pub type NeuronsToPruneAtNextEpoch<T: Config> = StorageMap<_, Identity, u16, u16, ValueQuery>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> registrations_this_interval
    pub type RegistrationsThisInterval<T: Config> = StorageMap<_, Identity, u16, u16, ValueQuery>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> pow_registrations_this_interval
    pub type POWRegistrationsThisInterval<T: Config> =
        StorageMap<_, Identity, u16, u16, ValueQuery>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> burn_registrations_this_interval
    pub type BurnRegistrationsThisInterval<T: Config> =
        StorageMap<_, Identity, u16, u16, ValueQuery>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> max_allowed_uids
    pub type MaxAllowedUids<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxAllowedUids<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> immunity_period
    pub type ImmunityPeriod<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultImmunityPeriod<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> activity_cutoff
    pub type ActivityCutoff<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultActivityCutoff<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> max_weight_limit
    pub type MaxWeightsLimit<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxWeightsLimit<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> weights_version_key
    pub type WeightsVersionKey<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultWeightsVersionKey<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> min_allowed_weights
    pub type MinAllowedWeights<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMinAllowedWeights<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> max_allowed_validators
    pub type MaxAllowedValidators<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxAllowedValidators<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> adjustment_interval
    pub type AdjustmentInterval<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultAdjustmentInterval<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> bonds_moving_average
    pub type BondsMovingAverage<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultBondsMovingAverage<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> weights_set_rate_limit
    pub type WeightsSetRateLimit<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultWeightsSetRateLimit<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> validator_prune_len
    pub type ValidatorPruneLen<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultValidatorPruneLen<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> scaling_law_power
    pub type ScalingLawPower<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultScalingLawPower<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> target_registrations_this_interval
    pub type TargetRegistrationsPerInterval<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultTargetRegistrationsPerInterval<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> adjustment_alpha
    pub type AdjustmentAlpha<T: Config> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultAdjustmentAlpha<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> interval
    pub type CommitRevealWeightsEnabled<T> =
        StorageMap<_, Identity, u16, bool, ValueQuery, DefaultCommitRevealWeightsEnabled<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> Burn
    pub type Burn<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultBurn<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> Difficulty
    pub type Difficulty<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultDifficulty<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> MinBurn
    pub type MinBurn<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMinBurn<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> MaxBurn
    pub type MaxBurn<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMaxBurn<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> MinDifficulty
    pub type MinDifficulty<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMinDifficulty<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> MaxDifficulty
    pub type MaxDifficulty<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMaxDifficulty<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) -->  Block at last adjustment.
    pub type LastAdjustmentBlock<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultLastAdjustmentBlock<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> Registrations of this Block.
    pub type RegistrationsThisBlock<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultRegistrationsThisBlock<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> global_RAO_recycled_for_registration
    pub type RAORecycledForRegistration<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultRAORecycledForRegistration<T>>;
    #[pallet::storage]
    /// --- ITEM ( tx_rate_limit )
    pub type TxRateLimit<T> = StorageValue<_, u64, ValueQuery, DefaultTxRateLimit<T>>;
    #[pallet::storage]
    /// --- ITEM ( tx_delegate_take_rate_limit )
    pub type TxDelegateTakeRateLimit<T> =
        StorageValue<_, u64, ValueQuery, DefaultTxDelegateTakeRateLimit<T>>;
    #[pallet::storage]
    /// --- ITEM ( tx_childkey_take_rate_limit )
    pub type TxChildkeyTakeRateLimit<T> =
        StorageValue<_, u64, ValueQuery, DefaultTxChildKeyTakeRateLimit<T>>;
    #[pallet::storage]
    /// --- MAP ( netuid ) --> Whether or not Liquid Alpha is enabled
    pub type LiquidAlphaOn<T> =
        StorageMap<_, Blake2_128Concat, u16, bool, ValueQuery, DefaultLiquidAlpha<T>>;
    #[pallet::storage]
    ///  MAP ( netuid ) --> (alpha_low, alpha_high)
    pub type AlphaValues<T> =
        StorageMap<_, Identity, u16, (u16, u16), ValueQuery, DefaultAlphaValues<T>>;
    /// MAP ( netuid ) --> max stake allowed on a subnet.
    #[pallet::storage]
    pub type NetworkMaxStake<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultNetworkMaxStake<T>>;

    /// =======================================
    /// ==== Subnetwork Consensus Storage  ====
    /// =======================================
    #[pallet::storage] // --- DMAP ( netuid ) --> stake_weight | weight for stake used in YC.
    pub(super) type StakeWeight<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid, hotkey ) --> uid
    pub type Uids<T: Config> =
        StorageDoubleMap<_, Identity, u16, Blake2_128Concat, T::AccountId, u16, OptionQuery>;
    #[pallet::storage]
    /// --- DMAP ( netuid, uid ) --> hotkey
    pub type Keys<T: Config> =
        StorageDoubleMap<_, Identity, u16, Identity, u16, T::AccountId, ValueQuery, DefaultKey<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> (hotkey, se, ve)
    pub type LoadedEmission<T: Config> =
        StorageMap<_, Identity, u16, Vec<(T::AccountId, u64, u64)>, OptionQuery>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> active
    pub type Active<T: Config> =
        StorageMap<_, Identity, u16, Vec<bool>, ValueQuery, EmptyBoolVec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> rank
    pub type Rank<T: Config> = StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> trust
    pub type Trust<T: Config> = StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> consensus
    pub type Consensus<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> incentive
    pub type Incentive<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> dividends
    pub type Dividends<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> emission
    pub type Emission<T: Config> =
        StorageMap<_, Identity, u16, Vec<u64>, ValueQuery, EmptyU64Vec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> last_update
    pub type LastUpdate<T: Config> =
        StorageMap<_, Identity, u16, Vec<u64>, ValueQuery, EmptyU64Vec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> validator_trust
    pub type ValidatorTrust<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> pruning_scores
    pub type PruningScores<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid ) --> validator_permit
    pub type ValidatorPermit<T: Config> =
        StorageMap<_, Identity, u16, Vec<bool>, ValueQuery, EmptyBoolVec<T>>;
    #[pallet::storage]
    /// --- DMAP ( netuid, uid ) --> weights
    pub type Weights<T: Config> = StorageDoubleMap<
        _,
        Identity,
        u16,
        Identity,
        u16,
        Vec<(u16, u16)>,
        ValueQuery,
        DefaultWeights<T>,
    >;
    #[pallet::storage]
    /// --- DMAP ( netuid, uid ) --> bonds
    pub type Bonds<T: Config> = StorageDoubleMap<
        _,
        Identity,
        u16,
        Identity,
        u16,
        Vec<(u16, u16)>,
        ValueQuery,
        DefaultBonds<T>,
    >;
    #[pallet::storage]
    /// --- DMAP ( netuid, uid ) --> block_at_registration
    pub type BlockAtRegistration<T: Config> = StorageDoubleMap<
        _,
        Identity,
        u16,
        Identity,
        u16,
        u64,
        ValueQuery,
        DefaultBlockAtRegistration<T>,
    >;
    #[pallet::storage]
    /// --- MAP ( netuid, hotkey ) --> axon_info
    pub type Axons<T: Config> =
        StorageDoubleMap<_, Identity, u16, Blake2_128Concat, T::AccountId, AxonInfoOf, OptionQuery>;
    /// --- MAP ( netuid, hotkey ) --> certificate
    #[pallet::storage]
    pub type NeuronCertificates<T: Config> = StorageDoubleMap<
        _,
        Identity,
        u16,
        Blake2_128Concat,
        T::AccountId,
        NeuronCertificateOf,
        OptionQuery,
    >;
    #[pallet::storage]
    /// --- MAP ( netuid, hotkey ) --> prometheus_info
    pub type Prometheus<T: Config> = StorageDoubleMap<
        _,
        Identity,
        u16,
        Blake2_128Concat,
        T::AccountId,
        PrometheusInfoOf,
        OptionQuery,
    >;
    #[pallet::storage] // --- MAP ( coldkey ) --> identity
    pub type Identities<T: Config> =
        StorageMap<_, Blake2_128Concat, T::AccountId, ChainIdentityOf, OptionQuery>;

    #[pallet::storage] // --- MAP ( netuid ) --> identity
    pub type SubnetIdentities<T: Config> =
        StorageMap<_, Blake2_128Concat, u16, SubnetIdentityOf, OptionQuery>;

    /// =================================
    /// ==== Axon / Promo Endpoints =====
    /// =================================
    #[pallet::storage] // --- NMAP ( hot, netuid, name ) --> last_block | Returns the last block of a transaction for a given key, netuid, and name.
    pub type TransactionKeyLastBlock<T: Config> = StorageNMap<
        _,
        (
            NMapKey<Blake2_128Concat, T::AccountId>, // hot
            NMapKey<Identity, u16>,                  // netuid
            NMapKey<Identity, u16>,                  // extrinsic enum.
        ),
        u64,
        ValueQuery,
    >;
    #[pallet::storage]
    /// --- MAP ( key ) --> last_block
    pub type LastTxBlock<T: Config> =
        StorageMap<_, Identity, T::AccountId, u64, ValueQuery, DefaultLastTxBlock<T>>;
    #[pallet::storage]
    /// --- MAP ( key ) --> last_tx_block_childkey_take
    pub type LastTxBlockChildKeyTake<T: Config> =
        StorageMap<_, Identity, T::AccountId, u64, ValueQuery, DefaultLastTxBlock<T>>;
    #[pallet::storage]
    /// --- MAP ( key ) --> last_tx_block_delegate_take
    pub type LastTxBlockDelegateTake<T: Config> =
        StorageMap<_, Identity, T::AccountId, u64, ValueQuery, DefaultLastTxBlock<T>>;
    #[pallet::storage]
    /// ITEM( weights_min_stake )
    pub type WeightsMinStake<T> = StorageValue<_, u64, ValueQuery, DefaultWeightsMinStake<T>>;
    #[pallet::storage]
    /// --- MAP (netuid, who) --> VecDeque<(hash, commit_block)> | Stores a queue of commits for an account on a given netuid.
    pub type WeightCommits<T: Config> = StorageDoubleMap<
        _,
        Twox64Concat,
        u16,
        Twox64Concat,
        T::AccountId,
        VecDeque<(H256, u64)>,
        OptionQuery,
    >;
    #[pallet::storage]
    /// --- Map (netuid) --> Number of epochs allowed for commit reveal periods
    pub type RevealPeriodEpochs<T: Config> =
        StorageMap<_, Twox64Concat, u16, u64, ValueQuery, DefaultRevealPeriodEpochs<T>>;

    /// ==================
    /// ==== Genesis =====
    /// ==================
    #[pallet::storage] // --- Storage for migration run status
    pub type HasMigrationRun<T: Config> = StorageMap<_, Identity, Vec<u8>, bool, ValueQuery>;

    #[pallet::genesis_config]
    pub struct GenesisConfig<T: Config> {
        /// Stakes record in genesis.
        pub stakes: Vec<(T::AccountId, Vec<(T::AccountId, (u64, u16))>)>,
        /// The total issued balance in genesis
        pub balances_issuance: u64,
    }

    impl<T: Config> Default for GenesisConfig<T> {
        fn default() -> Self {
            Self {
                stakes: Default::default(),
                balances_issuance: 0,
            }
        }
    }

    // ---- Subtensor helper functions.
    impl<T: Config> Pallet<T> {
        /// Returns the transaction priority for setting weights.
        pub fn get_priority_set_weights(hotkey: &T::AccountId, netuid: u16) -> u64 {
            if let Ok(uid) = Self::get_uid_for_net_and_hotkey(netuid, hotkey) {
                let _stake = Self::get_total_stake_for_hotkey(hotkey);
                let current_block_number: u64 = Self::get_current_block_as_u64();
                let default_priority: u64 =
                    current_block_number.saturating_sub(Self::get_last_update_for_uid(netuid, uid));
                return default_priority.saturating_add(u32::MAX as u64);
            }
            0
        }

        /// Is the caller allowed to set weights
        pub fn check_weights_min_stake(hotkey: &T::AccountId) -> bool {
            // Blacklist weights transactions for low stake peers.
            Self::get_total_stake_for_hotkey(hotkey) >= Self::get_weights_min_stake()
        }

        /// Helper function to check if register is allowed
        pub fn checked_allowed_register(netuid: u16) -> bool {
            if netuid == Self::get_root_netuid() {
                return false;
            }
            if !Self::if_subnet_exist(netuid) {
                return false;
            }
            if !Self::get_network_registration_allowed(netuid) {
                return false;
            }
            if Self::get_registrations_this_block(netuid)
                >= Self::get_max_registrations_per_block(netuid)
            {
                return false;
            }
            if Self::get_registrations_this_interval(netuid)
                >= Self::get_target_registrations_per_interval(netuid).saturating_mul(3)
            {
                return false;
            }
            true
        }
    }
}

/************************************************************
    CallType definition
************************************************************/
#[derive(Debug, PartialEq, Default)]
pub enum CallType {
    SetWeights,
    AddStake,
    RemoveStake,
    AddDelegate,
    Register,
    Serve,
    RegisterNetwork,
    #[default]
    Other,
}

#[derive(Debug, PartialEq)]
pub enum CustomTransactionError {
    ColdkeyInSwapSchedule,
}

impl From<CustomTransactionError> for u8 {
    fn from(variant: CustomTransactionError) -> u8 {
        match variant {
            CustomTransactionError::ColdkeyInSwapSchedule => 0,
        }
    }
}

#[freeze_struct("61e2b893d5ce6701")]
#[derive(Encode, Decode, Clone, Eq, PartialEq, TypeInfo)]
pub struct SubtensorSignedExtension<T: Config + Send + Sync + TypeInfo>(pub PhantomData<T>);

impl<T: Config + Send + Sync + TypeInfo> Default for SubtensorSignedExtension<T>
where
    <T as frame_system::Config>::RuntimeCall:
        Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo>,
    <T as frame_system::Config>::RuntimeCall: IsSubType<Call<T>>,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Config + Send + Sync + TypeInfo> SubtensorSignedExtension<T>
where
    <T as frame_system::Config>::RuntimeCall:
        Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo>,
    <T as frame_system::Config>::RuntimeCall: IsSubType<Call<T>>,
{
    pub fn new() -> Self {
        Self(Default::default())
    }

    pub fn get_priority_vanilla() -> u64 {
        // Return high priority so that every extrinsic except set_weights function will
        // have a higher priority than the set_weights call
        u64::MAX
    }

    pub fn get_priority_set_weights(who: &T::AccountId, netuid: u16) -> u64 {
        Pallet::<T>::get_priority_set_weights(who, netuid)
    }

    pub fn check_weights_min_stake(who: &T::AccountId) -> bool {
        Pallet::<T>::check_weights_min_stake(who)
    }
}

impl<T: Config + Send + Sync + TypeInfo> sp_std::fmt::Debug for SubtensorSignedExtension<T> {
    fn fmt(&self, f: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
        write!(f, "SubtensorSignedExtension")
    }
}

impl<T: Config + Send + Sync + TypeInfo + pallet_balances::Config> SignedExtension
    for SubtensorSignedExtension<T>
where
    <T as frame_system::Config>::RuntimeCall:
        Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo>,
    <T as frame_system::Config>::RuntimeCall: IsSubType<Call<T>>,
    <T as frame_system::Config>::RuntimeCall: IsSubType<BalancesCall<T>>,
{
    const IDENTIFIER: &'static str = "SubtensorSignedExtension";

    type AccountId = T::AccountId;
    type Call = <T as frame_system::Config>::RuntimeCall;
    type AdditionalSigned = ();
    type Pre = (CallType, u64, Self::AccountId);

    fn additional_signed(&self) -> Result<Self::AdditionalSigned, TransactionValidityError> {
        Ok(())
    }

    fn validate(
        &self,
        who: &Self::AccountId,
        call: &Self::Call,
        _info: &DispatchInfoOf<Self::Call>,
        _len: usize,
    ) -> TransactionValidity {
        match call.is_sub_type() {
            Some(Call::commit_weights { netuid, .. }) => {
                if Self::check_weights_min_stake(who) {
                    let priority: u64 = Self::get_priority_set_weights(who, *netuid);
                    Ok(ValidTransaction {
                        priority,
                        longevity: 1,
                        ..Default::default()
                    })
                } else {
                    Err(InvalidTransaction::Custom(1).into())
                }
            }
            Some(Call::reveal_weights { netuid, .. }) => {
                if Self::check_weights_min_stake(who) {
                    let priority: u64 = Self::get_priority_set_weights(who, *netuid);
                    Ok(ValidTransaction {
                        priority,
                        longevity: 1,
                        ..Default::default()
                    })
                } else {
                    Err(InvalidTransaction::Custom(2).into())
                }
            }
            Some(Call::batch_reveal_weights { netuid, .. }) => {
                if Self::check_weights_min_stake(who) {
                    let priority: u64 = Self::get_priority_set_weights(who, *netuid);
                    Ok(ValidTransaction {
                        priority,
                        longevity: 1,
                        ..Default::default()
                    })
                } else {
                    Err(InvalidTransaction::Custom(6).into())
                }
            }
            Some(Call::set_weights { netuid, .. }) => {
                if Self::check_weights_min_stake(who) {
                    let priority: u64 = Self::get_priority_set_weights(who, *netuid);
                    Ok(ValidTransaction {
                        priority,
                        longevity: 1,
                        ..Default::default()
                    })
                } else {
                    Err(InvalidTransaction::Custom(3).into())
                }
            }
            Some(Call::set_root_weights { netuid, hotkey, .. }) => {
                if Self::check_weights_min_stake(hotkey) {
                    let priority: u64 = Self::get_priority_set_weights(hotkey, *netuid);
                    Ok(ValidTransaction {
                        priority,
                        longevity: 1,
                        ..Default::default()
                    })
                } else {
                    Err(InvalidTransaction::Custom(4).into())
                }
            }
            Some(Call::add_stake { .. }) => Ok(ValidTransaction {
                priority: Self::get_priority_vanilla(),
                ..Default::default()
            }),
            Some(Call::remove_stake { .. }) => Ok(ValidTransaction {
                priority: Self::get_priority_vanilla(),
                ..Default::default()
            }),
            Some(Call::register { netuid, .. } | Call::burned_register { netuid, .. }) => {
                let registrations_this_interval =
                    Pallet::<T>::get_registrations_this_interval(*netuid);
                let max_registrations_per_interval =
                    Pallet::<T>::get_target_registrations_per_interval(*netuid);
                if registrations_this_interval >= (max_registrations_per_interval.saturating_mul(3))
                {
                    // If the registration limit for the interval is exceeded, reject the transaction
                    return Err(InvalidTransaction::Custom(5).into());
                }
                Ok(ValidTransaction {
                    priority: Self::get_priority_vanilla(),
                    ..Default::default()
                })
            }
            Some(Call::register_network { .. }) => Ok(ValidTransaction {
                priority: Self::get_priority_vanilla(),
                ..Default::default()
            }),
            Some(Call::dissolve_network { .. }) => {
                if ColdkeySwapScheduled::<T>::contains_key(who) {
                    InvalidTransaction::Custom(CustomTransactionError::ColdkeyInSwapSchedule.into())
                        .into()
                } else {
                    Ok(ValidTransaction {
                        priority: Self::get_priority_vanilla(),
                        ..Default::default()
                    })
                }
            }
            _ => {
                if let Some(
                    BalancesCall::transfer_keep_alive { .. }
                    | BalancesCall::transfer_all { .. }
                    | BalancesCall::transfer_allow_death { .. },
                ) = call.is_sub_type()
                {
                    if ColdkeySwapScheduled::<T>::contains_key(who) {
                        return InvalidTransaction::Custom(
                            CustomTransactionError::ColdkeyInSwapSchedule.into(),
                        )
                        .into();
                    }
                }
                Ok(ValidTransaction {
                    priority: Self::get_priority_vanilla(),
                    ..Default::default()
                })
            }
        }
    }

    // NOTE: Add later when we put in a pre and post dispatch step.
    fn pre_dispatch(
        self,
        who: &Self::AccountId,
        call: &Self::Call,
        _info: &DispatchInfoOf<Self::Call>,
        _len: usize,
    ) -> Result<Self::Pre, TransactionValidityError> {
        match call.is_sub_type() {
            Some(Call::add_stake { .. }) => {
                let transaction_fee = 100000;
                Ok((CallType::AddStake, transaction_fee, who.clone()))
            }
            Some(Call::remove_stake { .. }) => {
                let transaction_fee = 0;
                Ok((CallType::RemoveStake, transaction_fee, who.clone()))
            }
            Some(Call::set_weights { .. }) => {
                let transaction_fee = 0;
                Ok((CallType::SetWeights, transaction_fee, who.clone()))
            }
            Some(Call::commit_weights { .. }) => {
                let transaction_fee = 0;
                Ok((CallType::SetWeights, transaction_fee, who.clone()))
            }
            Some(Call::reveal_weights { .. }) => {
                let transaction_fee = 0;
                Ok((CallType::SetWeights, transaction_fee, who.clone()))
            }
            Some(Call::register { .. }) => {
                let transaction_fee = 0;
                Ok((CallType::Register, transaction_fee, who.clone()))
            }
            Some(Call::serve_axon { .. }) => {
                let transaction_fee = 0;
                Ok((CallType::Serve, transaction_fee, who.clone()))
            }
            Some(Call::serve_axon_tls { .. }) => {
                let transaction_fee = 0;
                Ok((CallType::Serve, transaction_fee, who.clone()))
            }
            Some(Call::register_network { .. }) => {
                let transaction_fee = 0;
                Ok((CallType::RegisterNetwork, transaction_fee, who.clone()))
            }
            _ => {
                let transaction_fee = 0;
                Ok((CallType::Other, transaction_fee, who.clone()))
            }
        }
    }

    fn post_dispatch(
        maybe_pre: Option<Self::Pre>,
        _info: &DispatchInfoOf<Self::Call>,
        _post_info: &PostDispatchInfoOf<Self::Call>,
        _len: usize,
        _result: &dispatch::DispatchResult,
    ) -> Result<(), TransactionValidityError> {
        if let Some((call_type, _transaction_fee, _who)) = maybe_pre {
            match call_type {
                CallType::SetWeights => {
                    log::debug!("Not Implemented!");
                }
                CallType::AddStake => {
                    log::debug!("Not Implemented! Need to add potential transaction fees here.");
                }
                CallType::RemoveStake => {
                    log::debug!("Not Implemented! Need to add potential transaction fees here.");
                }
                CallType::Register => {
                    log::debug!("Not Implemented!");
                }
                _ => {
                    log::debug!("Not Implemented!");
                }
            }
        }
        Ok(())
    }
}

use sp_std::vec;

// TODO: unravel this rats nest, for some reason rustc thinks this is unused even though it's
// used not 25 lines below
#[allow(unused)]
use sp_std::vec::Vec;
use subtensor_macros::freeze_struct;

/// Trait for managing a membership pallet instance in the runtime
pub trait MemberManagement<AccountId> {
    /// Add member
    fn add_member(account: &AccountId) -> DispatchResultWithPostInfo;

    /// Remove a member
    fn remove_member(account: &AccountId) -> DispatchResultWithPostInfo;

    /// Swap member
    fn swap_member(remove: &AccountId, add: &AccountId) -> DispatchResultWithPostInfo;

    /// Get all members
    fn members() -> Vec<AccountId>;

    /// Check if an account is apart of the set
    fn is_member(account: &AccountId) -> bool;

    /// Get our maximum member count
    fn max_members() -> u32;
}

impl<T> MemberManagement<T> for () {
    /// Add member
    fn add_member(_: &T) -> DispatchResultWithPostInfo {
        Ok(().into())
    }

    // Remove a member
    fn remove_member(_: &T) -> DispatchResultWithPostInfo {
        Ok(().into())
    }

    // Swap member
    fn swap_member(_: &T, _: &T) -> DispatchResultWithPostInfo {
        Ok(().into())
    }

    // Get all members
    fn members() -> Vec<T> {
        vec![]
    }

    // Check if an account is apart of the set
    fn is_member(_: &T) -> bool {
        false
    }

    fn max_members() -> u32 {
        0
    }
}

/// Trait for interacting with collective pallets
pub trait CollectiveInterface<AccountId, Hash, ProposalIndex> {
    /// Remove vote
    fn remove_votes(hotkey: &AccountId) -> Result<bool, DispatchError>;

    fn add_vote(
        hotkey: &AccountId,
        proposal: Hash,
        index: ProposalIndex,
        approve: bool,
    ) -> Result<bool, DispatchError>;
}

impl<T, H, P> CollectiveInterface<T, H, P> for () {
    fn remove_votes(_: &T) -> Result<bool, DispatchError> {
        Ok(true)
    }

    fn add_vote(_: &T, _: H, _: P, _: bool) -> Result<bool, DispatchError> {
        Ok(true)
    }
}
