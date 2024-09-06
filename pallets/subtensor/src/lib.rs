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

    // removed all migrations
    // TODO add back.
    use crate::migrations;
    use frame_support::{
        dispatch::GetDispatchInfo,
        pallet_prelude::{DispatchResult, StorageMap, ValueQuery, *},
        traits::{tokens::fungible, UnfilteredDispatchable},
    };
    use frame_system::pallet_prelude::*;
    use sp_core::H256;
    use sp_runtime::traits::TrailingZeroInput;
    use sp_std::vec;
    use sp_std::vec::Vec;

    #[cfg(not(feature = "std"))]
    use alloc::boxed::Box;
    #[cfg(feature = "std")]
    use sp_std::prelude::Box;

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

    /// Data structure for Axon information.
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

    ///  Struct for Prometheus.
    pub type PrometheusInfoOf = PrometheusInfo;
    /// Data structure for Prometheus information.
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

    ///  Struct for Prometheus.
    pub type ChainIdentityOf = ChainIdentity;
    /// Data structure for Prometheus information.
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

    /// ============================
    /// ==== Staking + Accounts ====
    /// ============================

    #[pallet::type_value]
    /// Default value for zero.
    pub fn DefaultZeroU64<T: Config>() -> u64 {
        0
    }
    #[pallet::type_value]
    /// Default value for zero.
    pub fn DefaultZeroU16<T: Config>() -> u16 {
        0
    }
    #[pallet::type_value]
    /// Default value for false.
    pub fn DefaultFalse<T: Config>() -> bool {
        false
    }
    #[pallet::type_value]
    /// Default value for false.
    pub fn DefaultTrue<T: Config>() -> bool {
        true
    }
    #[pallet::type_value]
    /// Total Rao in circulation.
    pub fn TotalSupply<T: Config>() -> u64 {
        21_000_000_000_000_000
    }
    #[pallet::type_value]
    /// Default total stake.
    pub fn DefaultDefaultTake<T: Config>() -> u16 {
        T::InitialDefaultTake::get()
    }
    #[pallet::type_value]
    /// Default minimum take.
    pub fn DefaultMinTake<T: Config>() -> u16 {
        T::InitialMinTake::get()
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
    /// Default value for hotkeys.
    pub fn DefaultHotkeys<T: Config>() -> Vec<u16> {
        vec![]
    }
    #[pallet::type_value]
    /// Default value for network immunity period.
    pub fn DefaultNetworkImmunityPeriod<T: Config>() -> u64 {
        T::InitialNetworkImmunityPeriod::get()
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
    /// Default value for subnet owner.
    pub fn DefaultSubnetOwner<T: Config>() -> T::AccountId {
        T::AccountId::decode(&mut sp_runtime::traits::TrailingZeroInput::zeroes())
            .expect("trailing zeroes always produce a valid account ID; qed")
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
    /// Default value for serving rate limit.
    pub fn DefaultServingRateLimit<T: Config>() -> u64 {
        T::InitialServingRateLimit::get()
    }
    #[pallet::type_value]
    /// Default value for weight commit reveal interval.
    pub fn DefaultWeightCommitRevealInterval<T: Config>() -> u64 {
        1000
    }
    #[pallet::type_value]
    /// Senate requirements
    pub fn DefaultSenateRequiredStakePercentage<T: Config>() -> u64 {
        T::InitialSenateRequiredStakePercentage::get()
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
    /// Default value for lock interval blocks.
    pub fn DefaultLockIntervalBlocks<T: Config>() -> u64 {
        7200 * 180 // 180 days.
    }
    #[pallet::type_value]
    /// Default value for u16 max.
    pub fn DefaultMaxU16<T: Config>() -> u16 {
        u16::MAX
    }
    #[pallet::type_value]
    /// Default value for u16 max.
    pub fn DefaultMaxTempo<T: Config>() -> u16 {
        300 * 4 // 4 hours.
    }
    #[pallet::type_value]
    /// Default value for global weight.
    pub fn DefaultGlobalWeight<T: Config>() -> u64 {
        T::InitialGlobalWeight::get()
    }

    #[pallet::storage]
    pub type SenateRequiredStakePercentage<T> =
        StorageValue<_, u64, ValueQuery, DefaultSenateRequiredStakePercentage<T>>;

    /// ==================
    /// ==== Coinbase ====
    /// ==================
    #[pallet::storage] // --- ITEM ( global_block_emission )
    pub type BlockEmission<T> = StorageValue<_, u64, ValueQuery, DefaultBlockEmission<T>>;
    #[pallet::storage] // --- ITEM ( hotkey_emission_tempo )
    pub type HotkeyEmissionTempo<T> =
        StorageValue<_, u64, ValueQuery, DefaultHotkeyEmissionTempo<T>>;
    #[pallet::storage] // --- Map ( hot ) --> last_hotkey_emission_drain | Last block we drained this hotkey's emission.
    pub type LastHotkeyEmissionDrain<T: Config> =
        StorageMap<_, Blake2_128Concat, T::AccountId, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- DMap ( hot, netuid ) --> emission | Accumulated hotkey emission.
    pub type PendingdHotkeyEmissionOnNetuid<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        u16,
        u64,
        ValueQuery,
        DefaultZeroU64<T>,
    >;
    #[pallet::storage] // --- DMap ( hot, netuid ) --> emission | last hotkey emission on network.
    pub type LastHotkeyEmissionOnNetuid<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        u16,
        u64,
        ValueQuery,
        DefaultZeroU64<T>,
    >;
    #[pallet::storage] // --- NMAP ( hot, cold, netuid ) --> last_emission_on_hot_cold_net | Returns the last_emission_update_on_hot_cold_net
    pub type LastHotkeyColdkeyEmissionOnNetuid<T: Config> = StorageNMap<
        _,
        (
            NMapKey<Blake2_128Concat, T::AccountId>, // hot
            NMapKey<Blake2_128Concat, T::AccountId>, // cold
            NMapKey<Identity, u16>,                  // subnet
        ),
        u64, // Stake
        ValueQuery,
    >;

    /// ==========================
    /// ==== Staking Counters ====
    /// ==========================
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
    #[pallet::storage] // --- DMAP ( netuid ) --> tao_in_subnet | Returns the amount of TAO in the subnet.
    pub type SubnetTAO<T: Config> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> alpha_supply_in_pool | Returns the amount of alpha in the subnet.
    pub type SubnetAlphaIn<T: Config> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> alpha_supply_in_subnet | Returns the amount of alpha in the subnet.
    pub type SubnetAlphaOut<T: Config> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- DMAP ( hot, cold ) --> stake | Returns the stake under a coldkey prefixed by hotkey.
    pub type Stake<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        T::AccountId,
        u64,
        ValueQuery,
        DefaultZeroU64<T>,
    >;
    #[pallet::storage] // --- DMAP ( cold, netuid ) --> alpha | Returns the total amount of alpha a coldkey owns.
    pub type TotalColdkeyAlpha<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        u16,
        u64,
        ValueQuery,
        DefaultZeroU64<T>,
    >;
    #[pallet::storage] // --- DMAP ( hot, netuid ) --> alpha | Returns the total amount of alpha a hotkey owns.
    pub type TotalHotkeyAlpha<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        u16,
        u64,
        ValueQuery,
        DefaultZeroU64<T>,
    >;
    #[pallet::storage] // --- NMAP ( hot, cold, netuid ) --> alpha | Returns the alpha for an account on a subnet.
    pub type Alpha<T: Config> = StorageNMap<
        _,
        (
            NMapKey<Blake2_128Concat, T::AccountId>, // hot
            NMapKey<Blake2_128Concat, T::AccountId>, // cold
            NMapKey<Identity, u16>,                  // subnet
        ),
        u64, // Stake
        ValueQuery,
    >;

    /// ============================
    /// ==== Staking Variables ====
    /// ============================
    #[pallet::storage] // --- ITEM ( global_weight )
    pub type GlobalWeight<T> = StorageValue<_, u64, ValueQuery, DefaultGlobalWeight<T>>;
    #[pallet::storage] // --- ITEM ( default_take )
    pub type MaxTake<T> = StorageValue<_, u16, ValueQuery, DefaultDefaultTake<T>>;
    #[pallet::storage] // --- ITEM ( min_take )
    pub type MinTake<T> = StorageValue<_, u16, ValueQuery, DefaultMinTake<T>>;
    #[pallet::storage] // --- ITEM (target_stakes_per_interval)
    pub type TargetStakesPerInterval<T> =
        StorageValue<_, u64, ValueQuery, DefaultTargetStakesPerInterval<T>>;
    #[pallet::storage] // --- ITEM (default_stake_interval)
    pub type StakeInterval<T> = StorageValue<_, u64, ValueQuery, DefaultStakeInterval<T>>;
    #[pallet::storage] // --- MAP (hot, cold) --> stake | Returns a tuple (u64: stakes, u64: block_number)
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
    #[pallet::storage] // --- MAP ( hot ) --> cold | Returns the controlling coldkey for a hotkey.
    pub type Owner<T: Config> =
        StorageMap<_, Blake2_128Concat, T::AccountId, T::AccountId, ValueQuery, DefaultAccount<T>>;
    #[pallet::storage] // --- MAP ( hot ) --> take | Returns the hotkey delegation take. And signals that this key is open for delegation.
    pub type Delegates<T: Config> =
        StorageMap<_, Blake2_128Concat, T::AccountId, u16, ValueQuery, DefaultDefaultTake<T>>;
    #[pallet::storage] // --- Map ( hot, cold ) --> block_number | Last add stake increase.
    pub type LastAddStakeIncrease<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        T::AccountId,
        u64,
        ValueQuery,
        DefaultZeroU64<T>,
    >;
    #[pallet::storage] // --- DMAP ( parent, netuid ) --> Vec<(proportion,child)>
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
    #[pallet::storage] // --- DMAP ( child, netuid ) --> Vec<(proportion,parent)>
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

    /// ============================
    /// ==== Global Parameters =====
    /// ============================
    #[pallet::storage] // --- StorageItem Global Used Work.
    pub type UsedWork<T: Config> = StorageMap<_, Identity, Vec<u8>, u64, ValueQuery>;
    #[pallet::storage] // --- ITEM( global_max_registrations_per_block )
    pub type MaxRegistrationsPerBlock<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxRegistrationsPerBlock<T>>;
    #[pallet::storage] // --- ITEM( maximum_number_of_networks )
    pub type SubnetLimit<T> = StorageValue<_, u16, ValueQuery, DefaultSubnetLimit<T>>;
    #[pallet::storage] // --- ITEM( total_number_of_existing_networks )
    pub type TotalNetworks<T> = StorageValue<_, u16, ValueQuery>;
    #[pallet::storage] // --- ITEM( network_immunity_period )
    pub type NetworkImmunityPeriod<T> =
        StorageValue<_, u64, ValueQuery, DefaultNetworkImmunityPeriod<T>>;
    #[pallet::storage] // --- ITEM( network_last_registered_block )
    pub type NetworkLastRegistered<T> = StorageValue<_, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- ITEM( network_min_allowed_uids )
    pub type NetworkMinAllowedUids<T> =
        StorageValue<_, u16, ValueQuery, DefaultNetworkMinAllowedUids<T>>;
    #[pallet::storage] // --- ITEM( min_network_lock_cost )
    pub type NetworkMinLockCost<T> = StorageValue<_, u64, ValueQuery, DefaultNetworkMinLockCost<T>>;
    #[pallet::storage] // --- ITEM( last_network_lock_cost )
    pub type NetworkLastLockCost<T> =
        StorageValue<_, u64, ValueQuery, DefaultNetworkMinLockCost<T>>;
    #[pallet::storage] // --- ITEM( network_lock_reduction_interval )
    pub type NetworkLockReductionInterval<T> =
        StorageValue<_, u64, ValueQuery, DefaultNetworkLockReductionInterval<T>>;
    #[pallet::storage] // --- ITEM( subnet_owner_cut )
    pub type SubnetOwnerCut<T> = StorageValue<_, u16, ValueQuery, DefaultSubnetOwnerCut<T>>;
    #[pallet::storage] // --- ITEM( network_rate_limit )
    pub type NetworkRateLimit<T> = StorageValue<_, u64, ValueQuery, DefaultNetworkRateLimit<T>>;
    #[pallet::storage] // --- ITEM( nominator_min_required_stake )
    pub type NominatorMinRequiredStake<T> = StorageValue<_, u64, ValueQuery, DefaultZeroU64<T>>;

    /// ============================
    /// ==== Subnet Locks =====
    /// ============================
    #[pallet::storage] // --- MAP ( netuid ) --> subnet_owner
    pub type SubnetOwner<T: Config> =
        StorageMap<_, Identity, u16, T::AccountId, ValueQuery, DefaultSubnetOwner<T>>;
    // DEPRECATED
    #[pallet::storage] // --- MAP ( netuid ) --> total_subnet_locked
    pub type SubnetLocked<T: Config> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> largest_locked
    pub type LargestLocked<T: Config> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- ITEM( last_network_lock_cost )
    pub type LockIntervalBlocks<T> = StorageValue<_, u64, ValueQuery, DefaultLockIntervalBlocks<T>>;
    #[pallet::storage] // --- NMAP ( netuid, cold, hot ) --> (amount, start, end) | Returns the lock associated with a hotkey.
    pub type Locks<T: Config> = StorageNMap<
        _,
        (
            NMapKey<Identity, u16>,                  // subnet
            NMapKey<Blake2_128Concat, T::AccountId>, // hot
            NMapKey<Blake2_128Concat, T::AccountId>, // cold
        ),
        (u64, u64, u64), // Amount, Start, End
        ValueQuery,
    >;

    /// =================
    /// ==== Tempos =====
    /// =================
    #[pallet::storage] // --- ITEM( max_tempo )
    pub type AvgTempo<T> = StorageValue<_, u16, ValueQuery, DefaultTempo<T>>;
    #[pallet::storage] // --- ITEM( max_tempo )
    pub type MaxTempo<T> = StorageValue<_, u16, ValueQuery, DefaultMaxTempo<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> tempo
    pub type Tempo<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultTempo<T>>;

    /// ============================
    /// ==== Subnet Parameters =====
    /// ============================
    #[pallet::storage] // --- MAP ( netuid ) --> subnet mechanism
    pub type SubnetMechanism<T: Config> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultZeroU16<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> subnetwork_n (Number of UIDs in the network).
    pub type SubnetworkN<T: Config> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultZeroU16<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> modality   TEXT: 0, IMAGE: 1, TENSOR: 2
    pub type NetworkModality<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultZeroU16<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> network_is_added
    pub type NetworksAdded<T: Config> =
        StorageMap<_, Identity, u16, bool, ValueQuery, DefaultFalse<T>>;
    #[pallet::storage] // --- DMAP ( hotkey, netuid ) --> bool
    pub type IsNetworkMember<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Identity,
        u16,
        bool,
        ValueQuery,
        DefaultFalse<T>,
    >;
    #[pallet::storage] // --- MAP ( netuid ) --> network_registration_allowed
    pub type NetworkRegistrationAllowed<T: Config> =
        StorageMap<_, Identity, u16, bool, ValueQuery, DefaultFalse<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> network_pow_allowed
    pub type NetworkPowRegistrationAllowed<T: Config> =
        StorageMap<_, Identity, u16, bool, ValueQuery, DefaultFalse<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> block_created
    pub type NetworkRegisteredAt<T: Config> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> emission_values
    pub type EmissionValues<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> pending_emission
    pub type PendingEmission<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> blocks_since_last_step
    pub type BlocksSinceLastStep<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> last_mechanism_step_block
    pub type LastMechansimStepBlock<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> serving_rate_limit
    pub type ServingRateLimit<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultServingRateLimit<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> Rho
    pub type Rho<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultRho<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> Kappa
    pub type Kappa<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultKappa<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> uid, we use to record uids to prune at next epoch.
    pub type NeuronsToPruneAtNextEpoch<T: Config> = StorageMap<_, Identity, u16, u16, ValueQuery>;
    #[pallet::storage] // --- MAP ( netuid ) --> registrations_this_interval
    pub type RegistrationsThisInterval<T: Config> = StorageMap<_, Identity, u16, u16, ValueQuery>;
    #[pallet::storage] // --- MAP ( netuid ) --> pow_registrations_this_interval
    pub type POWRegistrationsThisInterval<T: Config> =
        StorageMap<_, Identity, u16, u16, ValueQuery>;
    #[pallet::storage] // --- MAP ( netuid ) --> burn_registrations_this_interval
    pub type BurnRegistrationsThisInterval<T: Config> =
        StorageMap<_, Identity, u16, u16, ValueQuery>;
    #[pallet::storage] // --- MAP ( netuid ) --> max_allowed_uids
    pub type MaxAllowedUids<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxAllowedUids<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> immunity_period
    pub type ImmunityPeriod<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultImmunityPeriod<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> activity_cutoff
    pub type ActivityCutoff<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultActivityCutoff<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> max_weight_limit
    pub type MaxWeightsLimit<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxWeightsLimit<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> weights_version_key
    pub type WeightsVersionKey<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultWeightsVersionKey<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> min_allowed_weights
    pub type MinAllowedWeights<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMinAllowedWeights<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> max_allowed_validators
    pub type MaxAllowedValidators<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxAllowedValidators<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> adjustment_interval
    pub type AdjustmentInterval<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultAdjustmentInterval<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> bonds_moving_average
    pub type BondsMovingAverage<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultBondsMovingAverage<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> weights_set_rate_limit
    pub type WeightsSetRateLimit<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultWeightsSetRateLimit<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> validator_prune_len
    pub type ValidatorPruneLen<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultValidatorPruneLen<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> scaling_law_power
    pub type ScalingLawPower<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultScalingLawPower<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> target_registrations_this_interval
    pub type TargetRegistrationsPerInterval<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultTargetRegistrationsPerInterval<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> adjustment_alpha
    pub type AdjustmentAlpha<T: Config> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultAdjustmentAlpha<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> interval
    pub type WeightCommitRevealInterval<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultWeightCommitRevealInterval<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> interval
    pub type CommitRevealWeightsEnabled<T> =
        StorageMap<_, Identity, u16, bool, ValueQuery, DefaultFalse<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> Burn
    pub type Burn<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultBurn<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> Difficulty
    pub type Difficulty<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultDifficulty<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> MinBurn
    pub type MinBurn<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMinBurn<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> MaxBurn
    pub type MaxBurn<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMaxBurn<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> MinDifficulty
    pub type MinDifficulty<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMinDifficulty<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> MaxDifficulty
    pub type MaxDifficulty<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMaxDifficulty<T>>;
    #[pallet::storage] // --- MAP ( netuid ) -->  Block at last adjustment.
    pub type LastAdjustmentBlock<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> Registrations of this Block.
    pub type RegistrationsThisBlock<T> =
        StorageMap<_, Identity, u16, u16, ValueQuery, DefaultZeroU16<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> global_RAO_recycled_for_registration
    pub type RAORecycledForRegistration<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultRAORecycledForRegistration<T>>;
    #[pallet::storage] // --- ITEM ( tx_rate_limit )
    pub type TxRateLimit<T> = StorageValue<_, u64, ValueQuery, DefaultTxRateLimit<T>>;
    #[pallet::storage] // --- ITEM ( tx_rate_limit )
    pub type TxDelegateTakeRateLimit<T> =
        StorageValue<_, u64, ValueQuery, DefaultTxDelegateTakeRateLimit<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> Whether or not Liquid Alpha is enabled
    pub type LiquidAlphaOn<T> =
        StorageMap<_, Blake2_128Concat, u16, bool, ValueQuery, DefaultFalse<T>>;
    #[pallet::storage] // --- MAP ( netuid ) --> (alpha_low, alpha_high)
    pub type AlphaValues<T> =
        StorageMap<_, Identity, u16, (u16, u16), ValueQuery, DefaultAlphaValues<T>>;
    #[pallet::storage]
    pub type NetworkMaxStake<T> =
        StorageMap<_, Identity, u16, u64, ValueQuery, DefaultNetworkMaxStake<T>>;

    /// =======================================
    /// ==== Subnetwork Consensus Storage  ====
    /// =======================================
    #[pallet::storage] // --- DMAP ( netuid ) --> local_stake_values | weight for stake used in YC.
    pub type LocalStake<T: Config> =
        StorageMap<_, Identity, u16, Vec<u64>, ValueQuery, EmptyU64Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> global_stake_values | weight for stake used in YC.
    pub type GlobalStake<T: Config> =
        StorageMap<_, Identity, u16, Vec<u64>, ValueQuery, EmptyU64Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> stake_weight | weight for stake used in YC.
    pub type StakeWeight<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid, hotkey ) --> uid
    pub type Uids<T: Config> =
        StorageDoubleMap<_, Identity, u16, Blake2_128Concat, T::AccountId, u16, OptionQuery>;
    #[pallet::storage] // --- DMAP ( netuid, uid ) --> hotkey
    pub type Keys<T: Config> =
        StorageDoubleMap<_, Identity, u16, Identity, u16, T::AccountId, ValueQuery, DefaultKey<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> (hotkey, se, ve)
    pub type LoadedEmission<T: Config> =
        StorageMap<_, Identity, u16, Vec<(T::AccountId, u64, u64)>, OptionQuery>;
    #[pallet::storage] // --- DMAP ( netuid ) --> active
    pub type Active<T: Config> =
        StorageMap<_, Identity, u16, Vec<bool>, ValueQuery, EmptyBoolVec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> rank
    pub type Rank<T: Config> = StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> trust
    pub type Trust<T: Config> = StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> consensus
    pub type Consensus<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> incentive
    pub type Incentive<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> dividends
    pub type Dividends<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> emission
    pub type Emission<T: Config> =
        StorageMap<_, Identity, u16, Vec<u64>, ValueQuery, EmptyU64Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> last_update
    pub type LastUpdate<T: Config> =
        StorageMap<_, Identity, u16, Vec<u64>, ValueQuery, EmptyU64Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> validator_trust
    pub type ValidatorTrust<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> pruning_scores
    pub type PruningScores<T: Config> =
        StorageMap<_, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
    #[pallet::storage] // --- DMAP ( netuid ) --> validator_permit
    pub type ValidatorPermit<T: Config> =
        StorageMap<_, Identity, u16, Vec<bool>, ValueQuery, EmptyBoolVec<T>>;
    #[pallet::storage] // --- DMAP ( netuid, uid ) --> weights
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
    #[pallet::storage] // --- DMAP ( netuid, uid ) --> bonds
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
    #[pallet::storage] // --- DMAP ( netuid, uid ) --> block_at_registration
    pub type BlockAtRegistration<T: Config> =
        StorageDoubleMap<_, Identity, u16, Identity, u16, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- MAP ( netuid, hotkey ) --> axon_info
    pub type Axons<T: Config> =
        StorageDoubleMap<_, Identity, u16, Blake2_128Concat, T::AccountId, AxonInfoOf, OptionQuery>;
    #[pallet::storage] // --- MAP ( netuid, hotkey ) --> prometheus_info
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
        StorageMap<_, Identity, T::AccountId, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- MAP ( key ) --> last_block
    pub type LastTxBlockDelegateTake<T: Config> =
        StorageMap<_, Identity, T::AccountId, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- ITEM( weights_min_stake )
    pub type WeightsMinStake<T> = StorageValue<_, u64, ValueQuery, DefaultZeroU64<T>>;
    #[pallet::storage] // --- MAP (netuid, who) --> (hash, weight) | Returns the hash and weight committed by an account for a given netuid.
    pub type WeightCommits<T: Config> = StorageDoubleMap<
        _,
        Twox64Concat,
        u16,
        Twox64Concat,
        T::AccountId,
        (H256, u64),
        OptionQuery,
    >;

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
                // TODO rethink this.
                let _stake = Self::get_global_for_hotkey(hotkey);
                let current_block_number: u64 = Self::get_current_block_as_u64();
                let default_priority: u64 =
                    current_block_number.saturating_sub(Self::get_last_update_for_uid(netuid, uid));
                return default_priority.saturating_add(u32::MAX as u64);
            }
            0
        }

        /// Is the caller allowed to set weights
        pub fn check_weights_min_stake(hotkey: &T::AccountId, netuid: u16) -> bool {
            // Blacklist weights transactions for low stake peers.
            let min_stake = Self::get_weights_min_stake();
            let hotkey_stake = Self::get_stake_for_hotkey_on_subnet(hotkey, netuid);
            let result = hotkey_stake >= min_stake;
            log::info!(
                "Checking weights min stake for hotkey: {:?}, netuid: {}, min_stake: {}, hotkey_stake: {}, result: {}",
                hotkey,
                netuid,
                min_stake,
                hotkey_stake,
                result
            );
            result
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

#[freeze_struct("61e2b893d5ce6701")]
#[derive(Encode, Decode, Clone, Eq, PartialEq, TypeInfo)]
pub struct SubtensorSignedExtension<T: Config + Send + Sync + TypeInfo>(pub PhantomData<T>);

impl<T: Config + Send + Sync + TypeInfo> Default for SubtensorSignedExtension<T>
where
    T::RuntimeCall: Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo>,
    <T as frame_system::Config>::RuntimeCall: IsSubType<Call<T>>,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Config + Send + Sync + TypeInfo> SubtensorSignedExtension<T>
where
    T::RuntimeCall: Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo>,
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

    pub fn check_weights_min_stake(who: &T::AccountId, netuid: u16) -> bool {
        Pallet::<T>::check_weights_min_stake(who, netuid)
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
    T::RuntimeCall: Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo>,
    <T as frame_system::Config>::RuntimeCall: IsSubType<Call<T>>,
    <T as frame_system::Config>::RuntimeCall: IsSubType<BalancesCall<T>>,
{
    const IDENTIFIER: &'static str = "SubtensorSignedExtension";

    type AccountId = T::AccountId;
    type Call = T::RuntimeCall;
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
                if Self::check_weights_min_stake(who, *netuid) {
                    let priority: u64 = Self::get_priority_set_weights(who, *netuid);
                    Ok(ValidTransaction {
                        priority,
                        longevity: 1,
                        ..Default::default()
                    })
                } else {
                    Err(InvalidTransaction::Call.into())
                }
            }
            Some(Call::reveal_weights { netuid, .. }) => {
                if Self::check_weights_min_stake(who, *netuid) {
                    let priority: u64 = Self::get_priority_set_weights(who, *netuid);
                    Ok(ValidTransaction {
                        priority,
                        longevity: 1,
                        ..Default::default()
                    })
                } else {
                    Err(InvalidTransaction::Call.into())
                }
            }
            Some(Call::set_weights { netuid, .. }) => {
                if Self::check_weights_min_stake(who, *netuid) {
                    let priority: u64 = Self::get_priority_set_weights(who, *netuid);
                    Ok(ValidTransaction {
                        priority,
                        longevity: 1,
                        ..Default::default()
                    })
                } else {
                    Err(InvalidTransaction::Call.into())
                }
            }
            Some(Call::set_root_weights { netuid, hotkey, .. }) => {
                if Self::check_weights_min_stake(hotkey, *netuid) {
                    let priority: u64 = Self::get_priority_set_weights(hotkey, *netuid);
                    Ok(ValidTransaction {
                        priority,
                        longevity: 1,
                        ..Default::default()
                    })
                } else {
                    Err(InvalidTransaction::Call.into())
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
                    return InvalidTransaction::ExhaustsResources.into();
                }
                Ok(ValidTransaction {
                    priority: Self::get_priority_vanilla(),
                    ..Default::default()
                })
            }
            _ => Ok(ValidTransaction {
                priority: Self::get_priority_vanilla(),
                ..Default::default()
            }),
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
