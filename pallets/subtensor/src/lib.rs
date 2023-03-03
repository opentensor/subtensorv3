#![cfg_attr(not(feature = "std"), no_std)]
#![recursion_limit = "512"]
/// Edit this file to define custom logic or remove it if it is not needed.
/// Learn more about FRAME and the core library of Substrate FRAME pallets:
/// <https://docs.substrate.io/reference/frame-pallets/>
pub use pallet::*;

use frame_system::{
	self as system,
	ensure_signed
};

use frame_support::{
	dispatch,
	dispatch::{
		DispatchInfo,
		PostDispatchInfo
	}, ensure, 
	traits::{
		Currency, 
		ExistenceRequirement,
		tokens::{
			WithdrawReasons
		},
		IsSubType,
		}
};

use sp_std::marker::PhantomData;
use codec::{Decode, Encode};
use sp_runtime::{
	traits::{
		Dispatchable,
		DispatchInfoOf,
		SignedExtension,
		PostDispatchInfoOf
	},
	transaction_validity::{
		TransactionValidity,
		TransactionValidityError
	}
};
use scale_info::TypeInfo;
use frame_support::sp_runtime::transaction_validity::ValidTransaction;

/// ============================
///	==== Benchmark Imports =====
/// ============================
#[cfg(feature = "runtime-benchmarks")]
mod benchmarks;

/// =========================
///	==== Pallet Imports =====
/// =========================
mod block_step;

mod epoch;
mod math;
mod networks;
mod registration;
mod serving;
mod staking;
mod utils;
mod uids;
mod weights;

pub mod delegate_info;
pub mod neuron_info;
pub mod subnet_info;

#[frame_support::pallet]
pub mod pallet {
	use frame_support::pallet_prelude::*;
	use frame_system::pallet_prelude::*;
	use frame_support::traits::{Currency};
	use frame_support::sp_std::vec;
	use serde::{Serialize, Deserialize};
	use serde_with::{serde_as, DisplayFromStr};
	use frame_support::inherent::Vec;


	#[pallet::pallet]
	#[pallet::generate_store(pub(super) trait Store)]
	#[pallet::without_storage_info]
	pub struct Pallet<T>(_);

	/// Configure the pallet by specifying the parameters and types on which it depends.
	#[pallet::config]
	pub trait Config: frame_system::Config {
		/// Because this pallet emits events, it depends on the runtime's definition of an event.
		type RuntimeEvent: From<Event<Self>> + IsType<<Self as frame_system::Config>::RuntimeEvent>;

		/// --- Currency type that will be used to place deposits on neurons
		type Currency: Currency<Self::AccountId> + Send + Sync;

		/// =================================
		/// ==== Initial Value Constants ====
		/// =================================
		#[pallet::constant] /// Initial currency issuance.
		type InitialIssuance: Get<u64>;
		#[pallet::constant] /// Initial min allowed weights setting.
		type InitialMinAllowedWeights: Get<u16>;
		#[pallet::constant] /// Initial Emission Ratio
		type InitialEmissionValue: Get<u16>;
		#[pallet::constant] /// Initial max weight limit.
		type InitialMaxWeightsLimit: Get<u16>;
		#[pallet::constant] /// Tempo for each network
		type InitialTempo: Get<u16>;
		#[pallet::constant] /// Initial Difficulty.
		type InitialDifficulty: Get<u64>;
		#[pallet::constant] /// Initial Max Difficulty.
		type InitialMaxDifficulty: Get<u64>;
		#[pallet::constant] /// Initial Min Difficulty.
		type InitialMinDifficulty: Get<u64>;
		#[pallet::constant] /// Initial Burn.
		type InitialBurn: Get<u64>;
		#[pallet::constant] /// Initial Max Burn.
		type InitialMaxBurn: Get<u64>;
		#[pallet::constant] /// Initial Min Burn.
		type InitialMinBurn: Get<u64>;
		#[pallet::constant] /// Initial adjustment interval.
		type InitialAdjustmentInterval: Get<u16>;
		#[pallet::constant] /// Initial bonds moving average.
		type InitialBondsMovingAverage: Get<u64>;
		#[pallet::constant] /// Initial target registrations per interval.
		type InitialTargetRegistrationsPerInterval: Get<u16>;
		#[pallet::constant] /// Initial number of weight cuts in epoch.
		type InitialWeightCuts: Get<u16>;
		#[pallet::constant] /// Rho constant
		type InitialRho: Get<u16>;
		#[pallet::constant] /// Kappa constant
		type InitialKappa: Get<u16>;		
		#[pallet::constant] /// Max UID constant.
		type InitialMaxAllowedUids: Get<u16>;
		#[pallet::constant] /// Default Batch size.
		type InitialValidatorBatchSize: Get<u16>;
		#[pallet::constant] /// Default Batch size.
		type InitialValidatorSequenceLen: Get<u16>;
		#[pallet::constant] /// Default Epoch length.
		type InitialValidatorEpochLen: Get<u16>;
		#[pallet::constant] /// Default Reset length.
		type InitialValidatorEpochsPerReset: Get<u16>;
		#[pallet::constant] /// Initial validator exclude quantile.
		type InitialValidatorExcludeQuantile: Get<u16>;
		#[pallet::constant] /// Initial validator logits divergence penalty/threshold.
		type InitialValidatorLogitsDivergence: Get<u64>;
		#[pallet::constant] /// Initial validator context pruning length.
		type InitialValidatorPruneLen: Get<u64>; 
		#[pallet::constant] /// Initial scaling law power.
		type InitialScalingLawPower: Get<u16>;
		#[pallet::constant] /// Initial synergy scaling law power.
		type InitialSynergyScalingLawPower: Get<u16>;
		#[pallet::constant] /// Immunity Period Constant.
		type InitialImmunityPeriod: Get<u16>;
		#[pallet::constant] /// Activity constant
		type InitialActivityCutoff: Get<u16>;
		#[pallet::constant] /// Initial max registrations per block.
		type InitialMaxRegistrationsPerBlock: Get<u16>;
		#[pallet::constant] /// Initial pruning score for each neuron
		type InitialPruningScore: Get<u16>;	
		#[pallet::constant] /// Initial allowed validators per network.
		type InitialMaxAllowedValidators: Get<u16>;
		#[pallet::constant] /// Initial default delegation take.
		type InitialDefaultTake: Get<u16>;
		#[pallet::constant] /// Initial weights version key.
		type InitialWeightsVersionKey: Get<u64>;
		#[pallet::constant] /// Initial serving rate limit.
		type InitialServingRateLimit: Get<u64>;
	}

	pub type AccountIdOf<T> = <T as frame_system::Config>::AccountId;

	#[derive(Decode, Encode, Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
	pub struct DeAccountId { // allows us to de/serialize the account id as a u8 vec
		#[serde(with = "serde_bytes")]
		id: Vec<u8>
	}

	impl From<Vec<u8>> for DeAccountId {
		fn from(v: Vec<u8>) -> Self {
			DeAccountId {
				id: v.clone()
			}
		}
	}

	/// ============================
	/// ==== Staking + Accounts ====
	/// ============================
	#[pallet::type_value] 
	pub fn DefaultDefaultTake<T: Config>() -> u16 { T::InitialDefaultTake::get() }
	#[pallet::type_value] 
	pub fn DefaultAccountTake<T: Config>() -> u64 { 0 }
	#[pallet::type_value]
	pub fn DefaultBlockEmission<T: Config>() -> u64 {1_000_000_000}
	#[pallet::type_value] 
	pub fn DefaultAllowsDelegation<T: Config>() -> bool { false }
	#[pallet::type_value] 
	pub fn DefaultTotalIssuance<T: Config>() -> u64 { T::InitialIssuance::get() }
	#[pallet::type_value] 
	pub fn DefaultAccount<T: Config>() -> T::AccountId { T::AccountId::decode(&mut sp_runtime::traits::TrailingZeroInput::zeroes()).unwrap()}

	#[pallet::storage] /// --- ITEM ( total_stake )
	pub type TotalStake<T> = StorageValue<_, u64, ValueQuery>;
	#[pallet::storage] /// --- ITEM ( default_take )
	pub type DefaultTake<T> = StorageValue<_, u16, ValueQuery, DefaultDefaultTake<T>>;
	#[pallet::storage] /// --- ITEM ( global_block_emission )
	pub type BlockEmission<T> = StorageValue<_, u64, ValueQuery, DefaultBlockEmission<T>>;
	#[pallet::storage] /// --- ITEM ( total_issuance )
	pub type TotalIssuance<T> = StorageValue<_, u64, ValueQuery, DefaultTotalIssuance<T>>;
	#[pallet::storage] /// --- MAP ( hot ) --> stake | Returns the total amount of stake under a hotkey.
    pub type TotalHotkeyStake<T:Config> = StorageMap<_, Identity, T::AccountId, u64, ValueQuery, DefaultAccountTake<T>>;
	#[pallet::storage] /// --- MAP ( cold ) --> stake | Returns the total amount of stake under a coldkey.
    pub type TotalColdkeyStake<T:Config> = StorageMap<_, Identity, T::AccountId, u64, ValueQuery, DefaultAccountTake<T>>;
	#[pallet::storage] /// --- MAP ( hot ) --> cold | Returns the controlling coldkey for a hotkey.
    pub type Owner<T:Config> = StorageMap<_, Blake2_128Concat, T::AccountId, T::AccountId, ValueQuery, DefaultAccount<T>>;
	#[pallet::storage] /// --- MAP ( hot ) --> take | Returns the hotkey delegation take. And signals that this key is open for delegation.
    pub type Delegates<T:Config> = StorageMap<_, Blake2_128Concat, T::AccountId, u16, ValueQuery, DefaultDefaultTake<T>>;
	#[pallet::storage] /// --- DMAP ( hot, cold ) --> stake | Returns the stake under a hotkey prefixed by hotkey.
    pub type Stake<T:Config> = StorageDoubleMap<_, Blake2_128Concat, T::AccountId, Identity, T::AccountId, u64, ValueQuery, DefaultAccountTake<T>>;

	/// =====================================
	/// ==== Difficulty / Registrations =====
	/// =====================================
	#[pallet::type_value] 
	pub fn DefaultLastAdjustmentBlock<T: Config>() -> u64 { 0 }
	#[pallet::type_value]
	pub fn DefaultRegistrationsThisBlock<T: Config>() ->  u16 { 0}
	#[pallet::type_value]
	pub fn DefaultBurn<T: Config>() -> u64 { T::InitialBurn::get() }
	#[pallet::type_value]
	pub fn DefaultMinBurn<T: Config>() -> u64 { T::InitialMinBurn::get()  }
	#[pallet::type_value]
	pub fn DefaultMaxBurn<T: Config>() -> u64 { T::InitialMaxBurn::get() }
	#[pallet::type_value]
	pub fn DefaultDifficulty<T: Config>() -> u64 { T::InitialDifficulty::get() }
	#[pallet::type_value]
	pub fn DefaultMinDifficulty<T: Config>() -> u64 { T::InitialMinDifficulty::get()  }
	#[pallet::type_value]
	pub fn DefaultMaxDifficulty<T: Config>() -> u64 { T::InitialMaxDifficulty::get() }
	#[pallet::type_value] 
	pub fn DefaultMaxRegistrationsPerBlock<T: Config>() -> u16 { T::InitialMaxRegistrationsPerBlock::get() }

	#[pallet::storage] /// ---- StorageItem Global Used Work.
    pub type UsedWork<T:Config> = StorageMap<_, Identity, Vec<u8>, u64, ValueQuery>;
	#[pallet::storage] /// --- MAP ( netuid ) --> Difficulty
	pub type Burn<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultBurn<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> Difficulty
	pub type Difficulty<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultDifficulty<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> MinBurn
	pub type MinBurn<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMinBurn<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> MaxBurn
	pub type MaxBurn<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMaxBurn<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> MinDifficulty
	pub type MinDifficulty<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMinDifficulty<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> MaxDifficulty
	pub type MaxDifficulty<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultMaxDifficulty<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) -->  Block at last adjustment.
	pub type LastAdjustmentBlock<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultLastAdjustmentBlock<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> Registration this Block.
	pub type RegistrationsThisBlock<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultRegistrationsThisBlock<T>>;
	#[pallet::storage] /// --- ITEM( global_max_registrations_per_block ) 
	pub type MaxRegistrationsPerBlock<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxRegistrationsPerBlock<T> >;

	/// ==============================
	/// ==== Subnetworks Storage =====
	/// ==============================
	#[pallet::type_value] 
	pub fn DefaultN<T:Config>() -> u16 { 0 }
	#[pallet::type_value] 
	pub fn DefaultModality<T:Config>() -> u16 { 0 }
	#[pallet::type_value] 
	pub fn DefaultHotkeys<T:Config>() -> Vec<u16> { vec![ ] }
	#[pallet::type_value]
	pub fn DefaultNeworksAdded<T: Config>() ->  bool { false }
	#[pallet::type_value]
	pub fn DefaultIsNetworkMember<T: Config>() ->  bool { false }


	#[pallet::storage] /// --- ITEM( tota_number_of_existing_networks )
	pub type TotalNetworks<T> = StorageValue<_, u16, ValueQuery>;
	#[pallet::storage] /// --- MAP ( netuid ) --> subnetwork_n (Number of UIDs in the network).
	pub type SubnetworkN<T:Config> = StorageMap< _, Identity, u16, u16, ValueQuery, DefaultN<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> modality   TEXT: 0, IMAGE: 1, TENSOR: 2
	pub type NetworkModality<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultModality<T>> ;
	#[pallet::storage] /// --- MAP ( netuid ) --> network_is_added
	pub type NetworksAdded<T:Config> = StorageMap<_, Identity, u16, bool, ValueQuery, DefaultNeworksAdded<T>>;	
	#[pallet::storage] /// --- DMAP ( netuid, netuid ) -> registration_requirement
	pub type NetworkConnect<T:Config> = StorageDoubleMap<_, Identity, u16, Identity, u16, u16, OptionQuery>;
	#[pallet::storage] /// --- DMAP ( hotkey, netuid ) --> bool
	pub type IsNetworkMember<T:Config> = StorageDoubleMap<_, Blake2_128Concat, T::AccountId, Identity, u16, bool, ValueQuery, DefaultIsNetworkMember<T>>;

	/// ==============================
	/// ==== Subnetwork Features =====
	/// ==============================
	#[pallet::type_value]
	pub fn DefaultEmissionValues<T: Config>() ->  u64 { 0 }
	#[pallet::type_value]
	pub fn DefaultPendingEmission<T: Config>() ->  u64 { 0 }
	#[pallet::type_value] 
	pub fn DefaultBlocksSinceLastStep<T: Config>() -> u64 { 0 }
	#[pallet::type_value] 
	pub fn DefaultLastMechansimStepBlock<T: Config>() -> u64 { 0 }
	#[pallet::type_value]
	pub fn DefaultTempo<T: Config>() -> u16 { T::InitialTempo::get() }

	#[pallet::storage] /// --- MAP ( netuid ) --> tempo
	pub type Tempo<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultTempo<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> emission_values
	pub type EmissionValues<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultEmissionValues<T>>;
	#[pallet::storage] /// --- MAP ( netuid ) --> pending_emission
	pub type PendingEmission<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultPendingEmission<T>>;
	#[pallet::storage] /// --- MAP ( netuid ) --> blocks_since_last_step.
	pub type BlocksSinceLastStep<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultBlocksSinceLastStep<T>>;
	#[pallet::storage] /// --- MAP ( netuid ) --> last_mechanism_step_block
	pub type LastMechansimStepBlock<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultLastMechansimStepBlock<T> >;

	/// =================================
	/// ==== Axon / Promo Endpoints =====
	/// =================================
	
	// --- Struct for Axon.
	pub type AxonInfoOf = AxonInfo;
	
	#[serde_as]
	#[derive(Encode, Decode, Default, TypeInfo, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
    pub struct AxonInfo {
		pub block: u64, // --- Axon serving block.
        pub version: u32, // --- Axon version
		#[serde_as(as = "DisplayFromStr")] // serialize as string, deserialize from string
        pub ip: u128, // --- Axon u128 encoded ip address of type v6 or v4.
        pub port: u16, // --- Axon u16 encoded port.
        pub ip_type: u8, // --- Axon ip type, 4 for ipv4 and 6 for ipv6.
		pub protocol: u8, // --- Axon protocol. TCP, UDP, other.
		pub placeholder1: u8, // --- Axon proto placeholder 1.
		pub placeholder2: u8, // --- Axon proto placeholder 1.
	}

	// --- Struct for Prometheus.
	pub type PrometheusInfoOf = PrometheusInfo;
	#[serde_as]
	#[derive(Encode, Decode, Default, TypeInfo, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
	pub struct PrometheusInfo {
		pub block: u64, // --- Prometheus serving block.
        pub version: u32, // --- Prometheus version.
		#[serde_as(as = "DisplayFromStr")] // serialize as string, deserialize from string
        pub ip: u128, // --- Prometheus u128 encoded ip address of type v6 or v4.
        pub port: u16, // --- Prometheus u16 encoded port.
        pub ip_type: u8, // --- Prometheus ip type, 4 for ipv4 and 6 for ipv6.
	}


	#[pallet::type_value] 
	pub fn DefaultServingRateLimit<T: Config>() -> u64 { T::InitialServingRateLimit::get() }

	#[pallet::storage] /// --- ITEM ( serving_rate_limit )
	pub(super) type ServingRateLimit<T> = StorageValue<_, u64, ValueQuery, DefaultServingRateLimit<T>>;
	#[pallet::storage] /// --- MAP ( hotkey ) --> axon_info
	pub(super) type Axons<T:Config> = StorageMap<_, Blake2_128Concat, T::AccountId, AxonInfoOf, OptionQuery>;
	#[pallet::storage] /// --- MAP ( hotkey ) --> prometheus_info
	pub(super) type Prometheus<T:Config> = StorageMap<_, Blake2_128Concat, T::AccountId, PrometheusInfoOf, OptionQuery>;

	/// =======================================
	/// ==== Subnetwork Hyperparam storage ====
	/// =======================================	
	#[pallet::type_value] 
	pub fn DefaultWeightsSetRateLimit<T: Config>() -> u64 { 0 }
	#[pallet::type_value] 
	pub fn DefaultBlockAtRegistration<T: Config>() -> u64 { 0 }
	#[pallet::type_value]
	pub fn DefaultWeightCuts<T: Config>() -> u16 { T::InitialWeightCuts::get() }
	#[pallet::type_value]
	pub fn DefaultRho<T: Config>() -> u16 { T::InitialRho::get() }
	#[pallet::type_value]
	pub fn DefaultKappa<T: Config>() -> u16 { T::InitialKappa::get() }
	#[pallet::type_value] 
	pub fn DefaultMaxAllowedUids<T: Config>() -> u16 { T::InitialMaxAllowedUids::get() }
	#[pallet::type_value] 
	pub fn DefaultImmunityPeriod<T: Config>() -> u16 { T::InitialImmunityPeriod::get() }
	#[pallet::type_value] 
	pub fn DefaultActivityCutoff<T: Config>() -> u16 { T::InitialActivityCutoff::get() }
	#[pallet::type_value] 
	pub fn DefaultMaxWeightsLimit<T: Config>() -> u16 { T::InitialMaxWeightsLimit::get() }
	#[pallet::type_value] 
	pub fn DefaultWeightsVersionKey<T: Config>() -> u64 { T::InitialWeightsVersionKey::get() }
	#[pallet::type_value] 
	pub fn DefaultMinAllowedWeights<T: Config>() -> u16 { T::InitialMinAllowedWeights::get() }
	#[pallet::type_value] 
	pub fn DefaultValidatorEpochLen<T: Config>() -> u16 { T::InitialValidatorEpochLen::get() }
	#[pallet::type_value] 
	pub fn DefaultMaxAllowedValidators<T: Config>() -> u16 { T::InitialMaxAllowedValidators::get() }
	#[pallet::type_value]
	pub fn DefaultAdjustmentInterval<T: Config>() -> u16 { T::InitialAdjustmentInterval::get() }
	#[pallet::type_value]
	pub fn DefaultBondsMovingAverage<T: Config>() -> u64 { T::InitialBondsMovingAverage::get() }
	#[pallet::type_value] 
	pub fn DefaultValidatorPruneLen<T: Config>() -> u64 { T::InitialValidatorPruneLen::get() }
	#[pallet::type_value] 
	pub fn DefaultValidatorBatchSize<T: Config>() -> u16 { T::InitialValidatorBatchSize::get() }
	#[pallet::type_value] 
	pub fn DefaultValidatorSequenceLen<T: Config>() -> u16 { T::InitialValidatorSequenceLen::get() }
	#[pallet::type_value] 
	pub fn DefaultValidatorEpochsPerReset<T: Config>() -> u16 { T::InitialValidatorEpochsPerReset::get() }
	#[pallet::type_value]
	pub fn DefaultValidatorExcludeQuantile<T: Config>() -> u16 { T::InitialValidatorExcludeQuantile::get() }
	#[pallet::type_value] 
	pub fn DefaultValidatorLogitsDivergence<T: Config>() -> u64 { T::InitialValidatorLogitsDivergence::get() }
	#[pallet::type_value]
	pub fn DefaultScalingLawPower<T: Config>() -> u16 { T::InitialScalingLawPower::get() }
	#[pallet::type_value]
	pub fn DefaultSynergyScalingLawPower<T: Config>() -> u16 { T::InitialSynergyScalingLawPower::get() }
	#[pallet::type_value] 
	pub fn DefaultTargetRegistrationsPerInterval<T: Config>() -> u16 { T::InitialTargetRegistrationsPerInterval::get() }


	#[pallet::storage] /// --- MAP ( netuid ) --> WeightCuts
	pub type WeightCuts<T> =  StorageMap<_, Identity, u16, u16, ValueQuery, DefaultWeightCuts<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> Rho
	pub type Rho<T> =  StorageMap<_, Identity, u16, u16, ValueQuery, DefaultRho<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> Kappa
	pub type Kappa<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultKappa<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> uid, we use to record uids to prune at next epoch.
    pub type NeuronsToPruneAtNextEpoch<T:Config> = StorageMap<_, Identity, u16, u16, ValueQuery>;
	#[pallet::storage] /// --- MAP ( netuid ) --> registrations_this_interval
	pub type RegistrationsThisInterval<T:Config> = StorageMap<_, Identity, u16, u16, ValueQuery>;
	#[pallet::storage] /// --- MAP ( netuid ) --> pow_registrations_this_interval
	pub type POWRegistrationsThisInterval<T:Config> = StorageMap<_, Identity, u16, u16, ValueQuery>;
	#[pallet::storage] /// --- MAP ( netuid ) --> burn_registrations_this_interval
	pub type BurnRegistrationsThisInterval<T:Config> = StorageMap<_, Identity, u16, u16, ValueQuery>;
	#[pallet::storage] /// --- MAP ( netuid ) --> max_allowed_uids
	pub type MaxAllowedUids<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxAllowedUids<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> immunity_period
	pub type ImmunityPeriod<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultImmunityPeriod<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> activity_cutoff
	pub type ActivityCutoff<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultActivityCutoff<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> max_weight_limit
	pub type MaxWeightsLimit<T> = StorageMap< _, Identity, u16, u16, ValueQuery, DefaultMaxWeightsLimit<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> weights_version_key
	pub type WeightsVersionKey<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultWeightsVersionKey<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> validator_epoch_len
	pub type ValidatorEpochLen<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultValidatorEpochLen<T> >; 
	#[pallet::storage] /// --- MAP ( netuid ) --> min_allowed_weights
	pub type MinAllowedWeights<T> = StorageMap< _, Identity, u16, u16, ValueQuery, DefaultMinAllowedWeights<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> max_allowed_validators
	pub type MaxAllowedValidators<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultMaxAllowedValidators<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> adjustment_interval
	pub type AdjustmentInterval<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultAdjustmentInterval<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> bonds_moving_average
	pub type BondsMovingAverage<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultBondsMovingAverage<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> validator_batch_size
	pub type ValidatorBatchSize<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultValidatorBatchSize<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> weights_set_rate_limit
	pub type WeightsSetRateLimit<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultWeightsSetRateLimit<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> validator_prune_len
	pub type ValidatorPruneLen<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultValidatorPruneLen<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> validator_sequence_length
	pub type ValidatorSequenceLength<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultValidatorSequenceLen<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> validator_epochs_per_reset
	pub type ValidatorEpochsPerReset<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultValidatorEpochsPerReset<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> validator_exclude_quantile
	pub type ValidatorExcludeQuantile<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultValidatorExcludeQuantile<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> validator_logits_divergence
	pub type ValidatorLogitsDivergence<T> = StorageMap<_, Identity, u16, u64, ValueQuery, DefaultValidatorLogitsDivergence<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> scaling_law_power
	pub type ScalingLawPower<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultScalingLawPower<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> synergy_scaling_law_power
	pub type SynergyScalingLawPower<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultSynergyScalingLawPower<T> >;
	#[pallet::storage] /// --- MAP ( netuid ) --> target_registrations_this_interval
	pub type TargetRegistrationsPerInterval<T> = StorageMap<_, Identity, u16, u16, ValueQuery, DefaultTargetRegistrationsPerInterval<T> >;
	#[pallet::storage] /// --- DMAP ( netuid, uid ) --> block_at_registration
	pub type BlockAtRegistration<T:Config> = StorageDoubleMap<_, Identity, u16, Identity, u16, u64, ValueQuery, DefaultBlockAtRegistration<T> >;

	/// =======================================
	/// ==== Subnetwork Consensus Storage  ====
	/// =======================================
	#[pallet::type_value] 
	pub fn EmptyU16Vec<T:Config>() -> Vec<u16> { vec![] }
	#[pallet::type_value] 
	pub fn EmptyU64Vec<T:Config>() -> Vec<u64> { vec![] }
	#[pallet::type_value] 
	pub fn EmptyBoolVec<T:Config>() -> Vec<bool> { vec![] }
	#[pallet::type_value] 
	pub fn DefaultBonds<T:Config>() -> Vec<(u16, u16)> { vec![] }
	#[pallet::type_value] 
	pub fn DefaultWeights<T:Config>() -> Vec<(u16, u16)> { vec![] }
	#[pallet::type_value] 
	pub fn DefaultKey<T:Config>() -> T::AccountId { T::AccountId::decode(&mut sp_runtime::traits::TrailingZeroInput::zeroes()).unwrap() }

	#[pallet::storage] /// --- DMAP ( netuid, hotkey ) --> uid
	pub(super) type Uids<T:Config> = StorageDoubleMap<_, Identity, u16, Blake2_128Concat, T::AccountId, u16, OptionQuery>;
	#[pallet::storage] /// --- DMAP ( netuid, uid ) --> hotkey
	pub(super) type Keys<T:Config> = StorageDoubleMap<_, Identity, u16, Identity, u16, T::AccountId, ValueQuery, DefaultKey<T> >;
	#[pallet::storage] /// --- DMAP ( netuid ) --> emission
	pub(super) type LoadedEmission<T:Config> = StorageMap< _, Identity, u16, Vec<(T::AccountId, u64)>, OptionQuery >;

	#[pallet::storage] /// --- DMAP ( netuid ) --> active
	pub(super) type Active<T:Config> = StorageMap< _, Identity, u16, Vec<bool>, ValueQuery, EmptyBoolVec<T> >;
	#[pallet::storage] /// --- DMAP ( netuid ) --> rank
	pub(super) type Rank<T:Config> = StorageMap< _, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
	#[pallet::storage] /// --- DMAP ( netuid ) --> trust
	pub(super) type Trust<T:Config> = StorageMap< _, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
	#[pallet::storage] /// --- DMAP ( netuid ) --> consensus
	pub(super) type Consensus<T:Config> = StorageMap< _, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
	#[pallet::storage] /// --- DMAP ( netuid ) --> incentive
	pub(super) type Incentive<T:Config> = StorageMap< _, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
	#[pallet::storage] /// --- DMAP ( netuid ) --> dividends
	pub(super) type Dividends<T:Config> = StorageMap< _, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
	#[pallet::storage] /// --- DMAP ( netuid ) --> dividends
	pub(super) type Emission<T:Config> = StorageMap< _, Identity, u16, Vec<u64>, ValueQuery, EmptyU64Vec<T>>;
	#[pallet::storage] /// --- DMAP ( netuid ) --> last_update
	pub(super) type LastUpdate<T:Config> = StorageMap< _, Identity, u16, Vec<u64>, ValueQuery, EmptyU64Vec<T>>;
	#[pallet::storage] /// --- DMAP ( netuid ) --> validator_trust
	pub(super) type ValidatorTrust<T:Config> = StorageMap< _, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
	#[pallet::storage] /// --- DMAP ( netuid ) --> weight_consensus
	pub(super) type WeightConsensus<T:Config> = StorageMap< _, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T>>;
	#[pallet::storage] /// --- DMAP ( netuid ) --> pruning_scores
	pub(super) type PruningScores<T:Config> = StorageMap< _, Identity, u16, Vec<u16>, ValueQuery, EmptyU16Vec<T> >;
	#[pallet::storage] /// --- DMAP ( netuid ) --> validator_permit
    pub(super) type ValidatorPermit<T:Config> = StorageMap<_, Identity, u16, Vec<bool>, ValueQuery, EmptyBoolVec<T> >;

	#[pallet::storage] /// --- DMAP ( netuid, uid ) --> weights
    pub(super) type Weights<T:Config> = StorageDoubleMap<_, Identity, u16, Identity, u16, Vec<(u16, u16)>, ValueQuery, DefaultWeights<T> >;
	#[pallet::storage] /// --- DMAP ( netuid, uid ) --> bonds
    pub(super) type Bonds<T:Config> = StorageDoubleMap<_, Identity, u16, Identity, u16, Vec<(u16, u16)>, ValueQuery, DefaultBonds<T> >;


	// The pallet's runtime storage items.
	// https://docs.substrate.io/main-docs/build/runtime-storage/
	#[pallet::storage]
	#[pallet::getter(fn something)]
	// Learn more about declaring storage items:
	// https://docs.substrate.io/main-docs/build/runtime-storage/#declaring-storage-items
	pub type Something<T> = StorageValue<_, u32>;

	// Pallets use events to inform users when important changes are made.
	// https://docs.substrate.io/main-docs/build/events-errors/
	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		/// Event documentation should end with an array that provides descriptive names for event
		/// parameters. [something, who]
		NetworkAdded( u16, u16 ),	// --- Event created when a new network is added.
		NetworkRemoved( u16 ), // --- Event created when a network is removed.
		StakeAdded( T::AccountId, u64 ), // --- Event created when stake has been transfered from the a coldkey account onto the hotkey staking account.
		StakeRemoved( T::AccountId, u64 ), // --- Event created when stake has been removed from the hotkey staking account onto the coldkey account.
		WeightsSet( u16, u16 ), // ---- Event created when a caller successfully set's their weights on a subnetwork.
		NeuronRegistered( u16, u16, T::AccountId ), // --- Event created when a new neuron account has been registered to the chain.
		BulkNeuronsRegistered( u16, u16 ), // --- Event created when multiple uids have been concurrently registered.
		BulkBalancesSet(u16, u16),
		MaxAllowedUidsSet( u16, u16 ), // --- Event created when max allowed uids has been set for a subnetwor.
		MaxWeightLimitSet( u16, u16 ), // --- Event created when the max weight limit has been set.
		DifficultySet( u16, u64 ), // --- Event created when the difficulty has been set for a subnet.
		AdjustmentIntervalSet( u16, u16 ), // --- Event created when the adjustment interval is set for a subnet.
		RegistrationPerIntervalSet( u16, u16 ), // --- Event created when registeration per interval is set for a subnet.
		MaxRegistrationsPerBlockSet( u16, u16), // --- Event created when we set max registrations per block
		ActivityCutoffSet( u16, u16 ), // --- Event created when an activity cutoff is set for a subnet.
		WeightCutsSet( u16, u16 ), // --- Event created when WeightCuts value is set.
		RhoSet( u16, u16 ), // --- Event created when Rho value is set.
		KappaSet( u16, u16 ), // --- Event created when kappa is set for a subnet.
		MinAllowedWeightSet( u16, u16 ), // --- Event created when minimun allowed weight is set for a subnet.
		ValidatorBatchSizeSet( u16, u16 ), // --- Event created when validator batch size is set for a subnet.
		ValidatorSequenceLengthSet( u16, u16 ), // --- Event created when validator sequence length i set for a subnet.
		ValidatorEpochPerResetSet( u16, u16 ), // --- Event created when validator epoch per reset is set for a subnet.
		ValidatorExcludeQuantileSet( u16, u16 ), // --- Event created when the validator exclude quantile has been set for a subnet.
		ValidatorEpochLengthSet( u16, u16 ), // --- Event created when the validator epoch length has been set for a subnet.
		ValidatorLogitsDivergenceSet( u16, u64 ), /// --- Event created when the validator logits divergence value has been set.
		ValidatorPruneLenSet( u16, u64 ), /// --- Event created when the validator pruning length has been set.
		ScalingLawPowerSet( u16, u16 ), // --- Event created when the scaling law power has been set for a subnet.
		SynergyScalingLawPowerSet( u16, u16 ), // --- Event created when the synergy scaling law has been set for a subnet.
		WeightsSetRateLimitSet( u16, u64 ), // --- Event create when weights set rate limit has been set for a subnet.
		ImmunityPeriodSet( u16, u16), // --- Event created when immunity period is set for a subnet.
		BondsMovingAverageSet( u16, u64), // --- Event created when bonds moving average is set for a subnet.
		MaxAllowedValidatorsSet( u16, u16), // --- Event created when setting the max number of allowed validators on a subnet.
		AxonServed( T::AccountId ), // --- Event created when the axon server information is added to the network.
		PrometheusServed( T::AccountId ), // --- Event created when the axon server information is added to the network.
		EmissionValuesSet(), // --- Event created when emission ratios fr all networks is set.
		NetworkConnectionAdded( u16, u16, u16 ), // --- Event created when a network connection requirement is added.
		NetworkConnectionRemoved( u16, u16 ), // --- Event created when a network connection requirement is removed.
		DelegateAdded( T::AccountId, T::AccountId, u16 ), // --- Event created to signal a hotkey has become a delegate.
		DefaultTakeSet( u16 ), // --- Event created when the default take is set.
		WeightsVersionKeySet( u16, u64 ), // --- Event created when weights version key is set for a network.
		MinDifficultySet( u16, u64 ), // --- Event created when setting min difficutly on a network.
		MaxDifficultySet( u16, u64 ), // --- Event created when setting max difficutly on a network.
		ServingRateLimitSet( u64 ), // --- Event created when setting the prometheus serving rate limit.
		BurnSet( u16, u64 ), // --- Event created when setting burn on a network.
		MaxBurnSet( u16, u64 ), // --- Event created when setting max burn on a network.
		MinBurnSet( u16, u64 ), // --- Event created when setting min burn on a network.
		SomethingStored { something: u32, who: T::AccountId },
	}

	// Errors inform users that something went wrong.
	#[pallet::error]
	pub enum Error<T> {
		/// Error names should be descriptive.
		NoneValue,
		/// Errors should have helpful documentation associated with them.
		StorageOverflow,
		InvalidConnectionRequirement, // --- Thrown if we are attempting to create an invalid connection requirement.
		NetworkDoesNotExist, // --- Thrown when the network does not exist.
		NetworkExist, // --- Thrown when the network already exist.
		InvalidModality, // --- Thrown when an invalid modality attempted on serve.
		InvalidIpType, // ---- Thrown when the user tries to serve an axon which is not of type	4 (IPv4) or 6 (IPv6).
		InvalidIpAddress, // --- Thrown when an invalid IP address is passed to the serve function.
		NotRegistered, // ---- Thrown when the caller requests setting or removing data from a neuron which does not exist in the active set.
		NonAssociatedColdKey, // ---- Thrown when a stake, unstake or subscribe request is made by a coldkey which is not associated with the hotkey account. 
		NotEnoughStaketoWithdraw, // ---- Thrown when the caller requests removing more stake then there exists in the staking account. See: fn remove_stake.
		NotEnoughBalanceToStake, //  ---- Thrown when the caller requests adding more stake than there exists in the cold key account. See: fn add_stake
		BalanceWithdrawalError, // ---- Thrown when the caller tries to add stake, but for some reason the requested amount could not be withdrawn from the coldkey account
		NoValidatorPermit, // ---- Thrown when the caller attempts to set non-self weights without being a permitted validator.
		WeightVecNotEqualSize, // ---- Thrown when the caller attempts to set the weight keys and values but these vectors have different size.
		DuplicateUids, // ---- Thrown when the caller attempts to set weights with duplicate uids in the weight matrix.
		InvalidUid, // ---- Thrown when a caller attempts to set weight to at least one uid that does not exist in the metagraph.
		NotSettingEnoughWeights, // ---- Thrown when the dispatch attempts to set weights on chain with fewer elements than are allowed.
		TooManyRegistrationsThisBlock, // ---- Thrown when registrations this block exceeds allowed number.
		AlreadyRegistered, // ---- Thrown when the caller requests registering a neuron which already exists in the active set.
		InvalidWorkBlock, // ---- Thrown if the supplied pow hash block is in the future or negative
		WorkRepeated, // ---- Thrown when the caller attempts to use a repeated work.
		InvalidDifficulty, // ---- Thrown if the supplied pow hash block does not meet the network difficulty.
		InvalidSeal, // ---- Thrown if the supplied pow hash seal does not match the supplied work.
		MaxAllowedUIdsNotAllowed, // ---  Thrown if the vaule is invalid for MaxAllowedUids
		CouldNotConvertToBalance, // ---- Thrown when the dispatch attempts to convert between a u64 and T::balance but the call fails.
		StakeAlreadyAdded, // --- Thrown when the caller requests adding stake for a hotkey to the total stake which already added
		MaxWeightExceeded, // --- Thrown when the dispatch attempts to set weights on chain with where any normalized weight is more than MaxWeightLimit.
		StorageValueOutOfRange, // --- Thrown when the caller attempts to set a storage value outside of its allowed range.
		TempoHasNotSet, // --- Thrown when tempo has not set
		InvalidTempo, // --- Thrown when tempo is not valid
		EmissionValuesDoesNotMatchNetworks, // --- Thrown when number or recieved emission rates does not match number of networks
		InvalidEmissionValues, // --- Thrown when emission ratios are not valid (did not sum up to 10^9)
		DidNotPassConnectedNetworkRequirement, // --- Thrown when a hotkey attempts to register into a network without passing the  registration requirment from another network.
		AlreadyDelegate, // --- Thrown if the hotkey attempt to become delegate when they are already.
		SettingWeightsTooFast, // --- Thrown if the hotkey attempts to set weights twice withing net_tempo/2 blocks.
		IncorrectNetworkVersionKey, // --- Thrown of a validator attempts to set weights from a validator with incorrect code base key.
		ServingRateLimitExceeded, // --- Thrown when an axon or prometheus serving exceeds the rate limit for a registered neuron.
		BalanceSetError, // --- Thrown when an error occurs setting a balance
		MaxAllowedUidsExceeded, // --- Thrown when number of accounts going to be registered exceed MaxAllowedUids for the network.
		TooManyUids, // ---- Thrown when the caller attempts to set weights with more uids than allowed.
	}

	/// ==================
	/// ==== Genesis =====
	/// ==================

	#[pallet::genesis_config]
	#[cfg(feature = "std")]
	pub struct GenesisConfig<T: Config> {
		pub stakes: Vec<(T::AccountId, Vec<(T::AccountId, u64)>)>
	}

	#[cfg(feature = "std")]
	impl<T: Config> Default for GenesisConfig<T> {
		fn default() -> Self {
			Self { 
				stakes: Default::default()
			}
		}
	}

	#[pallet::genesis_build]
	impl<T: Config> GenesisBuild<T> for GenesisConfig<T> {
		fn build(&self) {
			let netuid: u16 = 4;
			let mut next_uid = 0;

			for (coldkey, hotkeys) in self.stakes.iter() {
				for (hotkey, stake) in hotkeys.iter() {
					// Increase the uid count.
					SubnetworkN::<T>::insert( netuid, next_uid );
	
					// Expand Yuma Consensus with new position.
					Rank::<T>::mutate(netuid, |v| v.push(0) );
					Trust::<T>::mutate(netuid, |v| v.push(0) );
					Active::<T>::mutate(netuid, |v| v.push( true ) );
					Emission::<T>::mutate(netuid, |v| v.push(0) );
					Consensus::<T>::mutate(netuid, |v| v.push(0) );
					Incentive::<T>::mutate(netuid, |v| v.push(0) );
					Dividends::<T>::mutate(netuid, |v| v.push(0) );
					LastUpdate::<T>::mutate(netuid, |v| v.push( 0 ) );
					PruningScores::<T>::mutate(netuid, |v| v.push(0) );
					ValidatorTrust::<T>::mutate(netuid, |v| v.push(0) );
					ValidatorPermit::<T>::mutate(netuid, |v| v.push(false) );
			
					// Insert account information.
					Keys::<T>::insert( netuid, next_uid, hotkey.clone() ); // Make hotkey - uid association.
					Uids::<T>::insert( netuid, hotkey.clone(), next_uid ); // Make uid - hotkey association.
					BlockAtRegistration::<T>::insert( netuid, next_uid, 0 ); // Fill block at registration.
					IsNetworkMember::<T>::insert( hotkey.clone(), netuid, true ); // Fill network is member.
	
					// Fill stake information.
					Owner::<T>::insert(hotkey.clone(), coldkey.clone());
	
					TotalHotkeyStake::<T>::insert(hotkey.clone(), stake);
					TotalColdkeyStake::<T>::insert(coldkey.clone(), TotalColdkeyStake::<T>::get(coldkey).saturating_add(*stake));
	
					Stake::<T>::insert(hotkey.clone(), coldkey.clone(), stake);
	
					next_uid += 1;
				}
			}
		}
	}

	/// ================
	/// ==== Hooks =====
	/// ================
	#[pallet::hooks] 
	impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> { 
		/// ---- Called on the initialization of this pallet. (the order of on_finalize calls is determined in the runtime)
		///
		/// # Args:
		/// 	* 'n': (T::BlockNumber):
		/// 		- The number of the block we are initializing.
		fn on_initialize( _block_number: BlockNumberFor<T> ) -> Weight {
			Self::block_step();
			
			return Weight::from_ref_time(110_634_229_000 as u64)
						.saturating_add(T::DbWeight::get().reads(8304 as u64))
						.saturating_add(T::DbWeight::get().writes(110 as u64));
		}
	}

	// Dispatchable functions allows users to interact with the pallet and invoke state changes.
	// These functions materialize as "extrinsics", which are often compared to transactions.
	// Dispatchable functions must be annotated with a weight and must return a DispatchResult.
	#[pallet::call]
	impl<T: Config> Pallet<T> {

		/// --- Sets the caller weights for the incentive mechanism. The call can be
		/// made from the hotkey account so is potentially insecure, however, the damage
		/// of changing weights is minimal if caught early. This function includes all the
		/// checks that the passed weights meet the requirements. Stored as u16s they represent
		/// rational values in the range [0,1] which sum to 1 and can be interpreted as
		/// probabilities. The specific weights determine how inflation propagates outward
		/// from this peer. 
		/// 
		/// Note: The 16 bit integers weights should represent 1.0 as the max u16.
		/// However, the function normalizes all integers to u16_max anyway. This means that if the sum of all
		/// elements is larger or smaller than the amount of elements * u16_max, all elements
		/// will be corrected for this deviation. 
		/// 
		/// # Args:
		/// 	* `origin`: (<T as frame_system::Config>Origin):
		/// 		- The caller, a hotkey who wishes to set their weights.
		///
		/// 	* `netuid` (u16):
		/// 		- The network uid we are setting these weights on.
		/// 
		/// 	* `dests` (Vec<u16>):
		/// 		- The edge endpoint for the weight, i.e. j for w_ij.
		///
		/// 	* 'weights' (Vec<u16>):
		/// 		- The u16 integer encoded weights. Interpreted as rational
		/// 		values in the range [0,1]. They must sum to in32::MAX.
		///
		/// 	* 'version_key' ( u64 ):
    	/// 		- The network version key to check if the validator is up to date.
		///
		/// # Event:
		/// 	* WeightsSet;
		/// 		- On successfully setting the weights on chain.
		///
		/// # Raises:
		/// 	* 'NetworkDoesNotExist':
		/// 		- Attempting to set weights on a non-existent network.
		///
		/// 	* 'NotRegistered':
		/// 		- Attempting to set weights from a non registered account.
		///
		/// 	* 'WeightVecNotEqualSize':
		/// 		- Attempting to set weights with uids not of same length.
		///
		/// 	* 'DuplicateUids':
		/// 		- Attempting to set weights with duplicate uids.
		///		
		///     * 'TooManyUids':
		/// 		- Attempting to set weights above the max allowed uids.
		///
		/// 	* 'InvalidUid':
		/// 		- Attempting to set weights with invalid uids.
		///
		/// 	* 'NotSettingEnoughWeights':
		/// 		- Attempting to set weights with fewer weights than min.
		///
		/// 	* 'MaxWeightExceeded':
		/// 		- Attempting to set weights with max value exceeding limit.
        #[pallet::weight((Weight::from_ref_time(15_979_124_000 as u64)
		.saturating_add(T::DbWeight::get().reads(4104 as u64))
		.saturating_add(T::DbWeight::get().writes(2 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn set_weights(
			origin:OriginFor<T>, 
			netuid: u16,
			dests: Vec<u16>, 
			weights: Vec<u16>,
			version_key: u64 
		) -> DispatchResult {
			Self::do_set_weights( origin, netuid, dests, weights, version_key )
		}

		/// --- Sets the key as a delegate.
		///
		/// # Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- The signature of the caller's coldkey.
		///
		/// 	* 'hotkey' (T::AccountId):
		/// 		- The hotkey we are delegating (must be owned by the coldkey.)
		///
		/// 	* 'take' (u64):
		/// 		- The stake proportion that this hotkey takes from delegations.
		///
		/// # Event:
		/// 	* DelegateAdded;
		/// 		- On successfully setting a hotkey as a delegate.
		///
		/// # Raises:
		/// 	* 'NotRegistered':
		/// 		- The hotkey we are delegating is not registered on the network.
		///
		/// 	* 'NonAssociatedColdKey':
		/// 		- The hotkey we are delegating is not owned by the calling coldket.
		///
		///
		#[pallet::weight((Weight::from_ref_time(61_408_000 as u64)
		.saturating_add(T::DbWeight::get().reads(3 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::Yes))]
		pub fn become_delegate(
			origin: OriginFor<T>, 
			hotkey: T::AccountId
		) -> DispatchResult {
			Self::do_become_delegate(origin, hotkey, Self::get_default_take() )
		}

		/// --- Adds stake to a hotkey. The call is made from the
		/// coldkey account linked in the hotkey.
		/// Only the associated coldkey is allowed to make staking and
		/// unstaking requests. This protects the neuron against
		/// attacks on its hotkey running in production code.
		///
		/// # Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- The signature of the caller's coldkey.
		///
		/// 	* 'hotkey' (T::AccountId):
		/// 		- The associated hotkey account.
		///
		/// 	* 'amount_staked' (u64):
		/// 		- The amount of stake to be added to the hotkey staking account.
		///
		/// # Event:
		/// 	* StakeAdded;
		/// 		- On the successfully adding stake to a global account.
		///
		/// # Raises:
		/// 	* 'CouldNotConvertToBalance':
		/// 		- Unable to convert the passed stake value to a balance.
		///
		/// 	* 'NotEnoughBalanceToStake':
		/// 		- Not enough balance on the coldkey to add onto the global account.
		///
		/// 	* 'NonAssociatedColdKey':
		/// 		- The calling coldkey is not associated with this hotkey.
		///
		/// 	* 'BalanceWithdrawalError':
		/// 		- Errors stemming from transaction pallet.
		///
		///
		#[pallet::weight((Weight::from_ref_time(98_973_000 as u64)
		.saturating_add(T::DbWeight::get().reads(7 as u64))
		.saturating_add(T::DbWeight::get().writes(5 as u64)), DispatchClass::Normal, Pays::Yes))]
		pub fn add_stake(
			origin: OriginFor<T>, 
			hotkey: T::AccountId, 
			amount_staked: u64
		) -> DispatchResult {
			Self::do_add_stake(origin, hotkey, amount_staked)
		}

		/// ---- Remove stake from the staking account. The call must be made
		/// from the coldkey account attached to the neuron metadata. Only this key
		/// has permission to make staking and unstaking requests.
		///
		/// # Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- The signature of the caller's coldkey.
		///
		/// 	* 'hotkey' (T::AccountId):
		/// 		- The associated hotkey account.
		///
		/// 	* 'amount_unstaked' (u64):
		/// 		- The amount of stake to be added to the hotkey staking account.
		///
		/// # Event:
		/// 	* StakeRemoved;
		/// 		- On the successfully removing stake from the hotkey account.
		///
		/// # Raises:
		/// 	* 'NotRegistered':
		/// 		- Thrown if the account we are attempting to unstake from is non existent.
		///
		/// 	* 'NonAssociatedColdKey':
		/// 		- Thrown if the coldkey does not own the hotkey we are unstaking from.
		///
		/// 	* 'NotEnoughStaketoWithdraw':
		/// 		- Thrown if there is not enough stake on the hotkey to withdwraw this amount. 
		///
		/// 	* 'CouldNotConvertToBalance':
		/// 		- Thrown if we could not convert this amount to a balance.
		///
		///
		#[pallet::weight((Weight::from_ref_time(85_640_000 as u64)
		.saturating_add(T::DbWeight::get().reads(7 as u64))
		.saturating_add(T::DbWeight::get().writes(5 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn remove_stake(
			origin: OriginFor<T>, 
			hotkey: T::AccountId, 
			amount_unstaked: u64
		) -> DispatchResult {
			Self::do_remove_stake(origin, hotkey, amount_unstaked)
		}

		/// ---- Serves or updates axon /promethteus information for the neuron associated with the caller. If the caller is
		/// already registered the metadata is updated. If the caller is not registered this call throws NotRegistered.
		///
		/// # Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- The signature of the caller.
		///
		/// 	* 'netuid' (u16):
		/// 		- The u16 network identifier.
		///
		/// 	* 'version' (u64):
		/// 		- The bittensor version identifier.
		///
		/// 	* 'ip' (u64):
		/// 		- The endpoint ip information as a u128 encoded integer.
		///
		/// 	* 'port' (u16):
		/// 		- The endpoint port information as a u16 encoded integer.
		/// 
		/// 	* 'ip_type' (u8):
		/// 		- The endpoint ip version as a u8, 4 or 6.
		///
		/// 	* 'protocol' (u8):
		/// 		- UDP:1 or TCP:0 
		///
		/// 	* 'placeholder1' (u8):
		/// 		- Placeholder for further extra params.
		///
		/// 	* 'placeholder2' (u8):
		/// 		- Placeholder for further extra params.
		///
		/// # Event:
		/// 	* AxonServed;
		/// 		- On successfully serving the axon info.
		///
		/// # Raises:
		/// 	* 'NetworkDoesNotExist':
		/// 		- Attempting to set weights on a non-existent network.
		///
		/// 	* 'NotRegistered':
		/// 		- Attempting to set weights from a non registered account.
		///
		/// 	* 'InvalidIpType':
		/// 		- The ip type is not 4 or 6.
		///
		/// 	* 'InvalidIpAddress':
		/// 		- The numerically encoded ip address does not resolve to a proper ip.
		///
		/// 	* 'ServingRateLimitExceeded':
		/// 		- Attempting to set prometheus information withing the rate limit min.
		///
		#[pallet::weight((Weight::from_ref_time(49_988_000 as u64)
		.saturating_add(T::DbWeight::get().reads(2 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn serve_axon(
			origin:OriginFor<T>, 
			version: u32, 
			ip: u128, 
			port: u16, 
			ip_type: u8,
			protocol: u8, 
			placeholder1: u8, 
			placeholder2: u8,
		) -> DispatchResult {
			Self::do_serve_axon( origin, version, ip, port, ip_type, protocol, placeholder1, placeholder2 ) 
		}
		#[pallet::weight((Weight::from_ref_time(43_755_000 as u64)
		.saturating_add(T::DbWeight::get().reads(2 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn serve_prometheus(
			origin:OriginFor<T>, 
			version: u32, 
			ip: u128, 
			port: u16, 
			ip_type: u8,
		) -> DispatchResult {
			Self::do_serve_prometheus( origin, version, ip, port, ip_type ) 
		}


		/// ---- Registers a new neuron to the subnetwork. 
		///
		/// # Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- The signature of the calling hotkey.
		///
		/// 	* 'netuid' (u16):
		/// 		- The u16 network identifier.
		///
		/// 	* 'block_number' ( u64 ):
		/// 		- Block hash used to prove work done.
		///
		/// 	* 'nonce' ( u64 ):
		/// 		- Positive integer nonce used in POW.
		///
		/// 	* 'work' ( Vec<u8> ):
		/// 		- Vector encoded bytes representing work done.
		///
		/// 	* 'hotkey' ( T::AccountId ):
		/// 		- Hotkey to be registered to the network.
		///
		/// 	* 'coldkey' ( T::AccountId ):
		/// 		- Associated coldkey account.
		///
		/// # Event:
		/// 	* NeuronRegistered;
		/// 		- On successfully registereing a uid to a neuron slot on a subnetwork.
		///
		/// # Raises:
		/// 	* 'NetworkDoesNotExist':
		/// 		- Attempting to registed to a non existent network.
		///
		/// 	* 'TooManyRegistrationsThisBlock':
		/// 		- This registration exceeds the total allowed on this network this block.
		///
		/// 	* 'AlreadyRegistered':
		/// 		- The hotkey is already registered on this network.
		///
		/// 	* 'InvalidWorkBlock':
		/// 		- The work has been performed on a stale, future, or non existent block.
		///
		/// 	* 'WorkRepeated':
		/// 		- This work for block has already been used.
		///
		/// 	* 'InvalidDifficulty':
		/// 		- The work does not match the difficutly.
		///
		/// 	* 'InvalidSeal':
		/// 		- The seal is incorrect.
		///
		#[pallet::weight((Weight::from_ref_time(117_511_000 as u64)
		.saturating_add(T::DbWeight::get().reads(24 as u64))
		.saturating_add(T::DbWeight::get().writes(21 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn register( 
				origin:OriginFor<T>, 
				netuid: u16,
				block_number: u64, 
				nonce: u64, 
				work: Vec<u8>,
				hotkey: T::AccountId, 
				coldkey: T::AccountId,
		) -> DispatchResult { 
			Self::do_registration(origin, netuid, block_number, nonce, work, hotkey, coldkey)
		}
		#[pallet::weight((Weight::from_ref_time(117_511_000 as u64)
		.saturating_add(T::DbWeight::get().reads(24 as u64))
		.saturating_add(T::DbWeight::get().writes(21 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn burned_register( 
				origin:OriginFor<T>, 
				netuid: u16,
				hotkey: T::AccountId, 
		) -> DispatchResult { 
			Self::do_burned_registration(origin, netuid, hotkey)
		}
		#[pallet::weight((Weight::from_ref_time(110_442_000 as u64)
		.saturating_add(T::DbWeight::get().reads(20 as u64))
		.saturating_add(T::DbWeight::get().writes(22 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_register( 
				origin:OriginFor<T>, 
				netuid: u16,
				hotkey: T::AccountId, 
				coldkey: T::AccountId,
				stake: u64,
				balance: u64,
			) -> DispatchResult { 
			Self::do_sudo_registration(origin, netuid, hotkey, coldkey, stake, balance)
		}

		/// ---- SUDO ONLY FUNCTIONS ------------------------------------------------------------

		/// ---- Sudo add a network to the network set.
		/// # Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- Must be sudo.
		///
		/// 	* 'netuid' (u16):
		/// 		- The u16 network identifier.
		///
		/// 	* 'tempo' ( u16 ):
		/// 		- Number of blocks between epoch step.
		///
		/// 	* 'modality' ( u16 ):
		/// 		- Network modality specifier.
		///
		/// # Event:
		/// 	* NetworkAdded;
		/// 		- On successfully creation of a network.
		///
		/// # Raises:
		/// 	* 'NetworkExist':
		/// 		- Attempting to register an already existing.
		///
		/// 	* 'InvalidModality':
		/// 		- Attempting to register a network with an invalid modality.
		///
		/// 	* 'InvalidTempo':
		/// 		- Attempting to register a network with an invalid tempo.
		///
		#[pallet::weight((Weight::from_ref_time(62_096_000 as u64)
		.saturating_add(T::DbWeight::get().reads(15 as u64))
		.saturating_add(T::DbWeight::get().writes(18 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_add_network(
			origin: OriginFor<T>,
			netuid: u16,
			tempo: u16,
			modality: u16
		) -> DispatchResultWithPostInfo {
			Self::do_add_network(origin, netuid, tempo, modality)
		}

		/// ---- Sudo remove a network from the network set.
		/// # Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- Must be sudo.
		///
		/// 	* 'netuid' (u16):
		/// 		- The u16 network identifier.
		///
		/// # Event:
		/// 	* NetworkRemoved;
		/// 		- On the successfull removing of this network.
		///
		/// # Raises:
		/// 	* 'NetworkDoesNotExist':
		/// 		- Attempting to remove a non existent network.
		///
		#[pallet::weight((Weight::from_ref_time(64_781_000 as u64)
		.saturating_add(T::DbWeight::get().reads(2 as u64))
		.saturating_add(T::DbWeight::get().writes(30 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_remove_network(
			origin: OriginFor<T>,
			netuid: u16
		) -> DispatchResult {
			Self::do_remove_network(origin, netuid)
		} 

		/// ---- Sudo set emission values for all networks.
		/// Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- The caller, must be sudo.
		///
		/// 	* `netuids` (Vec<u16>):
		/// 		- A vector of network uids values. This must include all netuids.
		///
		/// 	* `emission` (Vec<u64>):
		/// 		- The emission values associated with passed netuids in order.
		/// 
		#[pallet::weight((Weight::from_ref_time(73_574_000 as u64)
		.saturating_add(T::DbWeight::get().reads(12 as u64))
		.saturating_add(T::DbWeight::get().writes(10 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_emission_values(
			origin: OriginFor<T>,
			netuids: Vec<u16>,
			emission: Vec<u64>,
		) -> DispatchResult {
			Self::do_set_emission_values( 
				origin,
				netuids,
				emission
			)
		}

		/// ---- Sudo add a network connect requirement.
		/// Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- The caller, must be sudo.
		///
		/// 	* `netuid_a` (u16):
		/// 		- The network we are adding the requirment to (parent network)
		///
		/// 	* `netuid_b` (u16):
		/// 		- The network we the requirement refers to (child network)
		///
		/// 	* `requirement` (u16):
		/// 		- The topk percentile prunning score requirement (u16:MAX normalized.)
		///
		#[pallet::weight((Weight::from_ref_time(56_907_000 as u64)
		.saturating_add(T::DbWeight::get().reads(2 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_add_network_connection_requirement( origin:OriginFor<T>, netuid_a: u16, netuid_b: u16, requirement: u16 ) -> DispatchResult { 
			Self::do_sudo_add_network_connection_requirement( origin, netuid_a, netuid_b, requirement )
		}

		/// ---- Sudo remove a network connection requirement.
		/// Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- The caller, must be sudo.
		///
		/// 	* `netuid_a` (u16):
		/// 		- The network we are removing the requirment from.
		///
		/// 	* `netuid_b` (u16):
		/// 		- The required network connection to remove.
		///   
		#[pallet::weight((Weight::from_ref_time(54_082_000 as u64)
		.saturating_add(T::DbWeight::get().reads(3 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_remove_network_connection_requirement( origin:OriginFor<T>, netuid_a: u16, netuid_b: u16 ) -> DispatchResult { 
			Self::do_sudo_remove_network_connection_requirement( origin, netuid_a, netuid_b )
		}

		/// ==================================
		/// ==== Parameter Sudo calls ========
		/// ==================================
		/// Each function sets the corresponding hyper paramter on the specified network
		/// Args:
		/// 	* 'origin': (<T as frame_system::Config>Origin):
		/// 		- The caller, must be sudo.
		///
		/// 	* `netuid` (u16):
		/// 		- The network identifier.
		///
		/// 	* `hyperparameter value` (u16):
		/// 		- The value of the hyper parameter.
		///   
		#[pallet::weight((Weight::from_ref_time(39_200_000 as u64)
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_default_take( origin:OriginFor<T>, default_take: u16 ) -> DispatchResult {  
			Self::do_sudo_set_default_take( origin, default_take )
		}
		#[pallet::weight((Weight::from_ref_time(26_650_000 as u64)
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_serving_rate_limit( origin:OriginFor<T>, serving_rate_limit: u64 ) -> DispatchResult {  
			Self::do_sudo_set_serving_rate_limit( origin, serving_rate_limit )
		}

		#[pallet::weight((Weight::from_ref_time(39_699_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_max_burn( origin:OriginFor<T>, netuid: u16, max_burn: u64 ) -> DispatchResult {  
			Self::do_sudo_set_max_burn( origin, netuid, max_burn )
		}
		#[pallet::weight((Weight::from_ref_time(47_276_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_min_burn( origin:OriginFor<T>, netuid: u16, min_burn: u64 ) -> DispatchResult {  
			Self::do_sudo_set_min_burn( origin, netuid, min_burn )
		}
		#[pallet::weight((Weight::from_ref_time(47_276_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_burn( origin:OriginFor<T>, netuid: u16, burn: u64 ) -> DispatchResult {  
			Self::do_sudo_set_burn( origin, netuid, burn )
		}

		#[pallet::weight((Weight::from_ref_time(39_699_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_max_difficulty( origin:OriginFor<T>, netuid: u16, max_difficulty: u64 ) -> DispatchResult {  
			Self::do_sudo_set_max_difficulty( origin, netuid, max_difficulty )
		}
		#[pallet::weight((Weight::from_ref_time(47_276_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_min_difficulty( origin:OriginFor<T>, netuid: u16, min_difficulty: u64 ) -> DispatchResult {  
			Self::do_sudo_set_min_difficulty( origin, netuid, min_difficulty )
		}
		#[pallet::weight((Weight::from_ref_time(63_512_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_weights_set_rate_limit( origin:OriginFor<T>, netuid: u16, weights_set_rate_limit: u64 ) -> DispatchResult {  
			Self::do_sudo_set_weights_set_rate_limit( origin, netuid, weights_set_rate_limit )
		}
		#[pallet::weight((Weight::from_ref_time(34_109_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_weights_version_key( origin:OriginFor<T>, netuid: u16, weights_version_key: u64 ) -> DispatchResult {  
			Self::do_sudo_set_weights_version_key( origin, netuid, weights_version_key )
		}
		#[pallet::weight((Weight::from_ref_time(38_150_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_bonds_moving_average( origin:OriginFor<T>, netuid: u16, bonds_moving_average: u64 ) -> DispatchResult {  
			Self::do_sudo_set_bonds_moving_average( origin, netuid, bonds_moving_average )
		}
		#[pallet::weight((Weight::from_ref_time(33_212_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_max_allowed_validators( origin:OriginFor<T>, netuid: u16, max_allowed_validators: u16 ) -> DispatchResult {  
			Self::do_sudo_set_max_allowed_validators( origin, netuid, max_allowed_validators )
		}
		#[pallet::weight((Weight::from_ref_time(38_092_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_difficulty( origin:OriginFor<T>, netuid: u16, difficulty: u64 ) -> DispatchResult {
			Self::do_sudo_set_difficulty( origin, netuid, difficulty )
		}
		#[pallet::weight((Weight::from_ref_time(40_837_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_adjustment_interval( origin:OriginFor<T>, netuid: u16, adjustment_interval: u16 ) -> DispatchResult { 
			Self::do_sudo_set_adjustment_interval( origin, netuid, adjustment_interval )
		}
		#[pallet::weight((Weight::from_ref_time(30_873_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_target_registrations_per_interval( origin:OriginFor<T>, netuid: u16, target_registrations_per_interval: u16 ) -> DispatchResult {
			Self::do_sudo_set_target_registrations_per_interval( origin, netuid, target_registrations_per_interval )
		}
		#[pallet::weight((Weight::from_ref_time(35_591_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_activity_cutoff( origin:OriginFor<T>, netuid: u16, activity_cutoff: u16 ) -> DispatchResult {
			Self::do_sudo_set_activity_cutoff( origin, netuid, activity_cutoff )
		}
		#[pallet::weight((Weight::from_ref_time(38_396_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_rho( origin:OriginFor<T>, netuid: u16, rho: u16 ) -> DispatchResult {
			Self::do_sudo_set_rho( origin, netuid, rho )
		}
		#[pallet::weight((Weight::from_ref_time(32_934_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_kappa( origin:OriginFor<T>, netuid: u16, kappa: u16 ) -> DispatchResult {
			Self::do_sudo_set_kappa( origin, netuid, kappa )
		}
		#[pallet::weight((Weight::from_ref_time(31_732_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_weight_cuts( origin:OriginFor<T>, netuid: u16, weight_cuts: u16 ) -> DispatchResult {
			Self::do_sudo_set_weight_cuts( origin, netuid, weight_cuts )
		}
		#[pallet::weight((Weight::from_ref_time(33_763_000 as u64)
		.saturating_add(T::DbWeight::get().reads(2 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_max_allowed_uids( origin:OriginFor<T>, netuid: u16, max_allowed_uids: u16 ) -> DispatchResult {
			Self::do_sudo_set_max_allowed_uids(origin, netuid, max_allowed_uids )
		}
		#[pallet::weight((Weight::from_ref_time(37_491_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_min_allowed_weights( origin:OriginFor<T>, netuid: u16, min_allowed_weights: u16 ) -> DispatchResult {
			Self::do_sudo_set_min_allowed_weights( origin, netuid, min_allowed_weights )
		}
		#[pallet::weight((Weight::from_ref_time(32_311_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_validator_batch_size( origin:OriginFor<T>, netuid: u16, validator_batch_size: u16 ) -> DispatchResult {
			Self::do_sudo_set_validator_batch_size( origin, netuid, validator_batch_size )
		}
		#[pallet::weight((Weight::from_ref_time(38_022_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_validator_sequence_length( origin:OriginFor<T>, netuid: u16, validator_sequence_length: u16 ) -> DispatchResult {
			Self::do_sudo_set_validator_sequence_length(origin, netuid, validator_sequence_length )
		}
		#[pallet::weight((Weight::from_ref_time(33_346_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_validator_epochs_per_reset( origin:OriginFor<T>, netuid: u16, validator_epochs_per_reset: u16 ) -> DispatchResult {
			Self::do_sudo_set_validator_epochs_per_reset( origin, netuid, validator_epochs_per_reset )
		}
		#[pallet::weight((Weight::from_ref_time(32_189_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_validator_exclude_quantile( origin:OriginFor<T>, netuid: u16, validator_exclude_quantile: u16 ) -> DispatchResult {
			Self::do_sudo_set_validator_exclude_quantile( origin, netuid, validator_exclude_quantile )
		}
		#[pallet::weight((Weight::from_ref_time(37_962_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_validator_prune_len( origin:OriginFor<T>, netuid: u16, validator_prune_len: u64 ) -> DispatchResult {
			Self::do_sudo_set_validator_prune_len( origin, netuid, validator_prune_len )
		}
		#[pallet::weight((Weight::from_ref_time(31_791_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_validator_logits_divergence( origin:OriginFor<T>, netuid: u16,validator_logits_divergence: u64 ) -> DispatchResult {
			Self::do_sudo_set_validator_logits_divergence( origin, netuid, validator_logits_divergence )
		}
		#[pallet::weight((Weight::from_ref_time(31_791_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_validator_epoch_len( origin:OriginFor<T>, netuid: u16,validator_epoch_length: u16 ) -> DispatchResult {
			Self::do_sudo_set_validator_epoch_length( origin, netuid, validator_epoch_length )
		}
		#[pallet::weight((Weight::from_ref_time(37_614_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_scaling_law_power( origin:OriginFor<T>, netuid: u16, scaling_law_power: u16 ) -> DispatchResult {
			Self::do_sudo_set_scaling_law_power( origin, netuid, scaling_law_power )
		}
		#[pallet::weight((Weight::from_ref_time(42_030_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_synergy_scaling_law_power( origin:OriginFor<T>, netuid: u16, synergy_scaling_law_power: u16 ) -> DispatchResult {
			Self::do_sudo_set_synergy_scaling_law_power( origin, netuid, synergy_scaling_law_power )
		}
		#[pallet::weight((Weight::from_ref_time(32_669_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_immunity_period( origin:OriginFor<T>, netuid: u16, immunity_period: u16 ) -> DispatchResult {
			Self::do_sudo_set_immunity_period( origin, netuid, immunity_period )
		}
		#[pallet::weight((Weight::from_ref_time(33_995_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_max_weight_limit( origin:OriginFor<T>, netuid: u16, max_weight_limit: u16 ) -> DispatchResult {
			Self::do_sudo_set_max_weight_limit( origin, netuid, max_weight_limit )
		}
		#[pallet::weight((Weight::from_ref_time(33_164_000 as u64)
		.saturating_add(T::DbWeight::get().reads(1 as u64))
		.saturating_add(T::DbWeight::get().writes(1 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn sudo_set_max_registrations_per_block(origin: OriginFor<T>, netuid: u16, max_registrations_per_block: u16 ) -> DispatchResult {
			Self::do_sudo_set_max_registrations_per_block(origin, netuid, max_registrations_per_block )
		}


		/// Benchmarking functions.
		#[pallet::weight((0, DispatchClass::Normal, Pays::No))]
		pub fn create_network( _: OriginFor<T>, netuid: u16, n: u16, tempo: u16 ) -> DispatchResult {
			Self::init_new_network( netuid, tempo, 1 );
			Self::set_max_allowed_uids( netuid, n );
			let mut seed : u32 = 1;
			for _ in 0..n {
				let block_number: u64 = Self::get_current_block_as_u64();
				let hotkey: T::AccountId = T::AccountId::decode(&mut sp_runtime::traits::TrailingZeroInput::zeroes()).unwrap();
				Self::append_neuron( netuid, &hotkey, block_number );
				seed = seed + 1;
			}
			Ok(())
		}

		#[pallet::weight((0, DispatchClass::Normal, Pays::No))]
		pub fn create_network_with_weights( _: OriginFor<T>, netuid: u16, n: u16, tempo: u16, n_vals: u16, n_weights: u16 ) -> DispatchResult {
			Self::init_new_network( netuid, tempo, 1 );
			Self::set_max_allowed_uids( netuid, n );
			Self::set_max_allowed_validators( netuid, n_vals );
			Self::set_min_allowed_weights( netuid, n_weights );
			Self::set_emission_for_network( netuid, 1_000_000_000 );
			let mut seed : u32 = 1;
			for _ in 0..n {
				let block_number: u64 = Self::get_current_block_as_u64();
				let hotkey: T::AccountId = T::AccountId::decode(&mut sp_runtime::traits::TrailingZeroInput::zeroes()).unwrap();
				Self::increase_stake_on_coldkey_hotkey_account( &hotkey, &hotkey, 1_000_000_000 );
				Self::append_neuron( netuid, &hotkey, block_number );
				seed = seed + 1;
			}
			for uid in 0..n {
				let uids: Vec<u16> = (0..n_weights).collect();
				let values: Vec<u16> = vec![1; n_weights as usize];
				let normalized_values = Self::normalize_weights( values );
				let mut zipped_weights: Vec<( u16, u16 )> = vec![];
				for ( uid, val ) in uids.iter().zip(normalized_values.iter()) { zipped_weights.push((*uid, *val)) }
				if uid < n_vals {
					Weights::<T>::insert( netuid, uid, zipped_weights );
				} else {
					break;
				}
			}
			Ok(())
		}

		#[pallet::weight((0, DispatchClass::Normal, Pays::No))]
		pub fn benchmark_epoch_with_weights( _:OriginFor<T> ) -> DispatchResult {
			Self::epoch( 11, 1_000_000_000 );
			Ok(())
		} 
		#[pallet::weight((Weight::from_ref_time(117_586_465_000 as u64)
		.saturating_add(T::DbWeight::get().reads(12299 as u64))
		.saturating_add(T::DbWeight::get().writes(110 as u64)), DispatchClass::Normal, Pays::No))]
		pub fn benchmark_epoch_without_weights( _:OriginFor<T> ) -> DispatchResult {
			let _: Vec<(T::AccountId, u64)> = Self::epoch( 11, 1_000_000_000 );
			Ok(())
		} 
		#[pallet::weight((0, DispatchClass::Normal, Pays::No))]
		pub fn benchmark_drain_emission( _:OriginFor<T> ) -> DispatchResult {
			Self::drain_emission( 11 );
			Ok(())
		} 
	}	

	// ---- Subtensor helper functions.
	impl<T: Config> Pallet<T> {
		// --- Returns the transaction priority for setting weights.
		pub fn get_priority_set_weights( hotkey: &T::AccountId, netuid: u16 ) -> u64 {
			if Uids::<T>::contains_key( netuid, &hotkey ) {
				let uid = Self::get_uid_for_net_and_hotkey(netuid, &hotkey.clone()).unwrap();
				let current_block_number: u64 = Self::get_current_block_as_u64();
				return current_block_number - Self::get_last_update_for_uid(netuid, uid as u16);
			}
			return 0;
		}
	}
}


/************************************************************
	CallType definition
************************************************************/
#[derive(Debug, PartialEq)]
pub enum CallType {
    SetWeights,
    AddStake,
    RemoveStake,
	AddDelegate,
    Register,
    Serve,
	Other,
}
impl Default for CallType {
    fn default() -> Self {
        CallType::Other
    }
}

#[derive(Encode, Decode, Clone, Eq, PartialEq, TypeInfo)]
pub struct SubtensorSignedExtension<T: Config + Send + Sync + TypeInfo>(pub PhantomData<T>);

impl<T: Config + Send + Sync + TypeInfo> SubtensorSignedExtension<T> where
	T::RuntimeCall: Dispatchable<Info=DispatchInfo, PostInfo=PostDispatchInfo>,
	<T as frame_system::Config>::RuntimeCall: IsSubType<Call<T>>,
{
	pub fn new() -> Self {
		Self(Default::default())
	}

	pub fn get_priority_vanilla() -> u64 {
		// Return high priority so that every extrinsic except set_weights function will 
		// have a higher priority than the set_weights call
		return u64::max_value();
	}

	pub fn get_priority_set_weights( who: &T::AccountId, netuid: u16 ) -> u64 {
		// Return the non vanilla priority for a set weights call.

		return Pallet::<T>::get_priority_set_weights( who, netuid );
	}
}

impl <T:Config + Send + Sync + TypeInfo> sp_std::fmt::Debug for SubtensorSignedExtension<T> {
	fn fmt(&self, f: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
		write!(f, "SubtensorSignedExtension")
	}
}

impl<T: Config + Send + Sync + TypeInfo> SignedExtension for SubtensorSignedExtension<T>
    where
        T::RuntimeCall: Dispatchable<Info=DispatchInfo, PostInfo=PostDispatchInfo>,
        <T as frame_system::Config>::RuntimeCall: IsSubType<Call<T>>,
{
	const IDENTIFIER: &'static str = "SubtensorSignedExtension";

	type AccountId = T::AccountId;
	type Call = T::RuntimeCall;
	type AdditionalSigned = ();
	type Pre = (CallType, u64, Self::AccountId);
	
	fn additional_signed( &self ) -> Result<Self::AdditionalSigned, TransactionValidityError> { 
		Ok(())
	}

	fn validate(
		&self,
		who: &Self::AccountId,
		call: &Self::Call,
		_info: &DispatchInfoOf<Self::Call>,
		len: usize,
	) -> TransactionValidity {
		match call.is_sub_type() {
			Some(Call::set_weights{netuid, ..}) => {
				let priority: u64 = Self::get_priority_set_weights(who, *netuid);
                Ok(ValidTransaction {
                    priority: priority,
                    longevity: 1,
                    ..Default::default()
                })
            }
			Some(Call::add_stake{..}) => {
                Ok(ValidTransaction {
                    priority: Self::get_priority_vanilla(),
                    ..Default::default()
                })
            }
            Some(Call::remove_stake{..}) => {
                Ok(ValidTransaction {
                    priority: Self::get_priority_vanilla(),
                    ..Default::default()
                })
            }
            Some(Call::register{..}) => {
                Ok(ValidTransaction {
                    priority: Self::get_priority_vanilla(),
                    ..Default::default()
                })
            }
			_ => {
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
            Some(Call::add_stake{..}) => {
				let transaction_fee = 0;
                Ok((CallType::AddStake, transaction_fee, who.clone()))
            }
            Some(Call::remove_stake{..}) => {
				let transaction_fee = 0;
                Ok((CallType::RemoveStake, transaction_fee, who.clone()))
            }
			Some(Call::set_weights{..}) => {
				let transaction_fee = 0;
                Ok((CallType::SetWeights, transaction_fee, who.clone())) 
            }
			Some(Call::register{..}) => {
                let transaction_fee = 0;
                Ok((CallType::Register, transaction_fee, who.clone()))
            }
            Some(Call::serve_axon{..}) => {
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
        info: &DispatchInfoOf<Self::Call>,
        post_info: &PostDispatchInfoOf<Self::Call>,
        len: usize,
        result: &dispatch::DispatchResult,
    ) -> Result<(), TransactionValidityError> {

		if let Some((call_type, transaction_fee, who)) = maybe_pre {
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
