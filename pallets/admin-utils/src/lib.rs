#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;
pub mod weights;
pub use weights::WeightInfo;

use frame_system::pallet_prelude::BlockNumberFor;
use sp_runtime::{traits::Member, RuntimeAppPublic};

mod benchmarking;

#[deny(missing_docs)]
#[frame_support::pallet]
pub mod pallet {
    use super::*;
    use frame_support::dispatch::DispatchResult;
    use frame_support::pallet_prelude::*;
    use frame_support::traits::tokens::Balance;
    use frame_system::pallet_prelude::*;
    use sp_runtime::BoundedVec;

    /// The main data structure of the module.
    #[pallet::pallet]
    #[pallet::without_storage_info]
    pub struct Pallet<T>(_);

    /// Configure the pallet by specifying the parameters and types on which it depends.
    #[pallet::config]
    pub trait Config: frame_system::Config + pallet_subtensor::pallet::Config {
        /// Because this pallet emits events, it depends on the runtime's definition of an event.
        type RuntimeEvent: From<Event<Self>> + IsType<<Self as frame_system::Config>::RuntimeEvent>;

        /// Implementation of the AuraInterface
        type Aura: crate::AuraInterface<Self::AuthorityId, Self::MaxAuthorities>;

        /// The identifier type for an authority.
        type AuthorityId: Member
            + Parameter
            + RuntimeAppPublic
            + MaybeSerializeDeserialize
            + MaxEncodedLen;

        /// The maximum number of authorities that the pallet can hold.
        type MaxAuthorities: Get<u32>;

        /// Weight information for extrinsics in this pallet.
        type WeightInfo: WeightInfo;

        /// Unit of assets
        type Balance: Balance;
    }

    #[pallet::event]
    pub enum Event<T: Config> {}

    // Errors inform users that something went wrong.
    #[pallet::error]
    pub enum Error<T> {
        /// The subnet does not exist, check the netuid parameter
        SubnetDoesNotExist,
        /// The maximum number of subnet validators must be less than the maximum number of allowed UIDs in the subnet.
        MaxValidatorsLargerThanMaxUIds,
        /// The maximum number of subnet validators must be more than the current number of UIDs already in the subnet.
        MaxAllowedUIdsLessThanCurrentUIds,
    }

    /// Dispatchable functions allows users to interact with the pallet and invoke state changes.
    #[pallet::call]
    impl<T: Config> Pallet<T> {
        /// The extrinsic sets the new authorities for Aura consensus.
        /// It is only callable by the root account.
        /// The extrinsic will call the Aura pallet to change the authorities.
        #[pallet::call_index(0)]
        #[pallet::weight(T::WeightInfo::swap_authorities(new_authorities.len() as u32))]
        pub fn swap_authorities(
            origin: OriginFor<T>,
            new_authorities: BoundedVec<T::AuthorityId, T::MaxAuthorities>,
        ) -> DispatchResult {
            ensure_root(origin)?;

            T::Aura::change_authorities(new_authorities.clone());

            log::debug!("Aura authorities changed: {:?}", new_authorities);

            // Return a successful DispatchResultWithPostInfo
            Ok(())
        }

        /// The extrinsic sets the default take for the network.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the default take.
        #[pallet::call_index(1)]
        #[pallet::weight(T::WeightInfo::sudo_set_default_take())]
        pub fn sudo_set_default_take(origin: OriginFor<T>, default_take: u16) -> DispatchResult {
            ensure_root(origin)?;
            pallet_subtensor::Pallet::<T>::set_max_delegate_take(default_take);
            log::debug!("DefaultTakeSet( default_take: {:?} ) ", default_take);
            Ok(())
        }

        /// The extrinsic sets the transaction rate limit for the network.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the transaction rate limit.
        #[pallet::call_index(2)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_tx_rate_limit(origin: OriginFor<T>, tx_rate_limit: u64) -> DispatchResult {
            ensure_root(origin)?;
            pallet_subtensor::Pallet::<T>::set_tx_rate_limit(tx_rate_limit);
            log::debug!("TxRateLimitSet( tx_rate_limit: {:?} ) ", tx_rate_limit);
            Ok(())
        }

        /// The extrinsic sets the serving rate limit for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the serving rate limit.
        #[pallet::call_index(3)]
        #[pallet::weight(T::WeightInfo::sudo_set_serving_rate_limit())]
        pub fn sudo_set_serving_rate_limit(
            origin: OriginFor<T>,
            netuid: u16,
            serving_rate_limit: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            pallet_subtensor::Pallet::<T>::set_serving_rate_limit(netuid, serving_rate_limit);
            log::debug!(
                "ServingRateLimitSet( serving_rate_limit: {:?} ) ",
                serving_rate_limit
            );
            Ok(())
        }

        /// The extrinsic sets the minimum difficulty for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the minimum difficulty.
        #[pallet::call_index(4)]
        #[pallet::weight(T::WeightInfo::sudo_set_min_difficulty())]
        pub fn sudo_set_min_difficulty(
            origin: OriginFor<T>,
            netuid: u16,
            min_difficulty: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_min_difficulty(netuid, min_difficulty);
            log::debug!(
                "MinDifficultySet( netuid: {:?} min_difficulty: {:?} ) ",
                netuid,
                min_difficulty
            );
            Ok(())
        }

        /// The extrinsic sets the maximum difficulty for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the maximum difficulty.
        #[pallet::call_index(5)]
        #[pallet::weight(T::WeightInfo::sudo_set_max_difficulty())]
        pub fn sudo_set_max_difficulty(
            origin: OriginFor<T>,
            netuid: u16,
            max_difficulty: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_max_difficulty(netuid, max_difficulty);
            log::debug!(
                "MaxDifficultySet( netuid: {:?} max_difficulty: {:?} ) ",
                netuid,
                max_difficulty
            );
            Ok(())
        }

        /// The extrinsic sets the weights version key for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the weights version key.
        #[pallet::call_index(6)]
        #[pallet::weight(T::WeightInfo::sudo_set_weights_version_key())]
        pub fn sudo_set_weights_version_key(
            origin: OriginFor<T>,
            netuid: u16,
            weights_version_key: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_weights_version_key(netuid, weights_version_key);
            log::debug!(
                "WeightsVersionKeySet( netuid: {:?} weights_version_key: {:?} ) ",
                netuid,
                weights_version_key
            );
            Ok(())
        }

        /// The extrinsic sets the weights set rate limit for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the weights set rate limit.
        #[pallet::call_index(7)]
        #[pallet::weight(T::WeightInfo::sudo_set_weights_set_rate_limit())]
        pub fn sudo_set_weights_set_rate_limit(
            origin: OriginFor<T>,
            netuid: u16,
            weights_set_rate_limit: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_weights_set_rate_limit(
                netuid,
                weights_set_rate_limit,
            );
            log::debug!(
                "WeightsSetRateLimitSet( netuid: {:?} weights_set_rate_limit: {:?} ) ",
                netuid,
                weights_set_rate_limit
            );
            Ok(())
        }

        /// The extrinsic sets the adjustment interval for a subnet.
        /// It is only callable by the root account, not changeable by the subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the adjustment interval.
        #[pallet::call_index(8)]
        #[pallet::weight(T::WeightInfo::sudo_set_adjustment_interval())]
        pub fn sudo_set_adjustment_interval(
            origin: OriginFor<T>,
            netuid: u16,
            adjustment_interval: u16,
        ) -> DispatchResult {
            ensure_root(origin)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_adjustment_interval(netuid, adjustment_interval);
            log::debug!(
                "AdjustmentIntervalSet( netuid: {:?} adjustment_interval: {:?} ) ",
                netuid,
                adjustment_interval
            );
            Ok(())
        }

        /// The extrinsic sets the adjustment alpha for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the adjustment alpha.
        #[pallet::call_index(9)]
        #[pallet::weight((
            Weight::from_parts(14_000_000, 0)
                .saturating_add(T::DbWeight::get().writes(1))
                .saturating_add(T::DbWeight::get().reads(1)),
            DispatchClass::Operational,
            Pays::No
        ))]
        pub fn sudo_set_adjustment_alpha(
            origin: OriginFor<T>,
            netuid: u16,
            adjustment_alpha: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_adjustment_alpha(netuid, adjustment_alpha);
            log::debug!(
                "AdjustmentAlphaSet( adjustment_alpha: {:?} ) ",
                adjustment_alpha
            );
            Ok(())
        }

        /// The extrinsic sets the adjustment beta for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the adjustment beta.
        #[pallet::call_index(12)]
        #[pallet::weight(T::WeightInfo::sudo_set_max_weight_limit())]
        pub fn sudo_set_max_weight_limit(
            origin: OriginFor<T>,
            netuid: u16,
            max_weight_limit: u16,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_max_weight_limit(netuid, max_weight_limit);
            log::debug!(
                "MaxWeightLimitSet( netuid: {:?} max_weight_limit: {:?} ) ",
                netuid,
                max_weight_limit
            );
            Ok(())
        }

        /// The extrinsic sets the immunity period for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the immunity period.
        #[pallet::call_index(13)]
        #[pallet::weight(T::WeightInfo::sudo_set_immunity_period())]
        pub fn sudo_set_immunity_period(
            origin: OriginFor<T>,
            netuid: u16,
            immunity_period: u16,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;
            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );

            pallet_subtensor::Pallet::<T>::set_immunity_period(netuid, immunity_period);
            log::debug!(
                "ImmunityPeriodSet( netuid: {:?} immunity_period: {:?} ) ",
                netuid,
                immunity_period
            );
            Ok(())
        }

        /// The extrinsic sets the minimum allowed weights for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the minimum allowed weights.
        #[pallet::call_index(14)]
        #[pallet::weight(T::WeightInfo::sudo_set_min_allowed_weights())]
        pub fn sudo_set_min_allowed_weights(
            origin: OriginFor<T>,
            netuid: u16,
            min_allowed_weights: u16,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_min_allowed_weights(netuid, min_allowed_weights);
            log::debug!(
                "MinAllowedWeightSet( netuid: {:?} min_allowed_weights: {:?} ) ",
                netuid,
                min_allowed_weights
            );
            Ok(())
        }

        /// The extrinsic sets the maximum allowed UIDs for a subnet.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the maximum allowed UIDs for a subnet.
        #[pallet::call_index(15)]
        #[pallet::weight(T::WeightInfo::sudo_set_max_allowed_uids())]
        pub fn sudo_set_max_allowed_uids(
            origin: OriginFor<T>,
            netuid: u16,
            max_allowed_uids: u16,
        ) -> DispatchResult {
            ensure_root(origin)?;
            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            ensure!(
                pallet_subtensor::Pallet::<T>::get_subnetwork_n(netuid) < max_allowed_uids,
                Error::<T>::MaxAllowedUIdsLessThanCurrentUIds
            );
            pallet_subtensor::Pallet::<T>::set_max_allowed_uids(netuid, max_allowed_uids);
            log::debug!(
                "MaxAllowedUidsSet( netuid: {:?} max_allowed_uids: {:?} ) ",
                netuid,
                max_allowed_uids
            );
            Ok(())
        }

        /// The extrinsic sets the kappa for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the kappa.
        #[pallet::call_index(16)]
        #[pallet::weight(T::WeightInfo::sudo_set_kappa())]
        pub fn sudo_set_kappa(origin: OriginFor<T>, netuid: u16, kappa: u16) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_kappa(netuid, kappa);
            log::debug!("KappaSet( netuid: {:?} kappa: {:?} ) ", netuid, kappa);
            Ok(())
        }

        /// The extrinsic sets the rho for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the rho.
        #[pallet::call_index(17)]
        #[pallet::weight(T::WeightInfo::sudo_set_rho())]
        pub fn sudo_set_rho(origin: OriginFor<T>, netuid: u16, rho: u16) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_rho(netuid, rho);
            log::debug!("RhoSet( netuid: {:?} rho: {:?} ) ", netuid, rho);
            Ok(())
        }

        /// The extrinsic sets the activity cutoff for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the activity cutoff.
        #[pallet::call_index(18)]
        #[pallet::weight(T::WeightInfo::sudo_set_activity_cutoff())]
        pub fn sudo_set_activity_cutoff(
            origin: OriginFor<T>,
            netuid: u16,
            activity_cutoff: u16,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_activity_cutoff(netuid, activity_cutoff);
            log::debug!(
                "ActivityCutoffSet( netuid: {:?} activity_cutoff: {:?} ) ",
                netuid,
                activity_cutoff
            );
            Ok(())
        }

        /// The extrinsic sets the network registration allowed for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the network registration allowed.
        #[pallet::call_index(19)]
        #[pallet::weight((
			Weight::from_parts(4_000_000, 0)
				.saturating_add(Weight::from_parts(0, 0))
				.saturating_add(T::DbWeight::get().writes(1)),
			DispatchClass::Operational,
			Pays::No
		))]
        pub fn sudo_set_network_registration_allowed(
            origin: OriginFor<T>,
            netuid: u16,
            registration_allowed: bool,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            pallet_subtensor::Pallet::<T>::set_network_registration_allowed(
                netuid,
                registration_allowed,
            );
            log::debug!(
                "NetworkRegistrationAllowed( registration_allowed: {:?} ) ",
                registration_allowed
            );
            Ok(())
        }

        /// The extrinsic sets the network PoW registration allowed for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the network PoW registration allowed.
        #[pallet::call_index(20)]
        #[pallet::weight((
			Weight::from_parts(14_000_000, 0)
				.saturating_add(T::DbWeight::get().writes(1)),
			DispatchClass::Operational,
			Pays::No
		))]
        pub fn sudo_set_network_pow_registration_allowed(
            origin: OriginFor<T>,
            netuid: u16,
            registration_allowed: bool,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            pallet_subtensor::Pallet::<T>::set_network_pow_registration_allowed(
                netuid,
                registration_allowed,
            );
            log::debug!(
                "NetworkPowRegistrationAllowed( registration_allowed: {:?} ) ",
                registration_allowed
            );
            Ok(())
        }

        /// The extrinsic sets the target registrations per interval for a subnet.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the target registrations per interval.
        #[pallet::call_index(21)]
        #[pallet::weight(T::WeightInfo::sudo_set_target_registrations_per_interval())]
        pub fn sudo_set_target_registrations_per_interval(
            origin: OriginFor<T>,
            netuid: u16,
            target_registrations_per_interval: u16,
        ) -> DispatchResult {
            ensure_root(origin)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_target_registrations_per_interval(
                netuid,
                target_registrations_per_interval,
            );
            log::debug!(
            "RegistrationPerIntervalSet( netuid: {:?} target_registrations_per_interval: {:?} ) ",
            netuid,
            target_registrations_per_interval
        );
            Ok(())
        }

        /// The extrinsic sets the minimum burn for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the minimum burn.
        #[pallet::call_index(22)]
        #[pallet::weight(T::WeightInfo::sudo_set_min_burn())]
        pub fn sudo_set_min_burn(
            origin: OriginFor<T>,
            netuid: u16,
            min_burn: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_min_burn(netuid, min_burn);
            log::debug!(
                "MinBurnSet( netuid: {:?} min_burn: {:?} ) ",
                netuid,
                min_burn
            );
            Ok(())
        }

        /// The extrinsic sets the maximum burn for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the maximum burn.
        #[pallet::call_index(23)]
        #[pallet::weight(T::WeightInfo::sudo_set_max_burn())]
        pub fn sudo_set_max_burn(
            origin: OriginFor<T>,
            netuid: u16,
            max_burn: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_max_burn(netuid, max_burn);
            log::debug!(
                "MaxBurnSet( netuid: {:?} max_burn: {:?} ) ",
                netuid,
                max_burn
            );
            Ok(())
        }

        /// The extrinsic sets the difficulty for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the difficulty.
        #[pallet::call_index(24)]
        #[pallet::weight(T::WeightInfo::sudo_set_difficulty())]
        pub fn sudo_set_difficulty(
            origin: OriginFor<T>,
            netuid: u16,
            difficulty: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;
            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_difficulty(netuid, difficulty);
            log::debug!(
                "DifficultySet( netuid: {:?} difficulty: {:?} ) ",
                netuid,
                difficulty
            );
            Ok(())
        }

        /// The extrinsic sets the maximum allowed validators for a subnet.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the maximum allowed validators.
        #[pallet::call_index(25)]
        #[pallet::weight(T::WeightInfo::sudo_set_max_allowed_validators())]
        pub fn sudo_set_max_allowed_validators(
            origin: OriginFor<T>,
            netuid: u16,
            max_allowed_validators: u16,
        ) -> DispatchResult {
            ensure_root(origin)?;
            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            ensure!(
                max_allowed_validators
                    <= pallet_subtensor::Pallet::<T>::get_max_allowed_uids(netuid),
                Error::<T>::MaxValidatorsLargerThanMaxUIds
            );

            pallet_subtensor::Pallet::<T>::set_max_allowed_validators(
                netuid,
                max_allowed_validators,
            );
            log::debug!(
                "MaxAllowedValidatorsSet( netuid: {:?} max_allowed_validators: {:?} ) ",
                netuid,
                max_allowed_validators
            );
            Ok(())
        }

        /// The extrinsic sets the bonds moving average for a subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the bonds moving average.
        #[pallet::call_index(26)]
        #[pallet::weight(T::WeightInfo::sudo_set_bonds_moving_average())]
        pub fn sudo_set_bonds_moving_average(
            origin: OriginFor<T>,
            netuid: u16,
            bonds_moving_average: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_bonds_moving_average(netuid, bonds_moving_average);
            log::debug!(
                "BondsMovingAverageSet( netuid: {:?} bonds_moving_average: {:?} ) ",
                netuid,
                bonds_moving_average
            );
            Ok(())
        }

        /// The extrinsic sets the maximum registrations per block for a subnet.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the maximum registrations per block.
        #[pallet::call_index(27)]
        #[pallet::weight(T::WeightInfo::sudo_set_max_registrations_per_block())]
        pub fn sudo_set_max_registrations_per_block(
            origin: OriginFor<T>,
            netuid: u16,
            max_registrations_per_block: u16,
        ) -> DispatchResult {
            ensure_root(origin)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_max_registrations_per_block(
                netuid,
                max_registrations_per_block,
            );
            log::debug!(
                "MaxRegistrationsPerBlock( netuid: {:?} max_registrations_per_block: {:?} ) ",
                netuid,
                max_registrations_per_block
            );
            Ok(())
        }

        /// The extrinsic sets the subnet owner cut for a subnet.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the subnet owner cut.
        #[pallet::call_index(28)]
        #[pallet::weight((
			Weight::from_parts(14_000_000, 0)
				.saturating_add(T::DbWeight::get().writes(1)),
			DispatchClass::Operational,
			Pays::No
		))]
        pub fn sudo_set_subnet_owner_cut(
            origin: OriginFor<T>,
            subnet_owner_cut: u16,
        ) -> DispatchResult {
            ensure_root(origin)?;
            pallet_subtensor::Pallet::<T>::set_subnet_owner_cut(subnet_owner_cut);
            log::debug!(
                "SubnetOwnerCut( subnet_owner_cut: {:?} ) ",
                subnet_owner_cut
            );
            Ok(())
        }

        /// The extrinsic sets the network rate limit for the network.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the network rate limit.
        #[pallet::call_index(29)]
        #[pallet::weight((
			Weight::from_parts(14_000_000, 0)
				.saturating_add(T::DbWeight::get().writes(1)),
			DispatchClass::Operational,
			Pays::No
		))]
        pub fn sudo_set_network_rate_limit(
            origin: OriginFor<T>,
            rate_limit: u64,
        ) -> DispatchResult {
            ensure_root(origin)?;
            pallet_subtensor::Pallet::<T>::set_network_rate_limit(rate_limit);
            log::debug!("NetworkRateLimit( rate_limit: {:?} ) ", rate_limit);
            Ok(())
        }

        /// The extrinsic sets the tempo for a subnet.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the tempo.
        #[pallet::call_index(30)]
        #[pallet::weight(T::WeightInfo::sudo_set_tempo())]
        pub fn sudo_set_tempo(origin: OriginFor<T>, netuid: u16, tempo: u16) -> DispatchResult {
            ensure_root(origin)?;
            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_tempo(netuid, tempo);
            log::debug!("TempoSet( netuid: {:?} tempo: {:?} ) ", netuid, tempo);
            Ok(())
        }

        /// The extrinsic sets the total issuance for the network.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the issuance for the network.
        #[pallet::call_index(33)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_total_issuance(
            origin: OriginFor<T>,
            total_issuance: u64,
        ) -> DispatchResult {
            ensure_root(origin)?;

            pallet_subtensor::Pallet::<T>::set_total_issuance(total_issuance);

            Ok(())
        }

        /// The extrinsic sets the immunity period for the network.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the immunity period for the network.
        #[pallet::call_index(35)]
        #[pallet::weight((
			Weight::from_parts(14_000_000, 0)
				.saturating_add(T::DbWeight::get().writes(1)),
			DispatchClass::Operational,
			Pays::No
		))]
        pub fn sudo_set_network_immunity_period(
            origin: OriginFor<T>,
            immunity_period: u64,
        ) -> DispatchResult {
            ensure_root(origin)?;

            pallet_subtensor::Pallet::<T>::set_network_immunity_period(immunity_period);

            log::debug!("NetworkImmunityPeriod( period: {:?} ) ", immunity_period);

            Ok(())
        }

        /// The extrinsic sets the min lock cost for the network.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the min lock cost for the network.
        #[pallet::call_index(36)]
        #[pallet::weight((
			Weight::from_parts(14_000_000, 0)
				.saturating_add(T::DbWeight::get().writes(1)),
			DispatchClass::Operational,
			Pays::No
		))]
        pub fn sudo_set_network_min_lock_cost(
            origin: OriginFor<T>,
            lock_cost: u64,
        ) -> DispatchResult {
            ensure_root(origin)?;

            pallet_subtensor::Pallet::<T>::set_network_min_lock(lock_cost);

            log::debug!("NetworkMinLockCost( lock_cost: {:?} ) ", lock_cost);

            Ok(())
        }

        /// The extrinsic sets the subnet limit for the network.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the subnet limit.
        #[pallet::call_index(37)]
        #[pallet::weight((
			Weight::from_parts(14_000_000, 0)
				.saturating_add(T::DbWeight::get().writes(1)),
			DispatchClass::Operational,
			Pays::No
		))]
        pub fn sudo_set_subnet_limit(origin: OriginFor<T>, max_subnets: u16) -> DispatchResult {
            ensure_root(origin)?;
            pallet_subtensor::Pallet::<T>::set_max_subnets(max_subnets);

            log::debug!("SubnetLimit( max_subnets: {:?} ) ", max_subnets);

            Ok(())
        }

        /// The extrinsic sets the lock reduction interval for the network.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the lock reduction interval.
        #[pallet::call_index(38)]
        #[pallet::weight((
			Weight::from_parts(14_000_000, 0)
				.saturating_add(T::DbWeight::get().writes(1)),
			DispatchClass::Operational,
			Pays::No
		))]
        pub fn sudo_set_lock_reduction_interval(
            origin: OriginFor<T>,
            interval: u64,
        ) -> DispatchResult {
            ensure_root(origin)?;

            pallet_subtensor::Pallet::<T>::set_lock_reduction_interval(interval);

            log::debug!("NetworkLockReductionInterval( interval: {:?} ) ", interval);

            Ok(())
        }

        /// The extrinsic sets the recycled RAO for a subnet.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the recycled RAO.
        #[pallet::call_index(39)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_rao_recycled(
            origin: OriginFor<T>,
            netuid: u16,
            rao_recycled: u64,
        ) -> DispatchResult {
            ensure_root(origin)?;
            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );
            pallet_subtensor::Pallet::<T>::set_rao_recycled(netuid, rao_recycled);
            Ok(())
        }

        /// The extrinsic sets the weights min stake.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the weights min stake.
        #[pallet::call_index(42)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_weights_min_stake(origin: OriginFor<T>, min_stake: u64) -> DispatchResult {
            ensure_root(origin)?;
            pallet_subtensor::Pallet::<T>::set_weights_min_stake(min_stake);
            Ok(())
        }

        /// The extrinsic sets the minimum stake required for nominators.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the minimum stake required for nominators.
        #[pallet::call_index(43)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_nominator_min_required_stake(
            origin: OriginFor<T>,
            // The minimum stake required for nominators.
            min_stake: u64,
        ) -> DispatchResult {
            ensure_root(origin)?;
            let prev_min_stake = pallet_subtensor::Pallet::<T>::get_nominator_min_required_stake();
            log::trace!("Setting minimum stake to: {}", min_stake);
            pallet_subtensor::Pallet::<T>::set_nominator_min_required_stake(min_stake);
            if min_stake > prev_min_stake {
                log::trace!("Clearing small nominations");
                pallet_subtensor::Pallet::<T>::clear_small_nominations();
                log::trace!("Small nominations cleared");
            }
            Ok(())
        }

        /// The extrinsic sets the rate limit for delegate take transactions.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the rate limit for delegate take transactions.
        #[pallet::call_index(45)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_tx_delegate_take_rate_limit(
            origin: OriginFor<T>,
            tx_rate_limit: u64,
        ) -> DispatchResult {
            ensure_root(origin)?;
            pallet_subtensor::Pallet::<T>::set_tx_delegate_take_rate_limit(tx_rate_limit);
            log::debug!(
                "TxRateLimitDelegateTakeSet( tx_delegate_take_rate_limit: {:?} ) ",
                tx_rate_limit
            );
            Ok(())
        }

        /// The extrinsic sets the minimum delegate take.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set the minimum delegate take.
        #[pallet::call_index(46)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_min_delegate_take(origin: OriginFor<T>, take: u16) -> DispatchResult {
            ensure_root(origin)?;
            pallet_subtensor::Pallet::<T>::set_min_delegate_take(take);
            log::debug!("TxMinDelegateTakeSet( tx_min_delegate_take: {:?} ) ", take);
            Ok(())
        }

        /// The extrinsic sets the target stake per interval.
        /// It is only callable by the root account.
        /// The extrinsic will call the Subtensor pallet to set target stake per interval.
        #[pallet::call_index(47)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_target_stakes_per_interval(
            origin: OriginFor<T>,
            target_stakes_per_interval: u64,
        ) -> DispatchResult {
            ensure_root(origin)?;
            pallet_subtensor::Pallet::<T>::set_target_stakes_per_interval(
                target_stakes_per_interval,
            );
            log::debug!(
                "TxTargetStakesPerIntervalSet( set_target_stakes_per_interval: {:?} ) ",
                target_stakes_per_interval
            );
            Ok(())
        }

        /// The extrinsic enabled/disables commit/reaveal for a given subnet.
        /// It is only callable by the root account or subnet owner.
        /// The extrinsic will call the Subtensor pallet to set the value.
        #[pallet::call_index(49)]
        #[pallet::weight(T::WeightInfo::sudo_set_commit_reveal_weights_enabled())]
        pub fn sudo_set_commit_reveal_weights_enabled(
            origin: OriginFor<T>,
            netuid: u16,
            enabled: bool,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );

            pallet_subtensor::Pallet::<T>::set_commit_reveal_weights_enabled(netuid, enabled);
            log::debug!("ToggleSetWeightsCommitReveal( netuid: {:?} ) ", netuid);
            Ok(())
        }

        /// Enables or disables Liquid Alpha for a given subnet.
        ///
        /// # Parameters
        /// - `origin`: The origin of the call, which must be the root account or subnet owner.
        /// - `netuid`: The unique identifier for the subnet.
        /// - `enabled`: A boolean flag to enable or disable Liquid Alpha.
        ///
        /// # Weight
        /// This function has a fixed weight of 0 and is classified as an operational transaction that does not incur any fees.
        #[pallet::call_index(50)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_liquid_alpha_enabled(
            origin: OriginFor<T>,
            netuid: u16,
            enabled: bool,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;
            pallet_subtensor::Pallet::<T>::set_liquid_alpha_enabled(netuid, enabled);
            log::debug!(
                "LiquidAlphaEnableToggled( netuid: {:?}, Enabled: {:?} ) ",
                netuid,
                enabled
            );
            Ok(())
        }

        /// Sets values for liquid alpha
        #[pallet::call_index(51)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_alpha_values(
            origin: OriginFor<T>,
            netuid: u16,
            alpha_low: u16,
            alpha_high: u16,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin.clone(), netuid)?;
            pallet_subtensor::Pallet::<T>::do_set_alpha_values(
                origin, netuid, alpha_low, alpha_high,
            )
        }

        /// Sets the hotkey emission tempo.
        ///
        /// This extrinsic allows the root account to set the hotkey emission tempo, which determines
        /// the number of blocks before a hotkey drains accumulated emissions through to nominator staking accounts.
        ///
        /// # Arguments
        /// * `origin` - The origin of the call, which must be the root account.
        /// * `emission_tempo` - The new emission tempo value to set.
        ///
        /// # Emits
        /// * `Event::HotkeyEmissionTempoSet` - When the hotkey emission tempo is successfully set.
        ///
        /// # Errors
        /// * `DispatchError::BadOrigin` - If the origin is not the root account.
        // #[pallet::weight(T::WeightInfo::sudo_set_hotkey_emission_tempo())]
        #[pallet::call_index(52)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_hotkey_emission_tempo(
            origin: OriginFor<T>,
            emission_tempo: u64,
        ) -> DispatchResult {
            ensure_root(origin)?;
            pallet_subtensor::Pallet::<T>::set_hotkey_emission_tempo(emission_tempo);
            log::debug!(
                "HotkeyEmissionTempoSet( emission_tempo: {:?} )",
                emission_tempo
            );
            Ok(())
        }

        /// Sets the maximum stake allowed for a specific network.
        ///
        /// This function allows the root account to set the maximum stake for a given network.
        /// It updates the network's maximum stake value and logs the change.
        ///
        /// # Arguments
        ///
        /// * `origin` - The origin of the call, which must be the root account.
        /// * `netuid` - The unique identifier of the network.
        /// * `max_stake` - The new maximum stake value to set.
        ///
        /// # Returns
        ///
        /// Returns `Ok(())` if the operation is successful, or an error if it fails.
        ///
        /// # Example
        ///
        ///
        /// # Notes
        ///
        /// - This function can only be called by the root account.
        /// - The `netuid` should correspond to an existing network.
        ///
        /// # TODO
        ///
        // - Consider adding a check to ensure the `netuid` corresponds to an existing network.
        // - Implement a mechanism to gradually adjust the max stake to prevent sudden changes.
        // #[pallet::weight(T::WeightInfo::sudo_set_network_max_stake())]
        #[pallet::call_index(53)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_network_max_stake(
            origin: OriginFor<T>,
            netuid: u16,
            max_stake: u64,
        ) -> DispatchResult {
            // Ensure the call is made by the root account
            ensure_root(origin)?;

            // Set the new maximum stake for the specified network
            pallet_subtensor::Pallet::<T>::set_network_max_stake(netuid, max_stake);

            // Log the change
            log::trace!(
                "NetworkMaxStakeSet( netuid: {:?}, max_stake: {:?} )",
                netuid,
                max_stake
            );

            Ok(())
        }

        /// Sets the duration of the coldkey swap schedule.
        ///
        /// This extrinsic allows the root account to set the duration for the coldkey swap schedule.
        /// The coldkey swap schedule determines how long it takes for a coldkey swap operation to complete.
        ///
        /// # Arguments
        /// * `origin` - The origin of the call, which must be the root account.
        /// * `duration` - The new duration for the coldkey swap schedule, in number of blocks.
        ///
        /// # Errors
        /// * `BadOrigin` - If the caller is not the root account.
        ///
        /// # Weight
        /// Weight is handled by the `#[pallet::weight]` attribute.
        #[pallet::call_index(54)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_coldkey_swap_schedule_duration(
            origin: OriginFor<T>,
            duration: BlockNumberFor<T>,
        ) -> DispatchResult {
            // Ensure the call is made by the root account
            ensure_root(origin)?;

            // Set the new duration of schedule coldkey swap
            pallet_subtensor::Pallet::<T>::set_coldkey_swap_schedule_duration(duration);

            // Log the change
            log::trace!("ColdkeySwapScheduleDurationSet( duration: {:?} )", duration);

            Ok(())
        }

        /// Sets the duration of the dissolve network schedule.
        ///
        /// This extrinsic allows the root account to set the duration for the dissolve network schedule.
        /// The dissolve network schedule determines how long it takes for a network dissolution operation to complete.
        ///
        /// # Arguments
        /// * `origin` - The origin of the call, which must be the root account.
        /// * `duration` - The new duration for the dissolve network schedule, in number of blocks.
        ///
        /// # Errors
        /// * `BadOrigin` - If the caller is not the root account.
        ///
        /// # Weight
        /// Weight is handled by the `#[pallet::weight]` attribute.
        #[pallet::call_index(55)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_dissolve_network_schedule_duration(
            origin: OriginFor<T>,
            duration: BlockNumberFor<T>,
        ) -> DispatchResult {
            // Ensure the call is made by the root account
            ensure_root(origin)?;

            // Set the duration of schedule dissolve network
            pallet_subtensor::Pallet::<T>::set_dissolve_network_schedule_duration(duration);

            // Log the change
            log::trace!(
                "DissolveNetworkScheduleDurationSet( duration: {:?} )",
                duration
            );

            Ok(())
        }

        /// Sets the commit-reveal weights periods for a specific subnet.
        ///
        /// This extrinsic allows the subnet owner or root account to set the duration (in epochs) during which committed weights must be revealed.
        /// The commit-reveal mechanism ensures that users commit weights in advance and reveal them only within a specified period.
        ///
        /// # Arguments
        /// * `origin` - The origin of the call, which must be the subnet owner or the root account.
        /// * `netuid` - The unique identifier of the subnet for which the periods are being set.
        /// * `periods` - The number of epochs that define the commit-reveal period.
        ///
        /// # Errors
        /// * `BadOrigin` - If the caller is neither the subnet owner nor the root account.
        /// * `SubnetDoesNotExist` - If the specified subnet does not exist.
        ///
        /// # Weight
        /// Weight is handled by the `#[pallet::weight]` attribute.
        #[pallet::call_index(56)]
        #[pallet::weight(T::WeightInfo::sudo_set_commit_reveal_weights_periods())]
        pub fn sudo_set_commit_reveal_weights_periods(
            origin: OriginFor<T>,
            netuid: u16,
            periods: u64,
        ) -> DispatchResult {
            pallet_subtensor::Pallet::<T>::ensure_subnet_owner_or_root(origin, netuid)?;

            ensure!(
                pallet_subtensor::Pallet::<T>::if_subnet_exist(netuid),
                Error::<T>::SubnetDoesNotExist
            );

            pallet_subtensor::Pallet::<T>::set_reveal_period(netuid, periods);
            log::debug!(
                "SetWeightCommitPeriods( netuid: {:?}, periods: {:?} ) ",
                netuid,
                periods
            );
            Ok(())
        }
    }
}

impl<T: Config> sp_runtime::BoundToRuntimeAppPublic for Pallet<T> {
    type Public = T::AuthorityId;
}

// Interfaces to interact with other pallets
use sp_runtime::BoundedVec;

pub trait AuraInterface<AuthorityId, MaxAuthorities> {
    fn change_authorities(new: BoundedVec<AuthorityId, MaxAuthorities>);
}

impl<A, M> AuraInterface<A, M> for () {
    fn change_authorities(_: BoundedVec<A, M>) {}
}
