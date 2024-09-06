use frame_support::pallet_macros::pallet_section;

/// A [`pallet_section`] that defines the errors for a pallet.
/// This can later be imported into the pallet using [`import_section`].
#[pallet_section]
mod dispatches {
    /// Dispatchable functions allow users to interact with the pallet and invoke state changes.
    /// These functions materialize as "extrinsics", which are often compared to transactions.
    /// Dispatchable functions must be annotated with a weight and must return a DispatchResult.
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
        /// * `origin`: (<T as frame_system::Config>Origin):
        ///     - The caller, a hotkey who wishes to set their weights.
        ///
        /// * `netuid` (u16):
        /// 	- The network uid we are setting these weights on.
        ///
        /// * `dests` (Vec<u16>):
        /// 	- The edge endpoint for the weight, i.e. j for w_ij.
        ///
        /// * 'weights' (Vec<u16>):
        /// 	- The u16 integer encoded weights. Interpreted as rational
        /// 		values in the range [0,1]. They must sum to in32::MAX.
        ///
        /// * 'version_key' ( u64 ):
        /// 	- The network version key to check if the validator is up to date.
        ///
        /// # Event:
        /// * WeightsSet;
        /// 	- On successfully setting the weights on chain.
        ///
        /// # Raises:
        /// * 'SubNetworkDoesNotExist':
        /// 	- Attempting to set weights on a non-existent network.
        ///
        /// * 'NotRegistered':
        /// 	- Attempting to set weights from a non registered account.
        ///
        /// * 'WeightVecNotEqualSize':
        /// 	- Attempting to set weights with uids not of same length.
        ///
        /// * 'DuplicateUids':
        /// 	- Attempting to set weights with duplicate uids.
        ///
        ///     * 'UidsLengthExceedUidsInSubNet':
        /// 	- Attempting to set weights above the max allowed uids.
        ///
        /// * 'UidVecContainInvalidOne':
        /// 	- Attempting to set weights with invalid uids.
        ///
        /// * 'WeightVecLengthIsLow':
        /// 	- Attempting to set weights with fewer weights than min.
        ///
        /// * 'MaxWeightExceeded':
        /// 	- Attempting to set weights with max value exceeding limit.
        #[pallet::call_index(0)]
        #[pallet::weight((Weight::from_parts(22_060_000_000, 0)
        .saturating_add(T::DbWeight::get().reads(4106))
        .saturating_add(T::DbWeight::get().writes(2)), DispatchClass::Normal, Pays::No))]
        pub fn set_weights(
            origin: OriginFor<T>,
            netuid: u16,
            dests: Vec<u16>,
            weights: Vec<u16>,
            version_key: u64,
        ) -> DispatchResult {
            if !Self::get_commit_reveal_weights_enabled(netuid) {
                return Self::do_set_weights(origin, netuid, dests, weights, version_key);
            }

            Err(Error::<T>::CommitRevealEnabled.into())
        }

        /// ---- Used to commit a hash of your weight values to later be revealed.
        ///
        /// # Args:
        /// * `origin`: (`<T as frame_system::Config>::RuntimeOrigin`):
        ///   - The signature of the committing hotkey.
        ///
        /// * `netuid` (`u16`):
        ///   - The u16 network identifier.
        ///
        /// * `commit_hash` (`H256`):
        ///   - The hash representing the committed weights.
        ///
        /// # Raises:
        /// * `WeightsCommitNotAllowed`:
        ///   - Attempting to commit when it is not allowed.
        ///
        #[pallet::call_index(96)]
        #[pallet::weight((Weight::from_parts(46_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(1))
		.saturating_add(T::DbWeight::get().writes(1)), DispatchClass::Normal, Pays::No))]
        pub fn commit_weights(
            origin: T::RuntimeOrigin,
            netuid: u16,
            commit_hash: H256,
        ) -> DispatchResult {
            Self::do_commit_weights(origin, netuid, commit_hash)
        }

        /// ---- Used to reveal the weights for a previously committed hash.
        ///
        /// # Args:
        /// * `origin`: (`<T as frame_system::Config>::RuntimeOrigin`):
        ///   - The signature of the revealing hotkey.
        ///
        /// * `netuid` (`u16`):
        ///   - The u16 network identifier.
        ///
        /// * `uids` (`Vec<u16>`):
        ///   - The uids for the weights being revealed.
        ///
        /// * `values` (`Vec<u16>`):
        ///   - The values of the weights being revealed.
        ///
        /// * `salt` (`Vec<u8>`):
        ///   - The random salt to protect from brute-force guessing attack in case of small weight changes bit-wise.
        ///
        /// * `version_key` (`u64`):
        ///   - The network version key.
        ///
        /// # Raises:
        /// * `NoWeightsCommitFound`:
        ///   - Attempting to reveal weights without an existing commit.
        ///
        /// * `InvalidRevealCommitHashNotMatchTempo`:
        ///   - Attempting to reveal weights outside the valid tempo.
        ///
        /// * `InvalidRevealCommitHashNotMatch`:
        ///   - The revealed hash does not match the committed hash.
        ///
        #[pallet::call_index(97)]
        #[pallet::weight((Weight::from_parts(103_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(11))
		.saturating_add(T::DbWeight::get().writes(3)), DispatchClass::Normal, Pays::No))]
        pub fn reveal_weights(
            origin: T::RuntimeOrigin,
            netuid: u16,
            uids: Vec<u16>,
            values: Vec<u16>,
            salt: Vec<u16>,
            version_key: u64,
        ) -> DispatchResult {
            Self::do_reveal_weights(origin, netuid, uids, values, salt, version_key)
        }

        /// # Args:
        /// * `origin`: (<T as frame_system::Config>Origin):
        /// 	- The caller, a hotkey who wishes to set their weights.
        ///
        /// * `netuid` (u16):
        /// 	- The network uid we are setting these weights on.
        ///
        /// * `hotkey` (T::AccountId):
        /// 	- The hotkey associated with the operation and the calling coldkey.
        ///
        /// * `dests` (Vec<u16>):
        /// 	- The edge endpoint for the weight, i.e. j for w_ij.
        ///
        /// * 'weights' (Vec<u16>):
        /// 	- The u16 integer encoded weights. Interpreted as rational
        /// 		values in the range [0,1]. They must sum to in32::MAX.
        ///
        /// * 'version_key' ( u64 ):
        /// 	- The network version key to check if the validator is up to date.
        ///
        /// # Event:
        ///
        /// * WeightsSet;
        /// 	- On successfully setting the weights on chain.
        ///
        /// # Raises:
        ///
        /// * NonAssociatedColdKey;
        /// 	- Attempting to set weights on a non-associated cold key.
        ///
        /// * 'SubNetworkDoesNotExist':
        /// 	- Attempting to set weights on a non-existent network.
        ///
        /// * 'NotRootSubnet':
        /// 	- Attempting to set weights on a subnet that is not the root network.
        ///
        /// * 'WeightVecNotEqualSize':
        /// 	- Attempting to set weights with uids not of same length.
        ///
        /// * 'UidVecContainInvalidOne':
        /// 	- Attempting to set weights with invalid uids.
        ///
        /// * 'NotRegistered':
        /// 	- Attempting to set weights from a non registered account.
        ///
        /// * 'WeightVecLengthIsLow':
        /// 	- Attempting to set weights with fewer weights than min.
        ///
        ///  * 'IncorrectWeightVersionKey':
        ///      - Attempting to set weights with the incorrect network version key.
        ///
        ///  * 'SettingWeightsTooFast':
        ///      - Attempting to set weights too fast.
        ///
        /// * 'WeightVecLengthIsLow':
        /// 	- Attempting to set weights with fewer weights than min.
        ///
        /// * 'MaxWeightExceeded':
        /// 	- Attempting to set weights with max value exceeding limit.
        ///
        #[pallet::call_index(8)]
        #[pallet::weight((Weight::from_parts(10_151_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(4104))
		.saturating_add(T::DbWeight::get().writes(2)), DispatchClass::Normal, Pays::No))]
        pub fn set_root_weights(
            _origin: OriginFor<T>,
            _netuid: u16,
            _hotkey: T::AccountId,
            _dests: Vec<u16>,
            _weights: Vec<u16>,
            _version_key: u64,
        ) -> DispatchResult {
            // DEPRECATED
            // Self::do_set_root_weights(origin, netuid, hotkey, dests, weights, version_key)
            Ok(())
        }

        /// --- Sets the key as a delegate.
        ///
        /// # Args:
        /// * 'origin': (<T as frame_system::Config>Origin):
        /// 	- The signature of the caller's coldkey.
        ///
        /// * 'hotkey' (T::AccountId):
        /// 	- The hotkey we are delegating (must be owned by the coldkey.)
        ///
        /// * 'take' (u64):
        /// 	- The stake proportion that this hotkey takes from delegations.
        ///
        /// # Event:
        /// * DelegateAdded;
        /// 	- On successfully setting a hotkey as a delegate.
        ///
        /// # Raises:
        /// * 'NotRegistered':
        /// 	- The hotkey we are delegating is not registered on the network.
        ///
        /// * 'NonAssociatedColdKey':
        /// 	- The hotkey we are delegating is not owned by the calling coldket.
        ///
        #[pallet::call_index(1)]
        #[pallet::weight((Weight::from_parts(79_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(6))
		.saturating_add(T::DbWeight::get().writes(3)), DispatchClass::Normal, Pays::No))]
        pub fn become_delegate(origin: OriginFor<T>, hotkey: T::AccountId) -> DispatchResult {
            Self::do_become_delegate(origin, hotkey, Self::get_default_take())
        }

        /// --- Allows delegates to decrease its take value.
        ///
        /// # Args:
        /// * 'origin': (<T as frame_system::Config>::Origin):
        /// 	- The signature of the caller's coldkey.
        ///
        /// * 'hotkey' (T::AccountId):
        /// 	- The hotkey we are delegating (must be owned by the coldkey.)
        ///
        /// * 'netuid' (u16):
        /// 	- Subnet ID to decrease take for
        ///
        /// * 'take' (u16):
        /// 	- The new stake proportion that this hotkey takes from delegations.
        ///        The new value can be between 0 and 11_796 and should be strictly
        ///        lower than the previous value. It T is the new value (rational number),
        ///        the the parameter is calculated as [65535 * T]. For example, 1% would be
        ///        [0.01 * 65535] = [655.35] = 655
        ///
        /// # Event:
        /// * TakeDecreased;
        /// 	- On successfully setting a decreased take for this hotkey.
        ///
        /// # Raises:
        /// * 'NotRegistered':
        /// 	- The hotkey we are delegating is not registered on the network.
        ///
        /// * 'NonAssociatedColdKey':
        /// 	- The hotkey we are delegating is not owned by the calling coldkey.
        ///
        /// * 'DelegateTakeTooLow':
        /// 	- The delegate is setting a take which is not lower than the previous.
        ///
        #[pallet::call_index(65)]
        #[pallet::weight((0, DispatchClass::Normal, Pays::No))]
        pub fn decrease_take(
            origin: OriginFor<T>,
            hotkey: T::AccountId,
            take: u16,
        ) -> DispatchResult {
            Self::do_decrease_take(origin, hotkey, take)
        }

        /// --- Allows delegates to increase its take value. This call is rate-limited.
        ///
        /// # Args:
        /// * 'origin': (<T as frame_system::Config>::Origin):
        /// 	- The signature of the caller's coldkey.
        ///
        /// * 'hotkey' (T::AccountId):
        /// 	- The hotkey we are delegating (must be owned by the coldkey.)
        ///
        /// * 'take' (u16):
        /// 	- The new stake proportion that this hotkey takes from delegations.
        ///        The new value can be between 0 and 11_796 and should be strictly
        ///        greater than the previous value. T is the new value (rational number),
        ///        the the parameter is calculated as [65535 * T]. For example, 1% would be
        ///        [0.01 * 65535] = [655.35] = 655
        ///
        /// # Event:
        /// * TakeIncreased;
        /// 	- On successfully setting a increased take for this hotkey.
        ///
        /// # Raises:
        /// * 'NotRegistered':
        /// 	- The hotkey we are delegating is not registered on the network.
        ///
        /// * 'NonAssociatedColdKey':
        /// 	- The hotkey we are delegating is not owned by the calling coldkey.
        ///
        /// * 'DelegateTakeTooHigh':
        /// 	- The delegate is setting a take which is not greater than the previous.
        ///
        #[pallet::call_index(66)]
        #[pallet::weight((0, DispatchClass::Normal, Pays::No))]
        pub fn increase_take(
            origin: OriginFor<T>,
            hotkey: T::AccountId,
            take: u16,
        ) -> DispatchResult {
            Self::do_increase_take(origin, hotkey, take)
        }

        /// --- Adds stake to a hotkey. The call is made from the
        /// coldkey account linked in the hotkey.
        /// Only the associated coldkey is allowed to make staking and
        /// unstaking requests. This protects the neuron against
        /// attacks on its hotkey running in production code.
        ///
        /// # Args:
        ///  * 'origin': (<T as frame_system::Config>Origin):
        /// 	- The signature of the caller's coldkey.
        ///
        ///  * 'hotkey' (T::AccountId):
        /// 	- The associated hotkey account.
        ///
        ///  * 'amount_staked' (u64):
        /// 	- The amount of stake to be added to the hotkey staking account.
        ///
        /// # Event:
        ///  * StakeAdded;
        /// 	- On the successfully adding stake to a global account.
        ///
        /// # Raises:
        ///  * 'NotEnoughBalanceToStake':
        /// 	- Not enough balance on the coldkey to add onto the global account.
        ///
        ///  * 'NonAssociatedColdKey':
        /// 	- The calling coldkey is not associated with this hotkey.
        ///
        ///  * 'BalanceWithdrawalError':
        ///  	- Errors stemming from transaction pallet.
        ///
        #[pallet::call_index(2)]
        #[pallet::weight((Weight::from_parts(124_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(10))
		.saturating_add(T::DbWeight::get().writes(7)), DispatchClass::Normal, Pays::No))]
        pub fn add_stake(
            origin: OriginFor<T>,
            hotkey: T::AccountId,
            netuid: u16,
            amount_staked: u64,
        ) -> DispatchResult {
            Self::do_add_stake(origin, hotkey, netuid, amount_staked)
        }

        /// Remove stake from the staking account. The call must be made
        /// from the coldkey account attached to the neuron metadata. Only this key
        /// has permission to make staking and unstaking requests.
        ///
        /// # Args:
        /// * 'origin': (<T as frame_system::Config>Origin):
        /// 	- The signature of the caller's coldkey.
        ///
        /// * 'hotkey' (T::AccountId):
        /// 	- The associated hotkey account.
        ///
        /// * 'amount_unstaked' (u64):
        /// 	- The amount of stake to be added to the hotkey staking account.
        ///
        /// # Event:
        /// * StakeRemoved;
        /// 	- On the successfully removing stake from the hotkey account.
        ///
        /// # Raises:
        /// * 'NotRegistered':
        /// 	- Thrown if the account we are attempting to unstake from is non existent.
        ///
        /// * 'NonAssociatedColdKey':
        /// 	- Thrown if the coldkey does not own the hotkey we are unstaking from.
        ///
        /// * 'NotEnoughStakeToWithdraw':
        /// 	- Thrown if there is not enough stake on the hotkey to withdwraw this amount.
        ///
        #[pallet::call_index(3)]
        #[pallet::weight((Weight::from_parts(111_000_000, 0)
		.saturating_add(Weight::from_parts(0, 43991))
		.saturating_add(T::DbWeight::get().reads(10))
		.saturating_add(T::DbWeight::get().writes(7)), DispatchClass::Normal, Pays::No))]
        pub fn remove_stake(
            origin: OriginFor<T>,
            hotkey: T::AccountId,
            netuid: u16,
            amount_unstaked: u64,
        ) -> DispatchResult {
            Self::do_remove_stake(origin, hotkey, netuid, amount_unstaked)
        }

        /// Serves or updates axon /promethteus information for the neuron associated with the caller. If the caller is
        /// already registered the metadata is updated. If the caller is not registered this call throws NotRegistered.
        ///
        /// # Args:
        /// * 'origin': (<T as frame_system::Config>Origin):
        /// 	- The signature of the caller.
        ///
        /// * 'netuid' (u16):
        /// 	- The u16 network identifier.
        ///
        /// * 'version' (u64):
        /// 	- The bittensor version identifier.
        ///
        /// * 'ip' (u64):
        /// 	- The endpoint ip information as a u128 encoded integer.
        ///
        /// * 'port' (u16):
        /// 	- The endpoint port information as a u16 encoded integer.
        ///
        /// * 'ip_type' (u8):
        /// 	- The endpoint ip version as a u8, 4 or 6.
        ///
        /// * 'protocol' (u8):
        /// 	- UDP:1 or TCP:0
        ///
        /// * 'placeholder1' (u8):
        /// 	- Placeholder for further extra params.
        ///
        /// * 'placeholder2' (u8):
        /// 	- Placeholder for further extra params.
        ///
        /// # Event:
        /// * AxonServed;
        /// 	- On successfully serving the axon info.
        ///
        /// # Raises:
        /// * 'SubNetworkDoesNotExist':
        /// 	- Attempting to set weights on a non-existent network.
        ///
        /// * 'NotRegistered':
        /// 	- Attempting to set weights from a non registered account.
        ///
        /// * 'InvalidIpType':
        /// 	- The ip type is not 4 or 6.
        ///
        /// * 'InvalidIpAddress':
        /// 	- The numerically encoded ip address does not resolve to a proper ip.
        ///
        /// * 'ServingRateLimitExceeded':
        /// 	- Attempting to set prometheus information withing the rate limit min.
        ///
        #[pallet::call_index(4)]
        #[pallet::weight((Weight::from_parts(46_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(4))
		.saturating_add(T::DbWeight::get().writes(1)), DispatchClass::Normal, Pays::No))]
        pub fn serve_axon(
            origin: OriginFor<T>,
            netuid: u16,
            version: u32,
            ip: u128,
            port: u16,
            ip_type: u8,
            protocol: u8,
            placeholder1: u8,
            placeholder2: u8,
        ) -> DispatchResult {
            Self::do_serve_axon(
                origin,
                netuid,
                version,
                ip,
                port,
                ip_type,
                protocol,
                placeholder1,
                placeholder2,
            )
        }

        /// ---- Set prometheus information for the neuron.
        /// # Args:
        /// * 'origin': (<T as frame_system::Config>Origin):
        /// 	- The signature of the calling hotkey.
        ///
        /// * 'netuid' (u16):
        /// 	- The u16 network identifier.
        ///
        /// * 'version' (u16):
        /// 	-  The bittensor version identifier.
        ///
        /// * 'ip' (u128):
        /// 	- The prometheus ip information as a u128 encoded integer.
        ///
        /// * 'port' (u16):
        /// 	- The prometheus port information as a u16 encoded integer.
        ///
        /// * 'ip_type' (u8):
        /// 	- The ip type v4 or v6.
        ///
        #[pallet::call_index(5)]
        #[pallet::weight((Weight::from_parts(45_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(4))
		.saturating_add(T::DbWeight::get().writes(1)), DispatchClass::Normal, Pays::No))]
        pub fn serve_prometheus(
            origin: OriginFor<T>,
            netuid: u16,
            version: u32,
            ip: u128,
            port: u16,
            ip_type: u8,
        ) -> DispatchResult {
            Self::do_serve_prometheus(origin, netuid, version, ip, port, ip_type)
        }

        /// ---- Registers a new neuron to the subnetwork.
        ///
        /// # Args:
        /// * 'origin': (<T as frame_system::Config>Origin):
        /// 	- The signature of the calling hotkey.
        ///
        /// * 'netuid' (u16):
        /// 	- The u16 network identifier.
        ///
        /// * 'block_number' ( u64 ):
        /// 	- Block hash used to prove work done.
        ///
        /// * 'nonce' ( u64 ):
        /// 	- Positive integer nonce used in POW.
        ///
        /// * 'work' ( Vec<u8> ):
        /// 	- Vector encoded bytes representing work done.
        ///
        /// * 'hotkey' ( T::AccountId ):
        /// 	- Hotkey to be registered to the network.
        ///
        /// * 'coldkey' ( T::AccountId ):
        /// 	- Associated coldkey account.
        ///
        /// # Event:
        /// * NeuronRegistered;
        /// 	- On successfully registering a uid to a neuron slot on a subnetwork.
        ///
        /// # Raises:
        /// * 'SubNetworkDoesNotExist':
        /// 	- Attempting to register to a non existent network.
        ///
        /// * 'TooManyRegistrationsThisBlock':
        /// 	- This registration exceeds the total allowed on this network this block.
        ///
        /// * 'HotKeyAlreadyRegisteredInSubNet':
        /// 	- The hotkey is already registered on this network.
        ///
        /// * 'InvalidWorkBlock':
        /// 	- The work has been performed on a stale, future, or non existent block.
        ///
        /// * 'InvalidDifficulty':
        /// 	- The work does not match the difficulty.
        ///
        /// * 'InvalidSeal':
        /// 	- The seal is incorrect.
        ///
        #[pallet::call_index(6)]
        #[pallet::weight((Weight::from_parts(192_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(24))
		.saturating_add(T::DbWeight::get().writes(22)), DispatchClass::Normal, Pays::No))]
        pub fn register(
            origin: OriginFor<T>,
            netuid: u16,
            block_number: u64,
            nonce: u64,
            work: Vec<u8>,
            hotkey: T::AccountId,
            coldkey: T::AccountId,
        ) -> DispatchResult {
            Self::do_registration(origin, netuid, block_number, nonce, work, hotkey, coldkey)
        }

        /// Register the hotkey to root network
        #[pallet::call_index(62)]
        #[pallet::weight((Weight::from_parts(164_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(23))
		.saturating_add(T::DbWeight::get().writes(20)), DispatchClass::Normal, Pays::No))]
        pub fn root_register(origin: OriginFor<T>, hotkey: T::AccountId) -> DispatchResult {
            Self::do_root_register(origin, hotkey)
        }

        /// Attempt to adjust the senate membership to include a hotkey
        #[pallet::call_index(63)]
        #[pallet::weight((Weight::from_parts(0, 0)
		.saturating_add(T::DbWeight::get().reads(0))
		.saturating_add(T::DbWeight::get().writes(0)), DispatchClass::Normal, Pays::Yes))]
        pub fn adjust_senate(origin: OriginFor<T>, hotkey: T::AccountId) -> DispatchResult {
            Self::do_adjust_senate(origin, hotkey)
        }

        /// User register a new subnetwork via burning token
        #[pallet::call_index(7)]
        #[pallet::weight((Weight::from_parts(177_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(26))
		.saturating_add(T::DbWeight::get().writes(24)), DispatchClass::Normal, Pays::No))]
        pub fn burned_register(
            origin: OriginFor<T>,
            netuid: u16,
            hotkey: T::AccountId,
        ) -> DispatchResult {
            Self::do_burned_registration(origin, netuid, hotkey)
        }

        /// The extrinsic for user to change its hotkey
        #[pallet::call_index(70)]
        #[pallet::weight((Weight::from_parts(1_940_000_000, 0)
        .saturating_add(T::DbWeight::get().reads(272))
        .saturating_add(T::DbWeight::get().writes(527)), DispatchClass::Operational, Pays::No))]
        pub fn swap_hotkey(
            origin: OriginFor<T>,
            hotkey: T::AccountId,
            new_hotkey: T::AccountId,
        ) -> DispatchResultWithPostInfo {
            Self::do_swap_hotkey(origin, &hotkey, &new_hotkey)
        }

        /// The extrinsic for user to change the coldkey associated with their account.
        ///
        /// # Arguments
        ///
        /// * `origin` - The origin of the call, must be signed by the old coldkey.
        /// * `old_coldkey` - The current coldkey associated with the account.
        /// * `new_coldkey` - The new coldkey to be associated with the account.
        ///
        /// # Returns
        ///
        /// Returns a `DispatchResultWithPostInfo` indicating success or failure of the operation.
        ///
        /// # Weight
        ///
        /// Weight is calculated based on the number of database reads and writes.
        #[pallet::call_index(71)]
        #[pallet::weight((Weight::from_parts(127_713_000, 0)
        .saturating_add(Weight::from_parts(0, 11645))
        .saturating_add(T::DbWeight::get().reads(18))
        .saturating_add(T::DbWeight::get().writes(12)), DispatchClass::Operational, Pays::No))]
        pub fn swap_coldkey(
            origin: OriginFor<T>,
            new_coldkey: T::AccountId,
        ) -> DispatchResultWithPostInfo {
            Self::do_swap_coldkey(origin, &new_coldkey)
        }

        /// Unstakes all tokens associated with a hotkey and transfers them to a new coldkey.
        ///
        /// # Arguments
        ///
        /// * `origin` - The origin of the call, must be signed by the current coldkey.
        /// * `hotkey` - The hotkey associated with the stakes to be unstaked.
        /// * `new_coldkey` - The new coldkey to receive the unstaked tokens.
        ///
        /// # Returns
        ///
        /// Returns a `DispatchResult` indicating success or failure of the operation.
        ///
        /// # Weight
        ///
        /// Weight is calculated based on the number of database reads and writes.
        #[cfg(test)]
        #[pallet::call_index(72)]
        #[pallet::weight((Weight::from_parts(21_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(3))
		.saturating_add(T::DbWeight::get().writes(3)), DispatchClass::Operational, Pays::No))]
        pub fn schedule_coldkey_swap(
            _origin: OriginFor<T>,
            _new_coldkey: T::AccountId,
            _work: Vec<u8>,
            _block_number: u64,
            _nonce: u64,
        ) -> DispatchResult {
            Ok(())
        }

        // ---- SUDO ONLY FUNCTIONS ------------------------------------------------------------

        // ==================================
        // ==== Parameter Sudo calls ========
        // ==================================
        // Each function sets the corresponding hyper paramter on the specified network
        // Args:
        // 	* 'origin': (<T as frame_system::Config>Origin):
        // 		- The caller, must be sudo.
        //
        // 	* `netuid` (u16):
        // 		- The network identifier.
        //
        // 	* `hyperparameter value` (u16):
        // 		- The value of the hyper parameter.
        //

        /// Authenticates a council proposal and dispatches a function call with `Root` origin.
        ///
        /// The dispatch origin for this call must be a council majority.
        ///
        /// ## Complexity
        /// - O(1).
        #[pallet::call_index(51)]
        #[pallet::weight((Weight::from_parts(0, 0), DispatchClass::Operational, Pays::No))]
        pub fn sudo(
            origin: OriginFor<T>,
            call: Box<T::SudoRuntimeCall>,
        ) -> DispatchResultWithPostInfo {
            // This is a public call, so we ensure that the origin is a council majority.
            T::CouncilOrigin::ensure_origin(origin)?;

            let result = call.dispatch_bypass_filter(frame_system::RawOrigin::Root.into());
            let error = result.map(|_| ()).map_err(|e| e.error);
            Self::deposit_event(Event::Sudid(error));

            return result;
        }

        /// Authenticates a council proposal and dispatches a function call with `Root` origin.
        /// This function does not check the weight of the call, and instead allows the
        /// user to specify the weight of the call.
        ///
        /// The dispatch origin for this call must be a council majority.
        ///
        /// ## Complexity
        /// - O(1).
        #[allow(deprecated)]
        #[pallet::call_index(52)]
        #[pallet::weight((*weight, call.get_dispatch_info().class, Pays::No))]
        pub fn sudo_unchecked_weight(
            origin: OriginFor<T>,
            call: Box<T::SudoRuntimeCall>,
            weight: Weight,
        ) -> DispatchResultWithPostInfo {
            // We dont need to check the weight witness, suppress warning.
            // See https://github.com/paritytech/polkadot-sdk/pull/1818.
            let _ = weight;

            // This is a public call, so we ensure that the origin is a council majority.
            T::CouncilOrigin::ensure_origin(origin)?;

            let result = call.dispatch_bypass_filter(frame_system::RawOrigin::Root.into());
            let error = result.map(|_| ()).map_err(|e| e.error);
            Self::deposit_event(Event::Sudid(error));

            return result;
        }

        /// User vote on a proposal
        #[pallet::call_index(55)]
        #[pallet::weight((Weight::from_parts(0, 0)
		.saturating_add(Weight::from_parts(0, 0))
		.saturating_add(T::DbWeight::get().reads(0))
		.saturating_add(T::DbWeight::get().writes(0)), DispatchClass::Operational))]
        pub fn vote(
            origin: OriginFor<T>,
            hotkey: T::AccountId,
            proposal: T::Hash,
            #[pallet::compact] index: u32,
            approve: bool,
        ) -> DispatchResultWithPostInfo {
            Self::do_vote_root(origin, &hotkey, proposal, index, approve)
        }

        /// User register a new subnetwork
        /// DEPRECATED
        #[pallet::call_index(59)]
        #[pallet::weight((Weight::from_parts(157_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(16))
		.saturating_add(T::DbWeight::get().writes(30)), DispatchClass::Operational, Pays::No))]
        pub fn register_network(
            origin: OriginFor<T>,
            hotkey: T::AccountId,
            mechid: u16,
        ) -> DispatchResult {
            Self::do_register_network(origin, &hotkey, mechid)
        }

        /// Facility extrinsic for user to get taken from faucet
        /// It is only available when pow-faucet feature enabled
        /// Just deployed in testnet and devnet for testing purpose
        #[pallet::call_index(60)]
        #[pallet::weight((Weight::from_parts(91_000_000, 0)
        .saturating_add(T::DbWeight::get().reads(27))
		.saturating_add(T::DbWeight::get().writes(22)), DispatchClass::Normal, Pays::No))]
        pub fn faucet(
            origin: OriginFor<T>,
            block_number: u64,
            nonce: u64,
            work: Vec<u8>,
        ) -> DispatchResult {
            if cfg!(feature = "pow-faucet") {
                return Self::do_faucet(origin, block_number, nonce, work);
            }

            Err(Error::<T>::FaucetDisabled.into())
        }

        /// Remove a user's subnetwork
        /// The caller must be the owner of the network
        #[pallet::call_index(61)]
        #[pallet::weight((Weight::from_parts(119_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(6))
		.saturating_add(T::DbWeight::get().writes(31)), DispatchClass::Operational, Pays::No))]
        pub fn dissolve_network(_origin: OriginFor<T>, _netuid: u16) -> DispatchResult {
            Ok(())
            // Self::user_remove_network(origin, netuid)
        }

        /// Sets values for liquid alpha
        #[pallet::call_index(64)]
        #[pallet::weight((0, DispatchClass::Operational, Pays::No))]
        pub fn sudo_hotfix_swap_coldkey_delegates(
            _origin: OriginFor<T>,
            _old_coldkey: T::AccountId,
            _new_coldkey: T::AccountId,
        ) -> DispatchResult {
            Ok(())
        }

        /// Set a single child for a given hotkey on a specified network.
        ///
        /// This function allows a coldkey to set a single child for a given hotkey on a specified network.
        /// The proportion of the hotkey's stake to be allocated to the child is also specified.
        ///
        /// # Arguments:
        /// * `origin` (<T as frame_system::Config>::RuntimeOrigin):
        ///     - The signature of the calling coldkey. Setting a hotkey child can only be done by the coldkey.
        ///
        /// * `hotkey` (T::AccountId):
        ///     - The hotkey which will be assigned the child.
        ///
        /// * `child` (T::AccountId):
        ///     - The child which will be assigned to the hotkey.
        ///
        /// * `netuid` (u16):
        ///     - The u16 network identifier where the childkey will exist.
        ///
        /// * `proportion` (u64):
        ///     - Proportion of the hotkey's stake to be given to the child, the value must be u64 normalized.
        ///
        /// # Events:
        /// * `ChildAddedSingular`:
        ///     - On successfully registering a child to a hotkey.
        ///
        /// # Errors:
        /// * `SubNetworkDoesNotExist`:
        ///     - Attempting to register to a non-existent network.
        /// * `RegistrationNotPermittedOnRootSubnet`:
        ///     - Attempting to register a child on the root network.
        /// * `NonAssociatedColdKey`:
        ///     - The coldkey does not own the hotkey or the child is the same as the hotkey.
        /// * `HotKeyAccountNotExists`:
        ///     - The hotkey account does not exist.
        ///
        /// # Detailed Explanation of Checks:
        /// 1. **Signature Verification**: Ensures that the caller has signed the transaction, verifying the coldkey.
        /// 2. **Root Network Check**: Ensures that the delegation is not on the root network, as child hotkeys are not valid on the root.
        /// 3. **Network Existence Check**: Ensures that the specified network exists.
        /// 4. **Ownership Verification**: Ensures that the coldkey owns the hotkey.
        /// 5. **Hotkey Account Existence Check**: Ensures that the hotkey account already exists.
        /// 6. **Child-Hotkey Distinction**: Ensures that the child is not the same as the hotkey.
        /// 7. **Old Children Cleanup**: Removes the hotkey from the parent list of its old children.
        /// 8. **New Children Assignment**: Assigns the new child to the hotkey and updates the parent list for the new child.
        // TODO: Benchmark this call
        #[pallet::call_index(67)]
        #[pallet::weight((Weight::from_parts(119_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(6))
		.saturating_add(T::DbWeight::get().writes(31)), DispatchClass::Operational, Pays::Yes))]
        pub fn set_children(
            origin: T::RuntimeOrigin,
            hotkey: T::AccountId,
            netuid: u16,
            children: Vec<(u64, T::AccountId)>,
        ) -> DispatchResultWithPostInfo {
            Self::do_set_children(origin, hotkey, netuid, children)?;
            Ok(().into())
        }

        /// ---- Set prometheus information for the neuron.
        /// # Args:
        /// * 'origin': (<T as frame_system::Config>Origin):
        /// 	- The signature of the calling hotkey.
        ///
        /// * 'netuid' (u16):
        /// 	- The u16 network identifier.
        ///
        /// * 'version' (u16):
        /// 	-  The bittensor version identifier.
        ///
        /// * 'ip' (u128):
        /// 	- The prometheus ip information as a u128 encoded integer.
        ///
        /// * 'port' (u16):
        /// 	- The prometheus port information as a u16 encoded integer.
        ///
        /// * 'ip_type' (u8):
        /// 	- The ip type v4 or v6.
        ///
        #[pallet::call_index(68)]
        #[pallet::weight((Weight::from_parts(45_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(4))
		.saturating_add(T::DbWeight::get().writes(1)), DispatchClass::Normal, Pays::Yes))]
        pub fn set_identity(
            origin: OriginFor<T>,
            name: Vec<u8>,
            url: Vec<u8>,
            image: Vec<u8>,
            discord: Vec<u8>,
            description: Vec<u8>,
            additional: Vec<u8>,
        ) -> DispatchResult {
            Self::do_set_identity(origin, name, url, image, discord, description, additional)
        }

        /// Locks a specified amount of alpha for a given hotkey on a specific subnet.
        ///
        /// This function allows a user to lock a certain amount of alpha (stake) for a hotkey
        /// on a particular subnet. Locking alpha can be used to increase the influence or
        /// participation of the hotkey in the subnet's operations.
        ///
        /// # Arguments
        ///
        /// * `origin` - The origin of the call, typically representing the account initiating the lock.
        /// * `hotkey` - The account ID of the hotkey for which alpha is being locked.
        /// * `netuid` - The unique identifier of the subnet where the alpha is being locked.
        /// * `alpha_locked` - The amount of alpha to be locked, represented as a u64.
        ///
        /// # Returns
        ///
        /// Returns a `DispatchResult` indicating success or failure of the operation.
        ///
        /// # Weight
        ///
        /// - Base Weight: 124,000,000 + 10 DB Reads + 7 DB Writes
        /// - Dispatch Class: Normal
        /// - Pays Fee: No
        #[pallet::call_index(69)]
        #[pallet::weight((Weight::from_parts(124_000_000, 0)
		.saturating_add(T::DbWeight::get().reads(10))
		.saturating_add(T::DbWeight::get().writes(7)), DispatchClass::Normal, Pays::No))]
        pub fn lock_stake(
            origin: OriginFor<T>,
            hotkey: T::AccountId,
            netuid: u16,
            duration: u64,
            alpha_locked: u64,
        ) -> DispatchResult {
            Self::do_lock(origin, hotkey, netuid, duration, alpha_locked)
        }

        /// Sets the lock interval in blocks.
        ///
        /// This function allows setting the lock interval, which determines the minimum duration
        /// for which stakes can be locked. Only callable by the root origin.
        ///
        /// # Arguments
        ///
        /// * `origin` - The origin of the call, must be root.
        /// * `new_interval` - The new lock interval in blocks.
        ///
        /// # Returns
        ///
        /// Returns a `DispatchResult` indicating success or failure of the operation.
        ///
        /// # Errors
        ///
        /// * `BadOrigin` - If the caller is not the root origin.
        ///
        /// # Weight
        ///
        /// - Base Weight: 3,000,000 + 1 DB Write
        /// - Dispatch Class: Operational
        /// - Pays Fee: Yes
        #[pallet::call_index(74)]
        #[pallet::weight((Weight::from_parts(3_000_000, 0)
    .saturating_add(T::DbWeight::get().writes(1)), DispatchClass::Operational, Pays::No))]
        pub fn sudo_set_lock_interval_blocks(
            origin: OriginFor<T>,
            new_interval: u64,
        ) -> DispatchResult {
            // Ensure the caller is root
            ensure_root(origin)?;

            // Call the internal function to set the lock interval
            Self::set_lock_interval_blocks(new_interval);

            Ok(())
        }

        /// Moves stake from one hotkey to another across subnets.
        ///
        /// # Arguments
        /// * `origin` - The origin of the transaction, which must be signed by the `origin_hotkey`.
        /// * `origin_hotkey` - The account ID of the hotkey from which the stake is being moved.
        /// * `destination_hotkey` - The account ID of the hotkey to which the stake is being moved.
        /// * `origin_netuid` - The network ID of the origin subnet.
        /// * `destination_netuid` - The network ID of the destination subnet.
        ///
        /// # Returns
        /// * `DispatchResult` - Indicates the success or failure of the operation.
        ///
        /// # Errors
        /// This function will return an error if:
        /// * The origin is not signed by the `origin_hotkey`.
        /// * Either the origin or destination subnet does not exist.
        /// * The `origin_hotkey` or `destination_hotkey` does not exist.
        /// * There are locked funds that cannot be moved across subnets.
        ///
        /// # Events
        /// Emits a `StakeMoved` event upon successful completion of the stake movement.
        #[pallet::call_index(75)]
        #[pallet::weight((Weight::from_parts(3_000_000, 0).saturating_add(T::DbWeight::get().writes(1)), DispatchClass::Operational, Pays::No))]
        pub fn move_stake(
            origin: OriginFor<T>,
            origin_hotkey: T::AccountId,
            destination_hotkey: T::AccountId,
            origin_netuid: u16,
            destination_netuid: u16,
            amount_moved: Option<u64>,
        ) -> DispatchResult {
            Self::do_move_stake(
                origin,
                origin_hotkey,
                destination_hotkey,
                origin_netuid,
                destination_netuid,
                amount_moved,
            )
        }
    }
}
