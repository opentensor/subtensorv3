// The MIT License (MIT)
// Copyright © 2023 Yuma Rao

// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
// documentation files (the “Software”), to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all copies or substantial portions of
// the Software.

// THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

use super::*;
use frame_support::dispatch::Pays;
use frame_support::storage::IterableStorageDoubleMap;
use frame_support::weights::Weight;
use sp_core::Get;
use sp_std::vec;
use substrate_fixed::types::I64F64;

impl<T: Config> Pallet<T> {
    /// Fetches the total count of root network validators
    ///
    /// This function retrieves the total number of root network validators.
    ///
    /// # Returns:
    /// * 'u16': The total number of root network validators
    ///
    pub fn get_num_root_validators() -> u16 {
        Self::get_subnetwork_n(Self::get_root_netuid())
    }

    /// Fetches the max validators count of root network.
    ///
    /// This function retrieves the max validators count of root network.
    ///
    /// # Returns:
    /// * 'u16': The max validators count of root network.
    ///
    pub fn get_max_root_validators() -> u16 {
        Self::get_max_allowed_uids(Self::get_root_netuid())
    }

    /// Checks for any UIDs in the given list that are either equal to the root netuid or exceed the total number of subnets.
    ///
    /// It's important to check for invalid UIDs to ensure data integrity and avoid referencing nonexistent subnets.
    ///
    /// # Arguments:
    /// * 'uids': A reference to a vector of UIDs to check.
    ///
    /// # Returns:
    /// * 'bool': 'true' if any of the UIDs are invalid, 'false' otherwise.
    ///
    pub fn contains_invalid_root_uids(netuids: &[u16]) -> bool {
        for netuid in netuids {
            if !Self::if_subnet_exist(*netuid) {
                log::debug!(
                    "contains_invalid_root_uids: netuid {:?} does not exist",
                    netuid
                );
                return true;
            }
        }
        false
    }

    /// Sets the emission values for each netuid
    ///
    pub fn set_emission_values(netuids: &[u16], emission: Vec<u64>) -> Result<(), &'static str> {
        log::debug!(
            "set_emission_values: netuids: {:?} emission:{:?}",
            netuids,
            emission
        );

        // Be careful this function can fail.
        if Self::contains_invalid_root_uids(netuids) {
            log::error!("set_emission_values: contains_invalid_root_uids");
            return Err("Invalid netuids");
        }
        if netuids.len() != emission.len() {
            log::error!("set_emission_values: netuids.len() != emission.len()");
            return Err("netuids and emission must have the same length");
        }
        for (netuid_i, emission_i) in netuids.iter().zip(emission) {
            log::debug!("set netuid:{:?} emission:{:?}", netuid_i, emission_i);
            EmissionValues::<T>::insert(*netuid_i, emission_i);
        }
        Ok(())
    }

    /// Retrieves weight matrix associated with the root network.
    ///  Weights represent the preferences for each subnetwork.
    ///
    /// # Returns:
    /// A 2D vector ('Vec<Vec<I32F32>>') where each entry [i][j] represents the weight of subnetwork
    /// 'j' with according to the preferences of key. Validator 'i' within the root network.
    ///
    pub fn get_root_weights() -> Vec<Vec<I64F64>> {
        // --- 0. The number of validators on the root network.
        let n: usize = Self::get_num_root_validators() as usize;

        // --- 1 The number of subnets to validate.
        log::debug!("subnet size before cast: {:?}", Self::get_num_subnets());
        let k: usize = Self::get_num_subnets() as usize;
        log::debug!("n: {:?} k: {:?}", n, k);

        // --- 2. Initialize a 2D vector with zeros to store the weights. The dimensions are determined
        // by `n` (number of validators) and `k` (total number of subnets).
        let mut weights: Vec<Vec<I64F64>> = vec![vec![I64F64::from_num(0.0); k]; n];
        log::debug!("weights:\n{:?}\n", weights);

        let subnet_list = Self::get_all_subnet_netuids();

        // --- 3. Iterate over stored weights and fill the matrix.
        for (uid_i, weights_i) in
            <Weights<T> as IterableStorageDoubleMap<u16, u16, Vec<(u16, u16)>>>::iter_prefix(
                Self::get_root_netuid(),
            )
        {
            // --- 4. Iterate over each weight entry in `weights_i` to update the corresponding value in the
            // initialized `weights` 2D vector. Here, `uid_j` represents a subnet, and `weight_ij` is the
            // weight of `uid_i` with respect to `uid_j`.
            for (netuid, weight_ij) in &weights_i {
                let idx = uid_i as usize;
                if let Some(weight) = weights.get_mut(idx) {
                    if let Some((w, _)) = weight
                        .iter_mut()
                        .zip(&subnet_list)
                        .find(|(_, subnet)| *subnet == netuid)
                    {
                        *w = I64F64::from_num(*weight_ij);
                    }
                }
            }
        }

        // --- 5. Return the filled weights matrix.
        weights
    }

    /// Registers a user's hotkey to the root network.
    ///
    /// This function is responsible for registering the hotkey of a user.
    /// The root key with the least stake if pruned in the event of a filled network.
    ///
    /// # Arguments:
    /// * 'origin': Represents the origin of the call.
    /// * 'hotkey': The hotkey that the user wants to register to the root network.
    ///
    /// # Returns:
    /// * 'DispatchResult': A result type indicating success or failure of the registration.
    ///
    pub fn do_root_register(origin: T::RuntimeOrigin, hotkey: T::AccountId) -> DispatchResult {
        // --- 0. Get the unique identifier (UID) for the root network.
        let root_netuid: u16 = Self::get_root_netuid();
        let current_block_number: u64 = Self::get_current_block_as_u64();
        ensure!(
            Self::if_subnet_exist(root_netuid),
            Error::<T>::RootNetworkDoesNotExist
        );

        // --- 1. Ensure that the call originates from a signed source and retrieve the caller's account ID (coldkey).
        let coldkey = ensure_signed(origin)?;
        log::debug!(
            "do_root_register( coldkey: {:?}, hotkey: {:?} )",
            coldkey,
            hotkey
        );

        // --- 2. Ensure that the number of registrations in this block doesn't exceed the allowed limit.
        ensure!(
            Self::get_registrations_this_block(root_netuid)
                < Self::get_max_registrations_per_block(root_netuid),
            Error::<T>::TooManyRegistrationsThisBlock
        );

        // --- 3. Ensure that the number of registrations in this interval doesn't exceed thrice the target limit.
        ensure!(
            Self::get_registrations_this_interval(root_netuid)
                < Self::get_target_registrations_per_interval(root_netuid).saturating_mul(3),
            Error::<T>::TooManyRegistrationsThisInterval
        );

        // --- 4. Check if the hotkey is already registered. If so, error out.
        ensure!(
            !Uids::<T>::contains_key(root_netuid, &hotkey),
            Error::<T>::HotKeyAlreadyRegisteredInSubNet
        );

        // --- 6. Create a network account for the user if it doesn't exist.
        Self::create_account_if_non_existent(&coldkey, &hotkey);

        // --- 7. Fetch the current size of the subnetwork.
        let current_num_root_validators: u16 = Self::get_num_root_validators();

        // Declare a variable to hold the root UID.
        let subnetwork_uid: u16;

        // --- 8. Check if the root net is below its allowed size.
        // max allowed is senate size.
        if current_num_root_validators < Self::get_max_root_validators() {
            // --- 12.1.1 We can append to the subnetwork as it's not full.
            subnetwork_uid = current_num_root_validators;

            // --- 12.1.2 Add the new account and make them a member of the Senate.
            Self::append_neuron(root_netuid, &hotkey, current_block_number);
            log::debug!("add new neuron: {:?} on uid {:?}", hotkey, subnetwork_uid);
        } else {
            // --- 13.1.1 The network is full. Perform replacement.
            // Find the neuron with the lowest stake value to replace.
            let mut lowest_stake: u64 = u64::MAX;
            let mut lowest_uid: u16 = 0;

            // Iterate over all keys in the root network to find the neuron with the lowest stake.
            for (uid_i, hotkey_i) in
                <Keys<T> as IterableStorageDoubleMap<u16, u16, T::AccountId>>::iter_prefix(
                    root_netuid,
                )
            {
                let stake_i: u64 = Self::get_stake_for_hotkey_on_subnet(&hotkey_i, 0);
                if stake_i < lowest_stake {
                    lowest_stake = stake_i;
                    lowest_uid = uid_i;
                }
            }
            subnetwork_uid = lowest_uid;
            let replaced_hotkey: T::AccountId =
                Self::get_hotkey_for_net_and_uid(root_netuid, subnetwork_uid)?;

            // --- 13.1.2 The new account has a higher stake than the one being replaced.
            ensure!(
                lowest_stake < Self::get_stake_for_hotkey_on_subnet(&hotkey, 0),
                Error::<T>::StakeTooLowForRoot
            );

            // --- 13.1.3 The new account has a higher stake than the one being replaced.
            // Replace the neuron account with new information.
            Self::replace_neuron(root_netuid, lowest_uid, &hotkey, current_block_number);

            log::debug!(
                "replace neuron: {:?} with {:?} on uid {:?}",
                replaced_hotkey,
                hotkey,
                subnetwork_uid
            );
        }

        // --- 13. Join the Senate if eligible.
        // Returns the replaced member, if any.
        let _ = Self::join_senate_if_eligible(&hotkey)?;

        // --- 14. Force all members on root to become a delegate.
        if !Self::hotkey_is_delegate(&hotkey) {
            Self::delegate_hotkey(&hotkey, 11_796); // 18% cut defaulted.
        }

        // --- 15. Update the registration counters for both the block and interval.
        #[allow(clippy::arithmetic_side_effects)]
        // note this RA + clippy false positive is a known substrate issue
        RegistrationsThisInterval::<T>::mutate(root_netuid, |val| *val += 1);
        #[allow(clippy::arithmetic_side_effects)]
        // note this RA + clippy false positive is a known substrate issue
        RegistrationsThisBlock::<T>::mutate(root_netuid, |val| *val += 1);

        // --- 16. Log and announce the successful registration.
        log::debug!(
            "RootRegistered(netuid:{:?} uid:{:?} hotkey:{:?})",
            root_netuid,
            subnetwork_uid,
            hotkey
        );
        Self::deposit_event(Event::NeuronRegistered(root_netuid, subnetwork_uid, hotkey));

        // --- 17. Finish and return success.
        Ok(())
    }

    // Checks if a hotkey should be a member of the Senate, and if so, adds them.
    //
    // This function is responsible for adding a hotkey to the Senate if they meet the requirements.
    // The root key with the least stake is pruned in the event of a filled membership.
    //
    // # Arguments:
    // * 'origin': Represents the origin of the call.
    // * 'hotkey': The hotkey that the user wants to register to the root network.
    //
    // # Returns:
    // * 'DispatchResult': A result type indicating success or failure of the registration.
    //
    pub fn do_adjust_senate(origin: T::RuntimeOrigin, hotkey: T::AccountId) -> DispatchResult {
        // --- 0. Get the unique identifier (UID) for the root network.
        let root_netuid: u16 = Self::get_root_netuid();
        ensure!(
            Self::if_subnet_exist(root_netuid),
            Error::<T>::RootNetworkDoesNotExist
        );

        // --- 1. Ensure that the call originates from a signed source and retrieve the caller's account ID (coldkey).
        let coldkey = ensure_signed(origin)?;
        log::debug!(
            "do_root_register( coldkey: {:?}, hotkey: {:?} )",
            coldkey,
            hotkey
        );

        // --- 2. Check if the hotkey is already registered to the root network. If not, error out.
        ensure!(
            Uids::<T>::contains_key(root_netuid, &hotkey),
            Error::<T>::HotKeyNotRegisteredInSubNet
        );

        // --- 3. Create a network account for the user if it doesn't exist.
        Self::create_account_if_non_existent(&coldkey, &hotkey);

        // --- 4. Join the Senate if eligible.
        // Returns the replaced member, if any.
        let replaced = Self::join_senate_if_eligible(&hotkey)?;

        if replaced.is_none() {
            // Not eligible to join the Senate, or no replacement needed.
            // Check if the hotkey is *now* a member of the Senate.
            // Otherwise, error out.
            ensure!(
                T::SenateMembers::is_member(&hotkey),
                Error::<T>::StakeTooLowForRoot, // Had less stake than the lowest stake incumbent.
            );
        }

        // --- 5. Log and announce the successful Senate adjustment.
        log::debug!(
            "SenateAdjusted(old_hotkey:{:?} hotkey:{:?})",
            replaced,
            hotkey
        );
        Self::deposit_event(Event::SenateAdjusted {
            old_member: replaced.cloned(),
            new_member: hotkey,
        });

        // --- 6. Finish and return success.
        Ok(())
    }

    // Checks if a hotkey should be a member of the Senate, and if so, adds them.
    //
    // # Arguments:
    // * 'hotkey': The hotkey that the user wants to register to the root network.
    //
    // # Returns:
    // * 'Result<Option<&T::AccountId>, Error<T>>': A result containing the replaced member, if any.
    //
    fn join_senate_if_eligible(hotkey: &T::AccountId) -> Result<Option<&T::AccountId>, Error<T>> {
        // Get the root network UID.
        let root_netuid: u16 = Self::get_root_netuid();

        // --- 1. Check the hotkey is registered in the root network.
        ensure!(
            Uids::<T>::contains_key(root_netuid, hotkey),
            Error::<T>::HotKeyNotRegisteredInSubNet
        );

        // --- 2. Verify the hotkey is NOT already a member of the Senate.
        ensure!(
            !T::SenateMembers::is_member(hotkey),
            Error::<T>::HotKeyAlreadyRegisteredInSubNet
        );

        // --- 3. Grab the hotkey's stake.
        let current_stake = Self::get_stake_for_hotkey_on_subnet(hotkey, Self::get_root_netuid());

        // Add the hotkey to the Senate.
        // If we're full, we'll swap out the lowest stake member.
        let members = T::SenateMembers::members();
        let last: Option<&T::AccountId> = None;
        if (members.len() as u32) == T::SenateMembers::max_members() {
            let mut sorted_members = members.clone();
            sorted_members.sort_by(|a, b| {
                let a_stake = Self::get_stake_for_hotkey_on_subnet(a, Self::get_root_netuid());
                let b_stake = Self::get_stake_for_hotkey_on_subnet(b, Self::get_root_netuid());

                b_stake.cmp(&a_stake)
            });

            if let Some(last) = sorted_members.last() {
                let last_stake =
                    Self::get_stake_for_hotkey_on_subnet(last, Self::get_root_netuid());

                if last_stake < current_stake {
                    // Swap the member with the lowest stake.
                    T::SenateMembers::swap_member(last, hotkey)
                        .map_err(|_| Error::<T>::CouldNotJoinSenate)?;
                }
            }
        } else {
            T::SenateMembers::add_member(hotkey).map_err(|_| Error::<T>::CouldNotJoinSenate)?;
        }

        // Return the swapped out member, if any.
        Ok(last)
    }

    pub fn do_vote_root(
        origin: T::RuntimeOrigin,
        hotkey: &T::AccountId,
        proposal: T::Hash,
        index: u32,
        approve: bool,
    ) -> DispatchResultWithPostInfo {
        // --- 1. Ensure that the caller has signed with their coldkey.
        let coldkey = ensure_signed(origin.clone())?;

        // --- 2. Ensure that the calling coldkey owns the associated hotkey.
        ensure!(
            Self::coldkey_owns_hotkey(&coldkey, hotkey),
            Error::<T>::NonAssociatedColdKey
        );

        // --- 3. Ensure that the calling hotkey is a member of the senate.
        ensure!(
            T::SenateMembers::is_member(hotkey),
            Error::<T>::NotSenateMember
        );

        // --- 4. Detects first vote of the member in the motion
        let is_account_voting_first_time =
            T::TriumvirateInterface::add_vote(hotkey, proposal, index, approve)?;

        // --- 5. Calculate extrinsic weight
        let members = T::SenateMembers::members();
        let member_count = members.len() as u32;
        let vote_weight = Weight::from_parts(20_528_275, 4980)
            .saturating_add(Weight::from_parts(48_856, 0).saturating_mul(member_count.into()))
            .saturating_add(T::DbWeight::get().reads(2_u64))
            .saturating_add(T::DbWeight::get().writes(1_u64))
            .saturating_add(Weight::from_parts(0, 128).saturating_mul(member_count.into()));

        Ok((
            Some(vote_weight),
            if is_account_voting_first_time {
                Pays::No
            } else {
                Pays::Yes
            },
        )
            .into())
    }

    /// Facilitates the removal of a user's subnetwork.
    ///
    /// # Args:
    /// * 'origin': ('T::RuntimeOrigin'): The calling origin. Must be signed.
    /// * 'netuid': ('u16'): The unique identifier of the network to be removed.
    ///
    /// # Event:
    /// * 'NetworkRemoved': Emitted when a network is successfully removed.
    ///
    /// # Raises:
    /// * 'SubNetworkDoesNotExist': If the specified network does not exist.
    /// * 'NotSubnetOwner': If the caller does not own the specified subnet.
    ///
    pub fn user_remove_network(coldkey: T::AccountId, netuid: u16) -> dispatch::DispatchResult {
        // --- 1. Ensure this subnet exists.
        ensure!(
            Self::if_subnet_exist(netuid),
            Error::<T>::SubNetworkDoesNotExist
        );

        // --- 2. Ensure the caller owns this subnet.
        ensure!(
            SubnetOwner::<T>::get(netuid) == coldkey,
            Error::<T>::NotSubnetOwner
        );

        // --- 4. Remove the subnet identity if it exists.
        if SubnetIdentities::<T>::take(netuid).is_some() {
            Self::deposit_event(Event::SubnetIdentityRemoved(netuid));
        }

        // --- 5. Explicitly erase the network and all its parameters.
        Self::remove_network(netuid);

        // --- 6. Emit the NetworkRemoved event.
        log::debug!("NetworkRemoved( netuid:{:?} )", netuid);
        Self::deposit_event(Event::NetworkRemoved(netuid));

        // --- 7. Return success.
        Ok(())
    }

    /// Removes a network (identified by netuid) and all associated parameters.
    ///
    /// This function is responsible for cleaning up all the data associated with a network.
    /// It ensures that all the storage values related to the network are removed, any
    /// reserved balance is returned to the network owner, and the subnet identity is removed if it exists.
    ///
    /// # Args:
    ///  * 'netuid': ('u16'): The unique identifier of the network to be removed.
    ///
    /// # Note:
    /// This function does not emit any events, nor does it raise any errors. It silently
    /// returns if any internal checks fail.
    pub fn remove_network(netuid: u16) {
        // --- 1. Return balance to subnet owner.
        let owner_coldkey: T::AccountId = SubnetOwner::<T>::get(netuid);
        let reserved_amount: u64 = Self::get_subnet_locked_balance(netuid);

        // --- 2. Remove network count.
        SubnetworkN::<T>::remove(netuid);

        // --- 3. Remove network modality storage.
        NetworkModality::<T>::remove(netuid);

        // --- 4. Remove netuid from added networks.
        NetworksAdded::<T>::remove(netuid);

        // --- 5. Decrement the network counter.
        TotalNetworks::<T>::mutate(|n: &mut u16| *n = n.saturating_sub(1));

        // --- 6. Remove various network-related storages.
        NetworkRegisteredAt::<T>::remove(netuid);

        // --- 7. Remove incentive mechanism memory.
        let _ = Uids::<T>::clear_prefix(netuid, u32::MAX, None);
        let _ = Keys::<T>::clear_prefix(netuid, u32::MAX, None);
        let _ = Bonds::<T>::clear_prefix(netuid, u32::MAX, None);

        // --- 8. Removes the weights for this subnet (do not remove).
        let _ = Weights::<T>::clear_prefix(netuid, u32::MAX, None);

        // --- 9. Iterate over stored weights and fill the matrix.
        for (uid_i, weights_i) in
            <Weights<T> as IterableStorageDoubleMap<u16, u16, Vec<(u16, u16)>>>::iter_prefix(
                Self::get_root_netuid(),
            )
        {
            // Create a new vector to hold modified weights.
            let mut modified_weights: Vec<(u16, u16)> = weights_i.clone();
            // Iterate over each weight entry to potentially update it.
            for (subnet_id, weight) in modified_weights.iter_mut() {
                if subnet_id == &netuid {
                    // If the condition matches, modify the weight
                    *weight = 0; // Set weight to 0 for the matching subnet_id.
                }
            }
            Weights::<T>::insert(Self::get_root_netuid(), uid_i, modified_weights);
        }

        // --- 10. Remove various network-related parameters.
        Rank::<T>::remove(netuid);
        Trust::<T>::remove(netuid);
        Active::<T>::remove(netuid);
        Emission::<T>::remove(netuid);
        Incentive::<T>::remove(netuid);
        Consensus::<T>::remove(netuid);
        Dividends::<T>::remove(netuid);
        PruningScores::<T>::remove(netuid);
        LastUpdate::<T>::remove(netuid);
        ValidatorPermit::<T>::remove(netuid);
        ValidatorTrust::<T>::remove(netuid);

        // --- 11. Erase network parameters.
        Tempo::<T>::remove(netuid);
        Kappa::<T>::remove(netuid);
        Difficulty::<T>::remove(netuid);
        MaxAllowedUids::<T>::remove(netuid);
        ImmunityPeriod::<T>::remove(netuid);
        ActivityCutoff::<T>::remove(netuid);
        EmissionValues::<T>::remove(netuid);
        MaxWeightsLimit::<T>::remove(netuid);
        MinAllowedWeights::<T>::remove(netuid);
        RegistrationsThisInterval::<T>::remove(netuid);
        POWRegistrationsThisInterval::<T>::remove(netuid);
        BurnRegistrationsThisInterval::<T>::remove(netuid);

        // --- 12. Add the balance back to the owner.
        Self::add_balance_to_coldkey_account(&owner_coldkey, reserved_amount);
        Self::set_subnet_locked_balance(netuid, 0);
        SubnetOwner::<T>::remove(netuid);

        // --- 13. Remove subnet identity if it exists.
        if SubnetIdentities::<T>::contains_key(netuid) {
            SubnetIdentities::<T>::remove(netuid);
            Self::deposit_event(Event::SubnetIdentityRemoved(netuid));
        }
    }

    #[allow(clippy::arithmetic_side_effects)]
    /// This function calculates the lock cost for a network based on the last lock amount, minimum lock cost, last lock block, and current block.
    /// The lock cost is calculated using the formula:
    /// lock_cost = (last_lock * mult) - (last_lock / lock_reduction_interval) * (current_block - last_lock_block)
    /// where:
    /// - last_lock is the last lock amount for the network
    /// - mult is the multiplier which increases lock cost each time a registration occurs
    /// - last_lock_block is the block number at which the last lock occurred
    /// - lock_reduction_interval the number of blocks before the lock returns to previous value.
    /// - current_block is the current block number
    /// - DAYS is the number of blocks in a day
    /// - min_lock is the minimum lock cost for the network
    ///
    /// If the calculated lock cost is less than the minimum lock cost, the minimum lock cost is returned.
    ///
    /// # Returns:
    ///  * 'u64':
    ///     - The lock cost for the network.
    ///
    pub fn get_network_lock_cost() -> u64 {
        #[cfg(feature = "pow-faucet")]
        return 0_u64;

        #[cfg(not(feature = "pow-faucet"))]
        {
            let last_lock = Self::get_network_last_lock();
            let min_lock = Self::get_network_min_lock();
            let last_lock_block = Self::get_network_last_lock_block();
            let current_block = Self::get_current_block_as_u64();
            let lock_reduction_interval = Self::get_lock_reduction_interval();
            let mult = if last_lock_block == 0 { 1 } else { 2 };

            let mut lock_cost = last_lock.saturating_mul(mult).saturating_sub(
                last_lock
                    .saturating_div(lock_reduction_interval)
                    .saturating_mul(current_block.saturating_sub(last_lock_block)),
            );

            if lock_cost < min_lock {
                lock_cost = min_lock;
            }

            log::debug!( "last_lock: {:?}, min_lock: {:?}, last_lock_block: {:?}, lock_reduction_interval: {:?}, current_block: {:?}, mult: {:?} lock_cost: {:?}",
        last_lock, min_lock, last_lock_block, lock_reduction_interval, current_block, mult, lock_cost);

            lock_cost
        }
    }

    /// This function is used to determine which subnet to prune when the total number of networks has reached the limit.
    /// It iterates over all the networks and finds the oldest subnet with the minimum emission value that is not in the immunity period.
    ///
    /// # Returns:
    /// * 'u16':
    ///     - The uid of the network to be pruned.
    ///
    pub fn get_subnet_to_prune() -> u16 {
        let mut netuids: Vec<u16> = vec![];
        let current_block = Self::get_current_block_as_u64();

        // Even if we don't have a root subnet, this still works
        for netuid in NetworksAdded::<T>::iter_keys_from(NetworksAdded::<T>::hashed_key_for(0)) {
            if current_block.saturating_sub(Self::get_network_registered_block(netuid))
                < Self::get_network_immunity_period()
            {
                continue;
            }

            // This iterator seems to return them in order anyways, so no need to sort by key
            netuids.push(netuid);
        }

        // Now we sort by emission, and then by subnet creation time.
        netuids.sort_by(|a, b| {
            use sp_std::cmp::Ordering;

            match Self::get_emission_value(*b).cmp(&Self::get_emission_value(*a)) {
                Ordering::Equal => {
                    if Self::get_network_registered_block(*b)
                        < Self::get_network_registered_block(*a)
                    {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                }
                v => v,
            }
        });

        log::debug!("Netuids Order: {:?}", netuids);

        match netuids.last() {
            Some(netuid) => *netuid,
            None => 0,
        }
    }

    pub fn get_network_registered_block(netuid: u16) -> u64 {
        NetworkRegisteredAt::<T>::get(netuid)
    }
    pub fn get_network_immunity_period() -> u64 {
        NetworkImmunityPeriod::<T>::get()
    }
    pub fn set_network_immunity_period(net_immunity_period: u64) {
        NetworkImmunityPeriod::<T>::set(net_immunity_period);
        Self::deposit_event(Event::NetworkImmunityPeriodSet(net_immunity_period));
    }
    pub fn set_network_min_lock(net_min_lock: u64) {
        NetworkMinLockCost::<T>::set(net_min_lock);
        Self::deposit_event(Event::NetworkMinLockCostSet(net_min_lock));
    }
    pub fn get_network_min_lock() -> u64 {
        NetworkMinLockCost::<T>::get()
    }
    pub fn set_network_last_lock(net_last_lock: u64) {
        NetworkLastLockCost::<T>::set(net_last_lock);
    }
    pub fn get_network_last_lock() -> u64 {
        NetworkLastLockCost::<T>::get()
    }
    pub fn get_network_last_lock_block() -> u64 {
        NetworkLastRegistered::<T>::get()
    }
    pub fn set_network_last_lock_block(block: u64) {
        NetworkLastRegistered::<T>::set(block);
    }
    pub fn set_lock_reduction_interval(interval: u64) {
        NetworkLockReductionInterval::<T>::set(interval);
        Self::deposit_event(Event::NetworkLockCostReductionIntervalSet(interval));
    }
    pub fn get_lock_reduction_interval() -> u64 {
        NetworkLockReductionInterval::<T>::get()
    }
}
