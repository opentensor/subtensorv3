use super::*;
use frame_support::weights::Weight;
use sp_core::Get;
use substrate_fixed::types::U64F64;

impl<T: Config> Pallet<T> {
    /// Swaps the hotkey of a coldkey account.
    ///
    /// # Arguments
    ///
    /// * `origin` - The origin of the transaction, and also the coldkey account.
    /// * `old_hotkey` - The old hotkey to be swapped.
    /// * `new_hotkey` - The new hotkey to replace the old one.
    ///
    /// # Returns
    ///
    /// * `DispatchResultWithPostInfo` - The result of the dispatch.
    ///
    /// # Errors
    ///
    /// * `NonAssociatedColdKey` - If the coldkey does not own the old hotkey.
    /// * `HotKeySetTxRateLimitExceeded` - If the transaction rate limit is exceeded.
    /// * `NewHotKeyIsSameWithOld` - If the new hotkey is the same as the old hotkey.
    /// * `HotKeyAlreadyRegisteredInSubNet` - If the new hotkey is already registered in the subnet.
    /// * `NotEnoughBalanceToPaySwapHotKey` - If there is not enough balance to pay for the swap.
    pub fn do_swap_hotkey(
        origin: T::RuntimeOrigin,
        old_hotkey: &T::AccountId,
        new_hotkey: &T::AccountId,
    ) -> DispatchResultWithPostInfo {
        // 1. Ensure the origin is signed and get the coldkey
        let coldkey = ensure_signed(origin)?;

        // 2. Initialize the weight for this operation
        let mut weight = T::DbWeight::get().reads(2);

        // 3. Ensure the new hotkey is different from the old one
        ensure!(old_hotkey != new_hotkey, Error::<T>::NewHotKeyIsSameWithOld);

        // 4. Ensure the new hotkey is not already registered on any network
        ensure!(
            !Self::is_hotkey_registered_on_any_network(new_hotkey),
            Error::<T>::HotKeyAlreadyRegisteredInSubNet
        );

        // 5. Update the weight for the checks above
        weight.saturating_accrue(T::DbWeight::get().reads_writes(2, 0));

        // 6. Ensure the coldkey owns the old hotkey
        ensure!(
            Self::coldkey_owns_hotkey(&coldkey, old_hotkey),
            Error::<T>::NonAssociatedColdKey
        );

        // 7. Get the current block number
        let block: u64 = Self::get_current_block_as_u64();

        // 8. Ensure the transaction rate limit is not exceeded
        ensure!(
            !Self::exceeds_tx_rate_limit(Self::get_last_tx_block(&coldkey), block),
            Error::<T>::HotKeySetTxRateLimitExceeded
        );

        // 9. Update the weight for reading the total networks
        weight.saturating_accrue(
            T::DbWeight::get().reads((TotalNetworks::<T>::get().saturating_add(1u16)) as u64),
        );

        // 10. Get the cost for swapping the key
        let swap_cost = Self::get_key_swap_cost();
        log::debug!("Swap cost: {:?}", swap_cost);

        // 11. Ensure the coldkey has enough balance to pay for the swap
        ensure!(
            Self::can_remove_balance_from_coldkey_account(&coldkey, swap_cost),
            Error::<T>::NotEnoughBalanceToPaySwapHotKey
        );

        // 12. Remove the swap cost from the coldkey's account
        let actual_burn_amount = Self::remove_balance_from_coldkey_account(&coldkey, swap_cost)?;

        // 13. Burn the tokens
        Self::burn_tokens(actual_burn_amount);

        // 14. Perform the hotkey swap
        let _ = Self::perform_hotkey_swap(old_hotkey, new_hotkey, &coldkey, &mut weight);

        // 15. Update the last transaction block for the coldkey
        Self::set_last_tx_block(&coldkey, block);
        weight.saturating_accrue(T::DbWeight::get().writes(1));

        // 16. Emit an event for the hotkey swap
        Self::deposit_event(Event::HotkeySwapped {
            coldkey,
            old_hotkey: old_hotkey.clone(),
            new_hotkey: new_hotkey.clone(),
        });

        // 17. Return the weight of the operation
        Ok(Some(weight).into())
    }

    /// Performs the hotkey swap operation, transferring all associated data and state from the old hotkey to the new hotkey.
    ///
    /// This function executes a series of steps to ensure a complete transfer of all relevant information:
    /// 1. Swaps the owner of the hotkey.
    /// 2. Updates the list of owned hotkeys for the coldkey.
    /// 3. Transfers the total hotkey stake.
    /// 4. Moves all stake-related data for the interval.
    /// 5. Updates the last transaction block for the new hotkey.
    /// 6. Transfers the delegate take information.
    /// 7. Swaps Senate membership if applicable.
    /// 8. Updates delegate information.
    /// 9. For each subnet:
    ///    - Updates network membership status.
    ///    - Transfers UID and key information.
    ///    - Moves Prometheus data.
    ///    - Updates axon information.
    ///    - Transfers weight commits.
    ///    - Updates loaded emission data.
    /// 10. Transfers all stake information, including updating staking hotkeys for each coldkey.
    ///
    /// Throughout the process, the function accumulates the computational weight of operations performed.
    ///
    /// # Arguments
    /// * `old_hotkey` - The AccountId of the current hotkey to be replaced.
    /// * `new_hotkey` - The AccountId of the new hotkey to replace the old one.
    /// * `coldkey` - The AccountId of the coldkey that owns both hotkeys.
    /// * `weight` - A mutable reference to the Weight, updated as operations are performed.
    ///
    /// # Returns
    /// * `DispatchResult` - Ok(()) if the swap was successful, or an error if any operation failed.
    ///
    /// # Note
    /// This function performs extensive storage reads and writes, which can be computationally expensive.
    /// The accumulated weight should be carefully considered in the context of block limits.
    pub fn perform_hotkey_swap(
        old_hotkey: &T::AccountId,
        new_hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        weight: &mut Weight,
    ) -> DispatchResult {
        // 1. Swap owner.
        // Owner( hotkey ) -> coldkey -- the coldkey that owns the hotkey.
        Owner::<T>::remove(old_hotkey);
        Owner::<T>::insert(new_hotkey, coldkey.clone());
        weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 1));

        // 2. Swap OwnedHotkeys.
        // OwnedHotkeys( coldkey ) -> Vec<hotkey> -- the hotkeys that the coldkey owns.
        let mut hotkeys = OwnedHotkeys::<T>::get(coldkey);
        // Add the new key if needed.
        if !hotkeys.contains(new_hotkey) {
            hotkeys.push(new_hotkey.clone());
        }
        // Remove the old key.
        hotkeys.retain(|hk| *hk != *old_hotkey);
        OwnedHotkeys::<T>::insert(coldkey, hotkeys);
        weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 1));

        // 3. Swap total hotkey alpha for all subnets.
        // TotalHotkeyAlpha( hotkey, netuid ) -> alpha -- the total alpha that the hotkey has on a specific subnet.
        let all_netuids: Vec<u16> = Self::get_all_subnet_netuids();
        for netuid in all_netuids {
            let old_total_hotkey_alpha = TotalHotkeyAlpha::<T>::get(old_hotkey, netuid);
            let new_total_hotkey_alpha = TotalHotkeyAlpha::<T>::get(new_hotkey, netuid);
            TotalHotkeyAlpha::<T>::remove(old_hotkey, netuid);
            TotalHotkeyAlpha::<T>::insert(
                new_hotkey,
                netuid,
                old_total_hotkey_alpha.saturating_add(new_total_hotkey_alpha),
            );
            weight.saturating_accrue(T::DbWeight::get().reads_writes(2, 2));
        }

        // 4. Swap total hotkey shares on all subnets
        // TotalHotkeyShares( hotkey, netuid ) -> alpha -- the total alpha that the hotkey has on a specific subnet.
        let all_netuids: Vec<u16> = Self::get_all_subnet_netuids();
        for netuid in all_netuids {
            let old_total_hotkey_shares = TotalHotkeyShares::<T>::get(old_hotkey, netuid);
            let new_total_hotkey_shares = TotalHotkeyShares::<T>::get(new_hotkey, netuid);
            TotalHotkeyShares::<T>::remove(old_hotkey, netuid);
            TotalHotkeyShares::<T>::insert(
                new_hotkey,
                netuid,
                old_total_hotkey_shares.saturating_add(new_total_hotkey_shares),
            );
            weight.saturating_accrue(T::DbWeight::get().reads_writes(2, 2));
        }

        // 5. Swap LastTxBlock
        // LastTxBlock( hotkey ) --> u64 -- the last transaction block for the hotkey.
        LastTxBlock::<T>::remove(old_hotkey);
        LastTxBlock::<T>::insert(new_hotkey, Self::get_current_block_as_u64());
        weight.saturating_accrue(T::DbWeight::get().reads_writes(0, 2));

        // 6. Swap LastTxBlockDelegateTake
        // LastTxBlockDelegateTake( hotkey ) --> u64 -- the last transaction block for the hotkey delegate take.
        LastTxBlockDelegateTake::<T>::remove(old_hotkey);
        LastTxBlockDelegateTake::<T>::insert(new_hotkey, Self::get_current_block_as_u64());
        weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));

        // 7. Swap Senate members.
        // Senate( hotkey ) --> ?
        if T::SenateMembers::is_member(old_hotkey) {
            T::SenateMembers::swap_member(old_hotkey, new_hotkey).map_err(|e| e.error)?;
            weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));
        }

        // 8. Swap delegates.
        // Delegates( hotkey ) -> take value -- the hotkey delegate take value.
        if Delegates::<T>::contains_key(old_hotkey) {
            let old_delegate_take = Delegates::<T>::get(old_hotkey);
            Delegates::<T>::remove(old_hotkey);
            Delegates::<T>::insert(new_hotkey, old_delegate_take);
            weight.saturating_accrue(T::DbWeight::get().reads_writes(2, 2));
        }

        // 9. swap PendingHotkeyEmissionOnNetuid
        // (DEPRECATED.)

        // 10. Swap all subnet specific info.
        let all_netuids: Vec<u16> = Self::get_all_subnet_netuids();
        all_netuids.iter().for_each(|netuid| {
            // 10.1 Remove the previous hotkey and insert the new hotkey from membership.
            // IsNetworkMember( hotkey, netuid ) -> bool -- is the hotkey a subnet member.
            let is_network_member: bool = IsNetworkMember::<T>::get(old_hotkey, netuid);
            IsNetworkMember::<T>::remove(old_hotkey, netuid);
            IsNetworkMember::<T>::insert(new_hotkey, netuid, is_network_member);
            weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));

            // 10.2 Swap Uids + Keys.
            // Keys( netuid, hotkey ) -> uid -- the uid the hotkey has in the network if it is a member.
            // Uids( netuid, hotkey ) -> uid -- the uids that the hotkey has.
            if is_network_member {
                // 10.2.1 Swap the UIDS
                if let Ok(old_uid) = Uids::<T>::try_get(netuid, old_hotkey) {
                    Uids::<T>::remove(netuid, old_hotkey);
                    Uids::<T>::insert(netuid, new_hotkey, old_uid);
                    weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));

                    // 10.2.2 Swap the keys.
                    Keys::<T>::insert(netuid, old_uid, new_hotkey.clone());
                    weight.saturating_accrue(T::DbWeight::get().reads_writes(0, 1));
                }
            }

            // 10.3 Swap Prometheus.
            // Prometheus( netuid, hotkey ) -> prometheus -- the prometheus data that a hotkey has in the network.
            if is_network_member {
                if let Ok(old_prometheus_info) = Prometheus::<T>::try_get(netuid, old_hotkey) {
                    Prometheus::<T>::remove(netuid, old_hotkey);
                    Prometheus::<T>::insert(netuid, new_hotkey, old_prometheus_info);
                    weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));
                }
            }

            // 10.4. Swap axons.
            // Axons( netuid, hotkey ) -> axon -- the axon that the hotkey has.
            if is_network_member {
                if let Ok(old_axon_info) = Axons::<T>::try_get(netuid, old_hotkey) {
                    Axons::<T>::remove(netuid, old_hotkey);
                    Axons::<T>::insert(netuid, new_hotkey, old_axon_info);
                    weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));
                }
            }

            // 10.5 Swap WeightCommits
            // WeightCommits( hotkey ) --> Vec<u64> -- the weight commits for the hotkey.
            if is_network_member {
                if let Ok(old_weight_commits) = WeightCommits::<T>::try_get(netuid, old_hotkey) {
                    WeightCommits::<T>::remove(netuid, old_hotkey);
                    WeightCommits::<T>::insert(netuid, new_hotkey, old_weight_commits);
                    weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));
                }
            }

            // 10.6. Swap the subnet loaded emission.
            // LoadedEmission( netuid ) --> Vec<(hotkey, u64)> -- the loaded emission for the subnet.
            if is_network_member {
                if let Some(mut old_loaded_emission) = LoadedEmission::<T>::get(netuid) {
                    for emission in old_loaded_emission.iter_mut() {
                        if emission.0 == *old_hotkey {
                            emission.0 = new_hotkey.clone();
                        }
                    }
                    LoadedEmission::<T>::remove(netuid);
                    LoadedEmission::<T>::insert(netuid, old_loaded_emission);
                    weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));
                }
            }

            // 10.7. Swap neuron TLS certificates.
            // NeuronCertificates( netuid, hotkey ) -> Vec<u8> -- the neuron certificate for the hotkey.
            if is_network_member {
                if let Ok(old_neuron_certificates) =
                    NeuronCertificates::<T>::try_get(netuid, old_hotkey)
                {
                    NeuronCertificates::<T>::remove(netuid, old_hotkey);
                    NeuronCertificates::<T>::insert(netuid, new_hotkey, old_neuron_certificates);
                    weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));
                }
            }
        });

        // 11. Swap Alpha
        // Alpha( hotkey, coldkey, netuid ) -> alpha
        let old_alpha_values: Vec<((T::AccountId, u16), U64F64)> =
            Alpha::<T>::iter_prefix((old_hotkey,)).collect();
        // Clear the entire old prefix here.
        let _ = Alpha::<T>::clear_prefix((old_hotkey,), old_alpha_values.len() as u32, None);
        weight.saturating_accrue(T::DbWeight::get().reads(old_alpha_values.len() as u64));
        weight.saturating_accrue(T::DbWeight::get().writes(old_alpha_values.len() as u64));

        // Insert the new alpha values.
        for ((coldkey, netuid), alpha) in old_alpha_values {
            let new_alpha = Alpha::<T>::get((new_hotkey, &coldkey, netuid));
            Alpha::<T>::insert(
                (new_hotkey, &coldkey, netuid),
                new_alpha.saturating_add(alpha),
            );
            weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 1));

            // Swap StakingHotkeys.
            // StakingHotkeys( coldkey ) --> Vec<hotkey> -- the hotkeys that the coldkey stakes.
            let mut staking_hotkeys = StakingHotkeys::<T>::get(&coldkey);
            weight.saturating_accrue(T::DbWeight::get().reads(1));
            if staking_hotkeys.contains(old_hotkey) {
                staking_hotkeys.retain(|hk| *hk != *old_hotkey && *hk != *new_hotkey);
                staking_hotkeys.push(new_hotkey.clone());
                StakingHotkeys::<T>::insert(&coldkey, staking_hotkeys);
                weight.saturating_accrue(T::DbWeight::get().writes(1));
            }
        }

        // 11. Swap Stake.
        // Stake( hotkey, coldkey ) -> stake -- the stake that the hotkey controls on behalf of the coldkey.
        let stakes: Vec<(T::AccountId, u64)> = Stake::<T>::iter_prefix(old_hotkey).collect();
        // Clear the entire old prefix here.
        let _ = Stake::<T>::clear_prefix(old_hotkey, stakes.len() as u32, None);
        // Iterate over all the staking rows and insert them into the new hotkey.
        for (coldkey, old_stake_amount) in stakes {
            weight.saturating_accrue(T::DbWeight::get().reads(1));

            // Swap Stake value
            // Stake( hotkey, coldkey ) -> stake -- the stake that the hotkey controls on behalf of the coldkey.
            // Get the new stake value.
            let new_stake_value: u64 = Stake::<T>::get(new_hotkey, &coldkey);
            // Insert the new stake value.
            Stake::<T>::insert(
                new_hotkey,
                &coldkey,
                new_stake_value.saturating_add(old_stake_amount),
            );
            weight.saturating_accrue(T::DbWeight::get().writes(1));
        }

        // 12. Swap ChildKeys.
        // ChildKeys( parent, netuid ) --> Vec<(proportion,child)> -- the child keys of the parent.
        for netuid in Self::get_all_subnet_netuids() {
            // Get the children of the old hotkey for this subnet
            let my_children: Vec<(u64, T::AccountId)> = ChildKeys::<T>::get(old_hotkey, netuid);
            // Remove the old hotkey's child entries
            ChildKeys::<T>::remove(old_hotkey, netuid);
            // Insert the same child entries for the new hotkey
            ChildKeys::<T>::insert(new_hotkey, netuid, my_children.clone());
            for (_, child_key_i) in my_children {
                // For each child, update their parent list
                let mut child_parents: Vec<(u64, T::AccountId)> =
                    ParentKeys::<T>::get(child_key_i.clone(), netuid);
                for parent in child_parents.iter_mut() {
                    // If the parent is the old hotkey, replace it with the new hotkey
                    if parent.1 == *old_hotkey {
                        parent.1 = new_hotkey.clone();
                    }
                }
                // Update the child's parent list
                ParentKeys::<T>::insert(child_key_i, netuid, child_parents);
            }
        }

        // 13. Swap ParentKeys.
        // ParentKeys( child, netuid ) --> Vec<(proportion,parent)> -- the parent keys of the child.
        for netuid in Self::get_all_subnet_netuids() {
            // Get the parents of the old hotkey for this subnet
            let parents: Vec<(u64, T::AccountId)> = ParentKeys::<T>::get(old_hotkey, netuid);
            // Remove the old hotkey's parent entries
            ParentKeys::<T>::remove(old_hotkey, netuid);
            // Insert the same parent entries for the new hotkey
            ParentKeys::<T>::insert(new_hotkey, netuid, parents.clone());
            for (_, parent_key_i) in parents {
                // For each parent, update their children list
                let mut parent_children: Vec<(u64, T::AccountId)> =
                    ChildKeys::<T>::get(parent_key_i.clone(), netuid);
                for child in parent_children.iter_mut() {
                    // If the child is the old hotkey, replace it with the new hotkey
                    if child.1 == *old_hotkey {
                        child.1 = new_hotkey.clone();
                    }
                }
                // Update the parent's children list
                ChildKeys::<T>::insert(parent_key_i, netuid, parent_children);
            }
        }

        // 14. Swap Stake Delta for all coldkeys.
        // DEPRECATED

        // Return successful after swapping all the relevant terms.
        Ok(())
    }

    pub fn swap_senate_member(
        old_hotkey: &T::AccountId,
        new_hotkey: &T::AccountId,
        weight: &mut Weight,
    ) -> DispatchResult {
        weight.saturating_accrue(T::DbWeight::get().reads(1));
        if T::SenateMembers::is_member(old_hotkey) {
            T::SenateMembers::swap_member(old_hotkey, new_hotkey).map_err(|e| e.error)?;
            weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));
        }
        Ok(())
    }
}
