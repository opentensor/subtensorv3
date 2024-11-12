use super::*;
use frame_support::weights::Weight;
use sp_core::Get;

impl<T: Config> Pallet<T> {
    /// Swaps the coldkey associated with a set of hotkeys from an old coldkey to a new coldkey.
    ///
    /// # Arguments
    ///
    /// * `origin` - The origin of the call, which must be signed by the old coldkey.
    /// * `new_coldkey` - The account ID of the new coldkey.
    ///
    /// # Returns
    ///
    /// Returns a `DispatchResultWithPostInfo` indicating success or failure, along with the weight consumed.
    ///
    /// # Errors
    ///
    /// This function will return an error if:
    /// - The caller is not a valid signed origin.
    /// - The old coldkey (caller) is in arbitration.
    /// - The new coldkey is already associated with other hotkeys or is a hotkey itself.
    /// - There's not enough balance to pay for the swap.
    ///
    /// # Events
    ///
    /// Emits a `ColdkeySwapped` event when successful.
    ///
    /// # Weight
    ///
    /// Weight is tracked and updated throughout the function execution.
    pub fn do_swap_coldkey(
        old_coldkey: &T::AccountId,
        new_coldkey: &T::AccountId,
    ) -> DispatchResultWithPostInfo {
        // 2. Initialize the weight for this operation
        let mut weight: Weight = T::DbWeight::get().reads(2);
        // 3. Ensure the new coldkey is not associated with any hotkeys
        ensure!(
            StakingHotkeys::<T>::get(new_coldkey).is_empty(),
            Error::<T>::ColdKeyAlreadyAssociated
        );
        weight = weight.saturating_add(T::DbWeight::get().reads(1));

        // 4. Ensure the new coldkey is not a hotkey
        ensure!(
            !Self::hotkey_account_exists(new_coldkey),
            Error::<T>::NewColdKeyIsHotkey
        );
        weight = weight.saturating_add(T::DbWeight::get().reads(1));

        // 5. Swap the identity if the old coldkey has one
        if let Some(identity) = Identities::<T>::take(old_coldkey) {
            Identities::<T>::insert(new_coldkey, identity);
        }

        // 6. Calculate the swap cost and ensure sufficient balance
        let swap_cost = Self::get_key_swap_cost();
        ensure!(
            Self::can_remove_balance_from_coldkey_account(old_coldkey, swap_cost),
            Error::<T>::NotEnoughBalanceToPaySwapColdKey
        );

        // 7. Remove and burn the swap cost from the old coldkey's account
        let actual_burn_amount = Self::remove_balance_from_coldkey_account(old_coldkey, swap_cost)?;
        Self::burn_tokens(actual_burn_amount);

        // 8. Update the weight for the balance operations
        weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 1));

        // 9. Perform the actual coldkey swap
        let _ = Self::perform_swap_coldkey(old_coldkey, new_coldkey, &mut weight);

        // 10. Update the last transaction block for the new coldkey
        Self::set_last_tx_block(new_coldkey, Self::get_current_block_as_u64());
        weight.saturating_accrue(T::DbWeight::get().writes(1));

        // 11. Remove the coldkey swap scheduled record
        ColdkeySwapScheduled::<T>::remove(old_coldkey);

        // 12. Emit the ColdkeySwapped event
        Self::deposit_event(Event::ColdkeySwapped {
            old_coldkey: old_coldkey.clone(),
            new_coldkey: new_coldkey.clone(),
        });

        // 12. Return the result with the updated weight
        Ok(Some(weight).into())
    }

    /// Performs the actual coldkey swap operation, transferring all associated data and balances from the old coldkey to the new coldkey.
    ///
    /// # Arguments
    ///
    /// * `old_coldkey` - The account ID of the old coldkey.
    /// * `new_coldkey` - The account ID of the new coldkey.
    /// * `weight` - A mutable reference to the current transaction weight.
    ///
    /// # Returns
    ///
    /// Returns a `DispatchResult` indicating success or failure of the operation.
    ///
    /// # Steps
    ///
    /// 1. Swap TotalHotkeyColdkeyStakesThisInterval:
    ///    - For each hotkey owned by the old coldkey, transfer its stake and block data to the new coldkey.
    ///
    /// 2. Swap subnet ownership:
    ///    - For each subnet, if the old coldkey is the owner, transfer ownership to the new coldkey.
    ///
    /// 3. Swap Stakes:
    ///    - For each hotkey staking for the old coldkey, transfer its stake to the new coldkey.
    ///
    /// 4. Swap total coldkey stake:
    ///    - Transfer the total stake from the old coldkey to the new coldkey.
    ///
    /// 5. Swap StakingHotkeys:
    ///    - Transfer the list of staking hotkeys from the old coldkey to the new coldkey.
    ///
    /// 6. Swap hotkey owners:
    ///    - For each hotkey owned by the old coldkey, transfer ownership to the new coldkey.
    ///    - Update the list of owned hotkeys for both old and new coldkeys.
    ///
    /// 7. Transfer remaining balance:
    ///    - Transfer any remaining balance from the old coldkey to the new coldkey.
    ///
    /// Throughout the process, the function updates the transaction weight to reflect the operations performed.
    ///
    /// # Notes
    ///
    /// This function is a critical part of the coldkey swap process and should be called only after all necessary checks and validations have been performed.
    pub fn perform_swap_coldkey(
        old_coldkey: &T::AccountId,
        new_coldkey: &T::AccountId,
        weight: &mut Weight,
    ) -> DispatchResult {
        // 1. Swap TotalHotkeyColdkeyStakesThisInterval
        // TotalHotkeyColdkeyStakesThisInterval: MAP ( hotkey, coldkey ) --> ( stake, block ) | Stake of the hotkey for the coldkey.
        for hotkey in OwnedHotkeys::<T>::get(old_coldkey).iter() {
            let (stake, block) =
                TotalHotkeyColdkeyStakesThisInterval::<T>::get(&hotkey, old_coldkey);
            TotalHotkeyColdkeyStakesThisInterval::<T>::remove(&hotkey, old_coldkey);
            TotalHotkeyColdkeyStakesThisInterval::<T>::insert(&hotkey, new_coldkey, (stake, block));
            weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 2));
        }

        // 2. Swap subnet owner.
        // SubnetOwner: MAP ( netuid ) --> (coldkey) | Owner of the subnet.
        for netuid in Self::get_all_subnet_netuids() {
            let subnet_owner = SubnetOwner::<T>::get(netuid);
            if subnet_owner == *old_coldkey {
                SubnetOwner::<T>::insert(netuid, new_coldkey.clone());
            }
            weight.saturating_accrue(T::DbWeight::get().reads_writes(1, 1));
        }

        // 3. Swap Stake.
        // Stake: MAP ( hotkey, coldkey ) --> u64 | Stake of the hotkey for the coldkey.
        for hotkey in StakingHotkeys::<T>::get(old_coldkey) {
            // Get the stake on the old (hot,coldkey) account.
            let old_stake: u64 = Stake::<T>::get(&hotkey, old_coldkey);
            // Get the stake on the new (hot,coldkey) account.
            let new_stake: u64 = Stake::<T>::get(&hotkey, new_coldkey);
            // Add the stake to new account.
            Stake::<T>::insert(&hotkey, new_coldkey, new_stake.saturating_add(old_stake));
            // Remove the value from the old account.
            Stake::<T>::remove(&hotkey, old_coldkey);
            // Add the weight for the read and write.
            weight.saturating_accrue(T::DbWeight::get().reads_writes(2, 2));
        }

        // 4. Swap StakeDeltaSinceLastEmissionDrain
        for hotkey in StakingHotkeys::<T>::get(old_coldkey) {
            let old_stake_delta = StakeDeltaSinceLastEmissionDrain::<T>::get(&hotkey, old_coldkey);
            let new_stake_delta = StakeDeltaSinceLastEmissionDrain::<T>::get(&hotkey, new_coldkey);
            StakeDeltaSinceLastEmissionDrain::<T>::insert(
                &hotkey,
                new_coldkey,
                new_stake_delta.saturating_add(old_stake_delta),
            );
            StakeDeltaSinceLastEmissionDrain::<T>::remove(&hotkey, old_coldkey);
            weight.saturating_accrue(T::DbWeight::get().reads_writes(2, 2));
        }

        // 5. Swap total coldkey stake.
        // TotalColdkeyStake: MAP ( coldkey ) --> u64 | Total stake of the coldkey.
        let old_coldkey_stake: u64 = TotalColdkeyStake::<T>::get(old_coldkey);
        // Get the stake of the new coldkey.
        let new_coldkey_stake: u64 = TotalColdkeyStake::<T>::get(new_coldkey);
        // Remove the value from the old account.
        TotalColdkeyStake::<T>::insert(old_coldkey, 0);
        // Add the stake to new account.
        TotalColdkeyStake::<T>::insert(
            new_coldkey,
            new_coldkey_stake.saturating_add(old_coldkey_stake),
        );
        weight.saturating_accrue(T::DbWeight::get().reads_writes(2, 2));

        // 6. Swap StakingHotkeys.
        // StakingHotkeys: MAP ( coldkey ) --> Vec<hotkeys> | Hotkeys staking for the coldkey.
        let old_staking_hotkeys: Vec<T::AccountId> = StakingHotkeys::<T>::get(old_coldkey);
        let mut new_staking_hotkeys: Vec<T::AccountId> = StakingHotkeys::<T>::get(new_coldkey);
        for hotkey in old_staking_hotkeys {
            // If the hotkey is not already in the new coldkey, add it.
            if !new_staking_hotkeys.contains(&hotkey) {
                new_staking_hotkeys.push(hotkey);
            }
        }
        StakingHotkeys::<T>::remove(old_coldkey);
        StakingHotkeys::<T>::insert(new_coldkey, new_staking_hotkeys);
        weight.saturating_accrue(T::DbWeight::get().reads_writes(2, 2));

        // 7. Swap hotkey owners.
        // Owner: MAP ( hotkey ) --> coldkey | Owner of the hotkey.
        // OwnedHotkeys: MAP ( coldkey ) --> Vec<hotkeys> | Hotkeys owned by the coldkey.
        let old_owned_hotkeys: Vec<T::AccountId> = OwnedHotkeys::<T>::get(old_coldkey);
        let mut new_owned_hotkeys: Vec<T::AccountId> = OwnedHotkeys::<T>::get(new_coldkey);
        for owned_hotkey in old_owned_hotkeys.iter() {
            // Remove the hotkey from the old coldkey.
            Owner::<T>::remove(owned_hotkey);
            // Add the hotkey to the new coldkey.
            Owner::<T>::insert(owned_hotkey, new_coldkey.clone());
            // Addd the owned hotkey to the new set of owned hotkeys.
            if !new_owned_hotkeys.contains(owned_hotkey) {
                new_owned_hotkeys.push(owned_hotkey.clone());
            }
        }
        OwnedHotkeys::<T>::remove(old_coldkey);
        OwnedHotkeys::<T>::insert(new_coldkey, new_owned_hotkeys);
        weight.saturating_accrue(T::DbWeight::get().reads_writes(2, 2));

        // 8. Transfer remaining balance.
        // Balance: MAP ( coldkey ) --> u64 | Balance of the coldkey.
        // Transfer any remaining balance from old_coldkey to new_coldkey
        let remaining_balance = Self::get_coldkey_balance(old_coldkey);
        if remaining_balance > 0 {
            Self::kill_coldkey_account(old_coldkey, remaining_balance)?;
            Self::add_balance_to_coldkey_account(new_coldkey, remaining_balance);
        }
        weight.saturating_accrue(T::DbWeight::get().reads_writes(2, 2));

        // Return ok.
        Ok(())
    }
}
