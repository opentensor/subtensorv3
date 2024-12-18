use super::*;
use sp_core::Get;

impl<T: Config> Pallet<T> {
    /// ---- The implementation for the extrinsic do_set_child_singular: Sets a single child.
    /// This function allows a coldkey to set children keys.
    ///
    /// Adds a childkey vector to the PendingChildKeys map and performs a few checks:
    ///    **Signature Verification**: Ensures that the caller has signed the transaction, verifying the coldkey.
    ///    **Root Network Check**: Ensures that the delegation is not on the root network, as child hotkeys are not valid on the root.
    ///    **Network Existence Check**: Ensures that the specified network exists.
    ///    **Ownership Verification**: Ensures that the coldkey owns the hotkey.
    ///    **Hotkey Account Existence Check**: Ensures that the hotkey account already exists.
    ///    **Child count**: Only allow to add up to 5 children per parent
    ///    **Child-Hotkey Distinction**: Ensures that the child is not the same as the hotkey.
    ///    **Minimum stake**: Ensures that the parent key has at least the minimum stake.
    ///    **Proportion check**: Ensure that the sum of the proportions does not exceed u64::MAX.
    ///    **Duplicate check**: Ensure there are no duplicates in the list of children.
    ///
    /// # Events:
    /// * `SetChildrenScheduled`:
    ///     - If all checks pass and setting the childkeys is scheduled.
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
    /// * `TooManyChildren`:
    ///     - Too many children in request
    ///
    pub fn do_schedule_children(
        origin: T::RuntimeOrigin,
        hotkey: T::AccountId,
        netuid: u16,
        children: Vec<(u64, T::AccountId)>,
    ) -> DispatchResult {
        // Check that the caller has signed the transaction. (the coldkey of the pairing)
        let coldkey = ensure_signed(origin)?;
        log::trace!(
            "do_set_children( coldkey:{:?} hotkey:{:?} netuid:{:?} children:{:?} )",
            coldkey,
            netuid,
            hotkey,
            children
        );

        // Ensure the hotkey passes the rate limit.
        ensure!(
            Self::passes_rate_limit_on_subnet(
                &TransactionType::SetChildren, // Set children.
                &hotkey,                       // Specific to a hotkey.
                netuid,                        // Specific to a subnet.
            ),
            Error::<T>::TxRateLimitExceeded
        );

        // Set last transaction block
        let current_block = Self::get_current_block_as_u64();
        Self::set_last_transaction_block_on_subnet(
            &hotkey,
            netuid,
            &TransactionType::SetChildren,
            current_block,
        );

        // Check that this delegation is not on the root network. Child hotkeys are not valid on root.
        ensure!(
            netuid != Self::get_root_netuid(),
            Error::<T>::RegistrationNotPermittedOnRootSubnet
        );

        // Check that the network we are trying to create the child on exists.
        ensure!(
            Self::if_subnet_exist(netuid),
            Error::<T>::SubNetworkDoesNotExist
        );

        // Check that the coldkey owns the hotkey.
        ensure!(
            Self::coldkey_owns_hotkey(&coldkey, &hotkey),
            Error::<T>::NonAssociatedColdKey
        );

        // Ensure that the number of children does not exceed 5.
        ensure!(children.len() <= 5, Error::<T>::TooManyChildren);

        // Check that the parent coldkey is whitelisted or childkey whitelist is empty
        for (_, child_i) in &children {
            let child_whitelist = ChildkeyWhitelist::<T>::get(child_i, netuid);
            ensure!(
                child_whitelist.is_empty() || child_whitelist.contains(&coldkey),
                Error::<T>::ColdkeyIsNotWhitelisted
            );
        }

        // Ensure that each child is not the hotkey.
        for (_, child_i) in &children {
            ensure!(child_i != &hotkey, Error::<T>::InvalidChild);
        }
        // Ensure that the sum of the proportions does not exceed u64::MAX.
        let _total_proportion: u64 = children
            .iter()
            .try_fold(0u64, |acc, &(proportion, _)| acc.checked_add(proportion))
            .ok_or(Error::<T>::ProportionOverflow)?;

        // Ensure there are no duplicates in the list of children.
        let mut unique_children = Vec::new();
        for (_, child_i) in &children {
            ensure!(
                !unique_children.contains(child_i),
                Error::<T>::DuplicateChild
            );
            unique_children.push(child_i.clone());
        }

        // Check that the parent key has at least the minimum own stake
        // (checking with check_weights_min_stake wouldn't work because it considers
        // grandparent stake in this case)
        ensure!(
            Self::get_total_stake_for_hotkey(&hotkey) >= StakeThreshold::<T>::get(),
            Error::<T>::NotEnoughStakeToSetChildkeys
        );

        // Calculate cool-down block
        let cooldown_block =
            Self::get_current_block_as_u64().saturating_add(DefaultPendingCooldown::<T>::get());

        // Insert or update PendingChildKeys
        PendingChildKeys::<T>::insert(netuid, hotkey.clone(), (children.clone(), cooldown_block));

        // --- 8. Log and return.
        log::trace!(
            "SetChildrenScheduled( netuid:{:?}, cooldown_block:{:?}, hotkey:{:?}, children:{:?} )",
            cooldown_block,
            hotkey,
            netuid,
            children.clone()
        );
        Self::deposit_event(Event::SetChildrenScheduled(
            hotkey.clone(),
            netuid,
            cooldown_block,
            children.clone(),
        ));

        // Ok and return.
        Ok(())
    }

    /// This function executes setting children keys when called during hotkey draining.
    ///
    /// * `netuid` (u16):
    ///     - The u16 network identifier where the child keys will exist.
    ///
    /// # Events:
    /// * `SetChildren`:
    ///     - On successfully registering children to a hotkey.
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
    /// # Detailed Explanation of actions:
    /// 1. **Old Children Cleanup**: Removes the hotkey from the parent list of its old children.
    /// 2. **New Children Assignment**: Assigns the new child to the hotkey and updates the parent list for the new child.
    ///
    pub fn do_set_pending_children(netuid: u16) {
        let current_block = Self::get_current_block_as_u64();

        // Iterate over all pending children of this subnet and set as needed
        PendingChildKeys::<T>::iter_prefix(netuid).for_each(
            |(hotkey, (children, cool_down_block))| {
                if cool_down_block < current_block {
                    // Erase myself from old children's parents.
                    let old_children: Vec<(u64, T::AccountId)> =
                        ChildKeys::<T>::get(hotkey.clone(), netuid);

                    // Iterate over all my old children and remove myself from their parent's map.
                    for (_, old_child_i) in old_children.clone().iter() {
                        // Get the old child's parents on this network.
                        let my_old_child_parents: Vec<(u64, T::AccountId)> =
                            ParentKeys::<T>::get(old_child_i.clone(), netuid);

                        // Filter my hotkey from my old children's parents list.
                        let filtered_parents: Vec<(u64, T::AccountId)> = my_old_child_parents
                            .into_iter()
                            .filter(|(_, parent)| *parent != hotkey)
                            .collect();

                        // Update the parent list in storage
                        ParentKeys::<T>::insert(old_child_i, netuid, filtered_parents);
                    }

                    // Insert my new children + proportion list into the map.
                    ChildKeys::<T>::insert(hotkey.clone(), netuid, children.clone());

                    // Update the parents list for my new children.
                    for (proportion, new_child_i) in children.clone().iter() {
                        // Get the child's parents on this network.
                        let mut new_child_previous_parents: Vec<(u64, T::AccountId)> =
                            ParentKeys::<T>::get(new_child_i.clone(), netuid);

                        // Append my hotkey and proportion to my new child's parents list.
                        // NOTE: There are no duplicates possible because I previously removed my self from my old children.
                        new_child_previous_parents.push((*proportion, hotkey.clone()));

                        // Update the parents list in storage.
                        ParentKeys::<T>::insert(
                            new_child_i.clone(),
                            netuid,
                            new_child_previous_parents,
                        );
                    }

                    // Log and emit event.
                    log::trace!(
                        "SetChildren( netuid:{:?}, hotkey:{:?}, children:{:?} )",
                        hotkey,
                        netuid,
                        children.clone()
                    );
                    Self::deposit_event(Event::SetChildren(
                        hotkey.clone(),
                        netuid,
                        children.clone(),
                    ));

                    // Remove pending children
                    PendingChildKeys::<T>::remove(netuid, hotkey);
                }
            },
        );
    }

    /// The implementation for the extrinsic set_childkey_whitelist: Sets the white list of coldkeys
    /// that a childkey can have as parents.
    ///
    /// Sets the value of ChildkeyWhitelist and performs a few checks:
    ///    **Signature Verification**: Ensures that the caller has signed the transaction, verifying the coldkey.
    ///    **Root Network Check**: Ensures netuid is not the root network, as child hotkeys are not valid on the root.
    ///    **Network Existence Check**: Ensures that the specified network exists.
    ///    **Ownership Verification**: Ensures that the coldkey owns the hotkey.
    ///    **Hotkey Account Existence Check**: Ensures that the hotkey account already exists.
    ///    **Coldkey count**: Only allow to add up to 5 entries in the white list
    ///
    /// # Events:
    /// * `ChildkeyWhitelistSet`:
    ///     - If all checks pass and the white list is set.
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
    /// * `TooManyWhitelistEntries`:
    ///     - Too many white list entries
    ///
    pub fn do_set_childkey_whitelist(
        coldkey: T::RuntimeOrigin,
        childkey: T::AccountId,
        netuid: u16,
        coldkey_list: Vec<T::AccountId>,
    ) -> DispatchResult {
        // Check that the caller has signed the transaction. (the coldkey of the pairing)
        let coldkey = ensure_signed(coldkey)?;
        log::trace!(
            "do_set_childkey_whitelist( coldkey:{:?} childkey:{:?} netuid:{:?} coldkey_list:{:?} )",
            coldkey,
            childkey,
            netuid,
            coldkey_list,
        );

        // Ensures netuid is not the root network. Child hotkeys are not valid on root.
        ensure!(
            netuid != Self::get_root_netuid(),
            Error::<T>::RegistrationNotPermittedOnRootSubnet
        );

        // Check that the specified network exists.
        ensure!(
            Self::if_subnet_exist(netuid),
            Error::<T>::SubNetworkDoesNotExist
        );

        // Check that the coldkey owns the childkey.
        ensure!(
            Self::coldkey_owns_hotkey(&coldkey, &childkey),
            Error::<T>::NonAssociatedColdKey
        );

        // Ensure that the white list length does not exceed 5.
        ensure!(coldkey_list.len() <= 5, Error::<T>::TooManyWhitelistEntries);

        // Insert or update ChildkeyWhitelist
        ChildkeyWhitelist::<T>::insert(childkey.clone(), netuid, coldkey_list.clone());

        // --- 8. Log and return.
        log::trace!(
            "ChildkeyWhitelistSet( childkey:{:?}, netuid:{:?}, coldkey_list:{:?} )",
            childkey,
            netuid,
            coldkey_list.clone()
        );
        Self::deposit_event(Event::ChildkeyWhitelistSet(
            childkey.clone(),
            netuid,
            coldkey_list,
        ));

        // Ok and return.
        Ok(())
    }

    /* Retrieves the list of children for a given hotkey and network.
    ///
    /// # Arguments
    /// * `hotkey` - The hotkey whose children are to be retrieved.
    /// * `netuid` - The network identifier.
    ///
    /// # Returns
    /// * `Vec<(u64, T::AccountId)>` - A vector of tuples containing the proportion and child account ID.
    ///
    /// # Example
    /// ```
    /// let children = SubtensorModule::get_children(&hotkey, netuid);
     */
    pub fn get_children(hotkey: &T::AccountId, netuid: u16) -> Vec<(u64, T::AccountId)> {
        ChildKeys::<T>::get(hotkey, netuid)
    }

    /* Retrieves the list of parents for a given child and network.
    ///
    /// # Arguments
    /// * `child` - The child whose parents are to be retrieved.
    /// * `netuid` - The network identifier.
    ///
    /// # Returns
    /// * `Vec<(u64, T::AccountId)>` - A vector of tuples containing the proportion and parent account ID.
    ///
    /// # Example
    /// ```
    /// let parents = SubtensorModule::get_parents(&child, netuid);
     */
    pub fn get_parents(child: &T::AccountId, netuid: u16) -> Vec<(u64, T::AccountId)> {
        ParentKeys::<T>::get(child, netuid)
    }

    /// Sets the childkey take for a given hotkey.
    ///
    /// This function allows a coldkey to set the childkey take for a given hotkey.
    /// The childkey take determines the proportion of stake that the hotkey keeps for itself
    /// when distributing stake to its children.
    ///
    /// # Arguments:
    /// * `coldkey` (T::AccountId):
    ///     - The coldkey that owns the hotkey.
    ///
    /// * `hotkey` (T::AccountId):
    ///     - The hotkey for which the childkey take will be set.
    ///
    /// * `take` (u16):
    ///     - The new childkey take value. This is a percentage represented as a value between 0 and 10000,
    ///       where 10000 represents 100%.
    ///
    /// # Returns:
    /// * `DispatchResult` - The result of the operation.
    ///
    /// # Errors:
    /// * `NonAssociatedColdKey`:
    ///     - The coldkey does not own the hotkey.
    /// * `InvalidChildkeyTake`:
    ///     - The provided take value is invalid (greater than the maximum allowed take).
    /// * `TxChildkeyTakeRateLimitExceeded`:
    ///     - The rate limit for changing childkey take has been exceeded.
    pub fn do_set_childkey_take(
        coldkey: T::AccountId,
        hotkey: T::AccountId,
        netuid: u16,
        take: u16,
    ) -> DispatchResult {
        // Ensure the coldkey owns the hotkey
        ensure!(
            Self::coldkey_owns_hotkey(&coldkey, &hotkey),
            Error::<T>::NonAssociatedColdKey
        );

        // Ensure the take value is valid
        ensure!(
            take <= Self::get_max_childkey_take(),
            Error::<T>::InvalidChildkeyTake
        );

        // Ensure the hotkey passes the rate limit.
        ensure!(
            Self::passes_rate_limit_on_subnet(
                &TransactionType::SetChildkeyTake, // Set childkey take.
                &hotkey,                           // Specific to a hotkey.
                netuid,                            // Specific to a subnet.
            ),
            Error::<T>::TxChildkeyTakeRateLimitExceeded
        );

        // Set last transaction block
        let current_block = Self::get_current_block_as_u64();
        Self::set_last_transaction_block_on_subnet(
            &hotkey,
            netuid,
            &TransactionType::SetChildkeyTake,
            current_block,
        );

        // Set the new childkey take value for the given hotkey and network
        ChildkeyTake::<T>::insert(hotkey.clone(), netuid, take);

        // Update the last transaction block
        Self::set_last_transaction_block_on_subnet(
            &hotkey,
            netuid,
            &TransactionType::SetChildkeyTake,
            current_block,
        );

        // Emit the event
        Self::deposit_event(Event::ChildKeyTakeSet(hotkey.clone(), take));
        log::debug!(
            "Childkey take set for hotkey: {:?} and take: {:?}",
            hotkey,
            take
        );
        Ok(())
    }

    /// Gets the childkey take for a given hotkey.
    ///
    /// This function retrieves the current childkey take value for a specified hotkey.
    /// If no specific take value has been set, it returns the default childkey take.
    ///
    /// # Arguments:
    /// * `hotkey` (&T::AccountId):
    ///     - The hotkey for which to retrieve the childkey take.
    ///
    /// # Returns:
    /// * `u16` - The childkey take value. This is a percentage represented as a value between 0 and 10000,
    ///           where 10000 represents 100%.
    pub fn get_childkey_take(hotkey: &T::AccountId, netuid: u16) -> u16 {
        ChildkeyTake::<T>::get(hotkey, netuid)
    }
}
