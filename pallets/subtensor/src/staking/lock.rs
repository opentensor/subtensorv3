use super::*;
use crate::epoch::math::*;
use alloc::collections::BTreeMap;
use substrate_fixed::types::I96F32;

impl<T: Config> Pallet<T> {
    /// Sets the lock interval in blocks.
    ///
    /// This function updates the minimum duration for which stakes can be locked.
    ///
    /// # Arguments
    ///
    /// * `new_interval` - The new lock interval in blocks.
    ///
    /// # Events
    ///
    /// Emits a `LockIntervalSet` event with the new interval value.
    pub fn set_lock_interval_blocks(new_interval: u64) {
        // Update the lock interval storage
        <LockIntervalBlocks<T>>::put(new_interval);

        // Emit an event for the new lock interval
        Self::deposit_event(Event::LockIntervalSet(new_interval));
    }

    /// Gets the current lock interval in blocks.
    ///
    /// This function retrieves the current value of the lock interval.
    ///
    /// # Returns
    ///
    /// * `u64` - The current lock interval in blocks.
    pub fn get_lock_interval_blocks() -> u64 {
        <LockIntervalBlocks<T>>::get()
    }

    /// Retrieves the amount of locked stake for a specific hotkey and coldkey pair on a given subnet.
    ///
    /// This function queries the `Locks` storage to get the locked stake amount for the specified
    /// hotkey and coldkey combination on the given subnet.
    ///
    /// # Arguments
    ///
    /// * `hotkey` - The hotkey account ID.
    /// * `coldkey` - The coldkey account ID.
    /// * `netuid` - The subnet ID.
    ///
    /// # Returns
    ///
    /// * `u64` - The amount of locked stake. Returns the first element of the lock tuple,
    ///           which represents the locked amount.
    pub fn get_locked_for_hotkey_and_coldkey_on_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
    ) -> u64 {
        Locks::<T>::get((netuid, hotkey.clone(), coldkey.clone())).0
    }

    /// Calculates the conviction score for a specific hotkey and coldkey pair on a given subnet.
    ///
    /// This function retrieves the locked stake amount from the `Locks` storage and calculates
    /// the conviction score based on the locked amount and the lock duration.
    ///
    /// # Arguments
    ///
    /// * `hotkey` - The hotkey account ID.
    /// * `coldkey` - The coldkey account ID.
    /// * `netuid` - The subnet ID.
    ///
    /// # Returns
    ///
    /// * `u64` - The conviction score calculated from the locked stake.
    pub fn get_conviction_for_hotkey_and_coldkey_on_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
    ) -> u64 {
        let (locked, _, end) = Locks::<T>::get((netuid, hotkey.clone(), coldkey.clone()));

        Self::calculate_conviction(locked, end, Self::get_current_block_as_u64())
    }

    /// Locks a specified amount of stake for a given duration on a subnet.
    ///
    /// This function allows a user to lock their stake, increasing their conviction score.
    /// The locked stake cannot be withdrawn until the lock period expires, and the new lock
    /// must not decrease the current conviction score.
    ///
    /// # Arguments
    ///
    /// * `origin` - The origin of the call, must be signed by the coldkey.
    /// * `hotkey` - The hotkey associated with the stake to be locked.
    /// * `netuid` - The ID of the subnet where the stake is locked.
    /// * `duration` - The duration (in blocks) for which the stake will be locked.
    /// * `alpha_locked` - The amount of stake to be locked.
    ///
    /// # Returns
    ///
    /// * `DispatchResult` - The result of the lock operation.
    ///
    /// # Errors
    ///
    /// * `SubnetNotExists` - If the specified subnet does not exist.
    /// * `HotKeyAccountNotExists` - If the hotkey account does not exist.
    /// * `HotKeyNotRegisteredInSubNet` - If the hotkey is not registered on the specified subnet.
    /// * `NotEnoughStakeToWithdraw` - If the user doesn't have enough stake to lock, or if the new lock would decrease the current conviction.
    ///
    /// # Events
    ///
    /// * `LockIncreased` - Emitted when the lock is successfully increased.
    ///
    /// # TODO
    ///
    /// * Consider implementing a maximum lock duration to prevent excessively long locks.
    /// * Implement a mechanism to partially unlock stakes as the lock period progresses.
    /// * Add more granular error handling for different failure scenarios.
    pub fn do_lock(
        origin: T::RuntimeOrigin,
        hotkey: T::AccountId,
        netuid: u16,
        duration: u64,
        alpha_locked: u64,
    ) -> dispatch::DispatchResult {
        // Step 1: Validate inputs and check conditions
        // Ensure the origin is valid.
        let coldkey = ensure_signed(origin)?;

        // Ensure that the subnet exists.
        ensure!(Self::if_subnet_exist(netuid), Error::<T>::SubnetNotExists);

        // Ensure that the hotkey account exists.
        ensure!(
            Self::hotkey_account_exists(&hotkey),
            Error::<T>::HotKeyAccountNotExists
        );

        // Ensure the hotkey is registered on this subnet.
        // DEPRECATED.
        // ensure!(
        //     Self::is_hotkey_registered_on_network(netuid, &hotkey),
        //     Error::<T>::HotKeyNotRegisteredInSubNet
        // );

        // Ensure the the lock is above zero.
        ensure!(alpha_locked > 0, Error::<T>::NotEnoughStakeToWithdraw);

        // Get the lockers current stake.
        let current_alpha_stake =
            Self::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);

        // Ensure that the caller has enough stake to lock.
        ensure!(
            alpha_locked <= current_alpha_stake,
            Error::<T>::NotEnoughStakeToWithdraw
        );

        // Step 2: Calculate and compare convictions
        // Get the current block.
        let current_block = Self::get_current_block_as_u64();
        let new_end_block = current_block.saturating_add(duration);

        // Check that we are not decreasing the current conviction.
        if Locks::<T>::contains_key((netuid, hotkey.clone(), coldkey.clone())) {
            // Get the current lock.
            let (current_locked, _current_start, current_end) =
                Locks::<T>::get((netuid, hotkey.clone(), coldkey.clone()));

            // Calculate the current conviction.
            let current_conviction =
                Self::calculate_conviction(current_locked, current_end, current_block);

            // Calculate the new conviction.
            let new_conviction =
                Self::calculate_conviction(alpha_locked, new_end_block, current_block);

            // Ensure the new lock does not decrease the current conviction
            ensure!(
                new_conviction >= current_conviction,
                Error::<T>::NotEnoughStakeToWithdraw
            );
        }

        // Step 3: Set the new lock
        Locks::<T>::insert(
            (netuid, hotkey.clone(), coldkey.clone()),
            (
                alpha_locked,
                current_block,
                current_block.saturating_add(duration),
            ),
        );

        // Step 4: Emit event and return
        // Lock increased event.
        log::info!(
            "LockIncreased( coldkey:{:?}, hotkey:{:?}, netuid:{:?}, alpha_locked:{:?} )",
            coldkey.clone(),
            hotkey.clone(),
            netuid,
            alpha_locked
        );
        Self::deposit_event(Event::LockIncreased(
            coldkey.clone(),
            hotkey.clone(),
            netuid,
            alpha_locked,
        ));

        // Ok and return.
        Ok(())
    }

    /// Calculates the "lion's share" distribution for a given set of convictions.
    ///
    /// This function applies an exponential function to create a sharp drop-off in the
    /// distribution, favoring higher conviction scores more heavily.
    ///
    /// # Arguments
    ///
    /// * `convictions` - A vector of conviction scores.
    /// * `sharpness` - The sharpness parameter for the exponential function (default: 20).
    ///
    /// # Returns
    ///
    /// * `Vec<I96F32>` - A vector of shares, represented as fixed-point numbers with 96 integer bits and 32 fractional bits.
    pub fn calculate_lions_share(convictions: Vec<u64>, sharpness: u32) -> Vec<I96F32> {
        // Handle empty convictions vector
        if convictions.is_empty() {
            return Vec::new();
        }

        // For a single conviction, return a vector with a single element of value 1
        if convictions.len() == 1 {
            return vec![I96F32::from_num(1)];
        }

        // Find the maximum conviction
        let max_conviction = convictions.iter().max().cloned().unwrap_or(1);
        // If the maximum conviction is zero, return a vector of zeros
        if max_conviction == 0 {
            return vec![I96F32::from_num(0); convictions.len()];
        }

        // Normalize convictions and apply exponential function
        let mut powered_convictions: Vec<I96F32> = Vec::with_capacity(convictions.len());
        for c in convictions.iter() {
            let normalized = I96F32::from_num(*c).saturating_div(I96F32::from_num(max_conviction));
            // Use checked_mul to prevent overflow in exponentiation
            let powered = exp_safe_f96(
                I96F32::from_num(sharpness)
                    .saturating_mul(normalized.saturating_sub(I96F32::from_num(1))),
            );
            powered_convictions.push(powered);
        }

        // Calculate total powered conviction
        let total_powered: I96F32 = powered_convictions.iter().sum();

        // Handle case where total_powered is zero to avoid division by zero
        if total_powered == I96F32::from_num(0) {
            return vec![I96F32::from_num(0); convictions.len()];
        }

        // Calculate shares
        let shares: Vec<I96F32> = powered_convictions
            .into_iter()
            .map(|pc| pc.saturating_div(total_powered))
            .collect();

        shares
    }

    /// Distributes the owner payment among hotkeys based on their conviction scores.
    ///
    /// This function calculates the conviction scores for all locked hotkeys in a subnet,
    /// and then distributes the payment proportionally based on these scores.
    ///
    /// # Arguments
    ///
    /// * `netuid` - The network ID of the subnet.
    /// * `amount` - The total amount of payment to distribute.
    ///
    /// # Effects
    ///
    /// * Calculates conviction scores for all locked hotkeys in the subnet.
    /// * Distributes the payment proportionally based on conviction scores.
    /// * Adds the distributed share to each hotkey's balance.
    /// * Emits an `OwnerPaymentDistributed` event for each distribution.
    pub fn distribute_owner_cut(netuid: u16, amount: u64) -> u64 {
        // Get the current block number
        let current_block = Self::get_current_block_as_u64();

        // Initialize variables to track total conviction and individual hotkey convictions
        let mut total_conviction: u64 = 0;
        let mut hotkey_convictions: BTreeMap<T::AccountId, u64> = BTreeMap::new();

        // Calculate total conviction and individual hotkey convictions
        for ((iter_netuid, hotkey, _), (lock_amount, _, end_block)) in Locks::<T>::iter() {
            if iter_netuid != netuid {
                continue;
            }
            // Calculate conviction for each lock
            let conviction = Self::calculate_conviction(lock_amount, end_block, current_block);
            // Add conviction to the hotkey's total
            hotkey_convictions
                .entry(hotkey.clone())
                .and_modify(|e| *e = e.saturating_add(conviction))
                .or_insert(conviction);

            // Add to the total conviction
            total_conviction = total_conviction.saturating_add(conviction);
        }

        // If there's no conviction, return the full amount
        if total_conviction == 0 {
            log::debug!("No conviction, returning full amount: {}", amount);
            return amount;
        }

        // Convert convictions to a vector for the lion's share calculation
        let convictions: Vec<u64> = hotkey_convictions.values().cloned().collect();

        // Set the total subnet Conviction.
        let largest_conviction = convictions.iter().max().cloned().unwrap_or(1);
        SubnetLocked::<T>::insert(netuid, total_conviction);
        LargestLocked::<T>::insert(netuid, largest_conviction);

        // Calculate shares using the lion's share distribution
        let shares: Vec<I96F32> = Self::calculate_lions_share(convictions, 20);

        // Initialize variable to track remaining amount to distribute
        let mut remaining_amount = amount;

        // Distribute the owner cut based on calculated shares
        for ((hotkey, _conviction), share) in hotkey_convictions.iter().zip(shares.iter()) {
            // Calculate the share for this hotkey
            let share_amount = I96F32::from_num(amount)
                .checked_mul(*share)
                .unwrap_or(I96F32::from_num(0))
                .to_num::<u64>();

            // Get the coldkey associated with this hotkey
            let owner_coldkey = Self::get_owning_coldkey_for_hotkey(hotkey);

            // Emit the calculated share into the subnet for this hotkey
            Self::emit_into_subnet(hotkey, &owner_coldkey, netuid, share_amount);
            Self::increase_lock_by_amount(netuid, hotkey, &owner_coldkey, share_amount);

            // Subtract the distributed share from the remaining amount
            remaining_amount = remaining_amount.saturating_sub(share_amount);
        }

        // Return any undistributed amount
        remaining_amount
    }

    /// Increases the lock amount for a given hotkey and coldkey in the specified subnet.
    ///
    /// # Arguments
    /// * `netuid` - The network ID of the subnet.
    /// * `hotkey` - The account ID of the hotkey.
    /// * `coldkey` - The account ID of the coldkey.
    /// * `amount` - The amount to increase the lock by.
    ///
    /// # Effects
    /// - If a lock already exists for the hotkey and coldkey, it increases the lock amount.
    /// - If no lock exists, it creates a new lock with the specified amount.
    pub fn increase_lock_by_amount(
        netuid: u16,
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        amount: u64,
    ) {
        // Check if the lock exists for the given hotkey and coldkey
        let current_block = Self::get_current_block_as_u64();
        if Locks::<T>::contains_key((netuid, hotkey.clone(), coldkey.clone())) {
            // Retrieve the current lock details
            let (current_lock, start_block, end_block) =
                Locks::<T>::get((netuid, hotkey.clone(), coldkey.clone()));
            // Calculate the new lock amount by adding the specified amount
            let new_lock = current_lock.saturating_add(amount);
            // Update the lock with the new amount
            Locks::<T>::insert(
                (netuid, hotkey.clone(), coldkey.clone()),
                (new_lock, start_block, end_block),
            );
        } else {
            // If the lock does not exist, create a new lock with the specified amount
            Locks::<T>::insert(
                (netuid, hotkey.clone(), coldkey.clone()),
                (
                    amount,
                    current_block,
                    current_block.saturating_add(Self::get_lock_interval_blocks()),
                ),
            );
        }
    }

    /// Updates the owners of all subnets periodically.
    ///
    /// This function checks if it's time to update subnet owners based on the current block number
    /// and a predefined update interval. If the condition is met, it iterates through all subnet
    /// network IDs and calls the `update_subnet_owner` function for each subnet.
    ///
    /// # Details
    /// - The update interval is set to 7200 * 15 blocks (approximately 15 days, assuming 7200 blocks per day).
    /// - The update is triggered every two intervals (30 days) when the current block number is divisible by twice the update interval.
    ///
    /// # Effects
    /// - When the update condition is met, it calls `update_subnet_owner` for each subnet,
    ///   potentially changing the owner of each subnet based on conviction scores.
    pub fn update_all_subnet_owners() {
        let current_block = Self::get_current_block_as_u64();
        let update_interval: u64 = 7200 * 15; // Approx 15 days.
        if current_block.rem_euclid(update_interval.saturating_mul(2_u64)) == 0 {
            for netuid in Self::get_all_subnet_netuids() {
                Self::update_subnet_owner(netuid);
            }
        }
    }

    /// Determines the subnet owner based on the highest conviction score.
    ///
    /// This function calculates the conviction score for each hotkey in the subnet,
    /// considering the lock amount and duration. The hotkey with the highest total
    /// conviction score becomes the subnet owner.
    ///
    /// # Arguments
    /// * `netuid` - The network ID of the subnet
    ///
    /// # Effects
    /// * Updates the SubnetOwner storage item with the coldkey of the highest conviction hotkey
    pub fn update_subnet_owner(netuid: u16) {
        let mut max_total_conviction: I96F32 = I96F32::from_num(0.0);
        let mut max_conviction_hotkey = None;
        let mut hotkey_convictions = BTreeMap::new();
        let mut total_conviction_across_all_hotkeys: I96F32 = I96F32::from_num(0.0);
        let current_block = Self::get_current_block_as_u64();

        // Iterate through all locks in the subnet
        for ((iter_netuid, iter_hotkey, _), (lock_amount, _, end_block)) in Locks::<T>::iter() {
            // Skip if the subnet does not match.
            if iter_netuid != netuid {
                continue;
            }

            // Calculate conviction score based on lock amount and duration
            let conviction_score = I96F32::from_num(Self::calculate_conviction(
                lock_amount,
                end_block,
                current_block,
            ));

            // Accumulate conviction scores for each hotkey
            let total_conviction = hotkey_convictions
                .entry(iter_hotkey.clone())
                .or_insert(I96F32::from_num(0));
            *total_conviction = total_conviction.saturating_add(conviction_score);

            // Increment the total.
            total_conviction_across_all_hotkeys =
                total_conviction_across_all_hotkeys.saturating_add(conviction_score);

            // Update max conviction if current hotkey has higher total conviction
            if *total_conviction > max_total_conviction {
                max_total_conviction = *total_conviction;
                max_conviction_hotkey = Some(iter_hotkey.clone());
            }
        }

        // Handle the case where no locks exist for the subnet
        if hotkey_convictions.is_empty() {
            log::warn!("No locks found for subnet {}", netuid);
            return;
        }

        // Implement a minimum conviction threshold for becoming a subnet owner
        let min_conviction_threshold = I96F32::from_num(1000); // Example threshold, adjust as needed
        if max_total_conviction < min_conviction_threshold {
            return;
        }

        // Set the subnet owner to the coldkey of the hotkey with highest conviction
        if let Some(hotkey) = max_conviction_hotkey {
            let owning_coldkey = Self::get_owning_coldkey_for_hotkey(&hotkey);
            SubnetOwner::<T>::insert(netuid, owning_coldkey.clone());
        }

        // Set the total subnet Conviction.
        let largest_conviction: I96F32 = hotkey_convictions
            .values()
            .cloned()
            .max()
            .unwrap_or(I96F32::from_num(1));
        SubnetLocked::<T>::insert(netuid, total_conviction_across_all_hotkeys.to_num::<u64>());
        LargestLocked::<T>::insert(netuid, largest_conviction.to_num::<u64>());

        // Implement a tie-breaking mechanism for equal conviction scores
        let tied_hotkeys: Vec<_> = hotkey_convictions
            .iter()
            .filter(|(_, &conviction)| conviction == max_total_conviction)
            .collect();

        if tied_hotkeys.len() > 1 {
            // Use a deterministic method to break ties, e.g., lowest hotkey value
            if let Some((winning_hotkey, _)) = tied_hotkeys.iter().min_by_key(|(hotkey, _)| hotkey)
            {
                let owning_coldkey = Self::get_owning_coldkey_for_hotkey(winning_hotkey);
                SubnetOwner::<T>::insert(netuid, owning_coldkey.clone());
            }
        }

        // Log performance metrics for large subnets
        if hotkey_convictions.len() > 1000 {
            log::warn!(
                "Large subnet {} processed with {} hotkeys",
                netuid,
                hotkey_convictions.len()
            );
        }
    }

    /// Calculates the conviction score for a locked stake.
    ///
    /// This function computes a conviction score based on the amount of locked stake and the
    /// remaining lock duration. The score increases with both the lock amount and duration,
    /// but with diminishing returns for longer lock periods.
    ///
    /// # Arguments
    ///
    /// * `lock_amount` - The amount of stake locked, as a u64.
    /// * `end_block` - The block number when the lock expires, as a u64.
    /// * `current_block` - The current block number, as a u64.
    ///
    /// # Returns
    ///
    /// * A u64 representing the calculated conviction score.
    ///
    /// # Formula
    ///
    /// The conviction score is calculated using the following formula:
    /// score = lock_amount * (1 - e^(-lock_duration / (365 * 24 * 60 * 60)))
    ///
    /// Where:
    /// - lock_duration is in blocks
    /// - The denominator converts days to blocks (assuming 1 block per second)
    /// - e is the mathematical constant (base of natural logarithm)
    pub fn calculate_conviction(lock_amount: u64, end_block: u64, current_block: u64) -> u64 {
        let lock_duration = end_block.saturating_sub(current_block);
        let lock_interval_blocks = Self::get_lock_interval_blocks();
        let time_factor = I96F32::from_num(0).saturating_sub(
            I96F32::from_num(lock_duration).saturating_div(I96F32::from_num(lock_interval_blocks)),
        );
        let exp_term =
            I96F32::from_num(1).saturating_sub(exp_safe_f96(I96F32::from_num(time_factor)));
        let conviction_score = I96F32::from_num(lock_amount).saturating_mul(exp_term);

        conviction_score.to_num::<u64>()
    }

    /// Calculates the maximum amount of stake that can be unlocked for a given neuron.
    ///
    /// This function determines the maximum amount of stake that can be unlocked based on
    /// the current lock status and conviction of the stake. If there's no lock, the entire
    /// stake can be unlocked.
    ///
    /// # Arguments
    ///
    /// * `netuid` - The network UID.
    /// * `hotkey` - The hotkey of the neuron.
    /// * `coldkey` - The coldkey associated with the neuron.
    ///
    /// # Returns
    ///
    /// * `u64` - The maximum amount of stake that can be unlocked.
    pub fn max_unlockable_stake(netuid: u16, hotkey: &T::AccountId, coldkey: &T::AccountId) -> u64 {
        let current_block = Self::get_current_block_as_u64();
        let total_stake: u64 =
            Self::get_stake_for_hotkey_and_coldkey_on_subnet(hotkey, coldkey, netuid);

        if Locks::<T>::contains_key((netuid, hotkey, coldkey)) {
            let (alpha_locked, _, end_block) = Locks::<T>::get((netuid, hotkey, coldkey));
            let conviction = Self::calculate_conviction(alpha_locked, end_block, current_block);
            total_stake.saturating_sub(conviction)
        } else {
            total_stake
        }
    }
}
