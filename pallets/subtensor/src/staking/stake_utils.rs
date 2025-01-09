use super::*;
use share_pool::{SharePool, SharePoolDataOperations};
use sp_std::ops::Neg;
use substrate_fixed::types::{I64F64, I96F32, U64F64};

impl<T: Config> Pallet<T> {
    /// Retrieves the total alpha issuance for a given subnet.
    ///
    /// This function calculates the total alpha issuance by summing the alpha
    /// values from `SubnetAlphaIn` and `SubnetAlphaOut` for the specified subnet.
    ///
    /// # Arguments
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `u64` - The total alpha issuance for the specified subnet.
    pub fn get_alpha_issuance(netuid: u16) -> u64 {
        SubnetAlphaIn::<T>::get(netuid).saturating_add(SubnetAlphaOut::<T>::get(netuid))
    }

    /// Calculates the price of alpha for a given subnet.
    ///
    /// This function determines the price of alpha by dividing the total TAO
    /// reserves by the total alpha reserves (`SubnetAlphaIn`) for the specified subnet.
    /// If the alpha reserves are zero, the function returns zero to avoid division by zero.
    ///
    /// # Arguments
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `I96F32` - The price of alpha for the specified subnet.
    pub fn get_alpha_price(netuid: u16) -> I96F32 {
        if netuid == Self::get_root_netuid() {
            return I96F32::from_num(1.0); // Root.
        }
        if SubnetMechanism::<T>::get(netuid) == 0 {
            return I96F32::from_num(1.0); // Stable
        }
        if SubnetAlphaIn::<T>::get(netuid) == 0 {
            I96F32::from_num(0)
        } else {
            I96F32::from_num(SubnetTAO::<T>::get(netuid))
                .checked_div(I96F32::from_num(SubnetAlphaIn::<T>::get(netuid)))
                .unwrap_or(I96F32::from_num(0))
        }
    }

    /// Retrieves the global global weight as a normalized value between 0 and 1.
    ///
    /// This function performs the following steps:
    /// 1. Fetches the global weight from storage using the TaoWeight storage item.
    /// 2. Converts the retrieved u64 value to a fixed-point number (I96F32).
    /// 3. Normalizes the weight by dividing it by the maximum possible u64 value.
    /// 4. Returns the normalized weight as an I96F32 fixed-point number.
    ///
    /// The normalization ensures that the returned value is always between 0 and 1,
    /// regardless of the actual stored weight value.
    ///
    /// # Returns
    /// * `I96F32` - The normalized global global weight as a fixed-point number between 0 and 1.
    ///
    /// # Note
    /// This function uses saturating division to prevent potential overflow errors.
    pub fn get_tao_weight() -> I96F32 {
        // Step 1: Fetch the global weight from storage
        let stored_weight = TaoWeight::<T>::get();

        // Step 2: Convert the u64 weight to I96F32
        let weight_fixed = I96F32::from_num(stored_weight);

        // Step 3: Normalize the weight by dividing by u64::MAX
        // This ensures the result is always between 0 and 1
        weight_fixed.saturating_div(I96F32::from_num(u64::MAX))
    }

    /// Sets the global global weight in storage.
    ///
    /// This function performs the following steps:
    /// 1. Takes the provided weight value as a u64.
    /// 2. Updates the TaoWeight storage item with the new value.
    ///
    /// # Arguments
    /// * `weight` - The new global weight value to be set, as a u64.
    ///
    /// # Effects
    /// This function modifies the following storage item:
    /// - `TaoWeight`: Updates it with the new weight value.
    ///
    /// # Note
    /// The weight is stored as a raw u64 value. To get the normalized weight between 0 and 1,
    /// use the `get_tao_weight()` function.
    pub fn set_tao_weight(weight: u64) {
        // Update the TaoWeight storage with the new weight value
        TaoWeight::<T>::set(weight);
    }

    /// Calculates the weighted combination of alpha and global tao for a single hotkey onet a subnet.
    ///
    pub fn get_stake_weights_for_hotkey_on_subnet(
        hotkey: &T::AccountId,
        netuid: u16,
    ) -> (I64F64, I64F64, I64F64) {
        // Retrieve the global tao weight.
        let tao_weight = I64F64::from_num(Self::get_tao_weight());
        log::debug!("tao_weight: {:?}", tao_weight);

        // Step 1: Get stake of hotkey (neuron)
        let alpha_stake =
            I64F64::from_num(Self::get_inherited_for_hotkey_on_subnet(hotkey, netuid));
        log::trace!("alpha_stake: {:?}", alpha_stake);

        // Step 2: Get the global tao stake for the hotkey
        let tao_stake = I64F64::from_num(Self::get_inherited_for_hotkey_on_subnet(hotkey, 0));
        log::trace!("tao_stake: {:?}", tao_stake);

        // Step 3: Combine alpha and tao stakes
        let total_stake = alpha_stake.saturating_add(tao_stake.saturating_mul(tao_weight));
        log::trace!("total_stake: {:?}", total_stake);

        (total_stake, alpha_stake, tao_stake)
    }

    /// Calculates the weighted combination of alpha and global tao for hotkeys on a subnet.
    ///
    pub fn get_stake_weights_for_network(netuid: u16) -> (Vec<I64F64>, Vec<I64F64>, Vec<I64F64>) {
        // Retrieve the global tao weight.
        let tao_weight: I64F64 = I64F64::from_num(Self::get_tao_weight());
        log::debug!("tao_weight: {:?}", tao_weight);

        // Step 1: Get subnetwork size
        let n: u16 = Self::get_subnetwork_n(netuid);

        // Step 2: Get stake of all hotkeys (neurons) ordered by uid
        let alpha_stake: Vec<I64F64> = (0..n)
            .map(|uid| {
                if Keys::<T>::contains_key(netuid, uid) {
                    let hotkey: T::AccountId = Keys::<T>::get(netuid, uid);
                    I64F64::from_num(Self::get_inherited_for_hotkey_on_subnet(&hotkey, netuid))
                } else {
                    I64F64::from_num(0)
                }
            })
            .collect();
        log::trace!("alpha_stake: {:?}", alpha_stake);

        // Step 3: Calculate the global tao stake vector.
        // Initialize a vector to store global tao stakes for each neuron.
        let tao_stake: Vec<I64F64> = (0..n)
            .map(|uid| {
                if Keys::<T>::contains_key(netuid, uid) {
                    let hotkey: T::AccountId = Keys::<T>::get(netuid, uid);
                    I64F64::from_num(Self::get_inherited_for_hotkey_on_subnet(&hotkey, 0))
                } else {
                    I64F64::from_num(0)
                }
            })
            .collect();
        log::trace!("tao_stake: {:?}", tao_stake);

        // Step 4: Combine alpha and root tao stakes.
        // Calculate the weighted average of alpha and global tao stakes for each neuron.
        let total_stake: Vec<I64F64> = alpha_stake
            .iter()
            .zip(tao_stake.iter())
            .map(|(alpha_i, tao_i)| alpha_i.saturating_add(tao_i.saturating_mul(tao_weight)))
            .collect();
        log::trace!("total_stake: {:?}", total_stake);

        (total_stake, alpha_stake, tao_stake)
    }

    /// Calculates the total inherited stake (alpha) held by a hotkey on a network, considering child/parent relationships.
    ///
    /// This function performs the following steps:
    /// 1. Retrieves the initial alpha (stake) for the hotkey on the specified subnet.
    /// 2. Retrieves the list of children and parents for the hotkey on the subnet.
    /// 3. Calculates the alpha allocated to children:
    ///    a. For each child, computes the proportion of alpha to be allocated.
    ///    b. Accumulates the total alpha allocated to all children.
    /// 4. Calculates the alpha received from parents:
    ///    a. For each parent, retrieves the parent's stake on the subnet.
    ///    b. Computes the proportion of the parent's stake to be inherited.
    ///    c. Accumulates the total alpha inherited from all parents.
    /// 5. Computes the final inherited alpha by adjusting the initial alpha:
    ///    a. Subtracts the alpha allocated to children.
    ///    b. Adds the alpha inherited from parents.
    /// 6. Returns the final inherited alpha value.
    ///
    /// # Arguments
    /// * `hotkey` - AccountId of the hotkey whose total inherited stake is to be calculated.
    /// * `netuid` - Network unique identifier specifying the subnet context.
    ///
    /// # Returns
    /// * `u64` - The total inherited alpha for the hotkey on the subnet after considering the stakes
    ///           allocated to children and inherited from parents.
    ///
    /// # Note
    /// This function uses saturating arithmetic to prevent overflows.
    pub fn get_inherited_for_hotkey_on_subnet(hotkey: &T::AccountId, netuid: u16) -> u64 {
        // Step 1: Retrieve the initial total stake (alpha) for the hotkey on the specified subnet.
        let initial_alpha: I96F32 =
            I96F32::from_num(Self::get_stake_for_hotkey_on_subnet(hotkey, netuid));
        log::trace!(
            "Initial alpha for hotkey {:?} on subnet {}: {:?}",
            hotkey,
            netuid,
            initial_alpha
        );
        if netuid == 0 {
            return initial_alpha.to_num::<u64>();
        }

        // Initialize variables to track alpha allocated to children and inherited from parents.
        let mut alpha_to_children: I96F32 = I96F32::from_num(0);
        let mut alpha_from_parents: I96F32 = I96F32::from_num(0);

        // Step 2: Retrieve the lists of parents and children for the hotkey on the subnet.
        let parents: Vec<(u64, T::AccountId)> = Self::get_parents(hotkey, netuid);
        let children: Vec<(u64, T::AccountId)> = Self::get_children(hotkey, netuid);
        log::trace!(
            "Parents for hotkey {:?} on subnet {}: {:?}",
            hotkey,
            netuid,
            parents
        );
        log::trace!(
            "Children for hotkey {:?} on subnet {}: {:?}",
            hotkey,
            netuid,
            children
        );

        // Step 3: Calculate the total alpha allocated to children.
        for (proportion, _) in children {
            // Convert the proportion to a normalized value between 0 and 1.
            let normalized_proportion: I96F32 =
                I96F32::from_num(proportion).saturating_div(I96F32::from_num(u64::MAX));
            log::trace!(
                "Normalized proportion for child: {:?}",
                normalized_proportion
            );

            // Calculate the amount of alpha to be allocated to this child.
            let alpha_proportion_to_child: I96F32 =
                I96F32::from_num(initial_alpha).saturating_mul(normalized_proportion);
            log::trace!("Alpha proportion to child: {:?}", alpha_proportion_to_child);

            // Add this child's allocation to the total alpha allocated to children.
            alpha_to_children = alpha_to_children.saturating_add(alpha_proportion_to_child);
        }
        log::trace!("Total alpha allocated to children: {:?}", alpha_to_children);

        // Step 4: Calculate the total alpha inherited from parents.
        for (proportion, parent) in parents {
            // Retrieve the parent's total stake on this subnet.
            let parent_alpha: I96F32 =
                I96F32::from_num(Self::get_stake_for_hotkey_on_subnet(&parent, netuid));
            log::trace!(
                "Parent alpha for parent {:?} on subnet {}: {:?}",
                parent,
                netuid,
                parent_alpha
            );

            // Convert the proportion to a normalized value between 0 and 1.
            let normalized_proportion: I96F32 =
                I96F32::from_num(proportion).saturating_div(I96F32::from_num(u64::MAX));
            log::trace!(
                "Normalized proportion from parent: {:?}",
                normalized_proportion
            );

            // Calculate the amount of alpha to be inherited from this parent.
            let alpha_proportion_from_parent: I96F32 =
                I96F32::from_num(parent_alpha).saturating_mul(normalized_proportion);
            log::trace!(
                "Alpha proportion from parent: {:?}",
                alpha_proportion_from_parent
            );

            // Add this parent's contribution to the total alpha inherited from parents.
            alpha_from_parents = alpha_from_parents.saturating_add(alpha_proportion_from_parent);
        }
        log::trace!(
            "Total alpha inherited from parents: {:?}",
            alpha_from_parents
        );

        // Step 5: Calculate the final inherited alpha for the hotkey.
        let finalized_alpha: I96F32 = initial_alpha
            .saturating_sub(alpha_to_children) // Subtract alpha allocated to children
            .saturating_add(alpha_from_parents); // Add alpha inherited from parents
        log::trace!(
            "Finalized alpha for hotkey {:?} on subnet {}: {:?}",
            hotkey,
            netuid,
            finalized_alpha
        );

        // Step 6: Return the final inherited alpha value.
        finalized_alpha.to_num::<u64>()
    }

    /// Checks if a specific hotkey-coldkey pair has enough stake on a subnet to fulfill a given decrement.
    ///
    /// This function performs the following steps:
    /// 1. Retrieves the current stake for the hotkey-coldkey pair on the specified subnet.
    /// 2. Compares this stake with the requested decrement amount.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey.
    /// * `coldkey` - The account ID of the coldkey.
    /// * `netuid` - The unique identifier of the subnet.
    /// * `decrement` - The amount of stake to be potentially decremented.
    ///
    /// # Returns
    /// * `bool` - True if the account has enough stake to fulfill the decrement, false otherwise.
    ///
    /// # Note
    /// This function only checks the stake for the specific hotkey-coldkey pair, not the total stake of the hotkey or coldkey individually.
    pub fn has_enough_stake_on_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
        decrement: u64,
    ) -> bool {
        // Retrieve the current stake for this hotkey-coldkey pair on the subnet
        let current_stake =
            Self::get_stake_for_hotkey_and_coldkey_on_subnet(hotkey, coldkey, netuid);

        // Compare the current stake with the requested decrement
        // Return true if the current stake is greater than or equal to the decrement
        current_stake >= decrement
    }

    /// Retrieves the alpha (stake) value for a given hotkey and coldkey pair on a specific subnet.
    ///
    /// This function performs the following steps:
    /// 1. Takes the hotkey, coldkey, and subnet ID as input parameters.
    /// 2. Accesses the Alpha storage map to retrieve the stake value.
    /// 3. Returns the retrieved stake value as a u64.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey (neuron).
    /// * `coldkey` - The account ID of the coldkey (owner).
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `u64` - The alpha (stake) value for the specified hotkey-coldkey pair on the given subnet.
    ///
    /// # Note
    /// This function retrieves the stake specific to the hotkey-coldkey pair, not the total stake of the hotkey or coldkey individually.
    pub fn get_stake_for_hotkey_and_coldkey_on_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
    ) -> u64 {
        let alpha_share_pool = Self::get_alpha_share_pool(hotkey.clone(), netuid);
        alpha_share_pool.try_get_value(coldkey).unwrap_or(0)
    }

    /// Retrieves the total stake (alpha) for a given hotkey on a specific subnet.
    ///
    /// This function performs the following step:
    /// 1. Retrieves and returns the total alpha value associated with the hotkey on the specified subnet.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey.
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `u64` - The total alpha value for the hotkey on the specified subnet.
    ///
    /// # Note
    /// This function returns the cumulative stake across all coldkeys associated with this hotkey on the subnet.
    pub fn get_stake_for_hotkey_on_subnet(hotkey: &T::AccountId, netuid: u16) -> u64 {
        // Retrieve and return the total alpha this hotkey owns on this subnet.
        // This value represents the sum of stakes from all coldkeys associated with this hotkey.
        TotalHotkeyAlpha::<T>::get(hotkey, netuid)
    }

    /// Increase hotkey stake on a subnet.
    ///
    /// The function updates share totals given current prices.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey.
    /// * `netuid` - The unique identifier of the subnet.
    /// * `amount` - The amount of alpha to be added.
    ///
    pub fn increase_stake_for_hotkey_on_subnet(hotkey: &T::AccountId, netuid: u16, amount: u64) {
        let mut alpha_share_pool = Self::get_alpha_share_pool(hotkey.clone(), netuid);
        alpha_share_pool.update_value_for_all(amount as i64);
    }

    /// Decrease hotkey stake on a subnet.
    ///
    /// The function updates share totals given current prices.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey.
    /// * `netuid` - The unique identifier of the subnet.
    /// * `amount` - The amount of alpha to be added.
    ///
    pub fn decrease_stake_for_hotkey_on_subnet(hotkey: &T::AccountId, netuid: u16, amount: u64) {
        let mut alpha_share_pool = Self::get_alpha_share_pool(hotkey.clone(), netuid);
        alpha_share_pool.update_value_for_all((amount as i64).neg());
    }

    /// Buys shares in the hotkey on a given subnet
    ///
    /// The function updates share totals given current prices.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey.
    /// * `coldkey` - The account ID of the coldkey (owner).
    /// * `netuid` - The unique identifier of the subnet.
    /// * `amount` - The amount of alpha to be added.
    ///
    pub fn increase_stake_for_hotkey_and_coldkey_on_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
        amount: u64,
    ) {
        let mut alpha_share_pool = Self::get_alpha_share_pool(hotkey.clone(), netuid);
        alpha_share_pool.update_value_for_one(coldkey, amount as i64);
    }

    /// Sell shares in the hotkey on a given subnet
    ///
    /// The function updates share totals given current prices.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey.
    /// * `coldkey` - The account ID of the coldkey (owner).
    /// * `netuid` - The unique identifier of the subnet.
    /// * `amount` - The amount of alpha to be added.
    ///
    pub fn decrease_stake_for_hotkey_and_coldkey_on_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
        amount: u64,
    ) {
        let mut alpha_share_pool = Self::get_alpha_share_pool(hotkey.clone(), netuid);
        if let Ok(value) = alpha_share_pool.try_get_value(coldkey) {
            if value >= amount {
                alpha_share_pool.update_value_for_one(coldkey, (amount as i64).neg());
            }
        }
    }

    /// Swaps TAO for the alpha token on the subnet.
    ///
    /// Updates TaoIn, AlphaIn, and AlphaOut
    pub fn sim_swap_tao_for_alpha(netuid: u16, tao: u64) -> u64 {
        // Step 1: Get the mechanism type for the subnet (0 for Stable, 1 for Dynamic)
        let mechanism_id: u16 = SubnetMechanism::<T>::get(netuid);
        // Step 2: Initialized vars.
        let alpha: I96F32 = if mechanism_id == 1 {
            // Step 3.a.1: Dynamic mechanism calculations
            let tao_reserves: I96F32 = I96F32::from_num(SubnetTAO::<T>::get(netuid));
            let alpha_reserves: I96F32 = I96F32::from_num(SubnetAlphaIn::<T>::get(netuid));
            // Step 3.a.2: Compute constant product k = alpha * tao
            let k: I96F32 = alpha_reserves.saturating_mul(tao_reserves);
            // Step 3.a.3: Calculate alpha staked using the constant product formula
            // alpha_stake_recieved = current_alpha - (k / (current_tao + new_tao))
            alpha_reserves.saturating_sub(
                k.checked_div(tao_reserves.saturating_add(I96F32::from_num(tao)))
                    .unwrap_or(I96F32::from_num(0)),
            )
        } else {
            // Step 3.b.1: Stable mechanism, just return the value 1:1
            I96F32::from_num(tao)
        };
        // Return simulated amount.
        alpha.to_num::<u64>()
    }

    /// Swaps a subnet's Alpba token for TAO.
    ///
    /// Updates TaoIn, AlphaIn, and AlphaOut
    pub fn sim_swap_alpha_for_tao(netuid: u16, alpha: u64) -> u64 {
        // Step 1: Get the mechanism type for the subnet (0 for Stable, 1 for Dynamic)
        let mechanism_id: u16 = SubnetMechanism::<T>::get(netuid);
        // Step 2: Swap alpha and attain tao
        let tao: I96F32 = if mechanism_id == 1 {
            // Step 3.a.1: Dynamic mechanism calculations
            let tao_reserves: I96F32 = I96F32::from_num(SubnetTAO::<T>::get(netuid));
            let alpha_reserves: I96F32 = I96F32::from_num(SubnetAlphaIn::<T>::get(netuid));
            // Step 3.a.2: Compute constant product k = alpha * tao
            let k: I96F32 = alpha_reserves.saturating_mul(tao_reserves);
            // Step 3.a.3: Calculate alpha staked using the constant product formula
            // tao_recieved = tao_reserves - (k / (alpha_reserves + new_tao))
            tao_reserves.saturating_sub(
                k.checked_div(alpha_reserves.saturating_add(I96F32::from_num(alpha)))
                    .unwrap_or(I96F32::from_num(0)),
            )
        } else {
            // Step 3.b.1: Stable mechanism, just return the value 1:1
            I96F32::from_num(alpha)
        };
        tao.to_num::<u64>()
    }

    /// Swaps TAO for the alpha token on the subnet.
    ///
    /// Updates TaoIn, AlphaIn, and AlphaOut
    pub fn swap_tao_for_alpha(netuid: u16, tao: u64) -> u64 {
        // Step 1: Get the mechanism type for the subnet (0 for Stable, 1 for Dynamic)
        let mechanism_id: u16 = SubnetMechanism::<T>::get(netuid);
        // Step 2: Initialized vars.
        let alpha: I96F32 = if mechanism_id == 1 {
            // Step 3.a.1: Dynamic mechanism calculations
            let tao_reserves: I96F32 = I96F32::from_num(SubnetTAO::<T>::get(netuid));
            let alpha_reserves: I96F32 = I96F32::from_num(SubnetAlphaIn::<T>::get(netuid));
            // Step 3.a.2: Compute constant product k = alpha * tao
            let k: I96F32 = alpha_reserves.saturating_mul(tao_reserves);
            // Step 3.a.3: Calculate alpha staked using the constant product formula
            // alpha_stake_recieved = current_alpha - (k / (current_tao + new_tao))
            alpha_reserves.saturating_sub(
                k.checked_div(tao_reserves.saturating_add(I96F32::from_num(tao)))
                    .unwrap_or(I96F32::from_num(0)),
            )
        } else {
            // Step 3.b.1: Stable mechanism, just return the value 1:1
            I96F32::from_num(tao)
        };
        // Step 4. Decrease Alpha reserves.
        SubnetAlphaIn::<T>::mutate(netuid, |total| {
            *total = total.saturating_sub(alpha.to_num::<u64>());
        });
        // Step 5: Increase Alpha outstanding.
        SubnetAlphaOut::<T>::mutate(netuid, |total| {
            *total = total.saturating_add(alpha.to_num::<u64>());
        });
        // Step 6: Increase Tao reserves.
        SubnetTAO::<T>::mutate(netuid, |total| {
            *total = total.saturating_add(tao);
        });
        // Step 7: Increase Total Tao reserves.
        TotalStake::<T>::mutate(|total| {
            *total = total.saturating_add(tao);
        });
        // Step 8. Decrease Alpha reserves.
        SubnetVolume::<T>::mutate(netuid, |total| {
            *total = total.saturating_sub(tao);
        });
        // Step 9. Return the alpha received.
        alpha.to_num::<u64>()
    }

    /// Swaps a subnet's Alpba token for TAO.
    ///
    /// Updates TaoIn, AlphaIn, and AlphaOut
    pub fn swap_alpha_for_tao(netuid: u16, alpha: u64) -> u64 {
        // Step 1: Get the mechanism type for the subnet (0 for Stable, 1 for Dynamic)
        let mechanism_id: u16 = SubnetMechanism::<T>::get(netuid);
        // Step 2: Swap alpha and attain tao
        let tao: I96F32 = if mechanism_id == 1 {
            // Step 3.a.1: Dynamic mechanism calculations
            let tao_reserves: I96F32 = I96F32::from_num(SubnetTAO::<T>::get(netuid));
            let alpha_reserves: I96F32 = I96F32::from_num(SubnetAlphaIn::<T>::get(netuid));
            // Step 3.a.2: Compute constant product k = alpha * tao
            let k: I96F32 = alpha_reserves.saturating_mul(tao_reserves);
            // Step 3.a.3: Calculate alpha staked using the constant product formula
            // tao_recieved = tao_reserves - (k / (alpha_reserves + new_tao))
            tao_reserves.saturating_sub(
                k.checked_div(alpha_reserves.saturating_add(I96F32::from_num(alpha)))
                    .unwrap_or(I96F32::from_num(0)),
            )
        } else {
            // Step 3.b.1: Stable mechanism, just return the value 1:1
            I96F32::from_num(alpha)
        };
        // Step 4: Increase Alpha reserves.
        SubnetAlphaIn::<T>::mutate(netuid, |total| {
            *total = total.saturating_add(alpha);
        });
        // Step 5: Decrease Alpha outstanding.
        SubnetAlphaOut::<T>::mutate(netuid, |total| {
            *total = total.saturating_sub(alpha);
        });
        // Step 6: Decrease tao reserves.
        SubnetTAO::<T>::mutate(netuid, |total| {
            *total = total.saturating_sub(tao.to_num::<u64>());
        });
        // Step 7: Reduce total TAO reserves.
        TotalStake::<T>::mutate(|total| {
            *total = total.saturating_sub(tao.to_num::<u64>());
        });
        // Step 8. Decrease Alpha reserves.
        SubnetVolume::<T>::mutate(netuid, |total| {
            *total = total.saturating_sub(tao.to_num::<u64>());
        });
        // Step 9. Return the tao received.
        tao.to_num::<u64>()
    }

    /// Unstakes alpha from a subnet for a given hotkey and coldkey pair.
    ///
    /// We update the pools associated with a subnet as well as update hotkey alpha shares.
    pub fn unstake_from_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
        alpha: u64,
    ) -> u64 {
        // Step 1: Swap the alpha for TAO.
        let tao: u64 = Self::swap_alpha_for_tao(netuid, alpha);

        // Step 2: Decrease alpha on subneet
        Self::decrease_stake_for_hotkey_and_coldkey_on_subnet(hotkey, coldkey, netuid, alpha);

        // Step 3: Update StakingHotkeys if the hotkey's total alpha, across all subnets, is zero
        // TODO const: fix.
        // if Self::get_stake(hotkey, coldkey) == 0 {
        //     StakingHotkeys::<T>::mutate(coldkey, |hotkeys| {
        //         hotkeys.retain(|k| k != hotkey);
        //     });
        // }

        // Step 4. Deposit and log the unstaking event.
        Self::deposit_event(Event::StakeRemoved(
            coldkey.clone(),
            hotkey.clone(),
            tao,
            alpha,
            netuid,
        ));
        log::info!(
            "StakeRemoved( coldkey: {:?}, hotkey:{:?}, tao: {:?}, alpha:{:?}, netuid: {:?} )",
            coldkey.clone(),
            hotkey.clone(),
            tao,
            alpha,
            netuid
        );

        // Step 5: Return the amount of TAO unstaked.
        tao
    }

    /// Stakes TAO into a subnet for a given hotkey and coldkey pair.
    ///
    /// We update the pools associated with a subnet as well as update hotkey alpha shares.
    pub fn stake_into_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
        tao: u64,
    ) -> u64 {
        // Step 1. Swap the tao to alpha.
        let alpha: u64 = Self::swap_tao_for_alpha(netuid, tao);

        // Step 2: Increase the alpha on the hotkey account.
        Self::increase_stake_for_hotkey_and_coldkey_on_subnet(hotkey, coldkey, netuid, alpha);

        // Step 4: Update the list of hotkeys staking for this coldkey
        let mut staking_hotkeys = StakingHotkeys::<T>::get(coldkey);
        if !staking_hotkeys.contains(hotkey) {
            staking_hotkeys.push(hotkey.clone());
            StakingHotkeys::<T>::insert(coldkey, staking_hotkeys.clone());
        }

        // Step 5. Deposit and log the staking event.
        Self::deposit_event(Event::StakeAdded(
            coldkey.clone(),
            hotkey.clone(),
            tao,
            alpha,
            netuid,
        ));
        log::info!(
            "StakeAdded( coldkey: {:?}, hotkey:{:?}, tao: {:?}, alpha:{:?}, netuid: {:?} )",
            coldkey.clone(),
            hotkey.clone(),
            tao,
            alpha,
            netuid
        );

        // Step 6: Return the amount of alpha staked
        alpha
    }

    pub fn get_alpha_share_pool(
        hotkey: <T as frame_system::Config>::AccountId,
        netuid: u16,
    ) -> SharePool<AlphaShareKey<T>, HotkeyAlphaSharePoolDataOperations<T>> {
        let ops = HotkeyAlphaSharePoolDataOperations::new(hotkey, netuid);
        SharePool::<AlphaShareKey<T>, HotkeyAlphaSharePoolDataOperations<T>>::new(ops)
    }
}

///////////////////////////////////////////
// Alpha share pool chain data layer

#[derive(Debug)]
pub struct HotkeyAlphaSharePoolDataOperations<T: frame_system::Config> {
    netuid: u16,
    hotkey: <T as frame_system::Config>::AccountId,
    _marker: sp_std::marker::PhantomData<T>,
}

impl<T: Config> HotkeyAlphaSharePoolDataOperations<T> {
    fn new(hotkey: <T as frame_system::Config>::AccountId, netuid: u16) -> Self {
        HotkeyAlphaSharePoolDataOperations {
            netuid,
            hotkey,
            _marker: sp_std::marker::PhantomData,
        }
    }
}

// Alpha share key is coldkey because the HotkeyAlphaSharePoolDataOperations struct already has hotkey and netuid
type AlphaShareKey<T> = <T as frame_system::Config>::AccountId;

impl<T: Config> SharePoolDataOperations<AlphaShareKey<T>>
    for HotkeyAlphaSharePoolDataOperations<T>
{
    fn get_shared_value(&self) -> U64F64 {
        U64F64::from_num(crate::TotalHotkeyAlpha::<T>::get(&self.hotkey, self.netuid))
    }

    fn get_share(&self, key: &AlphaShareKey<T>) -> U64F64 {
        crate::Alpha::<T>::get((&(self.hotkey), key, self.netuid))
    }

    fn try_get_share(&self, key: &AlphaShareKey<T>) -> Result<U64F64, ()> {
        crate::Alpha::<T>::try_get((&(self.hotkey), key, self.netuid))
    }

    fn get_denominator(&self) -> U64F64 {
        crate::TotalHotkeyShares::<T>::get(&(self.hotkey), self.netuid)
    }

    fn set_shared_value(&mut self, value: U64F64) {
        if value != 0 {
            crate::TotalHotkeyAlpha::<T>::insert(
                &(self.hotkey),
                self.netuid,
                value.to_num::<u64>(),
            );
        } else {
            crate::TotalHotkeyAlpha::<T>::remove(&(self.hotkey), self.netuid);
        }
    }

    fn set_share(&mut self, key: &AlphaShareKey<T>, share: U64F64) {
        if share != 0 {
            crate::Alpha::<T>::insert((&self.hotkey, key, self.netuid), share);
        } else {
            crate::Alpha::<T>::remove((&self.hotkey, key, self.netuid));
        }
    }

    fn set_denominator(&mut self, update: U64F64) {
        if update != 0 {
            crate::TotalHotkeyShares::<T>::insert(&self.hotkey, self.netuid, update);
        } else {
            crate::TotalHotkeyShares::<T>::remove(&self.hotkey, self.netuid);
        }
    }
}
