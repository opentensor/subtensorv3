use super::*;
use substrate_fixed::types::I96F32;

impl<T: Config> Pallet<T> {
    /// Emits alpha into a subnet for a given hotkey and coldkey pair.
    ///
    /// This function performs the following operations:
    /// 1. Increments the alpha (stake) for the specific hotkey-coldkey pair on the given subnet.
    /// 2. Increases the total outstanding alpha in the subnet.
    /// 3. Increases the total alpha associated with the coldkey for the subnet.
    /// 4. Increases the total alpha associated with the hotkey for the subnet.
    ///
    /// The function uses saturating addition to prevent overflow errors.
    ///
    /// # Arguments
    ///
    /// * `hotkey` - The account ID of the hotkey (neuron).
    /// * `coldkey` - The account ID of the coldkey (owner).
    /// * `netuid` - The unique identifier of the subnet.
    /// * `emitted_alpha` - The amount of alpha to emit (stake to add).
    ///
    /// # Effects
    ///
    /// This function modifies the following storage items:
    /// - `Alpha`: Increases the stake for the specific hotkey-coldkey pair on the subnet.
    /// - `SubnetAlphaOut`: Increases the total outstanding alpha in the subnet.
    /// - `TotalColdkeyAlpha`: Increases the total alpha for the coldkey on the subnet.
    /// - `TotalHotkeyAlpha`: Increases the total alpha for the hotkey on the subnet.
    pub fn emit_into_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
        emitted_alpha: u64,
    ) {
        // Step 1: Increment the alpha (stake) for the specific hotkey-coldkey pair on the subnet
        // This represents the stake of this particular neuron (hotkey) owned by this account (coldkey)
        Alpha::<T>::mutate((hotkey, coldkey, netuid), |alpha| {
            *alpha = alpha.saturating_add(emitted_alpha);
        });

        // Step 2: Increase the total outstanding alpha in the subnet
        // This represents the total stake emitted into this subnet
        SubnetAlphaOut::<T>::mutate(netuid, |total| {
            *total = total.saturating_add(emitted_alpha);
        });

        // Step 3: Increase the total alpha associated with the coldkey for this subnet
        // This represents the total stake owned by this account (coldkey) in this subnet
        TotalColdkeyAlpha::<T>::mutate(coldkey, netuid, |total| {
            *total = total.saturating_add(emitted_alpha);
        });

        // Step 4: Increase the total alpha associated with the hotkey for this subnet
        // This represents the total stake associated with this neuron (hotkey) in this subnet
        TotalHotkeyAlpha::<T>::mutate(hotkey, netuid, |total| {
            *total = total.saturating_add(emitted_alpha);
        });
    }

    /// Stakes TAO into a subnet for a given hotkey and coldkey pair.
    ///
    /// This function performs the following operations:
    /// 1. Retrieves the mechanism type for the subnet (0 for Stable, 1 for Dynamic).
    /// 2. Converts the input TAO amount to a fixed-point number for precise calculations.
    /// 3. Calculates the alpha to be staked based on the subnet's mechanism:
    ///    a. For Dynamic mechanism (mechanism_id == 1):
    ///       - Retrieves current TAO and Alpha in the subnet.
    ///       - Computes the constant product k = alpha * tao.
    ///       - Calculates alpha staked using the formula: alpha_staked = current_alpha - (k / (current_tao + new_tao)).
    ///       - Calculates new subnet alpha after staking.
    ///
    ///    b. For Stable mechanism (mechanism_id == 0):
    ///       - Sets alpha staked equal to TAO staked.
    ///       - Sets new subnet alpha to zero.
    /// 4. Converts calculated alpha values from fixed-point to u64 for storage.
    /// 5. Updates the hotkey's alpha for the specific subnet by adding the newly staked alpha.
    /// 6. Updates the subnet's alpha in the pool (SubnetAlphaIn) with the new subnet alpha.
    /// 7. Increases the total subnet alpha outstanding (SubnetAlphaOut) by the staked alpha.
    /// 8. Increases the total TAO in the subnet (SubnetTAO) by the staked TAO.
    /// 9. Increases the global total of staked TAO (TotalStake).
    /// 10. Increases the stake for the specific hotkey-coldkey pair (Stake).
    /// 11. Increases the total alpha for the coldkey in this subnet (TotalColdkeyAlpha).
    /// 12. Increases the total alpha for the hotkey in this subnet (TotalHotkeyAlpha).
    /// 13. Updates the list of hotkeys staking for this coldkey (StakingHotkeys) if necessary.
    ///
    /// # Arguments
    ///
    /// * `hotkey` - The account ID of the hotkey.
    /// * `coldkey` - The account ID of the coldkey.
    /// * `netuid` - The unique identifier of the subnet.
    /// * `tao_staked` - The amount of TAO to stake.
    ///
    /// # Returns
    ///
    /// * `u64` - The amount of alpha staked.
    ///
    /// # Effects
    ///
    /// This function mutates the following storage items:
    /// - `SubnetMechanism`: Read to determine the subnet's mechanism.
    /// - `SubnetTAO`: Increased by the staked TAO amount.
    /// - `SubnetAlphaIn`: Updated with the new subnet alpha.
    /// - `SubnetAlphaOut`: Increased by the staked alpha amount.
    /// - `Alpha`: Increased for the specific hotkey-coldkey pair.
    /// - `TotalStake`: Increased by the staked TAO amount.
    /// - `Stake`: Increased for the specific hotkey-coldkey pair.
    /// - `TotalColdkeyAlpha`: Increased for the coldkey in this subnet.
    /// - `TotalHotkeyAlpha`: Increased for the hotkey in this subnet.
    /// - `StakingHotkeys`: Updated if a new hotkey is staking for this coldkey.
    pub fn stake_into_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
        tao_staked: u64,
    ) -> u64 {
        // Step 1: Get the mechanism type for the subnet (0 for Stable, 1 for Dynamic)
        let mechanism_id: u16 = SubnetMechanism::<T>::get(netuid);

        let alpha_staked: I96F32;
        let new_subnet_alpha: I96F32;
        // Step 2: Convert tao_staked to a fixed-point number for precise calculations
        let tao_staked_float: I96F32 = I96F32::from_num(tao_staked);

        if mechanism_id == 1 {
            // Step 3: Dynamic mechanism calculations
            // Step 3a: Get current TAO and Alpha in the subnet
            let subnet_tao: I96F32 = I96F32::from_num(SubnetTAO::<T>::get(netuid));
            let subnet_alpha: I96F32 = I96F32::from_num(SubnetAlphaIn::<T>::get(netuid));

            // Step 3b: Compute constant product k = alpha * tao
            // This is the key to the dynamic mechanism: k remains constant
            let k: I96F32 = subnet_alpha.saturating_mul(subnet_tao);
            // Step 3c: Calculate alpha staked using the constant product formula
            // alpha_staked = current_alpha - (k / (current_tao + new_tao))
            // This ensures that the product of alpha and tao remains constant
            alpha_staked = subnet_alpha.saturating_sub(
                k.checked_div(subnet_tao.saturating_add(tao_staked_float))
                    .unwrap_or(I96F32::from_num(0)),
            );

            // Step 3d: Calculate new subnet alpha after staking
            // This is the remaining alpha in the subnet after staking
            new_subnet_alpha = subnet_alpha.saturating_sub(alpha_staked);
        } else {
            // Step 4: Stable mechanism calculations
            // Step 4a: In stable mechanism, alpha staked is equal to TAO staked
            alpha_staked = tao_staked_float;
            // Step 4b: Does not change for stable mechanism.
            new_subnet_alpha = I96F32::from_num(SubnetAlphaIn::<T>::get(netuid));
        }

        // Step 5: Convert alpha_staked from I96F32 to u64 for storage
        let alpha_staked_u64: u64 = alpha_staked.to_num::<u64>();

        // Step 6: Update hotkey alpha for the specific subnet
        // This increases the alpha associated with this hotkey-coldkey pair
        Alpha::<T>::mutate((hotkey, coldkey, netuid), |alpha| {
            *alpha = alpha.saturating_add(alpha_staked_u64);
        });

        // Step 7: Convert new_subnet_alpha from I96F32 to u64 for storage
        let new_subnet_alpha_u64: u64 = new_subnet_alpha.to_num::<u64>();

        // Step 8: Update subnet alpha in the pool
        // This sets the new amount of alpha available in the subnet
        SubnetAlphaIn::<T>::insert(netuid, new_subnet_alpha_u64);

        // Step 9: Update total subnet alpha outstanding (includes staked alpha)
        // This increases the total alpha in circulation for this subnet
        SubnetAlphaOut::<T>::mutate(netuid, |total| {
            *total = total.saturating_add(alpha_staked_u64);
        });

        // Step 10: Update total TAO in the subnet
        // This increases the total TAO staked in this subnet
        SubnetTAO::<T>::mutate(netuid, |total| {
            *total = total.saturating_add(tao_staked);
        });

        // Step 11: Update total stake across all subnets
        // This increases the global total of staked TAO
        TotalStake::<T>::mutate(|total| {
            *total = total.saturating_add(tao_staked);
        });

        // Step 12: Update stake for the specific hotkey-coldkey pair
        // This increases the TAO staked by this specific pair
        Stake::<T>::mutate(&hotkey, &coldkey, |stake| {
            *stake = stake.saturating_add(tao_staked);
        });

        // Step 13: Update total alpha for the coldkey in this subnet
        // This increases the total alpha associated with this coldkey in this subnet
        TotalColdkeyAlpha::<T>::mutate(coldkey, netuid, |total| {
            *total = total.saturating_add(alpha_staked_u64);
        });

        // Step 14: Update total alpha for the hotkey in this subnet
        // This increases the total alpha associated with this hotkey in this subnet
        TotalHotkeyAlpha::<T>::mutate(hotkey, netuid, |total| {
            *total = total.saturating_add(alpha_staked_u64);
        });

        // Step 15: Update the list of hotkeys staking for this coldkey
        // This ensures we keep track of which hotkeys are staking for each coldkey
        let mut staking_hotkeys = StakingHotkeys::<T>::get(coldkey);
        if !staking_hotkeys.contains(hotkey) {
            staking_hotkeys.push(hotkey.clone());
            StakingHotkeys::<T>::insert(coldkey, staking_hotkeys.clone());
        }

        // Step 16: Return the amount of alpha staked
        alpha_staked_u64
    }

    /// Unstakes alpha from a subnet for a given hotkey and coldkey pair.
    ///
    /// This function performs the following operations:
    /// 1. Retrieves the mechanism type for the subnet.
    /// 2. Calculates the amount of TAO to unstake based on the subnet's mechanism:
    ///    - For dynamic mechanism (mechid == 1):
    ///      a. Retrieves current TAO and alpha in the subnet.
    ///      b. Calculates the constant product k.
    ///      c. Computes TAO unstaked using the constant product formula.
    ///      d. Calculates the new subnet alpha.
    ///    - For stable mechanism (mechid != 1):
    ///      a. Sets TAO unstaked equal to alpha unstaked.
    ///      b. Sets new subnet alpha to zero.
    /// 3. Converts TAO unstaked and new subnet alpha to u64.
    /// 4. Updates the subnet's alpha in the pool (SubnetAlphaIn).
    /// 5. Decreases the outstanding alpha in the subnet (SubnetAlphaOut).
    /// 6. Decreases the total TAO in the subnet (SubnetTAO).
    /// 7. Updates or removes alpha for the hotkey-coldkey pair (Alpha):
    ///    - If new total is zero, removes the entry and updates StakingHotkeys.
    ///    - Otherwise, updates the total.
    /// 8. Updates or removes total alpha for the coldkey (TotalColdkeyAlpha):
    ///    - If new total is zero, removes the entry.
    ///    - Otherwise, updates the total.
    /// 9. Updates or removes total alpha for the hotkey (TotalHotkeyAlpha):
    ///    - If new total is zero, removes the entry.
    ///    - Otherwise, updates the total.
    /// 10. Decreases the total stake for the hotkey-coldkey pair (Stake).
    /// 11. Decreases the total stake across all subnets (TotalStake).
    ///
    /// # Arguments
    ///
    /// * `hotkey` - The account ID of the hotkey.
    /// * `coldkey` - The account ID of the coldkey.
    /// * `netuid` - The unique identifier of the subnet.
    /// * `alpha_unstaked` - The amount of alpha to unstake.
    ///
    /// # Returns
    ///
    /// * `u64` - The amount of TAO unstaked.
    ///
    /// # Effects
    ///
    /// This function mutates the following storage items:
    /// - `SubnetMechanism`: Read to determine the subnet's mechanism.
    /// - `SubnetTAO`: Read and updated to reflect the unstaked TAO.
    /// - `SubnetAlphaIn`: Updated with the new subnet alpha.
    /// - `SubnetAlphaOut`: Decreased by the unstaked alpha.
    /// - `Alpha`: Updated or removed for the hotkey-coldkey pair.
    /// - `StakingHotkeys`: Updated if a hotkey is fully unstaked.
    /// - `TotalColdkeyAlpha`: Updated or removed for the coldkey.
    /// - `TotalHotkeyAlpha`: Updated or removed for the hotkey.
    /// - `Stake`: Decreased for the hotkey-coldkey pair.
    /// - `TotalStake`: Decreased by the unstaked TAO.
    pub fn unstake_from_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
        alpha_unstaked: u64,
    ) -> u64 {
        // Step 1: Get the mechanism type for this subnet
        let mechid: u16 = SubnetMechanism::<T>::get(netuid);

        // Step 2: Initialize variables for TAO unstaked and new subnet alpha
        let tao_unstaked: I96F32;
        let new_subnet_alpha: I96F32;

        // Step 3a: Get the current stake for the hotkey-coldkey pair in this subnet
        let current_stake = Alpha::<T>::get((hotkey, coldkey, netuid));

        // Step 3b: Calculate the actual amount to unstake (minimum of requested and available)
        let actual_unstake = alpha_unstaked.min(current_stake);

        // Step 3c: Update the alpha_unstaked value
        let float_alpha_unstaked: I96F32 = I96F32::from_num(actual_unstake);

        // No operation.
        if actual_unstake == 0 {
            return 0;
        }

        if mechid == 1 {
            // Step 4: Dynamic mechanism
            // Step 4a: Get current TAO in the subnet
            let subnet_tao: I96F32 = I96F32::from_num(SubnetTAO::<T>::get(netuid));
            // Step 4b: Get current alpha in the subnet
            let subnet_alpha: I96F32 = I96F32::from_num(SubnetAlphaIn::<T>::get(netuid));
            // Step 4c: Calculate constant product k
            let k: I96F32 = subnet_alpha.saturating_mul(subnet_tao);
            // Step 4d: Calculate TAO unstaked using constant product formula
            tao_unstaked = subnet_tao.saturating_sub(
                k.checked_div(subnet_alpha.saturating_add(float_alpha_unstaked))
                    .unwrap_or(I96F32::from_num(0)),
            );
            // Step 4e: Calculate new subnet alpha
            new_subnet_alpha = subnet_alpha.saturating_add(float_alpha_unstaked);
        } else {
            // Step 5: Stable mechanism
            // Step 5a: TAO unstaked is equal to alpha unstaked
            tao_unstaked = float_alpha_unstaked;
            // Step 5b: New subnet alpha is always zero in stable mechanism
            new_subnet_alpha = I96F32::from_num(0.0);
        }

        // Step 6: Cap TAO unstaked at u64::MAX and convert to u64
        let tao_unstaked_u64: u64 = tao_unstaked.min(I96F32::from_num(u64::MAX)).to_num::<u64>();
        // Step 7: Cap new subnet alpha at u64::MAX and convert to u64
        let new_subnet_alpha_u64: u64 = new_subnet_alpha
            .min(I96F32::from_num(u64::MAX))
            .to_num::<u64>();

        // Step 8: Update subnet alpha
        SubnetAlphaIn::<T>::insert(netuid, new_subnet_alpha_u64);

        // Step 9: Decrease outstanding alpha in the subnet
        SubnetAlphaOut::<T>::mutate(netuid, |total| {
            *total = total.saturating_sub(alpha_unstaked);
        });

        // Step 10: Decrease total TAO in the subnet
        SubnetTAO::<T>::mutate(netuid, |total| {
            *total = total.saturating_sub(tao_unstaked_u64);
        });

        // Step 11: Update or remove alpha for the hotkey-coldkey pair
        Alpha::<T>::mutate_exists((hotkey, coldkey, netuid), |maybe_total| {
            if let Some(total) = maybe_total {
                let new_total = total.saturating_sub(alpha_unstaked);
                if new_total == 0 {
                    // Step 11a: Remove entry if new total is zero
                    *maybe_total = None;
                } else {
                    // Step 11c: Update total if not zero
                    *total = new_total;
                }
            }
        });

        // Step 12: Update or remove total alpha for coldkey
        TotalColdkeyAlpha::<T>::mutate_exists(coldkey, netuid, |maybe_total| {
            if let Some(total) = maybe_total {
                let new_total = total.saturating_sub(alpha_unstaked);
                if new_total == 0 {
                    // Step 12a: Remove entry if new total is zero
                    *maybe_total = None;
                } else {
                    // Step 12b: Update total if not zero
                    *total = new_total;
                }
            }
        });

        // Step 13: Update or remove total alpha for hotkey
        TotalHotkeyAlpha::<T>::mutate_exists(hotkey, netuid, |maybe_total| {
            if let Some(total) = maybe_total {
                let new_total = total.saturating_sub(alpha_unstaked);
                if new_total == 0 {
                    // Step 13a: Remove entry if new total is zero
                    *maybe_total = None;
                } else {
                    // Step 13b: Update total if not zero
                    *total = new_total;
                }
            }
        });

        // Step 14: Decrease total stake for the hotkey-coldkey pair
        Stake::<T>::mutate(&hotkey, &coldkey, |stake| {
            *stake = stake.saturating_sub(tao_unstaked_u64);
        });

        // Step 15: Decrease total stake across all subnets
        TotalStake::<T>::put(TotalStake::<T>::get().saturating_sub(tao_unstaked_u64));
        // Step 16: Update StakingHotkeys if the hotkey's total alpha is zero
        if Alpha::<T>::get((hotkey, coldkey, netuid)) == 0 {
            StakingHotkeys::<T>::mutate(coldkey, |hotkeys| {
                hotkeys.retain(|k| k != hotkey);
            });
        }

        // Step 17: Return the amount of TAO unstaked
        tao_unstaked_u64
    }
}
