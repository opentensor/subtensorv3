use super::*;
use substrate_fixed::types::I64F64;
use substrate_fixed::types::I96F32;

impl<T: Config> Pallet<T> {
    /// The `coinbase` function performs a four-part emission distribution process involving
    /// subnets, epochs, hotkeys, and nominators.
    // It is divided into several steps, each handling a specific part of the distribution:

    // Step 1: Compute the block-wise emission for each subnet.
    // This involves calculating how much (TAO) should be emitted into each subnet using the
    // root epoch function.

    // Step 2: Accumulate the subnet block emission.
    // After calculating the block-wise emission, these values are accumulated to keep track
    // of how much each subnet should emit before the next distribution phase. This accumulation
    // is a running total that gets updated each block.

    // Step 3: Distribute the accumulated emissions through epochs.
    // Subnets periodically distribute their accumulated emissions to hotkeys (active validators/miners)
    // in the network on a `tempo` --- the time between epochs. This step runs Yuma consensus to
    // determine how emissions are split among hotkeys based on their contributions and roles.
    // The accumulation of hotkey emissions is done through the `accumulate_hotkey_emission` function.
    // The function splits the rewards for a hotkey amongst itself and its `parents`. The parents are
    // the hotkeys that are delegating their stake to the hotkey.

    // Step 4: Further distribute emissions from hotkeys to nominators.
    // Finally, the emissions received by hotkeys are further distributed to their nominators,
    // who are stakeholders that support the hotkeys.
    pub fn run_coinbase() {
        // --- 0. Get current block.
        let current_block: u64 = Self::get_current_block_as_u64();
        log::debug!("Current block: {:?}", current_block);

        // --- 1. Get all netuids.
        let subnets: Vec<u16> = Self::get_all_subnet_netuids();
        log::debug!("All subnet netuids: {:?}", subnets);

        // --- 2. Run the root epoch function which computes the block emission for each subnet.
        // coinbase --> root() --> subnet_block_emission
        match Self::root_epoch(current_block) {
            Ok(_) => log::debug!("Root epoch run successfully for block: {:?}", current_block),
            Err(e) => {
                log::trace!("Did not run epoch with: {:?}", e);
            }
        }

        // --- 3. Drain the subnet block emission and accumulate it as subnet emission, which increases until the tempo is reached in #4.
        // subnet_blockwise_emission -> subnet_pending_emission
        for netuid in subnets.clone().iter() {
            if *netuid == 0 {
                continue;
            }
            // --- 3.1 Get the network's block-wise emission amount.
            // This value is newly minted TAO which has not reached staking accounts yet.
            let subnet_blockwise_emission: u64 = EmissionValues::<T>::get(*netuid);
            log::debug!(
                "Subnet block-wise emission for netuid {:?}: {:?}",
                *netuid,
                subnet_blockwise_emission
            );

            // --- 3.2 Accumulate the subnet emission on the subnet.
            PendingEmission::<T>::mutate(*netuid, |subnet_emission| {
                *subnet_emission = subnet_emission.saturating_add(subnet_blockwise_emission);
                log::debug!(
                    "Updated subnet emission for netuid {:?}: {:?}",
                    *netuid,
                    *subnet_emission
                );
            });
        }

        // --- 4. Drain the accumulated subnet emissions, pass them through the epoch().
        // Before accumulating on the hotkeys the function redistributes the emission towards hotkey parents.
        // subnet_emission --> epoch() --> hotkey_emission --> (hotkey + parent hotkeys)
        for netuid in subnets.clone().iter() {
            // --- 4.1 Check to see if the subnet should run its epoch.
            if Self::should_run_epoch(*netuid, current_block) {
                // --- 4.2 Drain the subnet emission.
                let mut subnet_emission: u64 = PendingEmission::<T>::get(*netuid);
                PendingEmission::<T>::insert(*netuid, 0);
                log::debug!(
                    "Drained subnet emission for netuid {:?}: {:?}",
                    *netuid,
                    subnet_emission
                );

                // --- 4.3 Set last step counter.
                Self::set_blocks_since_last_step(*netuid, 0);
                Self::set_last_mechanism_step_block(*netuid, current_block);

                if *netuid == 0 {
                    // Skip netuid 0 payouts
                    continue;
                }

                // --- 4.4 Distribute owner take.
                if SubnetOwner::<T>::contains_key(netuid) {
                    // Does the subnet have an owner?

                    // --- 4.4.1 Compute the subnet owner cut.
                    let owner_cut: I96F32 = I96F32::from_num(subnet_emission).saturating_mul(
                        I96F32::from_num(Self::get_subnet_owner_cut())
                            .saturating_div(I96F32::from_num(u16::MAX)),
                    );

                    // --- 4.4.2 Remove the cut from the subnet emission
                    subnet_emission = subnet_emission.saturating_sub(owner_cut.to_num::<u64>());

                    // --- 4.4.3 Add the cut to the balance of the owner
                    Self::add_balance_to_coldkey_account(
                        &Self::get_subnet_owner(*netuid),
                        owner_cut.to_num::<u64>(),
                    );

                    // --- 4.4.4 Increase total issuance on the chain.
                    Self::coinbase(owner_cut.to_num::<u64>());
                }

                // 4.3 Pass emission through epoch() --> hotkey emission.
                let hotkey_emission: Vec<(T::AccountId, u64, u64)> =
                    Self::epoch(*netuid, subnet_emission);
                log::debug!(
                    "Hotkey emission results for netuid {:?}: {:?}",
                    *netuid,
                    hotkey_emission
                );

                // 4.4 Accumulate the tuples on hotkeys:
                for (hotkey, mining_emission, validator_emission) in hotkey_emission {
                    // 4.5 Accumulate the emission on the hotkey and parent hotkeys.
                    Self::accumulate_hotkey_emission(
                        &hotkey,
                        *netuid,
                        validator_emission, // Amount received from validating
                        mining_emission,    // Amount recieved from mining.
                    );
                    log::debug!("Accumulated emissions on hotkey {:?} for netuid {:?}: mining {:?}, validator {:?}", hotkey, *netuid, mining_emission, validator_emission);
                }
            } else {
                // No epoch, increase blocks since last step and continue
                Self::set_blocks_since_last_step(
                    *netuid,
                    Self::get_blocks_since_last_step(*netuid).saturating_add(1),
                );
                log::debug!("Tempo not reached for subnet: {:?}", *netuid);
            }
        }

        // --- 5. Drain the accumulated hotkey emissions through to the nominators.
        // The hotkey takes a proportion of the emission, the remainder is drained through to the nominators.
        // We keep track of the last stake increase event for accounting purposes.
        // hotkeys --> nominators.
        let emission_tempo: u64 = Self::get_hotkey_emission_tempo();
        for (hotkey, hotkey_emission) in PendingdHotkeyEmission::<T>::iter() {
            // Check for zeros.
            // remove zero values.
            if hotkey_emission == 0 {
                continue;
            }

            // --- 5.1 Check if we should drain the hotkey emission on this block.
            if Self::should_drain_hotkey(&hotkey, current_block, emission_tempo) {
                // --- 5.2 Drain the hotkey emission and distribute it to nominators.
                let total_new_tao: u64 =
                    Self::drain_hotkey_emission(&hotkey, hotkey_emission, current_block);
                log::debug!(
                    "Drained hotkey emission for hotkey {:?} on block {:?}: {:?}",
                    hotkey,
                    current_block,
                    hotkey_emission
                );

                // --- 5.3 Increase total issuance on the chain.
                Self::coinbase(total_new_tao);
                log::debug!("Increased total issuance by {:?}", total_new_tao);
            }
        }
    }

    /// Accumulates the mining and validator emissions on a hotkey and distributes the validator emission among its parents.
    ///
    /// This function is responsible for accumulating the mining and validator emissions associated with a hotkey onto a hotkey.
    /// It first calculates the total stake of the hotkey, considering the stakes contributed by its parents and reduced by its children.
    /// It then retrieves the list of parents of the hotkey and distributes the validator emission proportionally based on the stake contributed by each parent.
    /// The remaining validator emission, after distribution to the parents, along with the mining emission, is then added to the hotkey's own accumulated emission.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey for which emissions are being calculated.
    /// * `netuid` - The unique identifier of the network to which the hotkey belongs.
    /// * `mining_emission` - The amount of mining emission allocated to the hotkey.
    /// * `validator_emission` - The amount of validator emission allocated to the hotkey.
    ///
    pub fn accumulate_hotkey_emission(
        hotkey: &T::AccountId,
        netuid: u16,
        validating_emission: u64,
        mining_emission: u64,
    ) {
        // --- 1. First, calculate the hotkey's share of the emission.
        let take_proportion: I64F64 = I64F64::from_num(Self::get_childkey_take(hotkey, netuid))
            .saturating_div(I64F64::from_num(u16::MAX));
        let hotkey_take: u64 = take_proportion
            .saturating_mul(I64F64::from_num(validating_emission))
            .to_num::<u64>();
        // NOTE: Only the validation emission should be split amongst parents.

        // --- 2. Compute the remaining emission after the hotkey's share is deducted.
        let emission_minus_take: u64 = validating_emission.saturating_sub(hotkey_take);

        // --- 3. Track the remaining emission for accounting purposes.
        let mut remaining_emission: u64 = emission_minus_take;

        // --- 4. Calculate the total stake of the hotkey, adjusted by the stakes of parents and children.
        // Parents contribute to the stake, while children reduce it.
        // If this value is zero, no distribution to anyone is necessary.
        let total_hotkey_stake: u64 = Self::get_stake_for_hotkey_on_subnet(hotkey, netuid);
        if total_hotkey_stake != 0 {
            // --- 5. If the total stake is not zero, iterate over each parent to determine their contribution to the hotkey's stake,
            // and calculate their share of the emission accordingly.
            for (proportion, parent) in Self::get_parents(hotkey, netuid) {
                // --- 5.1 Retrieve the parent's stake. This is the raw stake value including nominators.
                let parent_stake: u64 = Self::get_total_stake_for_hotkey(&parent);

                // --- 5.2 Calculate the portion of the hotkey's total stake contributed by this parent.
                // Then, determine the parent's share of the remaining emission.
                let stake_from_parent: I96F32 = I96F32::from_num(parent_stake).saturating_mul(
                    I96F32::from_num(proportion).saturating_div(I96F32::from_num(u64::MAX)),
                );
                let proportion_from_parent: I96F32 =
                    stake_from_parent.saturating_div(I96F32::from_num(total_hotkey_stake));
                let parent_emission_take: u64 = proportion_from_parent
                    .saturating_mul(I96F32::from_num(emission_minus_take))
                    .to_num::<u64>();

                // --- 5.5. Accumulate emissions for the parent hotkey.
                PendingdHotkeyEmission::<T>::mutate(parent, |parent_accumulated| {
                    *parent_accumulated = parent_accumulated.saturating_add(parent_emission_take)
                });

                // --- 5.6. Subtract the parent's share from the remaining emission for this hotkey.
                remaining_emission = remaining_emission.saturating_sub(parent_emission_take);
            }
        }

        // --- 6. Add the remaining emission plus the hotkey's initial take to the pending emission for this hotkey.
        PendingdHotkeyEmission::<T>::mutate(hotkey, |hotkey_pending| {
            *hotkey_pending = hotkey_pending.saturating_add(
                remaining_emission
                    .saturating_add(hotkey_take)
                    .saturating_add(mining_emission),
            )
        });
    }

    //. --- 4. Drains the accumulated hotkey emission through to the nominators. The hotkey takes a proportion of the emission.
    /// The remainder is drained through to the nominators keeping track of the last stake increase event to ensure that the hotkey does not
    /// gain more emission than it's stake since the last drain.
    /// hotkeys --> nominators.
    ///
    /// 1. It resets the accumulated emissions for the hotkey to zero.
    /// 4. It calculates the total stake for the hotkey and determines the hotkey's own take from the emissions based on its delegation status.
    /// 5. It then calculates the remaining emissions after the hotkey's take and distributes this remaining amount proportionally among the hotkey's nominators.
    /// 6. Each nominator's share of the emissions is added to their stake, but only if their stake was not manually increased since the last emission drain.
    /// 7. Finally, the hotkey's own take and any undistributed emissions are added to the hotkey's total stake.
    ///
    /// This function ensures that emissions are fairly distributed according to stake proportions and delegation agreements, and it updates the necessary records to reflect these changes.
    pub fn drain_hotkey_emission(hotkey: &T::AccountId, emission: u64, block_number: u64) -> u64 {
        // --- 0. For accounting purposes record the total new added stake.
        let mut total_new_tao: u64 = 0;

        // --- 1.0 Drain the hotkey emission.
        PendingdHotkeyEmission::<T>::insert(hotkey, 0);

        // --- 2 Update the block value to the current block number.
        LastHotkeyEmissionDrain::<T>::insert(hotkey, block_number);

        // --- 3 Retrieve the total stake for the hotkey from all nominations.
        let total_hotkey_stake: u64 = Self::get_total_stake_for_hotkey(hotkey);

        // --- 4 Calculate the emission take for the hotkey.
        let take_proportion: I64F64 = I64F64::from_num(Delegates::<T>::get(hotkey))
            .saturating_div(I64F64::from_num(u16::MAX));
        let hotkey_take: u64 =
            (take_proportion.saturating_mul(I64F64::from_num(emission))).to_num::<u64>();

        // --- 5 Compute the remaining emission after deducting the hotkey's take.
        let emission_minus_take: u64 = emission.saturating_sub(hotkey_take);

        // --- 6 Calculate the remaining emission after the hotkey's take.
        let mut remainder: u64 = emission_minus_take;

        // --- 7 Iterate over each nominator and get all viable stake.
        let mut total_viable_nominator_stake: u64 = total_hotkey_stake;
        for (nominator, _) in Stake::<T>::iter_prefix(hotkey) {
            let nonviable_nomintaor_stake = Self::get_nonviable_stake(hotkey, &nominator);

            total_viable_nominator_stake =
                total_viable_nominator_stake.saturating_sub(nonviable_nomintaor_stake);
        }

        // --- 8 Iterate over each nominator.
        if total_viable_nominator_stake != 0 {
            for (nominator, nominator_stake) in Stake::<T>::iter_prefix(hotkey) {
                // --- 9 Skip emission for any stake the was added by the nominator since the last emission drain.
                // This means the nominator will get emission on existing stake, but not on new stake, until the next emission drain.
                let viable_nominator_stake =
                    nominator_stake.saturating_sub(Self::get_nonviable_stake(hotkey, &nominator));

                // --- 10 Calculate this nominator's share of the emission.
                let nominator_emission: I64F64 = I64F64::from_num(viable_nominator_stake)
                    .checked_div(I64F64::from_num(total_viable_nominator_stake))
                    .unwrap_or(I64F64::from_num(0))
                    .saturating_mul(I64F64::from_num(emission_minus_take));

                // --- 11 Increase the stake for the nominator.
                Self::increase_stake_on_coldkey_hotkey_account(
                    &nominator,
                    hotkey,
                    nominator_emission.to_num::<u64>(),
                );

                // --- 12* Record event and Subtract the nominator's emission from the remainder.
                total_new_tao = total_new_tao.saturating_add(nominator_emission.to_num::<u64>());
                remainder = remainder.saturating_sub(nominator_emission.to_num::<u64>());
            }
        }

        // --- 13 Finally, add the stake to the hotkey itself, including its take and the remaining emission.
        let hotkey_new_tao: u64 = hotkey_take.saturating_add(remainder);
        Self::increase_stake_on_hotkey_account(hotkey, hotkey_new_tao);

        // --- 14 Reset the stake delta for the hotkey.
        let _ = StakeDeltaSinceLastEmissionDrain::<T>::clear_prefix(hotkey, u32::MAX, None);

        // --- 15 Record new tao creation event and return the amount created.
        total_new_tao = total_new_tao.saturating_add(hotkey_new_tao);
        total_new_tao
    }

    ///////////////
    /// Helpers ///
    ///////////////
    /// Determines whether the hotkey emission should be drained based on the current block and index.
    ///
    /// # Arguments
    /// * `hotkey_i` - The hotkey identifier.
    /// * `index` - The index of the hotkey in the iterable storage.
    /// * `block` - The current block number.
    ///
    /// # Returns
    /// * `bool` - True if the hotkey emission should be drained, false otherwise.
    pub fn should_drain_hotkey(hotkey: &T::AccountId, block: u64, emit_tempo: u64) -> bool {
        let hotkey_idx: u64 = Self::hash_hotkey_to_u64(hotkey);
        block.rem_euclid(emit_tempo.saturating_add(1))
            == hotkey_idx.rem_euclid(emit_tempo.saturating_add(1))
    }

    /// Checks if the epoch should run for a given subnet based on the current block.
    ///
    /// # Arguments
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `bool` - True if the epoch should run, false otherwise.
    pub fn should_run_epoch(netuid: u16, current_block: u64) -> bool {
        Self::blocks_until_next_epoch(netuid, Self::get_tempo(netuid), current_block) == 0
    }

    /// Helper function which returns the number of blocks remaining before we will run the epoch on this
    /// network. Networks run their epoch when (block_number + netuid + 1 ) % (tempo + 1) = 0
    /// tempo | netuid | # first epoch block
    ///   1        0               0
    ///   1        1               1
    ///   2        0               1
    ///   2        1               0
    ///   100      0              99
    ///   100      1              98
    /// Special case: tempo = 0, the network never runs.
    ///
    pub fn blocks_until_next_epoch(netuid: u16, tempo: u16, block_number: u64) -> u64 {
        if tempo == 0 {
            return u64::MAX;
        }
        let netuid_plus_one = (netuid as u64).saturating_add(1);
        let block_plus_netuid = block_number.saturating_add(netuid_plus_one);
        let tempo_plus_one = (tempo as u64).saturating_add(1);
        let remainder = block_plus_netuid.rem_euclid(tempo_plus_one);
        (tempo as u64).saturating_sub(remainder)
    }

    /// Calculates the nonviable stake for a nominator.
    /// The nonviable stake is the stake that was added by the nominator since the last emission drain.
    /// This stake will not receive emission until the next emission drain.
    /// Note: if the stake delta is below zero, we return zero. We don't allow more stake than the nominator has.
    pub fn get_nonviable_stake(hotkey: &T::AccountId, nominator: &T::AccountId) -> u64 {
        let stake_delta = StakeDeltaSinceLastEmissionDrain::<T>::get(hotkey, nominator);
        if stake_delta.is_negative() {
            0
        } else {
            // Should never fail the into, but we handle it anyway.
            stake_delta.try_into().unwrap_or(u64::MAX)
        }
    }
}
