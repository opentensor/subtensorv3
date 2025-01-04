use super::*;
use alloc::collections::BTreeMap;
use substrate_fixed::types::I96F32;

impl<T: Config> Pallet<T> {
    pub fn run_coinbase() {
        // --- 0. Get current block.
        let current_block: u64 = Self::get_current_block_as_u64();
        log::debug!("Current block: {:?}", current_block);

        // --- 1. Get all netuids.
        let subnets: Vec<u16> = Self::get_all_subnet_netuids();
        log::debug!("All subnet netuids: {:?}", subnets);

        // --- 2. Get the current coinbase emission.
        let block_emission: I96F32 = I96F32::from_num(Self::get_block_emission().unwrap_or(0));
        log::debug!("Block emission: {:?}", block_emission);

        // --- 4. Sum all the SubnetTAO associated with the same mechanism.
        // Mechanisms get emission based on the proportion of TAO across all their subnets
        let mut total_active_tao: I96F32 = I96F32::from_num(0);
        let mut mechanism_tao: BTreeMap<u16, I96F32> = BTreeMap::new();
        for netuid in subnets.iter() {
            if *netuid == 0 { continue; } // Skip root network
            let mechid = SubnetMechanism::<T>::get(*netuid);
            let subnet_tao = I96F32::from_num(SubnetTAO::<T>::get(*netuid));
            let new_subnet_tao = subnet_tao.saturating_add(*mechanism_tao.entry(mechid).or_insert(I96F32::from_num(0)));
            *mechanism_tao.entry(mechid).or_insert(I96F32::from_num(0)) = new_subnet_tao;
            total_active_tao = total_active_tao.saturating_add(subnet_tao);
        }
        log::debug!("Mechanism TAO sums: {:?}", mechanism_tao);

        // --- 5. Compute subnet emission values (amount of tao inflation this block).
        let mut tao_in_map: BTreeMap<u16, u64> = BTreeMap::new();
        for netuid in subnets.iter() {
            // Do not emit into root network.
            if *netuid == 0 {continue;}
            // 5.1: Get subnet mechanism ID
            let mechid: u16 = SubnetMechanism::<T>::get(*netuid);
            log::debug!("Netuid: {:?}, Mechanism ID: {:?}", netuid, mechid);
            // 5.2: Get subnet TAO (T_s)
            let subnet_tao: I96F32 = I96F32::from_num(SubnetTAO::<T>::get(*netuid));
            log::debug!("Subnet TAO (T_s) for netuid {:?}: {:?}", netuid, subnet_tao);
            // 5.3: Get the denominator as the sum of all TAO associated with a specific mechanism (T_m)
            let mech_tao: I96F32 = *mechanism_tao.get(&mechid).unwrap_or(&I96F32::from_num(0));
            log::debug!("Mechanism TAO (T_m) for mechanism ID {:?}: {:?}", mechid, mech_tao);
            // 5.4: Compute the mechanism emission proportion: P_m = T_m / T_total
            let mech_proportion: I96F32 = mech_tao.checked_div(total_active_tao).unwrap_or(I96F32::from_num(0));
            log::debug!("Mechanism proportion (P_m) for mechanism ID {:?}: {:?}", mechid, mech_proportion);
            // 5.5: Compute the mechanism emission: E_m = P_m * E_b
            let mech_emission: I96F32 = mech_proportion.saturating_mul(block_emission);
            log::debug!("Mechanism emission (E_m) for mechanism ID {:?}: {:?}", mechid, mech_emission);
            // 5.6: Calculate subnet's proportion of mechanism TAO: P_s = T_s / T_m
            let subnet_proportion: I96F32 = subnet_tao.checked_div(mech_tao).unwrap_or(I96F32::from_num(0));
            log::debug!("Subnet proportion (P_s) for netuid {:?}: {:?}", netuid, subnet_proportion);
            // 5.7: Calculate subnet's TAO emission: E_s = P_s * E_m
            let tao_in: u64 = mech_emission.checked_mul(subnet_proportion).unwrap_or(I96F32::from_num(0)).to_num::<u64>();
            log::debug!("Subnet TAO emission (E_s) for netuid {:?}: {:?}", netuid, tao_in);
            // 5.8: Store the subnet TAO emission. 
            *tao_in_map.entry(*netuid).or_insert(0) = tao_in;
            // 5.9: Store the block emission for this subnet for chain storage.
            EmissionValues::<T>::insert(*netuid, tao_in);
        }

        // --- 6. Distribute subnet emission into subnets based on mechanism type.
        for netuid in subnets.iter() {
            // Do not emit into root network.
            if *netuid == 0 {continue;}
            // 6.1. Get subnet mechanism ID
            let mechid: u16 = SubnetMechanism::<T>::get(*netuid);
            log::debug!("Netuid: {:?}, Mechanism ID: {:?}", netuid, mechid);
            // 6.2: Get the subnet emission TAO.
            let mut tao_emission: I96F32 = I96F32::from_num(*tao_in_map.get(&netuid).unwrap_or(&0));
            log::debug!("Subnet emission TAO for netuid {:?}: {:?}", netuid, tao_emission);
            if mechid == 0 {
                // The mechanism is Stable (FOR TESTING PURPOSES ONLY)
                // 6.2.1 Increase Tao in the subnet "reserves" unconditionally.
                SubnetTAO::<T>::mutate(*netuid, |total| { *total = total.saturating_add(tao_emission.to_num::<u64>()) });
                // 6.2.2 Increase total stake across all subnets.
                TotalStake::<T>::mutate(|total| *total = total.saturating_add(tao_emission.to_num::<u64>()));
                // 6.2.3 Increase total issuance of Tao.
                TotalIssuance::<T>::mutate(|total| *total = total.saturating_add(tao_emission.to_num::<u64>()));
                // 6.2.4 Increase this subnet pending emission.
                PendingEmission::<T>::mutate(*netuid, |total| { *total = total.saturating_add(tao_emission.to_num::<u64>()) });
                // 6.2.5 Go to next subnet.
                continue
            }
            // Dynamic TAO below:
            // 6.3: Get the alpha emission per block as a function of supply.
            let alpha_emission: I96F32 = I96F32::from_num(Self::get_block_emission_for_issuance( Self::get_alpha_issuance(*netuid) ).unwrap_or(0));
            log::debug!("netuid {:?}: alpha_emission: {:?}", netuid, tao_emission);
            // 6.4: Get tao_in, which is number between (0, 1), percent of block emission in tao.
            let tao_in: I96F32 = tao_emission / I96F32::from_num(block_emission);
            log::debug!("Emission proportion {:?}: {:?}", netuid, tao_in);
            // 6.5: Get alpha price as function of tao_reserve and alpha_reserve.
            let alpha_price: I96F32 = I96F32::from_num(SubnetTAO::<T>::get(*netuid)).checked_div(I96F32::from_num(SubnetAlphaIn::<T>::get(*netuid))).unwrap_or(I96F32::from_num(0));
            log::debug!("Alpha price for netuid {:?}: {:?}", netuid, alpha_price);
            // 6.5: Compute alpha_in from tao_in and alpha_price.
            let alpha_in: I96F32;
            if alpha_price <= tao_in {
                // 6.5.1: Set tao_in proportion to alpha_price.
                tao_emission = alpha_price * block_emission;
                log::debug!("Set tao_in to alpha_price: {:?}", tao_emission);
                // 6.5.2: Set alpha_in to a hard 1 (max value)
                alpha_in = I96F32::from_num(1);
                log::debug!("Set alpha_in to max value: {:?}", alpha_in);
            } else {
                // 6.5.2: tao_in remains unchanged
                tao_emission = tao_in * block_emission;
                log::debug!("tao_in remains unchanged: {:?}", tao_emission);
                // 6.5.3: alpha_in is computed based on tao_in and price to keep price fixed.
                alpha_in = I96F32::from_num(tao_in).checked_div(alpha_price).unwrap_or(I96F32::from_num(0));
                log::debug!("Computed alpha_in based on tao_in and alpha_price: {:?}", alpha_in);
            }
            // 6.6: Compute Alpha_in_issuance based on current issuance amount.
            let alpha_in_emission: I96F32 = alpha_emission.saturating_mul(alpha_in);
            SubnetAlphaInEmission::<T>::insert( *netuid, alpha_in_emission.to_num::<u64>()); 
            log::debug!("Computed alpha_in_emission: {:?}", alpha_in_emission);
            // 6.7: Compute Alpha_out_issuance based on alpha_in_issuance
            let alpha_out_emission: I96F32 = 2 * alpha_emission - alpha_in_emission;
            SubnetAlphaOutEmission::<T>::insert( *netuid, alpha_out_emission.to_num::<u64>()); 
            log::debug!("Computed alpha_out_emission: {:?}", alpha_out_emission);
            // 6.7: Inject Alpha into the pool reserves here: alpha_in.
            SubnetAlphaIn::<T>::mutate(*netuid, |total| { 
                *total = total.saturating_add(alpha_in_emission.to_num::<u64>());
                log::debug!("Injected alpha_in into SubnetAlphaIn: {:?}", *total);
            });
            // 6.8: Inject Alpha for distribution later.
            PendingEmission::<T>::mutate(*netuid, |total| { 
                *total = total.saturating_add(alpha_out_emission.to_num::<u64>());
                log::debug!("Injected alpha_out_emission into PendingEmission: {:?}", *total);
            });
            // 6.9: Increase Tao in the subnet reserve conditionally.
            SubnetTAO::<T>::mutate(*netuid, |total| { 
                *total = total.saturating_add(tao_emission.to_num::<u64>());
                log::debug!("Increased Tao in SubnetTAO: {:?}", *total);
            });
            // 6.10: Increase total stake counter.
            TotalStake::<T>::mutate(|total| { 
                *total = total.saturating_add(tao_emission.to_num::<u64>());
                log::debug!("Increased TotalStake: {:?}", *total);
            });
            // 6.11: Increase total Tao issuance counter.
            TotalIssuance::<T>::mutate(|total| { 
                *total = total.saturating_add(tao_emission.to_num::<u64>());
                log::debug!("Increased TotalIssuance: {:?}", *total);
            });
        }

        // --- 7. Drain pending emission through the subnet based on tempo.
        for &netuid in subnets.iter() {
            // 7.1: Pass on subnets that have not reached their tempo.
            // if !Self::should_run_epoch(netuid, current_block) {
            //     // 7.1.1: Increment blocks since last step for this subnet.
            //     BlocksSinceLastStep::<T>::mutate( netuid,|total| *total = total.saturating_add(1) );
            //     continue;
            // }
                
            // 7.2 Get and drain the subnet pending emission.
            let alpha_out: u64 = PendingEmission::<T>::get(netuid);
            log::debug!("Draining pending emission for netuid {:?}: {:?}", netuid, alpha_out);
            PendingEmission::<T>::insert(netuid, 0);

            // 7.3 Set counters for block emission.
            BlocksSinceLastStep::<T>::insert( netuid, 0 );
            LastMechansimStepBlock::<T>::insert( netuid, current_block );
        
            // 7.4 Distribute the 18% owner cut.
            let owner_cut: u64 = I96F32::from_num(alpha_out).saturating_mul(Self::get_float_subnet_owner_cut()).to_num::<u64>();
            log::debug!("Owner cut for netuid {:?}: {:?}", netuid, owner_cut);
            // 7.4.1: Check for existence of owner cold/hot pair and distribute emission directly to them.
            if let Ok(owner_coldkey) = SubnetOwner::<T>::try_get(netuid) {
                if let Ok(owner_hotkey) = SubnetOwnerHotkey::<T>::try_get(netuid) {
                    // Increase stake for both coldkey and hotkey on the subnet
                    Self::increase_stake_for_hotkey_and_coldkey_on_subnet(&owner_hotkey, &owner_coldkey, netuid, owner_cut);
                    // Increase alpha out.
                    SubnetAlphaOut::<T>::mutate(netuid, |total| *total = total.saturating_add(owner_cut));
                    log::debug!("Distributed owner cut for netuid {:?} to owner_hotkey {:?} and owner_coldkey {:?}", netuid, owner_hotkey, owner_coldkey);
                }
            }
            let remaining_emission: u64 = alpha_out.saturating_sub(owner_cut);
            log::debug!("Remaining emission for netuid {:?}: {:?}", netuid, remaining_emission);
            
            // 7.5 Run the epoch() --> hotkey emission.
            let hotkey_emission: Vec<(T::AccountId, u64, u64)> = Self::epoch_mock(netuid, remaining_emission);
            log::debug!("Hotkey emission for netuid {:?}: {:?}", netuid, hotkey_emission);

            // 7.6 Pay out the hotkey alpha dividends.
            // First clear the netuid from HotkeyDividends
            let mut total_root_alpha_divs: u64 = 0;
            let mut root_alpha_divs: BTreeMap<T::AccountId, u64> = BTreeMap::new();
            let _ = HotkeyDividendsPerSubnet::<T>::clear_prefix(netuid, u32::MAX, None);
            for (hotkey, incentive, dividends) in hotkey_emission {
                log::debug!("Processing hotkey {:?} with incentive {:?} and dividends {:?}", hotkey, incentive, dividends);

                // 7.6.1: Distribute mining incentive immediately.
                Self::increase_stake_for_hotkey_and_coldkey_on_subnet( &hotkey.clone(), &Owner::<T>::get( hotkey.clone() ), netuid, incentive );
                SubnetAlphaOut::<T>::mutate(netuid, |total| { *total = total.saturating_add( incentive ) });
                log::debug!("Distributed mining incentive for hotkey {:?} on netuid {:?}: {:?}", hotkey, netuid, incentive);

                // 7.6.2: Get dividend tuples for parents and self based on childkey relationships and child-take.
                let dividend_tuples: Vec<(T::AccountId, u64)> = Self::get_parent_dividends(
                    &hotkey,
                    netuid,
                    dividends,
                );
                log::debug!("Dividend tuples for hotkey {:?} on netuid {:?}: {:?}", hotkey, netuid, dividend_tuples);

                // 7.6.3 Pay out dividends to hotkeys based on the local vs root proportion.
                for (hotkey_j, divs_j) in dividend_tuples {
                    log::debug!("Processing dividend for hotkey {:?} to hotkey {:?}: {:?}", hotkey, hotkey_j, divs_j);

                    // 7.6.3.1: Remove the hotkey take straight off the top.
                    let take_prop: I96F32 = I96F32::from_num(Self::get_hotkey_take( &hotkey_j )).checked_div( I96F32::from_num(u16::MAX) ).unwrap_or( I96F32::from_num( 0.0 ) );
                    let validator_take: I96F32 = take_prop.saturating_mul(I96F32::from_num(divs_j));
                    let rem_divs_j: I96F32 = I96F32::from_num(divs_j).saturating_sub(validator_take);
                    log::debug!("Validator take for hotkey {:?}: {:?}, remaining dividends: {:?}", hotkey_j, validator_take, rem_divs_j);

                    // 7.6.3.2: Distribute validator take automatically.
                    Self::increase_stake_for_hotkey_and_coldkey_on_subnet( &hotkey_j, &Owner::<T>::get( hotkey_j.clone() ), netuid, validator_take.to_num::<u64>() );
                    SubnetAlphaOut::<T>::mutate( netuid, |total| { *total = total.saturating_add( validator_take.to_num::<u64>() ); });
                    log::debug!("Distributed validator take for hotkey {:?} on netuid {:?}: {:?}", hotkey_j, netuid, validator_take.to_num::<u64>());

                    // 7.6.3.3: Get the local alpha and root alpha.
                    let hotkey_tao: I96F32 = I96F32::from_num( Self::get_stake_for_hotkey_on_subnet( &hotkey, Self::get_root_netuid() ) );
                    let hotkey_tao_as_alpha: I96F32 = hotkey_tao.saturating_mul( Self::get_tao_weight(netuid) );
                    let hotkey_alpha = I96F32::from_num(Self::get_stake_for_hotkey_on_subnet( &hotkey, netuid ));
                    log::debug!("Hotkey tao for hotkey {:?} on root netuid: {:?}, hotkey tao as alpha: {:?}, hotkey alpha: {:?}", hotkey, hotkey_tao, hotkey_tao_as_alpha, hotkey_alpha);

                    // 7.6.3.3 Compute alpha and root proportions.
                    let alpha_prop: I96F32 = hotkey_alpha.checked_div( hotkey_alpha.saturating_add(hotkey_tao_as_alpha) ).unwrap_or( I96F32::from_num( 0.0 ) );
                    let root_prop: I96F32 = hotkey_tao_as_alpha.checked_div( hotkey_alpha.saturating_add(hotkey_tao_as_alpha) ).unwrap_or( I96F32::from_num( 0.0 ) );
                    log::debug!("Alpha proportion: {:?}, root proportion: {:?}", alpha_prop, root_prop);

                    // 7.6.3.5: Compute alpha and root dividends
                    let alpha_divs: I96F32 = I96F32::from_num( rem_divs_j ).saturating_mul( alpha_prop );
                    let root_divs: I96F32 = I96F32::from_num( rem_divs_j ).saturating_mul( root_prop );
                    log::debug!("Alpha dividends: {:?}, root dividends: {:?}", alpha_divs, root_divs);

                    // 7.6.3.6. Store the root alpha divs under hotkey_j
                    root_alpha_divs.entry(hotkey_j.clone())
                        .and_modify(|e| *e = e.saturating_add(root_divs.to_num::<u64>()))
                        .or_insert(root_divs.to_num::<u64>());
                    total_root_alpha_divs = total_root_alpha_divs.saturating_add(root_divs.to_num::<u64>());
                    log::debug!("Stored root alpha dividends for hotkey {:?}: {:?}", hotkey_j, root_divs.to_num::<u64>());

                    // 7.6.3.7: Distribute the alpha divs to the hotkey.
                    Self::increase_stake_for_hotkey_on_subnet( &hotkey_j, netuid, alpha_divs.to_num::<u64>() );
                    SubnetAlphaOut::<T>::mutate( netuid, |total| { *total = total.saturating_add( alpha_divs.to_num::<u64>() ); });
                    log::debug!("Distributed alpha dividends for hotkey {:?} on netuid {:?}: {:?}", hotkey_j, netuid, alpha_divs.to_num::<u64>());

                    // 7.6.3.9: Record dividends for this hotkey on this subnet.
                    HotkeyDividendsPerSubnet::<T>::mutate( netuid, hotkey_j.clone(), |divs| {
                        *divs = divs.saturating_add(divs_j);
                    });
                    log::debug!("Recorded dividends for hotkey {:?} on netuid {:?}: {:?}", hotkey_j, netuid, divs_j);
                }
            }

            // 7.7 Pay out the hotkey root dividends.
            // Swap the total_root_alpha_divs through the pool to attain the total tao to be distributed
            // Increase alpha out first before unstaking them.
            SubnetAlphaOut::<T>::mutate(netuid, |total| *total = total.saturating_add(total_root_alpha_divs));
            let total_tao_to_distribute: u64 = Self::swap_alpha_for_tao( netuid, total_root_alpha_divs );
            log::debug!("Total tao to distribute for netuid {:?}: {:?}", netuid, total_tao_to_distribute);
            
            // For all the root alpha divs give this proportion of the swapped tao to the root participants. 
            for (hotkey_j, root_divs) in root_alpha_divs.iter() {
                let proportion: I96F32 = I96F32::from_num(*root_divs).checked_div(I96F32::from_num(total_root_alpha_divs)).unwrap_or(I96F32::from_num(0));
                let tao_to_pay: u64 = proportion.saturating_mul(I96F32::from_num(total_tao_to_distribute)).to_num::<u64>();
                log::debug!("Proportion for hotkey {:?}: {:?}, tao to pay: {:?}", hotkey_j, proportion, tao_to_pay);

                // Pay the tao to the hotkey on netuid 0
                Self::increase_stake_for_hotkey_on_subnet(hotkey_j, Self::get_root_netuid(), tao_to_pay);
                log::debug!("Paid tao to hotkey {:?} on root netuid: {:?}", hotkey_j, tao_to_pay);
            }
        }
    }

    /// Returns a list of tuples for each parent associated with this hotkey including self
    /// Each tuples contains the dividends owed to that hotkey given their parent proportion
    /// The hotkey child take proportion is removed from this and added to the tuples for self.
    ///
    /// # Arguments
    /// * `hotkye` - The hotkey to distribute out from.
    /// * `netuid` - The netuid we are computing on.
    /// * `dividends` - the dividends to distribute.
    ///
    /// # Returns
    /// * dividend_tuples: `Vec<(T::AccountId, u64)>` - Vector of (hotkey, divs) for each parent including self.
    ///
    pub fn get_parent_dividends(
        hotkey: &T::AccountId,
        netuid: u16,
        dividends: u64,
    ) -> Vec<(T::AccountId, u64)> {

        // hotkey dividends.
        let mut dividend_tuples: Vec<(T::AccountId, u64)> = vec![];

        // Calculate the hotkey's share of the validator emission based on its childkey take
        let validating_emission: I96F32 = I96F32::from_num(dividends);
        let childkey_take_proportion: I96F32 =
            I96F32::from_num(Self::get_childkey_take(hotkey, netuid))
                .saturating_div(I96F32::from_num(u16::MAX));
        let mut total_childkey_take: u64 = 0;
        // NOTE: Only the validation emission should be split amongst parents.

        // Initialize variables to track emission distribution
        let mut to_parents: u64 = 0;

        // Initialize variables to calculate total stakes from parents
        let mut total_contribution: I96F32 = I96F32::from_num(0);
        let mut contributions: Vec<(T::AccountId, I96F32)> = Vec::new();

        // Get the weights for root and alpha stakes in emission distribution
        let tao_weight: I96F32 = Self::get_tao_weight(netuid);

        // Calculate total root and alpha (subnet-specific) stakes from all parents
        for (proportion, parent) in Self::get_parents(hotkey, netuid) {
          
            // Convert the parent's stake proportion to a fractional value
            let parent_proportion: I96F32 = I96F32::from_num(proportion).saturating_div(I96F32::from_num(u64::MAX));

            // Get the parent's root and subnet-specific (alpha) stakes
            let parent_root: I96F32 = I96F32::from_num(Self::get_stake_for_hotkey_on_subnet(&parent, Self::get_root_netuid()));
            let parent_alpha: I96F32 = I96F32::from_num(Self::get_stake_for_hotkey_on_subnet(&parent, netuid));

            // Calculate the parent's contribution to the hotkey's stakes
            let parent_alpha_contribution: I96F32 = parent_alpha.saturating_mul(parent_proportion);
            let parent_root_contribution: I96F32 = parent_root.saturating_mul(parent_proportion).saturating_mul( tao_weight );
            let combined_contribution: I96F32 = parent_alpha_contribution.saturating_add(parent_root_contribution);

            // Add to the total stakes
            total_contribution = total_contribution.saturating_add(combined_contribution);
            // Store the parent's contributions for later use
            contributions.push((
                parent.clone(),
                combined_contribution,
            ));
        }

        // Distribute emission to parents based on their contributions
        for (parent, contribution) in contributions {
            // Sum up the total emission for this parent
            let emission_factor: I96F32 = contribution.checked_div(total_contribution).unwrap_or(I96F32::from_num(0));
            let total_emission: u64 = ( validating_emission.saturating_mul(emission_factor) ).to_num::<u64>();

            // Reserve childkey take
            let child_emission_take: u64 = childkey_take_proportion.saturating_mul(I96F32::from_num(total_emission)).to_num::<u64>();
            total_childkey_take = total_childkey_take.saturating_add(child_emission_take);
            let parent_total_emission = total_emission.saturating_sub(child_emission_take);

            // Add the parent's emission to the distribution list
            dividend_tuples.push((parent, parent_total_emission));

            // Keep track of total emission distributed to parents
            to_parents = to_parents.saturating_add(parent_total_emission);
        }
        // Calculate the final emission for the hotkey itself
        let final_hotkey_emission = validating_emission.to_num::<u64>().saturating_sub(to_parents);

        // Add the hotkey's own emission to the distribution list
        dividend_tuples.push((hotkey.clone(), final_hotkey_emission));

        dividend_tuples
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
        let tempo_plus_one = (tempo as u64).saturating_add(1);
        let adjusted_block = block_number.wrapping_add(netuid_plus_one);
        let remainder = adjusted_block.checked_rem(tempo_plus_one).unwrap_or(0);
        (tempo as u64).saturating_sub(remainder)
    }
}
