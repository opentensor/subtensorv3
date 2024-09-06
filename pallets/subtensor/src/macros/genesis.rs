use frame_support::pallet_macros::pallet_section;

/// A [`pallet_section`] that defines the errors for a pallet.
/// This can later be imported into the pallet using [`import_section`].
#[pallet_section]
mod genesis {

    #[pallet::genesis_build]
    impl<T: Config> BuildGenesisConfig for GenesisConfig<T> {
        fn build(&self) {
            // Set initial total issuance from balances
            TotalIssuance::<T>::put(self.balances_issuance);

            // Get the root network uid.
            let root_netuid: u16 = 0;

            // Set the root network as added.
            NetworksAdded::<T>::insert(root_netuid, true);

            // Increment the number of total networks.
            TotalNetworks::<T>::mutate(|n| *n = n.saturating_add(1));

            // Set the number of validators to 1.
            SubnetworkN::<T>::insert(root_netuid, 0);

            // Set the maximum number to the number of senate members.
            MaxAllowedUids::<T>::insert(root_netuid, 64u16);

            // Set the maximum number to the number of validators to all members.
            MaxAllowedValidators::<T>::insert(root_netuid, 64u16);

            // Set the min allowed weights to zero, no weights restrictions.
            MinAllowedWeights::<T>::insert(root_netuid, 0);

            // Set the max weight limit to infitiy, no weight restrictions.
            MaxWeightsLimit::<T>::insert(root_netuid, u16::MAX);

            // Add default root tempo.
            Tempo::<T>::insert(root_netuid, 100);

            // Set the root network as open.
            NetworkRegistrationAllowed::<T>::insert(root_netuid, true);

            // Set target registrations for validators as 1 per block.
            TargetRegistrationsPerInterval::<T>::insert(root_netuid, 1);

            for net in 1..2 {
                let netuid: u16 = net as u16;
                let hotkey = DefaultAccount::<T>::get();
                SubnetMechanism::<T>::insert(netuid, 1); // Make dynamic.
                Owner::<T>::insert(hotkey.clone(), hotkey.clone());
                SubnetAlphaIn::<T>::insert(netuid, 1);
                SubnetTAO::<T>::insert(netuid, 10_000_000_000);
                NetworksAdded::<T>::insert(netuid, true);
                TotalNetworks::<T>::mutate(|n| *n = n.saturating_add(1));
                SubnetworkN::<T>::insert(netuid, 0);
                MaxAllowedUids::<T>::insert(netuid, 256u16);
                MaxAllowedValidators::<T>::insert(netuid, 64u16);
                MinAllowedWeights::<T>::insert(netuid, 0);
                MaxWeightsLimit::<T>::insert(netuid, u16::MAX);
                Tempo::<T>::insert(netuid, 100);
                NetworkRegistrationAllowed::<T>::insert(netuid, true);
                SubnetOwner::<T>::insert(netuid, hotkey.clone());
                SubnetLocked::<T>::insert(netuid, 1);
                LargestLocked::<T>::insert(netuid, 1);
                Locks::<T>::insert(
                    // Lock the initial funds making this key the owner.
                    (netuid, hotkey.clone(), hotkey.clone()),
                    (1, 0, 7200 * 30),
                );
                Alpha::<T>::insert(
                    // Lock the initial funds making this key the owner.
                    (hotkey.clone(), hotkey.clone(), netuid),
                    1_000_000_000,
                );
                TotalHotkeyAlpha::<T>::insert(hotkey.clone(), netuid, 1_000_000_000);
                TotalColdkeyAlpha::<T>::insert(hotkey.clone(), netuid, 1_000_000_000);
                SubnetAlphaOut::<T>::insert(netuid, 1_000_000_000);
                let mut staking_hotkeys = StakingHotkeys::<T>::get(hotkey.clone());
                if !staking_hotkeys.contains(&hotkey) {
                    staking_hotkeys.push(hotkey.clone());
                    StakingHotkeys::<T>::insert(hotkey.clone(), staking_hotkeys.clone());
                }

                let block_number = Pallet::<T>::get_current_block_as_u64();
                for next_uid_s in 0..1 {
                    let next_uid: u16 = next_uid_s as u16;
                    SubnetworkN::<T>::insert(netuid, next_uid.saturating_add(1));
                    Rank::<T>::mutate(netuid, |v| v.push(0));
                    Trust::<T>::mutate(netuid, |v| v.push(0));
                    Active::<T>::mutate(netuid, |v| v.push(true));
                    Emission::<T>::mutate(netuid, |v| v.push(0));
                    Consensus::<T>::mutate(netuid, |v| v.push(0));
                    Incentive::<T>::mutate(netuid, |v| v.push(0));
                    Dividends::<T>::mutate(netuid, |v| v.push(0));
                    LastUpdate::<T>::mutate(netuid, |v| v.push(block_number));
                    PruningScores::<T>::mutate(netuid, |v| v.push(0));
                    ValidatorTrust::<T>::mutate(netuid, |v| v.push(0));
                    ValidatorPermit::<T>::mutate(netuid, |v| v.push(false));
                    Keys::<T>::insert(netuid, next_uid, hotkey.clone()); // Make hotkey - uid association.
                    Uids::<T>::insert(netuid, hotkey.clone(), next_uid); // Make uid - hotkey association.
                    BlockAtRegistration::<T>::insert(netuid, next_uid, block_number); // Fill block at registration.
                    IsNetworkMember::<T>::insert(hotkey.clone(), netuid, true); // Fill network is member.
                }
            }
        }
    }
}
