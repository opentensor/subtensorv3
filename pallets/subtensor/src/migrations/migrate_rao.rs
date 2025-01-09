use super::*;
use alloc::string::String;
use frame_support::IterableStorageMap;
use frame_support::{traits::Get, weights::Weight};
use log;
use sp_runtime::format;
use substrate_fixed::types::U64F64;

pub fn migrate_rao<T: Config>() -> Weight {
    let migration_name = b"migrate_rao".to_vec();

    // Initialize the weight with one read operation.
    let mut weight = T::DbWeight::get().reads(1);

    // Check if the migration has already run
    if HasMigrationRun::<T>::get(&migration_name) {
        log::info!(
            "Migration '{:?}' has already run. Skipping.",
            migration_name
        );
        return weight;
    }
    log::info!(
        "Running migration '{}'",
        String::from_utf8_lossy(&migration_name)
    );

    let netuids: Vec<u16> = <NetworksAdded<T> as IterableStorageMap<u16, bool>>::iter()
        .map(|(netuid, _)| netuid)
        .collect();
    weight = weight.saturating_add(T::DbWeight::get().reads_writes(netuids.len() as u64, 0));

    // Set the Dynamic block.
    DynamicBlock::<T>::set(Pallet::<T>::get_current_block_as_u64());

    // Migrate all TAO to root.
    Stake::<T>::iter().for_each(|(hotkey, coldkey, stake)| {
        // Increase SubnetTAO on root.
        SubnetTAO::<T>::mutate(0, |total| {
            *total = total.saturating_add(stake);
        });
        // Increase SubnetAlphaOut on root.
        SubnetAlphaOut::<T>::mutate(0, |total| {
            *total = total.saturating_add(stake);
        });
        // Set all the stake on root 0 subnet.
        Alpha::<T>::mutate((hotkey.clone(), coldkey.clone(), 0), |total| {
            *total = total.saturating_add(U64F64::from_num(stake))
        });
        TotalHotkeyShares::<T>::mutate(hotkey.clone(), 0, |total| {
            *total = total.saturating_add(U64F64::from_num(stake))
        });
        // Set the total stake on the hotkey
        TotalHotkeyAlpha::<T>::mutate(hotkey.clone(), 0, |total| {
            *total = total.saturating_add(stake)
        });
        // 6 reads and 6 writes.
        weight = weight.saturating_add(T::DbWeight::get().reads_writes(6, 6));
    });

    // Convert subnets and give them lock.
    // Set global weight to 18% from the start
    TaoWeight::<T>::set(332_041_393_326_771_929);
    for netuid in netuids.iter().clone() {
        if *netuid == 0 {
            // Give root a single RAO in pool to avoid any catestrophic division by zero.
            SubnetAlphaIn::<T>::insert(netuid, 1);
            SubnetMechanism::<T>::insert(netuid, 0); // Set to zero mechanism.
            TokenSymbol::<T>::insert(netuid, Pallet::<T>::get_symbol_for_subnet(0));
            continue;
        }
        let owner: T::AccountId = SubnetOwner::<T>::get(netuid);
        let lock: u64 = SubnetLocked::<T>::get(netuid);
        let initial_liquidity: u64 = 100_000_000_000; // 100 TAO.
        let remaining_lock: u64 = lock.saturating_sub(initial_liquidity);
        Pallet::<T>::add_balance_to_coldkey_account(&owner, remaining_lock);
        SubnetTAO::<T>::insert(netuid, initial_liquidity); // Set TAO to the lock.
        SubnetAlphaIn::<T>::insert(netuid, initial_liquidity); // Set AlphaIn to the initial alpha distribution.
        SubnetAlphaOut::<T>::insert(netuid, 0); // Set zero subnet alpha out.
        SubnetMechanism::<T>::insert(netuid, 1); // Convert to dynamic immediately with initialization.
        Tempo::<T>::insert(netuid, DefaultTempo::<T>::get());
        // Set the token symbol for this subnet using Self instead of Pallet::<T>
        TokenSymbol::<T>::insert(netuid, Pallet::<T>::get_symbol_for_subnet(*netuid));
        SubnetTAO::<T>::insert(netuid, initial_liquidity); // Set TAO to the lock.
        TotalStakeAtDynamic::<T>::insert(netuid, 0);

        if let Ok(owner_coldkey) = SubnetOwner::<T>::try_get(netuid) {
            // Set Owner as the coldkey.
            SubnetOwnerHotkey::<T>::insert(netuid, owner_coldkey.clone());
            // Associate the coldkey to coldkey.
            Pallet::<T>::create_account_if_non_existent(&owner_coldkey, &owner_coldkey);
            // Register the owner_coldkey as neuron to the network.
            let _neuron_uid: u16 = Pallet::<T>::register_neuron(*netuid, &owner_coldkey);
            // Register the neuron immediately.
            if !Identities::<T>::contains_key(owner_coldkey.clone()) {
                // Set the identitiy for the Owner coldkey if non existent.
                let identity = ChainIdentityOf {
                    name: format!("Owner{}", netuid).as_bytes().to_vec(),
                    url: Vec::new(),
                    image: Vec::new(),
                    discord: Vec::new(),
                    description: Vec::new(),
                    additional: Vec::new(),
                };
                // Validate the created identity and set it.
                if Pallet::<T>::is_valid_identity(&identity) {
                    Identities::<T>::insert(owner_coldkey.clone(), identity.clone());
                }
            }
        }

        // HotkeyEmissionTempo::<T>::put(30); // same as subnet tempo // (DEPRECATED)
        // Set the target stakes per interval to 10.
        // TargetStakesPerInterval::<T>::put(10); (DEPRECATED)
    }

    // Mark the migration as completed
    HasMigrationRun::<T>::insert(&migration_name, true);
    weight = weight.saturating_add(T::DbWeight::get().writes(1));

    log::info!(
        "Migration '{:?}' completed. Storage version set to 7.",
        String::from_utf8_lossy(&migration_name)
    );

    // Return the migration weight.
    weight
}
