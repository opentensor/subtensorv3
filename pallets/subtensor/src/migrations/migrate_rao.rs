use super::*;
use alloc::string::String;
use frame_support::IterableStorageMap;
use frame_support::{traits::Get, weights::Weight};
use log;

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
        // Increase SubnetAlphaIn on root.
        SubnetAlphaIn::<T>::mutate(0, |total| {
            *total = total.saturating_add(stake);
        });
        // Set all the stake on root 0 subnet.
        Alpha::<T>::mutate((hotkey.clone(), coldkey.clone(), 0), |total| {
            *total = total.saturating_add(stake)
        });
        // Set the total stake on the coldkey
        TotalColdkeyAlpha::<T>::mutate(coldkey.clone(), 0, |total| {
            *total = total.saturating_add(stake)
        });
        // Set the total stake on the hotkey
        TotalHotkeyAlpha::<T>::mutate(hotkey.clone(), 0, |total| {
            *total = total.saturating_add(stake)
        });
        // 3 reads and 3 writes.
        weight = weight.saturating_add(T::DbWeight::get().reads_writes(3, 3));
    });

    // Convert subnets and give them lock.
    let current_block = Pallet::<T>::get_current_block_as_u64();
    for netuid in netuids.iter().clone() {
        if *netuid == 0 {
            continue;
        }
        let owner: T::AccountId = SubnetOwner::<T>::get(netuid);
        let lock: u64 = SubnetLocked::<T>::get(netuid); // Get the current locked.
        SubnetTAO::<T>::insert(netuid, lock); // Set TAO to the lock.
        SubnetAlphaIn::<T>::insert(netuid, 1); // Set AlphaIn to the initial alpha distribution.
        SubnetAlphaOut::<T>::insert(netuid, lock); // Set AlphaOut to the initial alpha distribution.
        TotalColdkeyAlpha::<T>::mutate(owner.clone(), 0, |total| {
            *total = total.saturating_add(lock)
        }); // Set the total coldkey alpha.
        TotalHotkeyAlpha::<T>::mutate(owner.clone(), 0, |total| {
            *total = total.saturating_add(lock)
        }); // Set the total hotkey alpha.
        Alpha::<T>::mutate((owner.clone(), owner.clone(), netuid), |total| {
            *total = total.saturating_add(lock)
        }); // Set the alpha.
        Stake::<T>::mutate(&owner, &owner, |total| {
            *total = total.saturating_add(lock);
        }); // Increase the stake.
        TotalStake::<T>::put(TotalStake::<T>::get().saturating_add(lock)); // Increase the total stake.
        SubnetMechanism::<T>::insert(netuid, 1); // Convert to dynamic immediately with initialization.
        SubnetLocked::<T>::insert(netuid, lock);
        LargestLocked::<T>::insert(netuid, lock);
        Locks::<T>::insert(
            // Lock the initial funds making this key the owner.
            (netuid, owner.clone(), owner.clone()), // Sets owner as initial lock.
            (
                lock,
                current_block,
                current_block.saturating_add(<LockIntervalBlocks<T>>::get()),
            ), // Starts initial lock at 2 months.
        );
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
