use super::*;
use frame_support::{traits::Get, weights::Weight};
use scale_info::prelude::string::String;

/// Migrates storage items with typos to their correctly named counterparts.
///
/// - `PendingdHotkeyEmission` is migrated to `PendingHotkeyEmission`.
/// - `LastMechansimStepBlock` is migrated to `LastMechanismStepBlock`.
///
/// # Returns
/// The cumulative weight of the migration process.
pub fn migrate_rename_storage_items<T: Config>() -> Weight {
    let migration_name = b"migrate_rename_storage_items_v1".to_vec();

    let mut weight = T::DbWeight::get().reads(1);

    // Check if the migration has already been run.
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

    // === Migrate `PendingdHotkeyEmission` to `PendingHotkeyEmission` and wipe old storage ===
    for (account_id, emission) in PendingdHotkeyEmission::<T>::drain() {
        // Insert the value into the new storage
        PendingHotkeyEmission::<T>::insert(account_id, emission);

        // Add weight for the write operation (the `drain` method takes care of the removal)
        weight = weight.saturating_add(T::DbWeight::get().writes(1));
    }

    // === Migrate `LastMechansimStepBlock` to `LastMechanismStepBlock` and wipe old storage ===
    for (netuid, block) in LastMechansimStepBlock::<T>::drain() {
        // Insert the value into the new storage
        LastMechanismStepBlock::<T>::insert(netuid, block);

        // Add weight for the write operation (the `drain` method takes care of the removal)
        weight = weight.saturating_add(T::DbWeight::get().writes(1));
    }

    // === Mark Migration as Completed ===
    HasMigrationRun::<T>::insert(&migration_name, true);
    weight = weight.saturating_add(T::DbWeight::get().writes(1));

    log::info!(
        "Migration '{:?}' completed.",
        String::from_utf8_lossy(&migration_name)
    );

    weight
}
