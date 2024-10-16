use super::*;
use frame_support::{traits::Get, weights::Weight};
use log;
use scale_info::prelude::string::String;

pub fn migrate_remove_zero_stakes<T: Config>() -> Weight {
    let migration_name = b"remove_zero_stakes_v1".to_vec();
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

    // Iterate through all the stake entries
    Stake::<T>::iter().for_each(|(hotkey, coldkey, stake)| {
        if stake == 0 {
            log::info!(
                "Removing zero stake for hotkey: {:?}, coldkey: {:?}",
                hotkey,
                coldkey
            );
            Stake::<T>::remove(hotkey.clone(), coldkey.clone());

            weight = weight.saturating_add(T::DbWeight::get().writes(1));
        }
        weight = weight.saturating_add(T::DbWeight::get().reads(1));
    });

    // Mark the migration as completed
    HasMigrationRun::<T>::insert(&migration_name, true);
    weight = weight.saturating_add(T::DbWeight::get().writes(1));

    log::info!(
        "Migration '{:?}' completed. All zero stakes removed.",
        String::from_utf8_lossy(&migration_name)
    );

    weight
}
