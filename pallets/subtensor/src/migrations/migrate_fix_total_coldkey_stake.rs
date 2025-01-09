use super::*;
use alloc::string::String;
use frame_support::{
    pallet_prelude::{Identity, OptionQuery},
    storage_alias,
    traits::{Get, StorageVersion},
    weights::Weight,
};
use sp_std::vec::Vec;

// TODO (camfairchild): TEST MIGRATION
pub mod deprecated_loaded_emission_format {
    use super::*;

    #[storage_alias]
    pub(super) type LoadedEmission<T: Config> =
        StorageMap<Pallet<T>, Identity, u16, Vec<(AccountIdOf<T>, u64)>, OptionQuery>;
}

/// Migrates and fixes the total coldkey stake.
///
/// This function iterates through all staking hotkeys, calculates the total stake for each coldkey,
/// and updates the `TotalColdkeyStake` storage accordingly. The migration is only performed if the
/// on-chain storage version is 6.
///
/// # Returns
/// The weight of the migration process.
pub fn do_migrate_fix_total_coldkey_stake<T: Config>() -> Weight {
    // Initialize the weight with one read operation.
    let mut weight = T::DbWeight::get().reads(1);

    // Iterate through all staking hotkeys.
    for (coldkey, hotkey_vec) in StakingHotkeys::<T>::iter() {
        // Init the zero value.
        let mut coldkey_stake_sum: u64 = 0;
        weight = weight.saturating_add(T::DbWeight::get().reads(1));

        // Calculate the total stake for the current coldkey.
        for hotkey in hotkey_vec {
            // Cant fail on retrieval.
            coldkey_stake_sum =
                coldkey_stake_sum.saturating_add(Stake::<T>::get(hotkey, coldkey.clone()));
            weight = weight.saturating_add(T::DbWeight::get().reads(1));
        }
        // Update the `TotalColdkeyStake` storage with the calculated stake sum.
        // Cant fail on insert.
        // TotalColdkeyStake::<T>::insert(coldkey.clone(), coldkey_stake_sum);
        // weight = weight.saturating_add(T::DbWeight::get().writes(1));
    }
    weight
}
// Public migrate function to be called by Lib.rs on upgrade.
pub fn migrate_fix_total_coldkey_stake<T: Config>() -> Weight {
    let migration_name = b"fix_total_coldkey_stake_v7".to_vec();

    // Initialize the weight with one read operation.
    let mut weight = T::DbWeight::get().reads(1);

    // Check if the migration has already run
    if HasMigrationRun::<T>::get(&migration_name) {
        log::info!(
            "Migration '{:?}' has already run. Skipping.",
            migration_name
        );
        return Weight::zero();
    }

    log::info!(
        "Running migration '{}'",
        String::from_utf8_lossy(&migration_name)
    );

    // Run the migration
    weight = weight.saturating_add(do_migrate_fix_total_coldkey_stake::<T>());

    // Mark the migration as completed
    HasMigrationRun::<T>::insert(&migration_name, true);
    weight = weight.saturating_add(T::DbWeight::get().writes(1));

    // Set the storage version to 7
    StorageVersion::new(7).put::<Pallet<T>>();
    weight = weight.saturating_add(T::DbWeight::get().writes(1));

    log::info!(
        "Migration '{:?}' completed. Storage version set to 7.",
        String::from_utf8_lossy(&migration_name)
    );

    // Return the migration weight.
    weight
}
