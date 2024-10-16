use frame_support::pallet_macros::pallet_section;

/// A [`pallet_section`] that defines the events for a pallet.
/// This can later be imported into the pallet using [`import_section`].
#[pallet_section]
mod hooks {
    // ================
    // ==== Hooks =====
    // ================
    #[pallet::hooks]
    impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {
        // ---- Called on the initialization of this pallet. (the order of on_finalize calls is determined in the runtime)
        //
        // # Args:
        // 	* 'n': (BlockNumberFor<T>):
        // 		- The number of the block we are initializing.
        fn on_initialize(_block_number: BlockNumberFor<T>) -> Weight {
            let block_step_result = Self::block_step();
            match block_step_result {
                Ok(_) => {
                    // --- If the block step was successful, return the weight.
                    log::debug!("Successfully ran block step.");
                    Weight::from_parts(110_634_229_000_u64, 0)
                        .saturating_add(T::DbWeight::get().reads(8304_u64))
                        .saturating_add(T::DbWeight::get().writes(110_u64))
                }
                Err(e) => {
                    // --- If the block step was unsuccessful, return the weight anyway.
                    log::error!("Error while stepping block: {:?}", e);
                    Weight::from_parts(110_634_229_000_u64, 0)
                        .saturating_add(T::DbWeight::get().reads(8304_u64))
                        .saturating_add(T::DbWeight::get().writes(110_u64))
                }
            }
        }

        fn on_runtime_upgrade() -> frame_support::weights::Weight {
            // --- Migrate storage
            let mut weight = frame_support::weights::Weight::from_parts(0, 0);

            // Hex encoded foundation coldkey
            let hex = hex_literal::hex![
                "feabaafee293d3b76dae304e2f9d885f77d2b17adab9e17e921b321eccd61c77"
            ];
            weight = weight
                // Initializes storage version (to 1)
                .saturating_add(migrations::migrate_to_v1_separate_emission::migrate_to_v1_separate_emission::<T>())
                // Storage version v1 -> v2
                .saturating_add(migrations::migrate_to_v2_fixed_total_stake::migrate_to_v2_fixed_total_stake::<T>())
                // Doesn't check storage version. TODO: Remove after upgrade
                .saturating_add(migrations::migrate_create_root_network::migrate_create_root_network::<T>())
                // Storage version v2 -> v3
                .saturating_add(migrations::migrate_transfer_ownership_to_foundation::migrate_transfer_ownership_to_foundation::<T>(
                    hex,
                ))
                // Storage version v3 -> v4
                .saturating_add(migrations::migrate_delete_subnet_21::migrate_delete_subnet_21::<T>())
                // Storage version v4 -> v5
                .saturating_add(migrations::migrate_delete_subnet_3::migrate_delete_subnet_3::<T>())
                // Doesn't check storage version. TODO: Remove after upgrade
                // Storage version v5 -> v6
                .saturating_add(migrations::migrate_total_issuance::migrate_total_issuance::<T>(false))
                // Populate OwnedHotkeys map for coldkey swap. Doesn't update storage vesion.
                // Storage version v6 -> v7
                .saturating_add(migrations::migrate_populate_owned_hotkeys::migrate_populate_owned::<T>())
                // Populate StakingHotkeys map for coldkey swap. Doesn't update storage vesion.
                // Storage version v7 -> v8
                .saturating_add(migrations::migrate_populate_staking_hotkeys::migrate_populate_staking_hotkeys::<T>())
                // Fix total coldkey stake.
                // Storage version v8 -> v9
                .saturating_add(migrations::migrate_fix_total_coldkey_stake::migrate_fix_total_coldkey_stake::<T>())
                // Migrate Delegate Ids on chain
                .saturating_add(migrations::migrate_chain_identity::migrate_set_hotkey_identities::<T>())
                // Migration to fix typos in storage items
                .saturating_add(migrations::migrate_fix_storage_typos::migrate_rename_storage_items::<T>());
            weight
        }

        #[cfg(feature = "try-runtime")]
        fn try_state(_n: BlockNumberFor<T>) -> Result<(), sp_runtime::TryRuntimeError> {
            Self::check_accounting_invariants()?;
            Ok(())
        }
    }
}
