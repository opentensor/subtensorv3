#![allow(unused, clippy::indexing_slicing, clippy::panic, clippy::unwrap_used)]

use super::mock::*;
use crate::*;
use codec::{Decode, Encode};
use frame_support::{
    assert_ok,
    storage::unhashed::{get_raw, put_raw},
    traits::{StorageInstance, StoredMap},
    weights::Weight,
    StorageHasher, Twox64Concat,
};
use frame_system::Config;
use sp_core::{crypto::Ss58Codec, H256, U256};
use sp_io::hashing::twox_128;
use sp_runtime::traits::Zero;
use substrate_fixed::types::extra::U2;

#[test]
fn test_initialise_ti() {
    use frame_support::traits::OnRuntimeUpgrade;

    new_test_ext(1).execute_with(|| {
        crate::SubnetLocked::<Test>::insert(1, 100);
        crate::SubnetLocked::<Test>::insert(2, 5);
        pallet_balances::TotalIssuance::<Test>::put(1000);
        crate::TotalStake::<Test>::put(25);

        // Ensure values are NOT initialized prior to running migration
        assert!(crate::TotalIssuance::<Test>::get() == 0);

        crate::migrations::migrate_init_total_issuance::initialise_total_issuance::Migration::<Test>::on_runtime_upgrade();

        // Ensure values were initialized correctly
        assert!(
            crate::TotalIssuance::<Test>::get()
                == 105u64.saturating_add(1000).saturating_add(25)
        );
    });
}

// #[test]
// fn test_migration_fix_total_stake_maps() {
//     new_test_ext(1).execute_with(|| {

//         let ck1 = U256::from(1);
//         let ck2 = U256::from(2);
//         let ck3 = U256::from(3);

//         let hk1 = U256::from(1 + 100);
//         let hk2 = U256::from(2 + 100);

//         let mut total_stake_amount = 0;

//         // Give each coldkey some stake in the maps
//         SubtensorModule::increase_stake_on_coldkey_hotkey_account(&ck1, &hk1, 100);
//         total_stake_amount += 100;

//         SubtensorModule::increase_stake_on_coldkey_hotkey_account(&ck2, &hk1, 10_101);
//         total_stake_amount += 10_101;

//         SubtensorModule::increase_stake_on_coldkey_hotkey_account(&ck3, &hk2, 100_000_000);
//         total_stake_amount += 100_000_000;

//         SubtensorModule::increase_stake_on_coldkey_hotkey_account(&ck1, &hk2, 1_123_000_000);
//         total_stake_amount += 1_123_000_000;

//         // Check that the total stake is correct
//         assert_eq!(SubtensorModule::get_total_stake(), total_stake_amount);

//         // Check that the total coldkey stake is correct
//         assert_eq!(
//             SubtensorModule::get_total_stake_for_coldkey(&ck1),
//             100 + 1_123_000_000
//         );
//         assert_eq!(SubtensorModule::get_total_stake_for_coldkey(&ck2), 10_101);
//         assert_eq!(
//             SubtensorModule::get_total_stake_for_coldkey(&ck3),
//             100_000_000
//         );

//         // Check that the total hotkey stake is correct
//         assert_eq!(
//             SubtensorModule::get_total_stake_for_hotkey(&hk1),
//             100 + 10_101
//         );
//         assert_eq!(
//             SubtensorModule::get_total_stake_for_hotkey(&hk2),
//             100_000_000 + 1_123_000_000
//         );

//         // Mess up the total coldkey stake
//         crate::TotalColdkeyStake::<Test>::insert(ck1, 0);
//         // Verify that the total coldkey stake is now 0 for ck1
//         assert_eq!(SubtensorModule::get_total_stake_for_coldkey(&ck1), 0);

//         // Mess up the total stake
//         crate::TotalStake::<Test>::put(123_456_789);
//         // Verify that the total stake is now wrong
//         assert_ne!(SubtensorModule::get_total_stake(), total_stake_amount);

//         // Run the migration to fix the total stake maps
//         crate::migrations::migrate_to_v2_fixed_total_stake::migrate_to_v2_fixed_total_stake::<Test>(
//         );

//         // Verify that the total stake is now correct
//         assert_eq!(SubtensorModule::get_total_stake(), total_stake_amount);
//         // Verify that the total coldkey stake is now correct for each coldkey
//         assert_eq!(
//             SubtensorModule::get_total_stake_for_coldkey(&ck1),
//             100 + 1_123_000_000
//         );
//         assert_eq!(SubtensorModule::get_total_stake_for_coldkey(&ck2), 10_101);
//         assert_eq!(
//             SubtensorModule::get_total_stake_for_coldkey(&ck3),
//             100_000_000
//         );

//         // Verify that the total hotkey stake is STILL correct for each hotkey
//         assert_eq!(
//             SubtensorModule::get_total_stake_for_hotkey(&hk1),
//             100 + 10_101
//         );
//         assert_eq!(
//             SubtensorModule::get_total_stake_for_hotkey(&hk2),
//             100_000_000 + 1_123_000_000
//         );

//         // Verify that the Stake map has no extra entries
//         assert_eq!(crate::Stake::<Test>::iter().count(), 4); // 4 entries total
//         assert_eq!(crate::Stake::<Test>::iter_key_prefix(hk1).count(), 2); // 2 stake entries for hk1
//         assert_eq!(crate::Stake::<Test>::iter_key_prefix(hk2).count(), 2); // 2 stake entries for hk2
//     })
// }

// #[test]
// // To run this test with cargo, use the following command:
// // cargo test --package pallet-subtensor --test migration test_migrate_total_issuance
// fn test_migrate_total_issuance() {
//     new_test_ext(1).execute_with(|| {
//         // Run the migration to check total issuance.
//         let test: bool = true;

//         assert_eq!(SubtensorModule::get_total_issuance(), 0);
//         crate::migrations::migrate_total_issuance::migrate_total_issuance::<Test>(test);
//         assert_eq!(SubtensorModule::get_total_issuance(), 0);

//         SubtensorModule::add_balance_to_coldkey_account(&U256::from(1), 10000);
//         assert_eq!(SubtensorModule::get_total_issuance(), 0);
//         crate::migrations::migrate_total_issuance::migrate_total_issuance::<Test>(test);
//         assert_eq!(SubtensorModule::get_total_issuance(), 10000);

//         SubtensorModule::increase_stake_on_coldkey_hotkey_account(
//             &U256::from(1),
//             &U256::from(1),
//             30000,
//         );
//         assert_eq!(SubtensorModule::get_total_issuance(), 10000);
//         crate::migrations::migrate_total_issuance::migrate_total_issuance::<Test>(test);
//         assert_eq!(SubtensorModule::get_total_issuance(), 10000 + 30000);
//     })
// }

//#[test]
// To run this test with cargo, use the following command:
// cargo test --package pallet-subtensor --test migration test_total_issuance_global
// fn test_total_issuance_global() {
//     new_test_ext(0).execute_with(|| {

//         // Initialize network unique identifier and keys for testing.
//         let netuid: u16 = 1; // Network unique identifier set to 1 for testing.
//         let coldkey = U256::from(0); // Coldkey initialized to 0, representing an account's public key for non-transactional operations.
//         let hotkey = U256::from(0); // Hotkey initialized to 0, representing an account's public key for transactional operations.
//         let owner: U256 = U256::from(0);

//         let lockcost: u64 = SubtensorModule::get_network_lock_cost();
//         SubtensorModule::add_balance_to_coldkey_account(&owner, lockcost); // Add a balance of 20000 to the coldkey account.
//         assert_eq!(SubtensorModule::get_total_issuance(), 0); // initial is zero.
//         assert_ok!(SubtensorModule::register_network(
//             <<Test as Config>::RuntimeOrigin>::signed(owner),
//         ));
//         SubtensorModule::set_max_allowed_uids(netuid, 1); // Set the maximum allowed unique identifiers for the network to 1.
//         assert_eq!(SubtensorModule::get_total_issuance(), 0); // initial is zero.
//         crate::migrations::migrate_total_issuance::migrate_total_issuance::<Test>(true); // Pick up lock.
//         assert_eq!(SubtensorModule::get_total_issuance(), lockcost); // Verify the total issuance is updated to 20000 after migration.
//         assert!(SubtensorModule::if_subnet_exist(netuid));

//         // Test the migration's effect on total issuance after adding balance to a coldkey account.
//         let account_balance: u64 = 20000;
//         let _hotkey_account_id_1 = U256::from(1); // Define a hotkey account ID for further operations.
//         let _coldkey_account_id_1 = U256::from(1); // Define a coldkey account ID for further operations.
//         assert_eq!(SubtensorModule::get_total_issuance(), lockcost); // Ensure the total issuance starts at 0 before the migration.
//         SubtensorModule::add_balance_to_coldkey_account(&coldkey, account_balance); // Add a balance of 20000 to the coldkey account.
//         crate::migrations::migrate_total_issuance::migrate_total_issuance::<Test>(true); // Execute the migration to update total issuance.
//         assert_eq!(
//             SubtensorModule::get_total_issuance(),
//             account_balance + lockcost
//         ); // Verify the total issuance is updated to 20000 after migration.

//         // Test the effect of burning on total issuance.
//         let burn_cost: u64 = 10000;
//         SubtensorModule::set_burn(netuid, burn_cost); // Set the burn amount to 10000 for the network.
//         assert_eq!(
//             SubtensorModule::get_total_issuance(),
//             account_balance + lockcost
//         ); // Confirm the total issuance remains 20000 before burning.
//         assert_ok!(SubtensorModule::burned_register(
//             <<Test as Config>::RuntimeOrigin>::signed(hotkey),
//             netuid,
//             hotkey
//         )); // Execute the burn operation, reducing the total issuance.
//         assert_eq!(SubtensorModule::get_subnetwork_n(netuid), 1); // Ensure the subnetwork count increases to 1 after burning
//         assert_eq!(
//             SubtensorModule::get_total_issuance(),
//             account_balance + lockcost - burn_cost
//         ); // Verify the total issuance is reduced to 10000 after burning.
//         crate::migrations::migrate_total_issuance::migrate_total_issuance::<Test>(true); // Execute the migration to update total issuance.
//         assert_eq!(
//             SubtensorModule::get_total_issuance(),
//             account_balance + lockcost - burn_cost
//         ); // Verify the total issuance is updated to 10000 nothing changes

//         // Test staking functionality and its effect on total issuance.
//         let new_stake: u64 = 10000;
//         assert_eq!(
//             SubtensorModule::get_total_issuance(),
//             account_balance + lockcost - burn_cost
//         ); // Same
//         SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey, &hotkey, new_stake); // Stake an additional 10000 to the coldkey-hotkey account. This is i
//         assert_eq!(
//             SubtensorModule::get_total_issuance(),
//             account_balance + lockcost - burn_cost
//         ); // Same
//         crate::migrations::migrate_total_issuance::migrate_total_issuance::<Test>(true); // Fix issuance
//         assert_eq!(
//             SubtensorModule::get_total_issuance(),
//             account_balance + lockcost - burn_cost + new_stake
//         ); // New

//         // Set emission values for the network and verify.
//         let emission: u64 = 1_000_000_000;
//         SubtensorModule::set_tempo(netuid, 1);
//         SubtensorModule::set_emission_values(&[netuid], vec![emission]).unwrap(); // Set the emission value for the network to 1_000_000_000.
//         assert_eq!(SubtensorModule::get_subnet_emission_value(netuid), emission); // Verify the emission value is set correctly for the network.
//         assert_eq!(
//             SubtensorModule::get_total_issuance(),
//             account_balance + lockcost - burn_cost + new_stake
//         );
//         run_to_block(2); // Advance to block number 2 to trigger the emission through the subnet.
//         assert_eq!(
//             SubtensorModule::get_total_issuance(),
//             account_balance + lockcost - burn_cost + new_stake + emission
//         ); // Verify the total issuance reflects the staked amount and emission value that has been put through the epoch.
//         crate::migrations::migrate_total_issuance::migrate_total_issuance::<Test>(true); // Test migration does not change amount.
//         assert_eq!(
//             SubtensorModule::get_total_issuance(),
//             account_balance + lockcost - burn_cost + new_stake + emission
//         ); // Verify the total issuance reflects the staked amount and emission value that has been put through the epoch.
//     })
// }

#[test]
fn test_migration_transfer_nets_to_foundation() {
    new_test_ext(1).execute_with(|| {
        // Create subnet 1
        add_network(1, 1, 0);
        // Create subnet 11
        add_network(11, 1, 0);

        log::info!("{:?}", SubtensorModule::get_subnet_owner(1));
        //assert_eq!(SubtensorModule::<T>::get_subnet_owner(1), );

        // Run the migration to transfer ownership
        let hex =
            hex_literal::hex!["feabaafee293d3b76dae304e2f9d885f77d2b17adab9e17e921b321eccd61c77"];
        crate::migrations::migrate_transfer_ownership_to_foundation::migrate_transfer_ownership_to_foundation::<Test>(hex);

        log::info!("new owner: {:?}", SubtensorModule::get_subnet_owner(1));
    })
}

#[test]
fn test_migration_delete_subnet_3() {
    new_test_ext(1).execute_with(|| {
        // Create subnet 3
        add_network(3, 1, 0);
        assert!(SubtensorModule::if_subnet_exist(3));

        // Run the migration to transfer ownership
        crate::migrations::migrate_delete_subnet_3::migrate_delete_subnet_3::<Test>();

        assert!(!SubtensorModule::if_subnet_exist(3));
    })
}

#[test]
fn test_migration_delete_subnet_21() {
    new_test_ext(1).execute_with(|| {
        // Create subnet 21
        add_network(21, 1, 0);
        assert!(SubtensorModule::if_subnet_exist(21));

        // Run the migration to transfer ownership
        crate::migrations::migrate_delete_subnet_21::migrate_delete_subnet_21::<Test>();

        assert!(!SubtensorModule::if_subnet_exist(21));
    })
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test migration -- test_migrate_fix_total_coldkey_stake --exact --nocapture
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test migration -- test_migrate_fix_total_coldkey_stake --exact --nocapture
// Deprecated
// #[test]
// fn test_migrate_fix_total_coldkey_stake() {
//     new_test_ext(1).execute_with(|| {
//         assert!(false);

//         let _migration_name = "fix_total_coldkey_stake_v7";
//         let coldkey = U256::from(0);
//         TotalColdkeyStake::<Test>::insert(coldkey, 0);
//         StakingHotkeys::<Test>::insert(coldkey, vec![U256::from(1), U256::from(2), U256::from(3)]);
//         Stake::<Test>::insert(U256::from(1), U256::from(0), 10000);
//         Stake::<Test>::insert(U256::from(2), U256::from(0), 10000);
//         Stake::<Test>::insert(U256::from(3), U256::from(0), 10000);
//         crate::migrations::migrate_fix_total_coldkey_stake::do_migrate_fix_total_coldkey_stake::<
//             Test,
//         >();
//         assert_eq!(TotalColdkeyStake::<Test>::get(coldkey), 30000);
//     })
// }

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test migration -- test_migrate_fix_total_coldkey_stake_value_already_in_total --exact --nocapture
// Deprecated
// #[test]
// fn test_migrate_fix_total_coldkey_stake_value_already_in_total() {
//     new_test_ext(1).execute_with(|| {

//         let _migration_name = "fix_total_coldkey_stake_v7";
//         let coldkey = U256::from(0);
//         TotalColdkeyStake::<Test>::insert(coldkey, 100000000);
//         StakingHotkeys::<Test>::insert(coldkey, vec![U256::from(1), U256::from(2), U256::from(3)]);
//         Stake::<Test>::insert(U256::from(1), U256::from(0), 10000);
//         Stake::<Test>::insert(U256::from(2), U256::from(0), 10000);
//         Stake::<Test>::insert(U256::from(3), U256::from(0), 10000);
//         crate::migrations::migrate_fix_total_coldkey_stake::do_migrate_fix_total_coldkey_stake::<
//             Test,
//         >();
//         assert_eq!(TotalColdkeyStake::<Test>::get(coldkey), 30000);
//     })
// }

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test migration -- test_migrate_fix_total_coldkey_stake_no_entry --exact --nocapture
// Deprecated
// #[test]
// fn test_migrate_fix_total_coldkey_stake_no_entry() {
//     new_test_ext(1).execute_with(|| {

//         let _migration_name = "fix_total_coldkey_stake_v7";
//         let coldkey = U256::from(0);
//         StakingHotkeys::<Test>::insert(coldkey, vec![U256::from(1), U256::from(2), U256::from(3)]);
//         Stake::<Test>::insert(U256::from(1), U256::from(0), 10000);
//         Stake::<Test>::insert(U256::from(2), U256::from(0), 10000);
//         Stake::<Test>::insert(U256::from(3), U256::from(0), 10000);
//         crate::migrations::migrate_fix_total_coldkey_stake::do_migrate_fix_total_coldkey_stake::<
//             Test,
//         >();
//         assert_eq!(TotalColdkeyStake::<Test>::get(coldkey), 30000);
//     })
// }

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test migration -- test_migrate_fix_total_coldkey_stake_no_entry_in_hotkeys --exact --nocapture
// Deprecated
// #[test]
// fn test_migrate_fix_total_coldkey_stake_no_entry_in_hotkeys() {
//     new_test_ext(1).execute_with(|| {
//         let _migration_name = "fix_total_coldkey_stake_v7";
//         let coldkey = U256::from(0);
//         TotalColdkeyStake::<Test>::insert(coldkey, 100000000);
//         StakingHotkeys::<Test>::insert(coldkey, vec![U256::from(1), U256::from(2), U256::from(3)]);
//         crate::migrations::migrate_fix_total_coldkey_stake::do_migrate_fix_total_coldkey_stake::<
//             Test,
//         >();
//         assert_eq!(TotalColdkeyStake::<Test>::get(coldkey), 0);
//     })
// }

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test migration -- test_migrate_fix_total_coldkey_stake_one_hotkey_stake_missing --exact --nocapture
// Deprecated
// #[test]
// fn test_migrate_fix_total_coldkey_stake_one_hotkey_stake_missing() {
//     new_test_ext(1).execute_with(|| {

//         let _migration_name = "fix_total_coldkey_stake_v7";
//         let coldkey = U256::from(0);
//         TotalColdkeyStake::<Test>::insert(coldkey, 100000000);
//         StakingHotkeys::<Test>::insert(coldkey, vec![U256::from(1), U256::from(2), U256::from(3)]);
//         Stake::<Test>::insert(U256::from(1), U256::from(0), 10000);
//         Stake::<Test>::insert(U256::from(2), U256::from(0), 10000);
//         crate::migrations::migrate_fix_total_coldkey_stake::do_migrate_fix_total_coldkey_stake::<
//             Test,
//         >();
//         assert_eq!(TotalColdkeyStake::<Test>::get(coldkey), 20000);
//     })
// }

// New test to check if migration runs only once
//  SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test migration -- test_migrate_fix_total_coldkey_stake_runs_once --exact --nocapture
// Deprecated
// #[test]
// fn test_migrate_fix_total_coldkey_stake_runs_once() {
//     new_test_ext(1).execute_with(|| {

//         let migration_name = "fix_total_coldkey_stake_v7";
//         let coldkey = U256::from(0);
//         TotalColdkeyStake::<Test>::insert(coldkey, 0);
//         StakingHotkeys::<Test>::insert(coldkey, vec![U256::from(1), U256::from(2), U256::from(3)]);
//         Stake::<Test>::insert(U256::from(1), coldkey, 10000);
//         Stake::<Test>::insert(U256::from(2), coldkey, 10000);
//         Stake::<Test>::insert(U256::from(3), coldkey, 10000);

//         // First run
//         let first_weight = run_migration_and_check(migration_name);
//         assert!(first_weight != Weight::zero());
//         assert_eq!(TotalColdkeyStake::<Test>::get(coldkey), 30000);

//         // Second run
//         let second_weight = run_migration_and_check(migration_name);
//         assert_eq!(second_weight, Weight::zero());
//         assert_eq!(TotalColdkeyStake::<Test>::get(coldkey), 30000);
//     })
// }

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test migration -- test_migrate_fix_total_coldkey_stake_starts_with_value_no_stake_map_entries --exact --nocapture
// Deprecated
// #[test]
// fn test_migrate_fix_total_coldkey_stake_starts_with_value_no_stake_map_entries() {
//     new_test_ext(1).execute_with(|| {

//         let migration_name = "fix_total_coldkey_stake_v7";
//         let coldkey = U256::from(0);
//         TotalColdkeyStake::<Test>::insert(coldkey, 123_456_789);

//         // Notably, coldkey has no stake map or staking_hotkeys map entries

//         let weight = run_migration_and_check(migration_name);
//         assert!(weight != Weight::zero());
//         // Therefore 0
//         assert_eq!(TotalColdkeyStake::<Test>::get(coldkey), 123_456_789);
//     })
// }

fn run_migration_and_check(migration_name: &'static str) -> frame_support::weights::Weight {
    // Execute the migration and store its weight
    let weight: frame_support::weights::Weight =
        crate::migrations::migrate_fix_total_coldkey_stake::migrate_fix_total_coldkey_stake::<Test>(
        );

    // Check if the migration has been marked as completed
    assert!(HasMigrationRun::<Test>::get(
        migration_name.as_bytes().to_vec()
    ));

    // Return the weight of the executed migration
    weight
}

#[test]
fn test_migrate_commit_reveal_2() {
    new_test_ext(1).execute_with(|| {
        // ------------------------------
        // Step 1: Simulate Old Storage Entries
        // ------------------------------
        const MIGRATION_NAME: &str = "migrate_commit_reveal_2_v2";

        let pallet_prefix = twox_128("SubtensorModule".as_bytes());
        let storage_prefix_interval = twox_128("WeightCommitRevealInterval".as_bytes());
        let storage_prefix_commits = twox_128("WeightCommits".as_bytes());

        let netuid: u16 = 1;
        let interval_value: u64 = 50u64;

        // Construct the full key for WeightCommitRevealInterval
        let mut interval_key = Vec::new();
        interval_key.extend_from_slice(&pallet_prefix);
        interval_key.extend_from_slice(&storage_prefix_interval);
        interval_key.extend_from_slice(&netuid.encode());

        put_raw(&interval_key, &interval_value.encode());

        let test_account: U256 = U256::from(1);

        // Construct the full key for WeightCommits (DoubleMap)
        let mut commit_key = Vec::new();
        commit_key.extend_from_slice(&pallet_prefix);
        commit_key.extend_from_slice(&storage_prefix_commits);

        // First key (netuid) hashed with Twox64Concat
        let netuid_hashed = Twox64Concat::hash(&netuid.encode());
        commit_key.extend_from_slice(&netuid_hashed);

        // Second key (account) hashed with Twox64Concat
        let account_hashed = Twox64Concat::hash(&test_account.encode());
        commit_key.extend_from_slice(&account_hashed);

        let commit_value: (H256, u64) = (H256::from_low_u64_be(42), 100);
        put_raw(&commit_key, &commit_value.encode());

        let stored_interval = get_raw(&interval_key).expect("Expected to get a value");
        assert_eq!(
            u64::decode(&mut &stored_interval[..]).expect("Failed to decode interval value"),
            interval_value
        );

        let stored_commit = get_raw(&commit_key).expect("Expected to get a value");
        assert_eq!(
            <(H256, u64)>::decode(&mut &stored_commit[..]).expect("Failed to decode commit value"),
            commit_value
        );

        assert!(
            !HasMigrationRun::<Test>::get(MIGRATION_NAME.as_bytes().to_vec()),
            "Migration should not have run yet"
        );

        // ------------------------------
        // Step 2: Run the Migration
        // ------------------------------
        let weight = crate::migrations::migrate_commit_reveal_v2::migrate_commit_reveal_2::<Test>();

        assert!(
            HasMigrationRun::<Test>::get(MIGRATION_NAME.as_bytes().to_vec()),
            "Migration should be marked as run"
        );

        // ------------------------------
        // Step 3: Verify Migration Effects
        // ------------------------------
        let stored_interval_after = get_raw(&interval_key);
        assert!(
            stored_interval_after.is_none(),
            "WeightCommitRevealInterval should be cleared"
        );

        let stored_commit_after = get_raw(&commit_key);
        assert!(
            stored_commit_after.is_none(),
            "WeightCommits entry should be cleared"
        );

        assert!(!weight.is_zero(), "Migration weight should be non-zero");
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --workspace --test migration -- test_migrate_rao --exact --nocapture
#[test]
fn test_migrate_rao() {
    new_test_ext(1).execute_with(|| {
        // Setup initial state
        let netuid_0: u16 = 0;
        let netuid_1: u16 = 1;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey1 = U256::from(3);
        let coldkey2 = U256::from(4);
        let coldkey3 = U256::from(5);
        let stake_amount: u64 = 1_000_000_000;
        let lock_amount: u64 = 500;

        // Add networks root and alpha
        add_network(netuid_0, 1, 0);
        add_network(netuid_1, 1, 0);

        // Set subnet lock
        SubnetLocked::<Test>::insert(netuid_1, lock_amount);

        // Add some initial stake
        Owner::<Test>::insert(hotkey1, coldkey1);
        Owner::<Test>::insert(hotkey2, coldkey2);
        Stake::<Test>::insert(hotkey1, coldkey1, stake_amount);
        Stake::<Test>::insert(hotkey1, coldkey2, stake_amount);
        Stake::<Test>::insert(hotkey2, coldkey2, stake_amount);
        Stake::<Test>::insert(hotkey2, coldkey3, stake_amount);

        // Verify initial conditions
        assert_eq!(SubnetTAO::<Test>::get(netuid_0), 0);
        assert_eq!(SubnetTAO::<Test>::get(netuid_1), 0);
        assert_eq!(SubnetAlphaOut::<Test>::get(netuid_0), 0);
        assert_eq!(SubnetAlphaOut::<Test>::get(netuid_1), 0);
        assert_eq!(SubnetAlphaIn::<Test>::get(netuid_0), 0);
        assert_eq!(SubnetAlphaIn::<Test>::get(netuid_1), 0);
        assert_eq!(TotalHotkeyShares::<Test>::get(hotkey1, netuid_0), 0);
        assert_eq!(TotalHotkeyShares::<Test>::get(hotkey1, netuid_1), 0);
        assert_eq!(TotalHotkeyAlpha::<Test>::get(hotkey1, netuid_0), 0);
        assert_eq!(TotalHotkeyAlpha::<Test>::get(hotkey2, netuid_1), 0);

        // Run migration
        crate::migrations::migrate_rao::migrate_rao::<Test>();

        // Verify root subnet (netuid 0) state after migration
        assert_eq!(SubnetTAO::<Test>::get(netuid_0), 4 * stake_amount); // Root has everything
        assert_eq!(SubnetTAO::<Test>::get(netuid_1), 100_000_000_000); // Initial Rao amount.
        assert_eq!(SubnetAlphaIn::<Test>::get(netuid_0), 1); // No Alpha in pool on root.
        assert_eq!(SubnetAlphaIn::<Test>::get(netuid_1), 100_000_000_000); // Initial Rao amount.
        assert_eq!(SubnetAlphaOut::<Test>::get(netuid_0), 4 * stake_amount); // All stake is outstanding.
        assert_eq!(SubnetAlphaOut::<Test>::get(netuid_1), 0); // No stake outstanding.

        // Assert share information for hotkey1 on netuid_0
        assert_eq!(
            TotalHotkeyShares::<Test>::get(hotkey1, netuid_0),
            2 * stake_amount
        ); // Shares
           // Assert no shares for hotkey1 on netuid_1
        assert_eq!(TotalHotkeyShares::<Test>::get(hotkey1, netuid_1), 0); // No shares
                                                                          // Assert alpha for hotkey1 on netuid_0
        assert_eq!(
            TotalHotkeyAlpha::<Test>::get(hotkey1, netuid_0),
            2 * stake_amount
        ); // Alpha
           // Assert no alpha for hotkey1 on netuid_1
        assert_eq!(TotalHotkeyAlpha::<Test>::get(hotkey1, netuid_1), 0); // No alpha.
                                                                         // Assert share information for hotkey2 on netuid_0
        assert_eq!(
            TotalHotkeyShares::<Test>::get(hotkey2, netuid_0),
            2 * stake_amount
        ); // Shares
           // Assert no shares for hotkey2 on netuid_1
        assert_eq!(TotalHotkeyShares::<Test>::get(hotkey2, netuid_1), 0); // No shares
                                                                          // Assert alpha for hotkey2 on netuid_0
        assert_eq!(
            TotalHotkeyAlpha::<Test>::get(hotkey2, netuid_0),
            2 * stake_amount
        ); // Alpha
           // Assert no alpha for hotkey2 on netuid_1
        assert_eq!(TotalHotkeyAlpha::<Test>::get(hotkey2, netuid_1), 0); // No alpha.

        // Assert stake balances for hotkey1 and coldkey1 on netuid_0
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &hotkey1, &coldkey1, netuid_0
            ),
            stake_amount
        );
        // Assert stake balances for hotkey1 and coldkey2 on netuid_0
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &hotkey1, &coldkey2, netuid_0
            ),
            stake_amount
        );
        // Assert stake balances for hotkey2 and coldkey2 on netuid_0
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &hotkey2, &coldkey2, netuid_0
            ),
            stake_amount
        );
        // Assert stake balances for hotkey2 and coldkey3 on netuid_0
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &hotkey2, &coldkey3, netuid_0
            ),
            stake_amount
        );
        // Assert total stake for hotkey1 on netuid_0
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey1, netuid_0),
            2 * stake_amount
        );
        // Assert total stake for hotkey2 on netuid_0
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey2, netuid_0),
            2 * stake_amount
        );
        // Increase stake for hotkey1 and coldkey1 on netuid_0
        SubtensorModule::increase_stake_for_hotkey_and_coldkey_on_subnet(
            &hotkey1,
            &coldkey1,
            netuid_0,
            stake_amount,
        );
        // Assert updated stake for hotkey1 and coldkey1 on netuid_0
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &hotkey1, &coldkey1, netuid_0
            ),
            2 * stake_amount
        );
        // Assert updated total stake for hotkey1 on netuid_0
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey1, netuid_0),
            3 * stake_amount
        );
        // Increase stake for hotkey1 and coldkey1 on netuid_1
        SubtensorModule::increase_stake_for_hotkey_and_coldkey_on_subnet(
            &hotkey1,
            &coldkey1,
            netuid_1,
            stake_amount,
        );
        // Assert updated stake for hotkey1 and coldkey1 on netuid_1
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &hotkey1, &coldkey1, netuid_1
            ),
            stake_amount
        );
        // Assert updated total stake for hotkey1 on netuid_1
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey1, netuid_1),
            stake_amount
        );
    });
}
