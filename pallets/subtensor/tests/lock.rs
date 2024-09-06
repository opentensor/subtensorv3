#![allow(
    clippy::arithmetic_side_effects,
    clippy::unwrap_used,
    clippy::indexing_slicing
)]
mod mock;
use frame_support::{assert_noop, assert_ok};
use mock::*;
use pallet_subtensor::*;
use sp_core::U256;
use sp_runtime::DispatchError;
use substrate_fixed::types::I96F32;

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_success --exact --nocapture
#[test]
fn test_do_lock_success() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let stake_amount = 500_000_000;
        let lock_amount = 250_000_000;
        let lock_duration = 7200 * 30; // 30 days

        // Set up initial balance and stake
        add_network(netuid, 0, 0);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, 1_000_000_000);
        register_ok_neuron(netuid, hotkey, coldkey, 11);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            stake_amount
        ));
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration,
            lock_amount
        ));

        // Check that the lock was created correctly
        let (locked_amount, start_block, end_block) = Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(locked_amount, lock_amount);
        assert_eq!(end_block, start_block + lock_duration);

        // Verify the event was emitted
        System::assert_last_event(
            Event::LockIncreased(coldkey, hotkey, netuid, lock_amount).into(),
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_subnet_not_exists --exact --nocapture
#[test]
fn test_do_lock_subnet_not_exists() {
    new_test_ext(1).execute_with(|| {
        let non_existent_netuid = 99;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let lock_amount = 250_000_000;
        let lock_duration = 7200 * 30; // 30 days

        // Attempt to lock stake on a non-existent subnet
        assert_noop!(
            SubtensorModule::lock_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                non_existent_netuid,
                lock_duration,
                lock_amount
            ),
            Error::<Test>::SubnetNotExists
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_hotkey_not_exists --exact --nocapture
#[test]
fn test_do_lock_hotkey_not_exists() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let non_existent_hotkey = U256::from(99);
        let lock_amount = 250_000_000;
        let lock_duration = 7200 * 30; // 30 days

        // Set up network
        add_network(netuid, 0, 0);

        // Attempt to lock stake with a non-existent hotkey
        assert_noop!(
            SubtensorModule::lock_stake(
                RuntimeOrigin::signed(coldkey),
                non_existent_hotkey,
                netuid,
                lock_duration,
                lock_amount
            ),
            Error::<Test>::HotKeyAccountNotExists
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_hotkey_not_registered --exact --nocapture
// DEPRECATED.
// #[test]
// fn test_do_lock_hotkey_not_registered() {
//     new_test_ext(1).execute_with(|| {
//         let netuid1 = 1;
//         let netuid2 = 2;
//         let coldkey = U256::from(1);
//         let hotkey = U256::from(2);
//         let lock_amount = 250_000_000;
//         let lock_duration = 7200 * 30; // 30 days

//         // Set up network
//         add_network(netuid1, 0, 0);
//         add_network(netuid2, 0, 0);
//         // Make hotkey exist.
//         register_ok_neuron(netuid2, hotkey, coldkey, 11);

//         // Add balance to coldkey
//         SubtensorModule::add_balance_to_coldkey_account(&coldkey, 1_000_000_000);

//         // Attempt to lock stake with an unregistered hotkey
//         assert_noop!(
//             SubtensorModule::lock_stake(
//                 RuntimeOrigin::signed(coldkey),
//                 hotkey,
//                 netuid1,
//                 lock_duration,
//                 lock_amount
//             ),
//             Error::<Test>::HotKeyNotRegisteredInSubNet
//         );
//     });
// }

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_zero_amount --exact --nocapture
#[test]
fn test_do_lock_zero_amount() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let lock_amount = 0;
        let lock_duration = 7200 * 30; // 30 days

        // Set up network and register neuron
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);

        // Add balance to coldkey
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, 1_000_000_000);

        // Attempt to lock zero stake
        assert_noop!(
            SubtensorModule::lock_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                lock_duration,
                lock_amount
            ),
            Error::<Test>::NotEnoughStakeToWithdraw
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_insufficient_stake --exact --nocapture
#[test]
fn test_do_lock_insufficient_stake() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let stake_amount = 500_000_000;
        let lock_amount = 750_000_000; // More than available stake
        let lock_duration = 7200 * 30; // 30 days

        // Set up network and register neuron
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);

        // Add balance to coldkey and stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, 1_000_000_000);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            stake_amount
        ));

        // Attempt to lock more stake than available
        assert_noop!(
            SubtensorModule::lock_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                lock_duration,
                lock_amount
            ),
            Error::<Test>::NotEnoughStakeToWithdraw
        );
    });
}
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_increase_conviction --exact --nocapture
#[test]
fn test_do_lock_increase_conviction() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let initial_lock_amount = 500_000_000;
        let initial_lock_duration = 7200 * 30; // 30 days
        let new_lock_amount = 750_000_000;
        let new_lock_duration = 7200 * 60; // 60 days

        // Set up network and register neuron
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);

        // Add balance to coldkey and stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // Initial lock
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_lock_duration,
            initial_lock_amount
        ));

        // Increase conviction with new lock
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            new_lock_duration,
            new_lock_amount
        ));

        // Verify the new lock
        let (locked_amount, _start_block, end_block) =
            Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(locked_amount, new_lock_amount);
        assert_eq!(
            end_block,
            SubtensorModule::get_current_block_as_u64() + new_lock_duration
        );

        // Verify event emission
        System::assert_last_event(
            Event::LockIncreased(coldkey, hotkey, netuid, new_lock_amount).into(),
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_decrease_conviction --exact --nocapture
#[test]
fn test_do_lock_decrease_conviction() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let initial_lock_amount = 500_000_000;
        let initial_lock_duration = 7200 * 30; // 30 days
        let new_lock_amount = 400_000_000;
        let new_lock_duration = 7200 * 20; // 20 days

        // Set up network and register neuron
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);

        // Add balance to coldkey and stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // Initial lock
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_lock_duration,
            initial_lock_amount
        ));

        // Attempt to decrease conviction with new lock
        assert_noop!(
            SubtensorModule::lock_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                new_lock_duration,
                new_lock_amount
            ),
            Error::<Test>::NotEnoughStakeToWithdraw
        );

        // Verify the lock remains unchanged
        let (locked_amount, _, end_block) = Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(locked_amount, initial_lock_amount);
        assert_eq!(
            end_block,
            SubtensorModule::get_current_block_as_u64() + initial_lock_duration
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_max_duration --exact --nocapture
#[test]
fn test_do_lock_max_duration() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount = 500_000_000;
        let max_lock_duration = 7200 * 365; // 1 year (assuming 7200 blocks per day)

        // Set up network and register neuron
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);

        // Add balance to coldkey and stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // Lock stake for maximum duration
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            max_lock_duration,
            lock_amount
        ));

        // Verify the lock
        let (locked_amount, _start_block, end_block) =
            Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(locked_amount, lock_amount);
        assert_eq!(
            end_block,
            SubtensorModule::get_current_block_as_u64() + max_lock_duration
        );

        // Verify event emission
        System::assert_last_event(
            Event::LockIncreased(coldkey, hotkey, netuid, lock_amount).into(),
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_multiple_times --exact --nocapture
#[test]
fn test_do_lock_multiple_times() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount_1 = 300_000_000;
        let lock_amount_2 = 500_000_000;
        let lock_duration_1 = 7200 * 30; // 30 days
        let lock_duration_2 = 7200 * 60; // 60 days

        // Set up network and register neuron
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);

        // Add balance to coldkey and stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // First lock
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration_1,
            lock_amount_1
        ));

        // Verify the first lock
        let (locked_amount_1, start_block_1, end_block_1) =
            Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(locked_amount_1, lock_amount_1);
        assert_eq!(end_block_1, start_block_1 + lock_duration_1);

        // Second lock
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration_2,
            lock_amount_2
        ));

        // Verify the second lock
        let (locked_amount_2, start_block_2, end_block_2) =
            Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(locked_amount_2, lock_amount_2);
        assert_eq!(end_block_2, start_block_2 + lock_duration_2);

        // Ensure the locked amount increased
        assert!(locked_amount_2 > locked_amount_1);

        // Verify event emissions
        System::assert_has_event(
            Event::LockIncreased(coldkey, hotkey, netuid, lock_amount_1).into(),
        );
        System::assert_last_event(
            Event::LockIncreased(coldkey, hotkey, netuid, lock_amount_2).into(),
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_do_lock_different_subnets --exact --nocapture
#[test]
fn test_do_lock_different_subnets() {
    new_test_ext(1).execute_with(|| {
        let netuid1 = 1;
        let netuid2 = 2;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount_1 = 300_000_000;
        let lock_amount_2 = 500_000_000;
        let lock_duration = 7200 * 30; // 30 days

        // Set up networks and register neuron
        add_network(netuid1, 0, 0);
        add_network(netuid2, 0, 0);
        register_ok_neuron(netuid1, hotkey, coldkey, 11);
        register_ok_neuron(netuid2, hotkey, coldkey, 12);

        // Add balance to coldkey and stake on both networks
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake * 2);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid1,
            initial_stake
        ));
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid2,
            initial_stake
        ));

        // Lock stake on first subnet
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid1,
            lock_duration,
            lock_amount_1
        ));

        // Verify the lock on first subnet
        let (locked_amount_1, start_block_1, end_block_1) =
            Locks::<Test>::get((netuid1, hotkey, coldkey));
        assert_eq!(locked_amount_1, lock_amount_1);
        assert_eq!(end_block_1, start_block_1 + lock_duration);

        // Lock stake on second subnet
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid2,
            lock_duration,
            lock_amount_2
        ));

        // Verify the lock on second subnet
        let (locked_amount_2, start_block_2, end_block_2) =
            Locks::<Test>::get((netuid2, hotkey, coldkey));
        assert_eq!(locked_amount_2, lock_amount_2);
        assert_eq!(end_block_2, start_block_2 + lock_duration);

        // Ensure the locks are independent
        assert_ne!(locked_amount_1, locked_amount_2);

        // Verify event emissions
        System::assert_has_event(
            Event::LockIncreased(coldkey, hotkey, netuid1, lock_amount_1).into(),
        );
        System::assert_last_event(
            Event::LockIncreased(coldkey, hotkey, netuid2, lock_amount_2).into(),
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_remove_stake_fully_locked --exact --nocapture
#[test]
fn test_remove_stake_fully_locked() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount = initial_stake - 1;
        let lock_duration = 7200 * 30; // 30 days

        // Set up network and register neuron
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);

        // Add balance to coldkey and stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // Lock all stake
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration,
            lock_amount
        ));

        // Attempt to remove stake
        assert_noop!(
            SubtensorModule::remove_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                initial_stake
            ),
            Error::<Test>::NotEnoughStakeToWithdraw
        );

        // Verify lock remains unchanged
        let (locked_amount, _, _) = Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(locked_amount, lock_amount);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_remove_stake_partially_locked --exact --nocapture
#[test]
fn test_remove_stake_partially_locked() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount = 600_000_000;
        let lock_duration = 7200 * 30; // 30 days

        // Set up network and register neuron
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);
        SubtensorModule::set_target_stakes_per_interval(10);

        // Add balance to coldkey and stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // Lock part of the stake
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration,
            lock_amount
        ));

        // Attempt to remove unlocked portion (should succeed)
        let unlocked_amount = SubtensorModule::max_unlockable_stake(netuid, &hotkey, &coldkey);
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            unlocked_amount
        ));

        // Verify stake and lock after removal
        let stake_after_removal =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);
        assert_eq!(stake_after_removal, initial_stake - unlocked_amount - 1);

        let (locked_amount, _, _) = Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(locked_amount, lock_amount); // lock amount should not change

        // Attempt to remove more than unlocked portion (should fail)
        assert_noop!(
            SubtensorModule::remove_stake(RuntimeOrigin::signed(coldkey), hotkey, netuid, 1000), // Some random number
            Error::<Test>::NotEnoughStakeToWithdraw
        );

        // Verify stake and lock remain unchanged
        let stake_after_failed_removal =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);
        assert_eq!(
            stake_after_failed_removal,
            initial_stake - unlocked_amount - 1
        );

        let (locked_amount_after_failed_removal, _, _) =
            Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(locked_amount_after_failed_removal, lock_amount);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_remove_stake_after_lock_expiry --exact --nocapture
#[test]
fn test_remove_stake_after_lock_expiry() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount = 600_000_000;
        let lock_duration = 10; // 10 blocks

        // Set up network and register neuron
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);
        SubtensorModule::set_target_stakes_per_interval(10);

        // Add balance to coldkey and stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // Lock part of the stake
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration,
            lock_amount
        ));

        // Fast forward to just after lock expiry
        run_to_block(lock_duration + 1);

        // Attempt to remove all stake (should succeed)
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake - 1
        ));

        // Verify stake and lock after removal
        let stake_after_removal =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);
        assert_eq!(stake_after_removal, 0);

        // Verify lock is removed
        assert!(!Locks::<Test>::contains_key((netuid, hotkey, coldkey)));

        // Verify balance is returned to coldkey
        let coldkey_balance = SubtensorModule::get_coldkey_balance(&coldkey);
        assert_eq!(coldkey_balance, initial_stake);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_remove_stake_multiple_locks --exact --nocapture
#[test]
fn test_remove_stake_multiple_locks() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount_1 = 300_000_000;
        let lock_amount_2 = 400_000_000;
        let lock_duration_1 = 10; // 10 blocks
        let lock_duration_2 = 10; // 10 blocks

        // Set up network and register neuron
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);
        SubtensorModule::set_target_stakes_per_interval(10);

        // Add balance to coldkey and stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // Create two locks
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration_1,
            lock_amount_1
        )); // first lock.
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration_2,
            lock_amount_2
        )); // second replaces first.

        // Attempt to remove more stake than unlocked (should fail)
        let max_removable = SubtensorModule::max_unlockable_stake(netuid, &hotkey, &coldkey);
        assert_noop!(
            SubtensorModule::remove_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                max_removable + 1
            ),
            Error::<Test>::NotEnoughStakeToWithdraw
        );

        // Remove unlocked stake (should succeed)
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            max_removable
        ));

        // Verify remaining stake
        let remaining_stake =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);
        assert_eq!(remaining_stake, initial_stake - max_removable - 1);

        // Fast forward to after first lock expiry
        run_to_block(lock_duration_2 + 1);

        // Attempt to remove more stake than available (should fail)
        assert_noop!(
            SubtensorModule::remove_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                initial_stake
            ),
            Error::<Test>::NotEnoughStakeToWithdraw
        );

        // Verify remaining stake
        let remaining_stake =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);
        assert_eq!(remaining_stake, initial_stake - max_removable - 1);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_remove_stake_conviction_calculation --exact --nocapture
#[test]
fn test_remove_stake_conviction_calculation() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount = 500_000_000;
        let lock_duration = 10; // 10 blocks

        // Register and add stake
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);
        SubtensorModule::set_lock_interval_blocks(lock_duration);
        SubtensorModule::set_target_stakes_per_interval(10);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // Lock stake
        assert_ok!(SubtensorModule::do_lock(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration,
            lock_amount
        ));

        // Try to remove more stake than allowed by conviction
        let max_removable = SubtensorModule::max_unlockable_stake(netuid, &hotkey, &coldkey);
        assert_noop!(
            SubtensorModule::remove_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                max_removable + 1
            ),
            Error::<Test>::NotEnoughStakeToWithdraw
        );

        // Remove allowed amount of stake
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            max_removable - 1
        ));

        // Verify remaining stake
        let remaining_stake =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);
        assert_eq!(remaining_stake, initial_stake - max_removable);

        // Fast forward to just before lock expiry
        run_to_block(lock_duration - 1);

        // Try to remove all remaining stake (should fail)
        assert_noop!(
            SubtensorModule::remove_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                remaining_stake
            ),
            Error::<Test>::NotEnoughStakeToWithdraw
        );

        // Fast forward to after lock expiry
        run_to_block(lock_duration + 1);

        // Now remove all remaining stake (should succeed)
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            remaining_stake
        ));

        // Verify no stake remains
        let final_stake =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);
        assert_eq!(final_stake, 0);

        // Verify lock is removed
        assert!(!Locks::<Test>::contains_key((netuid, hotkey, coldkey)));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_remove_stake_partial_lock_removal --exact --nocapture
#[test]
fn test_remove_stake_partial_lock_removal() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount = 500_000_000;
        let lock_duration = 7200 * 30; // 30 days

        // Register and add stake
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);
        SubtensorModule::set_lock_interval_blocks(lock_duration);
        SubtensorModule::set_target_stakes_per_interval(10);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // Lock stake
        assert_ok!(SubtensorModule::do_lock(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration,
            lock_amount
        ));

        // Verify initial lock state
        let (initial_locked_amount, _, _) = Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(initial_locked_amount, lock_amount);

        // Calculate max removable stake
        let max_removable = SubtensorModule::max_unlockable_stake(netuid, &hotkey, &coldkey);
        let partial_remove_amount = max_removable / 2;

        // Remove part of the stake
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            partial_remove_amount
        ));

        // Verify remaining stake and updated lock
        let remaining_stake =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);
        assert_eq!(remaining_stake, initial_stake - partial_remove_amount - 1);

        let (updated_locked_amount, _, _) = Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(updated_locked_amount, lock_amount); // lock never changes.

        // Ensure lock still exists
        assert!(Locks::<Test>::contains_key((netuid, hotkey, coldkey)));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_remove_stake_full_lock_removal --exact --nocapture
#[test]
fn test_remove_stake_full_lock_removal() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount = 500_000_000;
        let lock_duration = 10; // 10 blocks

        // Register and add stake
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 11);
        SubtensorModule::set_lock_interval_blocks(lock_duration);
        SubtensorModule::set_target_stakes_per_interval(10);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake
        ));

        // Lock stake
        assert_ok!(SubtensorModule::do_lock(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_duration,
            lock_amount
        ));

        // Verify initial lock state
        let (initial_locked_amount, _, _) = Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(initial_locked_amount, lock_amount);

        // Fast forward to just after lock expiry
        run_to_block(lock_duration + 1);

        // Remove all stake
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            initial_stake - 1
        ));

        // Verify remaining stake
        let remaining_stake =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);
        assert_eq!(remaining_stake, 0);

        // Verify lock is removed
        assert!(!Locks::<Test>::contains_key((netuid, hotkey, coldkey)));

        // Verify balance is returned to coldkey
        let coldkey_balance = SubtensorModule::get_coldkey_balance(&coldkey);
        assert_eq!(coldkey_balance, initial_stake);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_remove_stake_across_subnets --exact --nocapture
#[test]
fn test_remove_stake_across_subnets() {
    new_test_ext(1).execute_with(|| {
        let netuid1 = 1;
        let netuid2 = 2;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let initial_stake = 1_000_000_000;
        let lock_amount_1 = 300_000_000;
        let lock_amount_2 = 400_000_000;
        let lock_duration_1 = 10; // 10 blocks
        let lock_duration_2 = 20; // 20 blocks

        // Set up networks and register neuron
        add_network(netuid1, 0, 0);
        add_network(netuid2, 0, 0);
        register_ok_neuron(netuid1, hotkey, coldkey, 11);
        register_ok_neuron(netuid2, hotkey, coldkey, 11);
        SubtensorModule::set_target_stakes_per_interval(10);

        // Add balance to coldkey and stake on both networks
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_stake * 2);
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid1,
            initial_stake
        ));
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid2,
            initial_stake
        ));

        // Create locks on both networks
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid1,
            lock_duration_1,
            lock_amount_1
        ));
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid2,
            lock_duration_2,
            lock_amount_2
        ));

        // Attempt to remove more stake than unlocked from netuid1 (should fail)
        let max_removable_1 = SubtensorModule::max_unlockable_stake(netuid1, &hotkey, &coldkey);
        assert_noop!(
            SubtensorModule::remove_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid1,
                max_removable_1 + 1
            ),
            Error::<Test>::NotEnoughStakeToWithdraw
        );

        // Remove unlocked stake from netuid1 (should succeed)
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid1,
            max_removable_1
        ));

        // Verify remaining stake on netuid1
        let remaining_stake_1 =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid1);
        assert_eq!(remaining_stake_1, initial_stake - max_removable_1);

        // Fast forward to after first lock expiry
        run_to_block(lock_duration_1 + 1);

        // Remove all stake from netuid1 (should succeed now that lock has expired)
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid1,
            remaining_stake_1
        ));

        // Verify no stake remains on netuid1
        let final_stake_1 =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid1);
        assert_eq!(final_stake_1, 0);

        // Attempt to remove stake from netuid2 (should still be partially locked)
        let max_removable_2 = SubtensorModule::max_unlockable_stake(netuid2, &hotkey, &coldkey);
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid2,
            max_removable_2
        ));

        // Verify remaining stake on netuid2
        let remaining_stake_2 =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid2);
        assert_eq!(remaining_stake_2, initial_stake - max_removable_2 - 1);

        // Fast forward to after second lock expiry
        run_to_block(lock_duration_2 + 1);

        // Remove all remaining stake from netuid2
        assert_ok!(SubtensorModule::remove_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid2,
            remaining_stake_2
        ));

        // Verify no stake remains on netuid2
        let final_stake_2 =
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid2);
        assert_eq!(final_stake_2, 0);

        // Verify locks are removed from both networks
        assert!(!Locks::<Test>::contains_key((netuid1, hotkey, coldkey)));
        assert!(!Locks::<Test>::contains_key((netuid2, hotkey, coldkey)));
    });
}

// Test names for calculate_lions_share function:

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_lions_share_empty_input --exact --nocapture
#[test]
fn test_calculate_lions_share_empty_input() {
    new_test_ext(1).execute_with(|| {
        let result = SubtensorModule::calculate_lions_share(vec![], 20);
        assert!(result.is_empty());
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_lions_share_single_conviction --exact --nocapture
#[test]
fn test_calculate_lions_share_single_conviction() {
    new_test_ext(1).execute_with(|| {
        let result = SubtensorModule::calculate_lions_share(vec![100], 20);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], I96F32::from_num(1));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_lions_share_equal_convictions --exact --nocapture
#[test]
fn test_calculate_lions_share_equal_convictions() {
    new_test_ext(1).execute_with(|| {
        let result = SubtensorModule::calculate_lions_share(vec![100, 100, 100], 20);
        assert_eq!(result.len(), 3);
        for share in result {
            assert_eq!(share, I96F32::from_num(1) / I96F32::from_num(3));
        }
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_lions_share_varied_convictions --exact --nocapture
#[test]
fn test_calculate_lions_share_varied_convictions() {
    new_test_ext(1).execute_with(|| {
        let convictions = vec![100, 200, 300, 400];
        let result = SubtensorModule::calculate_lions_share(convictions, 20);
        assert_eq!(result.len(), 4);
        // Verify that shares are in ascending order and sum to approximately 1
        assert!(result[0] < result[1] && result[1] < result[2] && result[2] < result[3]);
        let sum: I96F32 = result.iter().sum();
        assert!((sum - I96F32::from_num(1)).abs() < I96F32::from_num(0.0001));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_lions_share_zero_convictions --exact --nocapture
#[test]
fn test_calculate_lions_share_zero_convictions() {
    new_test_ext(1).execute_with(|| {
        let result = SubtensorModule::calculate_lions_share(vec![0, 0, 0], 20);
        assert_eq!(result.len(), 3);
        for share in result {
            assert_eq!(share, I96F32::from_num(0));
        }
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_lions_share_large_convictions --exact --nocapture
#[test]
fn test_calculate_lions_share_large_convictions() {
    new_test_ext(1).execute_with(|| {
        let convictions = vec![1_000_000, 2_000_000, 3_000_000];
        let result = SubtensorModule::calculate_lions_share(convictions, 20);
        assert_eq!(result.len(), 3);
        // Verify that shares are in ascending order and sum to approximately 1
        assert!(result[0] < result[1] && result[1] < result[2]);
        let sum: I96F32 = result.iter().sum();
        assert!((sum - I96F32::from_num(1)).abs() < I96F32::from_num(0.0001));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_lions_share_different_sharpness --exact --nocapture
#[test]
fn test_calculate_lions_share_different_sharpness() {
    new_test_ext(1).execute_with(|| {
        let convictions = vec![100, 200, 300, 400];

        // Test with low sharpness
        let result_low = SubtensorModule::calculate_lions_share(convictions.clone(), 5);

        // Test with high sharpness
        let result_high = SubtensorModule::calculate_lions_share(convictions, 50);

        // Verify that higher sharpness leads to more extreme distribution
        assert!(result_high[3] > result_low[3]);
        assert!(result_high[0] < result_low[0]);

        // Verify sums are still approximately 1
        let sum_low: I96F32 = result_low.iter().sum();
        let sum_high: I96F32 = result_high.iter().sum();
        assert!((sum_low - I96F32::from_num(1)).abs() < I96F32::from_num(0.0001));
        assert!((sum_high - I96F32::from_num(1)).abs() < I96F32::from_num(0.0001));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_lions_share_extreme_differences --exact --nocapture
#[test]
fn test_calculate_lions_share_extreme_differences() {
    new_test_ext(1).execute_with(|| {
        let convictions = vec![1, 1_000_000];
        let result = SubtensorModule::calculate_lions_share(convictions, 20);

        assert_eq!(result.len(), 2);

        // The share of the larger conviction should be very close to 1
        assert!(result[1] > I96F32::from_num(0.9999));

        // The share of the smaller conviction should be very close to 0
        assert!(result[0] < I96F32::from_num(0.0001));

        // Sum should still be approximately 1
        let sum: I96F32 = result.iter().sum();
        assert!((sum - I96F32::from_num(1)).abs() < I96F32::from_num(0.0001));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_lions_share_overflow_handling --exact --nocapture
#[test]
fn test_calculate_lions_share_overflow_handling() {
    new_test_ext(1).execute_with(|| {
        // Use very large numbers to test overflow handling
        let convictions = vec![u64::MAX, u64::MAX - 1, u64::MAX - 2];
        let result = SubtensorModule::calculate_lions_share(convictions, 20);

        assert_eq!(result.len(), 3);

        // Verify that shares are still in descending order
        assert!(result[0] >= result[1] && result[1] >= result[2]);

        // Verify sum is still approximately 1
        let sum: I96F32 = result.iter().sum();
        assert!((sum - I96F32::from_num(1)).abs() < I96F32::from_num(0.0001));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_lions_share_precision --exact --nocapture
#[test]
fn test_calculate_lions_share_precision() {
    new_test_ext(1).execute_with(|| {
        // Test with convictions that are close in value
        let convictions = vec![1_000_000, 1_000_001, 1_000_002];
        let result = SubtensorModule::calculate_lions_share(convictions, 20);

        // Verify that shares are in ascending order
        assert!(result[0] < result[1] && result[1] < result[2]);

        // Verify that the differences between shares are small but detectable
        assert!(result[1] - result[0] > I96F32::from_num(0.000001));
        assert!(result[2] - result[1] > I96F32::from_num(0.000001));

        // Verify sum is still approximately 1
        let sum: I96F32 = result.iter().sum();
        assert!((sum - I96F32::from_num(1)).abs() < I96F32::from_num(0.0001));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction_zero_lock_amount --exact --nocapture
#[test]
fn test_calculate_conviction_zero_lock_amount() {
    new_test_ext(1).execute_with(|| {
        let current_block = 1000;
        let end_block = 2000;
        let conviction = SubtensorModule::calculate_conviction(0, end_block, current_block);
        assert_eq!(conviction, 0);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction_zero_duration --exact --nocapture
#[test]
fn test_calculate_conviction_zero_duration() {
    new_test_ext(1).execute_with(|| {
        let current_block = 1000;
        let end_block = 1000;
        let lock_amount = 1000000;
        let conviction =
            SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
        assert_eq!(conviction, 0);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction_max_lock_amount --exact --nocapture
#[test]
fn test_calculate_conviction_max_lock_amount() {
    new_test_ext(1).execute_with(|| {
        let current_block = 1000;
        let end_block = 2000;
        let lock_amount = u64::MAX;
        let conviction =
            SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
        assert!(conviction > 0);
        assert!(conviction < u64::MAX);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction_max_duration --exact --nocapture
#[test]
fn test_calculate_conviction_max_duration() {
    new_test_ext(1).execute_with(|| {
        let current_block = 0;
        let end_block = u64::MAX;
        let lock_amount = 1000000;
        let conviction =
            SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
        assert!(conviction > 0);
        assert!(conviction <= lock_amount);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction_overflow_check --exact --nocapture
#[test]
fn test_calculate_conviction_overflow_check() {
    new_test_ext(1).execute_with(|| {
        let current_block = 0;
        let end_block = u64::MAX;
        let lock_amount = u64::MAX;
        let conviction =
            SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
        assert!(conviction > 0);
        assert!(conviction < u64::MAX);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction_precision_small_values --exact --nocapture
#[test]
fn test_calculate_conviction_precision_small_values() {
    new_test_ext(1).execute_with(|| {
        let current_block = 1000;
        let end_block = 1001;
        let lock_amount = 1;
        let conviction =
            SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
        assert!(conviction < lock_amount);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction_precision_large_values --exact --nocapture
#[test]
fn test_calculate_conviction_precision_large_values() {
    new_test_ext(1).execute_with(|| {
        let current_block = 0;
        let end_block = u64::MAX / 2;
        let lock_amount = u64::MAX / 2;
        let conviction =
            SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
        assert!(conviction > 0);
        assert!(conviction < lock_amount);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction_rounding --exact --nocapture
#[test]
fn test_calculate_conviction_rounding() {
    new_test_ext(1).execute_with(|| {
        let current_block = 1000;
        let end_block = 1100;
        let lock_amount = 1000000;
        let conviction1 =
            SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
        let conviction2 =
            SubtensorModule::calculate_conviction(lock_amount, end_block + 1, current_block);
        assert!(conviction2 >= conviction1);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction_lock_interval_boundary --exact --nocapture
#[test]
fn test_calculate_conviction_lock_interval_boundary() {
    new_test_ext(1).execute_with(|| {
        let current_block = 1000;
        let lock_interval = SubtensorModule::get_lock_interval_blocks();
        let end_block = current_block + lock_interval;
        let lock_amount = 1000000;
        let conviction =
            SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
        assert!(conviction > 0);
        assert!(conviction < lock_amount);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction_consistency --exact --nocapture
#[test]
fn test_calculate_conviction_consistency() {
    new_test_ext(1).execute_with(|| {
        let current_block = 1000;
        let end_block = 2000;
        let lock_amount = 1000000;
        let base_conviction =
            SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
        log::info!("Base conviction: {}", base_conviction);

        // Increasing lock amount
        let higher_amount_conviction =
            SubtensorModule::calculate_conviction(lock_amount + 1000, end_block, current_block);
        assert!(higher_amount_conviction > base_conviction);

        // Increasing duration
        let longer_duration_conviction =
            SubtensorModule::calculate_conviction(lock_amount, end_block + 1000, current_block);
        assert!(longer_duration_conviction > base_conviction);
    });
}

// pub fn calculate_lions_share(convictions: Vec<u64>, sharpness: u32) -> Vec<I96F32> {
//     // Handle empty convictions vector
//     if convictions.is_empty() {
//         return Vec::new();
//     }

//     // For a single conviction, return a vector with a single element of value 1
//     if convictions.len() == 1 {
//         return vec![I96F32::from_num(1)];
//     }

//     // Find the maximum conviction
//     let max_conviction = convictions.iter().max().cloned().unwrap_or(1);
//     // If the maximum conviction is zero, return a vector of zeros
//     if max_conviction == 0 {
//         return vec![I96F32::from_num(0); convictions.len()];
//     }

//     // Normalize convictions and apply exponential function
//     let mut powered_convictions: Vec<I96F32> = Vec::with_capacity(convictions.len());
//     for c in convictions.iter() {
//         let normalized = I96F32::from_num(*c) / I96F32::from_num(max_conviction);
//         // Use checked_mul to prevent overflow in exponentiation
//         let powered = exp_safe_f96(I96F32::from_num(sharpness).saturating_mul(normalized - I96F32::from_num(1)));
//         powered_convictions.push(powered);
//     }

//     // Calculate total powered conviction
//     let total_powered: I96F32 = powered_convictions.iter().sum();

//     // Handle case where total_powered is zero to avoid division by zero
//     if total_powered == I96F32::from_num(0) {
//         return vec![I96F32::from_num(0); convictions.len()];
//     }

//     // Calculate shares
//     let shares: Vec<I96F32> = powered_convictions.into_iter().map(|pc| {
//         pc / total_powered
//     }).collect();

//     shares
// }
// pub fn calculate_conviction(lock_amount: u64, end_block: u64, current_block: u64) -> u64 {
//     let lock_duration = end_block.saturating_sub(current_block);
//     let time_factor = -I96F32::from_num(lock_duration).saturating_div(I96F32::from_num(Self::get_lock_interval_blocks())); // Convert days to blocks
//     let exp_term = I96F32::from_num(1) - exp_safe_f96(I96F32::from_num(time_factor));
//     let conviction_score = I96F32::from_num(lock_amount).saturating_mul(exp_term);
//     let final_score = conviction_score.to_num::<u64>();
//     final_score
// }

// pub fn get_owning_coldkey_for_hotkey(hotkey: &T::AccountId) -> T::AccountId {
//     Owner::<T>::get(hotkey)
// }
// pub fn distribute_owner_cut(netuid: u16, amount: u64) -> u64 {
//     // Get the current block number
//     let current_block = Self::get_current_block_as_u64();

//     // Initialize variables to track total conviction and individual hotkey convictions
//     let mut total_conviction: u64 = 0;
//     let mut hotkey_convictions: BTreeMap<T::AccountId, u64> = BTreeMap::new();

//     // Calculate total conviction and individual hotkey convictions
//     for ((iter_netuid, hotkey, _), (lock_amount, _, end_block)) in Locks::<T>::iter() {
//         if iter_netuid != netuid { continue; }
//         // Calculate conviction for each lock
//         let conviction = Self::calculate_conviction(lock_amount, end_block, current_block);
//         // Add conviction to the hotkey's total
//         *hotkey_convictions.entry(hotkey).or_default() += conviction;
//         // Add to the total conviction
//         total_conviction = total_conviction.saturating_add(conviction);
//     }

//     // If there's no conviction, return the full amount
//     if total_conviction == 0 {
//         return amount;
//     }

//     // Convert convictions to a vector for the lion's share calculation
//     let convictions: Vec<u64> = hotkey_convictions.values().cloned().collect();

//     // Calculate shares using the lion's share distribution
//     let shares: Vec<I96F32> = Self::calculate_lions_share(convictions, 20);

//     // Initialize variable to track remaining amount to distribute
//     let mut remaining_amount = amount;

//     // Distribute the owner cut based on calculated shares
//     for ((hotkey, _), share) in hotkey_convictions.iter().zip(shares.iter()) {
//         // Calculate the share for this hotkey
//         let share_amount = I96F32::from_num(amount)
//             .checked_mul(*share)
//             .unwrap_or(I96F32::from_num(0))
//             .to_num::<u64>();

//         // Get the coldkey associated with this hotkey
//         let owner_coldkey = Self::get_owning_coldkey_for_hotkey(&hotkey);

//         // Emit the calculated share into the subnet for this hotkey
//         Self::emit_into_subnet(&hotkey, &owner_coldkey, netuid, share_amount);

//         // Add the share to the lock.
//         if Locks::<T>::contains_key((netuid, hotkey.clone(), owner_coldkey.clone())) {
//             let (current_lock, start_block, end_block) = Locks::<T>::get((netuid, hotkey.clone(), owner_coldkey.clone()));
//             let new_lock = current_lock.saturating_add(share_amount);
//             Locks::<T>::insert(
//             (netuid, hotkey.clone(), owner_coldkey.clone()),
//                 (new_lock, start_block, end_block)
//             );
//         }

//         // Subtract the distributed share from the remaining amount
//         remaining_amount = remaining_amount.saturating_sub(share_amount);
//     }

//     // Return any undistributed amount
//     remaining_amount
// }

// pub fn update_subnet_owner(netuid: u16) {
//     let mut max_total_conviction: I96F32 = I96F32::from_num(0.0);
//     let mut max_conviction_hotkey = None;
//     let mut hotkey_convictions = BTreeMap::new();
//     let current_block = Self::get_current_block_as_u64();

//     // Iterate through all locks in the subnet
//     for ((iter_netuid, iter_hotkey, _), (lock_amount, _, end_block)) in Locks::<T>::iter() {
//         // Skip if the subnet does not match.
//         if iter_netuid != netuid { continue; }

//         // Calculate conviction score based on lock amount and duration
//         let conviction_score = I96F32::from_num(Self::calculate_conviction(lock_amount, end_block, current_block));

//         // Accumulate conviction scores for each hotkey
//         let total_conviction = hotkey_convictions.entry(iter_hotkey.clone()).or_insert(I96F32::from_num(0));
//         *total_conviction = total_conviction.saturating_add(conviction_score);

//         // Update max conviction if current hotkey has higher total conviction
//         if *total_conviction > max_total_conviction {
//             max_total_conviction = *total_conviction;
//             max_conviction_hotkey = Some(iter_hotkey.clone());
//         }
//     }

//     // Set the total subnet Conviction.
//     SubnetLocked::<T>::insert(netuid, max_total_conviction.to_num::<u64>());

//     // Handle the case where no locks exist for the subnet
//     if hotkey_convictions.is_empty() {
//         log::warn!("No locks found for subnet {}", netuid);
//         return;
//     }

//     // Implement a minimum conviction threshold for becoming a subnet owner
//     let min_conviction_threshold = I96F32::from_num(1000); // Example threshold, adjust as needed
//     if max_total_conviction < min_conviction_threshold {
//         log::info!("No hotkey meets the minimum conviction threshold for subnet {}", netuid);
//         return;
//     }

//     // Set the subnet owner to the coldkey of the hotkey with highest conviction
//     if let Some(hotkey) = max_conviction_hotkey {
//         let owning_coldkey = Self::get_owning_coldkey_for_hotkey(&hotkey);
//         SubnetOwner::<T>::insert(netuid, owning_coldkey);
//     }

//     // Implement a tie-breaking mechanism for equal conviction scores
//     let tied_hotkeys: Vec<_> = hotkey_convictions
//         .iter()
//         .filter(|(_, &conviction)| conviction == max_total_conviction)
//         .collect();

//     if tied_hotkeys.len() > 1 {
//         // Use a deterministic method to break ties, e.g., lowest hotkey value
//         if let Some((winning_hotkey, _)) = tied_hotkeys.iter().min_by_key(|(&ref hotkey, _)| hotkey) {
//             let owning_coldkey = Self::get_owning_coldkey_for_hotkey(winning_hotkey);
//             SubnetOwner::<T>::insert(netuid, owning_coldkey);
//         }
//     }

//     // Log performance metrics for large subnets
//     if hotkey_convictions.len() > 1000 {
//         log::warn!("Large subnet {} processed with {} hotkeys", netuid, hotkey_convictions.len());
//     }
// }

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_basic --exact --nocapture
#[test]
fn test_distribute_owner_cut_basic() {
    new_test_ext(1).execute_with(|| {
        // Setup
        let netuid = 1;
        let amount_to_distribute = 1000000;
        let current_block = 100;

        // Create multiple hotkeys with different lock amounts and end blocks
        let hotkey1 = AccountId::from([1u8; 32]);
        let hotkey2 = AccountId::from([2u8; 32]);
        let hotkey3 = AccountId::from([3u8; 32]);

        let coldkey1 = AccountId::from([4u8; 32]);
        let coldkey2 = AccountId::from([5u8; 32]);
        let coldkey3 = AccountId::from([6u8; 32]);
        SubtensorModule::set_lock_interval_blocks(10);

        // Set up locks
        Locks::<Test>::insert((netuid, hotkey1, coldkey1), (500, 0, 200));
        Locks::<Test>::insert((netuid, hotkey2, coldkey2), (300, 0, 150));
        Locks::<Test>::insert((netuid, hotkey3, coldkey3), (200, 0, 300));

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey1);
        Owner::<Test>::insert(hotkey2, coldkey2);
        Owner::<Test>::insert(hotkey3, coldkey3);

        // Mock the current block
        System::set_block_number(current_block);

        // Call the distribute_owner_cut function
        let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        // Verify distribution
        assert!(
            remaining < amount_to_distribute,
            "Some amount should be distributed"
        );

        // Check that locks have been updated
        let (new_lock1, _, _) = Locks::<Test>::get((netuid, hotkey1, coldkey1));
        let (new_lock2, _, _) = Locks::<Test>::get((netuid, hotkey2, coldkey2));
        let (new_lock3, _, _) = Locks::<Test>::get((netuid, hotkey3, coldkey3));

        assert!(new_lock1 > 500, "Hotkey1's lock should increase");
        assert!(new_lock2 > 300, "Hotkey2's lock should increase");
        assert!(new_lock3 > 200, "Hotkey3's lock should increase");

        // Verify that the total distributed amount matches the initial amount minus remaining
        let total_distributed = (new_lock1 - 500) + (new_lock2 - 300) + (new_lock3 - 200);
        assert_eq!(total_distributed, amount_to_distribute - remaining);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_single_hotkey --exact --nocapture
#[test]
fn test_distribute_owner_cut_single_hotkey() {
    new_test_ext(1).execute_with(|| {
        // Setup
        let netuid = 1;
        let amount_to_distribute = 1000000;
        let current_block = 100;

        // Create a single hotkey with lock
        let hotkey = AccountId::from([1u8; 32]);
        let coldkey = AccountId::from([2u8; 32]);
        SubtensorModule::set_lock_interval_blocks(10);

        // Set up lock
        Locks::<Test>::insert((netuid, hotkey, coldkey), (500, 0, 200));

        // Set up ownership
        Owner::<Test>::insert(hotkey, coldkey);

        // Mock the current block
        System::set_block_number(current_block);

        // Call the distribute_owner_cut function
        let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        // Verify distribution
        assert_eq!(remaining, 0, "All amount should be distributed");

        // Check that lock has been updated
        let (new_lock, _, _) = Locks::<Test>::get((netuid, hotkey, coldkey));

        assert_eq!(
            new_lock,
            500 + amount_to_distribute,
            "Hotkey's lock should increase by the full amount"
        );

        // Verify that the total distributed amount matches the initial amount
        let total_distributed = new_lock - 500;
        assert_eq!(total_distributed, amount_to_distribute);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_no_stake --exact --nocapture
#[test]
fn test_distribute_owner_cut_no_stake() {
    new_test_ext(1).execute_with(|| {
        // Setup
        let netuid = 1;
        let amount_to_distribute = 1000000;
        let current_block = 100;

        // No hotkeys or locks are set up

        // Mock the current block
        System::set_block_number(current_block);

        // Call the distribute_owner_cut function
        let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        // Verify that all amount is returned as remaining
        assert_eq!(
            remaining, amount_to_distribute,
            "All amount should be returned when no stake exists"
        );

        // Verify that no locks were created or modified
        assert_eq!(Locks::<Test>::iter().count(), 0, "No locks should exist");
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_zero_amount --exact --nocapture
#[test]
fn test_distribute_owner_cut_zero_amount() {
    new_test_ext(1).execute_with(|| {
        // Setup
        let netuid = 1;
        let amount_to_distribute = 0;
        let current_block = 100;
        let hotkey = U256::from(1);
        let coldkey = U256::from(2);
        let lock_amount = 500;
        let lock_duration = 1000;

        // Set up a lock
        Locks::<Test>::insert(
            (netuid, hotkey, coldkey),
            (lock_amount, current_block, current_block + lock_duration),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey, coldkey);

        // Mock the current block
        System::set_block_number(current_block);

        // Call the distribute_owner_cut function
        let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        // Verify that all amount is returned as remaining (which is 0 in this case)
        assert_eq!(
            remaining, 0,
            "Zero amount should be returned when distributing zero"
        );

        // Check that lock has not been updated
        let (new_lock, _, _) = Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(
            new_lock, lock_amount,
            "Hotkey's lock should remain unchanged"
        );

        // Verify that no distribution occurred
        assert_eq!(new_lock, lock_amount, "Lock amount should remain the same");
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_large_amount --exact --nocapture
#[test]
fn test_distribute_owner_cut_large_amount() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let amount_to_distribute = u64::MAX;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey = U256::from(3);
        let lock_amount = 1_000_000;
        let lock_duration = 1000;

        // Set up locks
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey),
            (lock_amount, current_block, current_block + lock_duration),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey),
            (lock_amount, current_block, current_block + lock_duration),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey);
        Owner::<Test>::insert(hotkey2, coldkey);

        System::set_block_number(current_block);

        let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        // Check that distribution occurred
        let (new_lock1, _, _) = Locks::<Test>::get((netuid, hotkey1, coldkey));
        let (new_lock2, _, _) = Locks::<Test>::get((netuid, hotkey2, coldkey));

        assert!(
            new_lock1 > lock_amount,
            "Hotkey1's lock should have increased"
        );
        assert!(
            new_lock2 > lock_amount,
            "Hotkey2's lock should have increased"
        );
        assert!(
            remaining < amount_to_distribute,
            "Some amount should have been distributed"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_uneven_stakes --exact --nocapture
#[test]
fn test_distribute_owner_cut_uneven_stakes() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let amount_to_distribute = 1_000_000;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey = U256::from(3);
        let lock_amount1 = 1_000_000;
        let lock_amount2 = 100_000;
        let lock_duration = 1000;

        // Set up locks with uneven stakes
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey),
            (lock_amount1, current_block, current_block + lock_duration),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey),
            (lock_amount2, current_block, current_block + lock_duration),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey);
        Owner::<Test>::insert(hotkey2, coldkey);

        System::set_block_number(current_block);

        let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        // Check that distribution occurred proportionally
        let (new_lock1, _, _) = Locks::<Test>::get((netuid, hotkey1, coldkey));
        let (new_lock2, _, _) = Locks::<Test>::get((netuid, hotkey2, coldkey));

        assert!(
            new_lock1 - lock_amount1 > new_lock2 - lock_amount2,
            "Hotkey1 should receive a larger share"
        );
        assert!(
            remaining < amount_to_distribute,
            "Some amount should have been distributed"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_different_lock_durations --exact --nocapture
#[test]
fn test_distribute_owner_cut_different_lock_durations() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let amount_to_distribute = 1_000_000;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey = U256::from(3);
        let lock_amount = 1_000_000;
        let lock_duration1 = 2000;
        let lock_duration2 = 1000;

        // Set up locks with different durations
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey),
            (lock_amount, current_block, current_block + lock_duration1),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey),
            (lock_amount, current_block, current_block + lock_duration2),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey);
        Owner::<Test>::insert(hotkey2, coldkey);

        System::set_block_number(current_block);

        let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        // Check that distribution occurred with preference to longer lock duration
        let (new_lock1, _, _) = Locks::<Test>::get((netuid, hotkey1, coldkey));
        let (new_lock2, _, _) = Locks::<Test>::get((netuid, hotkey2, coldkey));

        assert!(
            new_lock1 - lock_amount > new_lock2 - lock_amount,
            "Hotkey1 with longer lock duration should receive a larger share"
        );
        assert!(
            remaining < amount_to_distribute,
            "Some amount should have been distributed"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_expired_locks --exact --nocapture
#[test]
fn test_distribute_owner_cut_expired_locks() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let amount_to_distribute = 1_000_000;
        let current_block = 1000;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey = U256::from(3);
        let lock_amount = 500_000;
        let active_lock_duration = 2000;
        let expired_lock_duration = 500;

        // Set up one active lock and one expired lock
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey),
            (
                lock_amount,
                current_block - 100,
                current_block + active_lock_duration,
            ),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey),
            (
                lock_amount,
                current_block - expired_lock_duration,
                current_block - 1,
            ),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey);
        Owner::<Test>::insert(hotkey2, coldkey);

        System::set_block_number(current_block);

        let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        // Check that distribution occurred only for the active lock
        let (new_lock1, _, _) = Locks::<Test>::get((netuid, hotkey1, coldkey));
        let (new_lock2, _, _) = Locks::<Test>::get((netuid, hotkey2, coldkey));

        assert!(
            new_lock1 > lock_amount,
            "Active lock should receive a share"
        );
        assert_eq!(
            new_lock2, lock_amount,
            "Expired lock should not receive a share"
        );
        assert!(
            remaining < amount_to_distribute,
            "Some amount should have been distributed"
        );
        assert_eq!(
            new_lock1 - lock_amount,
            amount_to_distribute - remaining,
            "All distributed amount should go to the active lock"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_multiple_subnets --exact --nocapture
#[test]
fn test_distribute_owner_cut_multiple_subnets() {
    new_test_ext(1).execute_with(|| {
        let netuid1 = 1;
        let netuid2 = 2;
        let amount_to_distribute = 1_000_000;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey = U256::from(3);
        let lock_amount = 500_000;
        let lock_duration = 1000;

        // Set up locks in different subnets
        Locks::<Test>::insert(
            (netuid1, hotkey1, coldkey),
            (lock_amount, current_block, current_block + lock_duration),
        );
        Locks::<Test>::insert(
            (netuid2, hotkey2, coldkey),
            (lock_amount, current_block, current_block + lock_duration),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey);
        Owner::<Test>::insert(hotkey2, coldkey);

        System::set_block_number(current_block);

        // Distribute to netuid1
        SubtensorModule::set_lock_interval_blocks(10);
        let remaining1 = SubtensorModule::distribute_owner_cut(netuid1, amount_to_distribute);

        // Check distribution for netuid1
        let (new_lock1, _, _) = Locks::<Test>::get((netuid1, hotkey1, coldkey));
        let (lock2, _, _) = Locks::<Test>::get((netuid2, hotkey2, coldkey));

        assert!(
            new_lock1 > lock_amount,
            "Lock in netuid1 should receive a share"
        );
        assert_eq!(lock2, lock_amount, "Lock in netuid2 should not change");
        assert_eq!(remaining1, 0, "All amount should be distributed in netuid1");

        // Distribute to netuid2
        let remaining2 = SubtensorModule::distribute_owner_cut(netuid2, amount_to_distribute);

        // Check distribution for netuid2
        let (lock1, _, _) = Locks::<Test>::get((netuid1, hotkey1, coldkey));
        let (new_lock2, _, _) = Locks::<Test>::get((netuid2, hotkey2, coldkey));

        assert_eq!(lock1, new_lock1, "Lock in netuid1 should not change");
        assert!(
            new_lock2 > lock_amount,
            "Lock in netuid2 should receive a share"
        );
        assert_eq!(remaining2, 0, "All amount should be distributed in netuid2");

        // Verify total distribution
        assert_eq!(
            new_lock1 - lock_amount + new_lock2 - lock_amount,
            amount_to_distribute * 2,
            "Total distributed amount should match for both subnets"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_rounding --exact --nocapture
#[test]
fn test_distribute_owner_cut_rounding() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let amount_to_distribute = 1000; // Small amount to test rounding
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey = U256::from(3);
        let lock_amount1 = 500;
        let lock_amount2 = 501; // Slightly different to force rounding
        let lock_duration = 1000;

        // Set up locks
        add_network(netuid, 1, 1);
        SubtensorModule::set_lock_interval_blocks(10000);
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey),
            (lock_amount1, current_block, current_block + lock_duration),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey),
            (lock_amount2, current_block, current_block + lock_duration),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey);
        Owner::<Test>::insert(hotkey2, coldkey);

        System::set_block_number(current_block);

        let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        // Check that all funds were distributed despite potential rounding issues
        assert_eq!(remaining, 0, "All funds should be distributed");

        let (new_lock1, _, _) = Locks::<Test>::get((netuid, hotkey1, coldkey));
        let (new_lock2, _, _) = Locks::<Test>::get((netuid, hotkey2, coldkey));

        // Check that the sum of distributed amounts equals the original amount
        assert_eq!(
            (new_lock1 - lock_amount1) + (new_lock2 - lock_amount2),
            amount_to_distribute,
            "Sum of distributed amounts should equal the original amount"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_conviction_calculation --exact --nocapture
#[test]
fn test_distribute_owner_cut_conviction_calculation() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let amount_to_distribute = 1000000;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey = U256::from(3);
        let lock_amount = 1000;
        let lock_duration1 = 2000;
        let lock_duration2 = 1000;

        // Set up locks with different durations
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey),
            (lock_amount, current_block, current_block + lock_duration1),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey),
            (lock_amount, current_block, current_block + lock_duration2),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey);
        Owner::<Test>::insert(hotkey2, coldkey);

        System::set_block_number(current_block);

        let _remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        let (new_lock1, _, _) = Locks::<Test>::get((netuid, hotkey1, coldkey));
        let (new_lock2, _, _) = Locks::<Test>::get((netuid, hotkey2, coldkey));

        // Check that the hotkey with longer lock duration received more funds
        assert!(
            new_lock1 - lock_amount > new_lock2 - lock_amount,
            "Hotkey with longer lock duration should receive more funds"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_lions_share_distribution --exact --nocapture
#[test]
fn test_distribute_owner_cut_lions_share_distribution() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let amount_to_distribute = 1000000;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let hotkey3 = U256::from(3);
        let coldkey = U256::from(4);
        let lock_amount1 = 1000;
        let lock_amount2 = 500;
        let lock_amount3 = 100;
        let lock_duration = 1000;

        // Set up locks with different amounts
        SubtensorModule::set_lock_interval_blocks(100);
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey),
            (lock_amount1, current_block, current_block + lock_duration),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey),
            (lock_amount2, current_block, current_block + lock_duration),
        );
        Locks::<Test>::insert(
            (netuid, hotkey3, coldkey),
            (lock_amount3, current_block, current_block + lock_duration),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey);
        Owner::<Test>::insert(hotkey2, coldkey);
        Owner::<Test>::insert(hotkey3, coldkey);

        System::set_block_number(current_block);

        let _remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        let (new_lock1, _, _) = Locks::<Test>::get((netuid, hotkey1, coldkey));
        let (new_lock2, _, _) = Locks::<Test>::get((netuid, hotkey2, coldkey));
        let (new_lock3, _, _) = Locks::<Test>::get((netuid, hotkey3, coldkey));

        // Check that distribution follows the lion's share principle
        assert!(
            new_lock1 - lock_amount1 > new_lock2 - lock_amount2,
            "Hotkey with larger lock should receive a larger share"
        );
        assert!(
            new_lock2 - lock_amount2 > new_lock3 - lock_amount3,
            "Hotkey with larger lock should receive a larger share"
        );

        // Check that the total distributed amount matches the initial amount
        let total_distributed =
            (new_lock1 - lock_amount1) + (new_lock2 - lock_amount2) + (new_lock3 - lock_amount3);
        assert_eq!(
            total_distributed,
            amount_to_distribute - 1,
            "Total distributed amount should match initial amount"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_storage_updates --exact --nocapture
#[test]
fn test_distribute_owner_cut_storage_updates() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let amount_to_distribute = 1_000_000;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey = U256::from(3);
        let lock_amount1 = 500_000;
        let lock_amount2 = 250_000;
        let lock_duration = 1000;

        // Set up initial locks
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey),
            (lock_amount1, current_block, current_block + lock_duration),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey),
            (lock_amount2, current_block, current_block + lock_duration),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey);
        Owner::<Test>::insert(hotkey2, coldkey);

        System::set_block_number(current_block);

        // Distribute owner cut
        SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

        // Check that locks have been updated
        let (new_lock1, _, _) = Locks::<Test>::get((netuid, hotkey1, coldkey));
        let (new_lock2, _, _) = Locks::<Test>::get((netuid, hotkey2, coldkey));

        assert!(
            new_lock1 > lock_amount1,
            "Lock for hotkey1 should have increased"
        );
        assert!(
            new_lock2 > lock_amount2,
            "Lock for hotkey2 should have increased"
        );

        // Verify that the total increase matches the distributed amount
        let total_increase = (new_lock1 - lock_amount1) + (new_lock2 - lock_amount2);
        assert_eq!(
            total_increase,
            amount_to_distribute - 1,
            "Total lock increase should match the distributed amount"
        );

        // Check that other storage items remain unchanged
        assert_eq!(
            Owner::<Test>::get(hotkey1),
            coldkey,
            "Owner mapping for hotkey1 should remain unchanged"
        );
        assert_eq!(
            Owner::<Test>::get(hotkey2),
            coldkey,
            "Owner mapping for hotkey2 should remain unchanged"
        );

        // Verify that no new locks were created
        assert_eq!(
            Locks::<Test>::iter().count(),
            2,
            "No new locks should have been created"
        );
    });
}

// Test cases for update_subnet_owner function:
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner_no_locks --exact --nocapture
#[test]
fn test_update_subnet_owner_no_locks() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;

        // Ensure there are no locks in the subnet
        assert_eq!(
            Locks::<Test>::iter().count(),
            0,
            "There should be no locks initially"
        );

        // Call the update_subnet_owner function
        SubtensorModule::update_subnet_owner(netuid);

        // Verify that no subnet owner was set
        assert!(
            !SubnetOwner::<Test>::contains_key(netuid),
            "No subnet owner should be set when there are no locks"
        );

        // Verify that the subnet locked amount is zero
        assert_eq!(
            SubnetLocked::<Test>::get(netuid),
            0,
            "Subnet locked amount should be zero"
        );

        // Check that a warning log was emitted
        // Note: In a real test environment, you would need a way to capture and assert on log output
        // For this example, we'll just comment on the expected behavior
        // assert_eq!(last_log_message(), "No locks found for subnet 1");
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner_single_lock --exact --nocapture
#[test]
fn test_update_subnet_owner_single_lock() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let hotkey = U256::from(1);
        let coldkey = U256::from(2);
        let lock_amount = 1000000;
        let current_block = 100;
        let lock_duration = 1000000;

        // Set up a single lock
        Locks::<Test>::insert(
            (netuid, hotkey, coldkey),
            (lock_amount, current_block, current_block + lock_duration),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey, coldkey);

        // Mock the current block
        System::set_block_number(current_block);

        // Call the update_subnet_owner function
        SubtensorModule::update_subnet_owner(netuid);

        // Verify that the subnet owner was set correctly
        assert_eq!(
            SubnetOwner::<Test>::get(netuid),
            coldkey.clone(),
            "Subnet owner should be set to the coldkey of the only lock"
        );

        // Verify that the subnet locked amount is correct
        let expected_conviction = SubtensorModule::calculate_conviction(
            lock_amount,
            current_block + lock_duration,
            current_block,
        );
        assert_eq!(
            SubnetLocked::<Test>::get(netuid),
            expected_conviction,
            "Subnet locked amount should match the calculated conviction"
        );

        // Verify that no new locks were created
        assert_eq!(
            Locks::<Test>::iter().count(),
            1,
            "There should still be only one lock"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner_multiple_locks --exact --nocapture
#[test]
fn test_update_subnet_owner_multiple_locks() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let hotkey1 = U256::from(1);
        let coldkey1 = U256::from(2);
        let hotkey2 = U256::from(3);
        let coldkey2 = U256::from(4);
        let hotkey3 = U256::from(5);
        let coldkey3 = U256::from(6);
        let current_block = 100;

        // Set up multiple locks with different amounts and durations
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey1),
            (1000000, current_block, current_block + 1000000),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey2),
            (2000000, current_block, current_block + 500000),
        );
        Locks::<Test>::insert(
            (netuid, hotkey3, coldkey3),
            (1500000, current_block, current_block + 2000000),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey1);
        Owner::<Test>::insert(hotkey2, coldkey2);
        Owner::<Test>::insert(hotkey3, coldkey3);

        // Mock the current block
        System::set_block_number(current_block);

        // Call the update_subnet_owner function
        SubtensorModule::update_subnet_owner(netuid);

        // Calculate expected convictions
        let conviction1 =
            SubtensorModule::calculate_conviction(1000000, current_block + 1000000, current_block);
        let conviction2 =
            SubtensorModule::calculate_conviction(2000000, current_block + 500000, current_block);
        let conviction3 =
            SubtensorModule::calculate_conviction(1500000, current_block + 2000000, current_block);

        // Determine the expected owner (hotkey with highest conviction)
        let expected_owner = if conviction1 > conviction2 && conviction1 > conviction3 {
            coldkey1
        } else if conviction2 > conviction1 && conviction2 > conviction3 {
            coldkey2
        } else {
            coldkey3
        };

        // Verify that the subnet owner was set correctly
        assert_eq!(
            SubnetOwner::<Test>::get(netuid),
            expected_owner,
            "Subnet owner should be set to the coldkey of the hotkey with highest conviction"
        );

        // Verify that the subnet locked amount is correct
        assert_eq!(
            SubnetLocked::<Test>::get(netuid),
            conviction1 + conviction2 + conviction3,
            "Subnet locked amount should match the total calculated conviction"
        );

        // Verify that no new locks were created
        assert_eq!(
            Locks::<Test>::iter().count(),
            3,
            "There should still be only three locks"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner_tie_breaking --exact --nocapture
#[test]
fn test_update_subnet_owner_tie_breaking() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let hotkey3 = U256::from(3);
        let coldkey1 = U256::from(4);
        let coldkey2 = U256::from(5);
        let coldkey3 = U256::from(6);

        // Set up locks with equal convictions
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey1),
            (1000000, current_block, current_block + 1000000),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey2),
            (1000000, current_block, current_block + 1000000),
        );
        Locks::<Test>::insert(
            (netuid, hotkey3, coldkey3),
            (1000000, current_block, current_block + 1000000),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey1);
        Owner::<Test>::insert(hotkey2, coldkey2);
        Owner::<Test>::insert(hotkey3, coldkey3);

        // Mock the current block
        System::set_block_number(current_block);

        // Call the update_subnet_owner function
        SubtensorModule::update_subnet_owner(netuid);

        // The expected owner should be the coldkey of the hotkey with the lowest value
        let expected_owner = coldkey1;

        // Verify that the subnet owner was set correctly
        assert_eq!(
            SubnetOwner::<Test>::get(netuid),
            expected_owner,
            "Subnet owner should be set to the coldkey of the hotkey with the lowest value"
        );

        // Verify that the subnet locked amount is correct
        let conviction1 =
            SubtensorModule::calculate_conviction(1000000, current_block + 1000000, current_block);
        assert_eq!(
            SubnetLocked::<Test>::get(netuid),
            conviction1 * 3,
            "Subnet locked amount should match the total calculated conviction"
        );

        // Verify that no new locks were created
        assert_eq!(
            Locks::<Test>::iter().count(),
            3,
            "There should still be only three locks"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner_below_threshold --exact --nocapture
#[test]
fn test_update_subnet_owner_below_threshold() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey1 = U256::from(3);
        let coldkey2 = U256::from(4);

        // Set up locks with low conviction scores
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey1),
            (100, current_block, current_block + 100),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey2),
            (200, current_block, current_block + 200),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey1);
        Owner::<Test>::insert(hotkey2, coldkey2);

        // Mock the current block
        System::set_block_number(current_block);

        // Call the update_subnet_owner function
        SubtensorModule::update_subnet_owner(netuid);

        // Verify that no subnet owner was set due to low conviction scores
        assert!(
            !SubnetOwner::<Test>::contains_key(netuid),
            "No subnet owner should be set when all convictions are below the threshold"
        );

        // Verify that the subnet locked amount is correct (should be the sum of all convictions)
        let expected_total_conviction =
            SubtensorModule::calculate_conviction(100, current_block + 100, current_block)
                + SubtensorModule::calculate_conviction(200, current_block + 200, current_block);
        assert_eq!(
            SubnetLocked::<Test>::get(netuid),
            expected_total_conviction,
            "Subnet locked amount should match the total calculated conviction"
        );

        // Verify that no new locks were created
        assert_eq!(
            Locks::<Test>::iter().count(),
            2,
            "There should still be only two locks"
        );
    });
}
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner_conviction_calculation --exact --nocapture
#[test]
fn test_update_subnet_owner_conviction_calculation() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey1 = U256::from(3);
        let coldkey2 = U256::from(4);

        // Set up locks with different amounts and durations
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey1),
            (1000000, current_block, current_block + 1000000),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey2),
            (2000000, current_block, current_block + 500000),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey1);
        Owner::<Test>::insert(hotkey2, coldkey2);

        // Mock the current block
        System::set_block_number(current_block);

        // Call the update_subnet_owner function
        SubtensorModule::update_subnet_owner(netuid);

        // Calculate expected convictions
        let conviction1 =
            SubtensorModule::calculate_conviction(1000000, current_block + 1000000, current_block);
        let conviction2 =
            SubtensorModule::calculate_conviction(2000000, current_block + 500000, current_block);

        // Verify that the subnet owner is set to the hotkey with the highest conviction
        if conviction1 > conviction2 {
            assert_eq!(
                SubnetOwner::<Test>::get(netuid),
                coldkey1.clone(),
                "Subnet owner should be set to coldkey1"
            );
        } else {
            assert_eq!(
                SubnetOwner::<Test>::get(netuid),
                coldkey2.clone(),
                "Subnet owner should be set to coldkey2"
            );
        }

        // Verify that the subnet locked amount is correct
        assert_eq!(
            SubnetLocked::<Test>::get(netuid),
            conviction1 + conviction2,
            "Subnet locked amount should match the highest calculated conviction"
        );

        // Log the convictions for debugging
        println!("Conviction for hotkey1: {}", conviction1);
        println!("Conviction for hotkey2: {}", conviction2);
        println!("Total subnet locked: {}", SubnetLocked::<Test>::get(netuid));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner_different_subnets --exact --nocapture
#[test]
fn test_update_subnet_owner_different_subnets() {
    new_test_ext(1).execute_with(|| {
        let netuid1 = 1;
        let netuid2 = 2;
        let current_block = 100;
        let hotkey1 = U256::from(1);
        let hotkey2 = U256::from(2);
        let coldkey1 = U256::from(3);
        let coldkey2 = U256::from(4);

        // Set up locks in different subnets
        Locks::<Test>::insert(
            (netuid1, hotkey1, coldkey1),
            (1000000, current_block, current_block + 1000000),
        );
        Locks::<Test>::insert(
            (netuid2, hotkey2, coldkey2),
            (2000000, current_block, current_block + 500000),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey1);
        Owner::<Test>::insert(hotkey2, coldkey2);

        // Mock the current block
        System::set_block_number(current_block);

        // Call the update_subnet_owner function for netuid1
        SubtensorModule::update_subnet_owner(netuid1);

        // Verify that only the lock from netuid1 is considered
        let conviction1 =
            SubtensorModule::calculate_conviction(1000000, current_block + 1000000, current_block);
        assert_eq!(
            SubnetOwner::<Test>::get(netuid1),
            coldkey1.clone(),
            "Subnet owner for netuid1 should be set to coldkey1"
        );
        assert_eq!(
            SubnetLocked::<Test>::get(netuid1),
            conviction1,
            "Subnet locked amount for netuid1 should match the conviction of its only lock"
        );

        // Verify that netuid2 is unaffected
        assert!(
            !SubnetOwner::<Test>::contains_key(netuid2),
            "Subnet owner for netuid2 should not be set"
        );
        assert_eq!(
            SubnetLocked::<Test>::get(netuid2),
            0,
            "Subnet locked amount for netuid2 should be zero"
        );

        // Call the update_subnet_owner function for netuid2
        SubtensorModule::update_subnet_owner(netuid2);

        // Verify that only the lock from netuid2 is now considered
        let conviction2 =
            SubtensorModule::calculate_conviction(2000000, current_block + 500000, current_block);
        assert_eq!(
            SubnetOwner::<Test>::get(netuid2),
            coldkey2.clone(),
            "Subnet owner for netuid2 should be set to coldkey2"
        );
        assert_eq!(
            SubnetLocked::<Test>::get(netuid2),
            conviction2,
            "Subnet locked amount for netuid2 should match the conviction of its only lock"
        );

        // Verify that netuid1 remains unchanged
        assert_eq!(
            SubnetOwner::<Test>::get(netuid1),
            coldkey1.clone(),
            "Subnet owner for netuid1 should still be set to coldkey1"
        );
        assert_eq!(
            SubnetLocked::<Test>::get(netuid1),
            conviction1,
            "Subnet locked amount for netuid1 should remain unchanged"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner_large_subnet --exact --nocapture
#[test]
fn test_update_subnet_owner_large_subnet() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let num_locks = 1500; // Large number of locks
        let current_block = 100;

        // Create a large number of locks
        for i in 0..num_locks {
            let hotkey = AccountId::from([i as u8; 32]);
            let coldkey = AccountId::from([(i + num_locks) as u8; 32]);
            let lock_amount = (i + 1) * 1000; // Varying lock amounts
            let lock_duration = (i % 10 + 1) * 100; // Varying lock durations

            Locks::<Test>::insert(
                (netuid, hotkey, coldkey),
                (lock_amount, current_block, current_block + lock_duration),
            );

            // Set up ownership
            Owner::<Test>::insert(hotkey, coldkey);
        }

        // Mock the current block
        System::set_block_number(current_block);

        // Measure the time taken to update subnet owner
        let start_time = std::time::Instant::now();
        SubtensorModule::update_subnet_owner(netuid);
        let duration = start_time.elapsed();

        // Log the time taken
        println!("Time taken to update subnet owner: {:?}", duration);

        // Verify that a subnet owner was set
        assert!(
            SubnetOwner::<Test>::contains_key(netuid),
            "A subnet owner should be set"
        );

        // Verify that the subnet locked amount is non-zero
        assert!(
            SubnetLocked::<Test>::get(netuid) > 0,
            "Subnet locked amount should be non-zero"
        );

        // Verify that the subnet owner has the highest conviction
        let owner = SubnetOwner::<Test>::get(netuid);
        let mut max_conviction = 0;
        for ((iter_netuid, _hotkey, _), (lock_amount, _, end_block)) in Locks::<Test>::iter() {
            if iter_netuid == netuid {
                let conviction =
                    SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
                if conviction > max_conviction {
                    max_conviction = conviction;
                }
            }
        }
        let owner_hotkey = Locks::<Test>::iter()
            .find(|((_, _, coldkey), _)| *coldkey == owner)
            .map(|((_, hotkey, _), _)| hotkey)
            .unwrap();
        let (owner_lock_amount, _, owner_end_block) =
            Locks::<Test>::get((netuid, owner_hotkey, owner));
        let owner_conviction = SubtensorModule::calculate_conviction(
            owner_lock_amount,
            owner_end_block,
            current_block,
        );
        assert_eq!(
            owner_conviction, max_conviction,
            "Subnet owner should have the highest conviction"
        );

        // Check that a performance warning was logged
        // Note: In a real test environment, you would need a way to capture and assert on log output
        // For this example, we'll just comment on the expected behavior
        // assert!(log_contains("Large subnet 1 processed with 1500 hotkeys"));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner_ownership_change --exact --nocapture
#[test]
fn test_update_subnet_owner_ownership_change() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let hotkey1 = U256::from(1);
        let coldkey1 = U256::from(2);
        let hotkey2 = U256::from(3);
        let coldkey2 = U256::from(4);
        let initial_block = 100000;
        let lock_duration = 1000000;

        // Set up initial locks
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey1),
            (1000000, initial_block, initial_block + lock_duration),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey2),
            (500000, initial_block, initial_block + lock_duration),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey1);
        Owner::<Test>::insert(hotkey2, coldkey2);

        // Set initial block
        System::set_block_number(initial_block);

        // Update subnet owner
        SubtensorModule::update_subnet_owner(netuid);

        // Check initial subnet owner
        assert_eq!(
            SubnetOwner::<Test>::get(netuid),
            coldkey1.clone(),
            "Initial subnet owner should be coldkey1"
        );

        // Simulate time passing and conviction changing
        let new_block = initial_block + 500000;
        System::set_block_number(new_block);

        // Increase lock for hotkey2
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey2),
            (2000000, new_block, new_block + lock_duration),
        );

        // Update subnet owner again
        SubtensorModule::update_subnet_owner(netuid);

        // Check if subnet owner has changed
        assert_eq!(
            SubnetOwner::<Test>::get(netuid),
            coldkey2.clone(),
            "Subnet owner should change to coldkey2"
        );

        // Verify subnet locked amount has increased
        let subnet_locked = SubnetLocked::<Test>::get(netuid);
        assert!(subnet_locked > 0, "Subnet locked amount should be non-zero");
        assert!(
            subnet_locked
                > SubtensorModule::calculate_conviction(
                    1000000,
                    initial_block + lock_duration,
                    new_block
                ),
            "Subnet locked amount should increase"
        );

        // Simulate more time passing
        let final_block = new_block + 600000;
        System::set_block_number(final_block);

        // Update subnet owner one last time
        SubtensorModule::update_subnet_owner(netuid);

        // Check if subnet owner remains the same
        assert_eq!(
            SubnetOwner::<Test>::get(netuid),
            coldkey2.clone(),
            "Subnet owner should still be coldkey2"
        );

        // Verify subnet locked amount has decreased due to time passing
        let final_subnet_locked = SubnetLocked::<Test>::get(netuid);
        assert!(
            final_subnet_locked < subnet_locked,
            "Subnet locked amount should decrease over time"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner_storage_updates --exact --nocapture
#[test]
fn test_update_subnet_owner_storage_updates() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let hotkey1 = U256::from(1);
        let coldkey1 = U256::from(2);
        let hotkey2 = U256::from(3);
        let coldkey2 = U256::from(4);
        let initial_block = 100000;
        let lock_duration = 1000000;

        // Set up initial locks
        Locks::<Test>::insert(
            (netuid, hotkey1, coldkey1),
            (1000000, initial_block, initial_block + lock_duration),
        );
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey2),
            (500000, initial_block, initial_block + lock_duration),
        );

        // Set up ownership
        Owner::<Test>::insert(hotkey1, coldkey1);
        Owner::<Test>::insert(hotkey2, coldkey2);

        // Set initial block
        System::set_block_number(initial_block);

        // Update subnet owner
        SubtensorModule::update_subnet_owner(netuid);

        // Check initial subnet owner and locked amount
        assert_eq!(
            SubnetOwner::<Test>::get(netuid),
            coldkey1.clone(),
            "Initial subnet owner should be coldkey1"
        );
        let initial_locked = SubnetLocked::<Test>::get(netuid);
        assert!(
            initial_locked > 0,
            "Initial subnet locked amount should be non-zero"
        );

        // Simulate time passing and conviction changing
        let new_block = initial_block + 500000;
        System::set_block_number(new_block);

        // Increase lock for hotkey2
        Locks::<Test>::insert(
            (netuid, hotkey2, coldkey2),
            (2000000, new_block, new_block + lock_duration),
        );

        // Update subnet owner again
        SubtensorModule::update_subnet_owner(netuid);

        // Check if subnet owner has changed
        assert_eq!(
            SubnetOwner::<Test>::get(netuid),
            coldkey2.clone(),
            "Subnet owner should change to coldkey2"
        );

        // Verify subnet locked amount has increased
        let new_locked = SubnetLocked::<Test>::get(netuid);
        assert!(
            new_locked > initial_locked,
            "Subnet locked amount should increase"
        );

        // Simulate more time passing
        let final_block = new_block + 600000;
        System::set_block_number(final_block);

        // Update subnet owner one last time
        SubtensorModule::update_subnet_owner(netuid);

        // Check if subnet owner remains the same
        assert_eq!(
            SubnetOwner::<Test>::get(netuid),
            coldkey2.clone(),
            "Subnet owner should still be coldkey2"
        );

        // Verify subnet locked amount has decreased due to time passing
        let final_locked = SubnetLocked::<Test>::get(netuid);
        assert!(
            final_locked < new_locked,
            "Subnet locked amount should decrease over time"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_sudo_set_lock_interval_blocks_success --exact --nocapture
#[test]
fn test_sudo_set_lock_interval_blocks_success() {
    new_test_ext(1).execute_with(|| {
        let new_interval = 1000;
        assert_ok!(SubtensorModule::sudo_set_lock_interval_blocks(
            RuntimeOrigin::root(),
            new_interval
        ));
        assert_eq!(SubtensorModule::get_lock_interval_blocks(), new_interval);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_sudo_set_lock_interval_blocks_non_root_fails --exact --nocapture
#[test]
fn test_sudo_set_lock_interval_blocks_non_root_fails() {
    new_test_ext(1).execute_with(|| {
        let new_interval = 1000;
        let non_root = U256::from(1);
        assert_noop!(
            SubtensorModule::sudo_set_lock_interval_blocks(
                RuntimeOrigin::signed(non_root),
                new_interval
            ),
            DispatchError::BadOrigin
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_sudo_set_lock_interval_blocks_zero --exact --nocapture
#[test]
fn test_sudo_set_lock_interval_blocks_zero() {
    new_test_ext(1).execute_with(|| {
        let new_interval = 0;
        assert_ok!(SubtensorModule::sudo_set_lock_interval_blocks(
            RuntimeOrigin::root(),
            new_interval
        ));
        assert_eq!(SubtensorModule::get_lock_interval_blocks(), new_interval);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_sudo_set_lock_interval_blocks_max_value --exact --nocapture
#[test]
fn test_sudo_set_lock_interval_blocks_max_value() {
    new_test_ext(1).execute_with(|| {
        let new_interval = u64::MAX;
        assert_ok!(SubtensorModule::sudo_set_lock_interval_blocks(
            RuntimeOrigin::root(),
            new_interval
        ));
        assert_eq!(SubtensorModule::get_lock_interval_blocks(), new_interval);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_sudo_set_lock_interval_blocks_multiple_calls --exact --nocapture
#[test]
fn test_sudo_set_lock_interval_blocks_multiple_calls() {
    new_test_ext(1).execute_with(|| {
        let intervals = [1000, 2000, 500, 10000];
        for interval in intervals.iter() {
            assert_ok!(SubtensorModule::sudo_set_lock_interval_blocks(
                RuntimeOrigin::root(),
                *interval
            ));
            assert_eq!(SubtensorModule::get_lock_interval_blocks(), *interval);
        }
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_sudo_set_lock_interval_blocks_effect_on_existing_locks --exact --nocapture
#[test]
fn test_sudo_set_lock_interval_blocks_effect_on_existing_locks() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let lock_amount = 1_000_000;
        let initial_lock_interval = 7200;
        let new_lock_interval = 14400;

        // Set up initial lock interval
        assert_ok!(SubtensorModule::sudo_set_lock_interval_blocks(
            RuntimeOrigin::root(),
            initial_lock_interval
        ));

        // Create a lock
        let current_block = SubtensorModule::get_current_block_as_u64();
        Locks::<Test>::insert(
            (netuid, hotkey, coldkey),
            (
                lock_amount,
                current_block,
                current_block + initial_lock_interval,
            ),
        );

        // Change lock interval
        assert_ok!(SubtensorModule::sudo_set_lock_interval_blocks(
            RuntimeOrigin::root(),
            new_lock_interval
        ));

        // Verify that existing lock is not affected
        let (stored_amount, stored_start, stored_end) =
            Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(stored_amount, lock_amount);
        assert_eq!(stored_start, current_block);
        assert_eq!(stored_end, current_block + initial_lock_interval);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_sudo_set_lock_interval_blocks_effect_on_new_locks --exact --nocapture
#[test]
fn test_sudo_set_lock_interval_blocks_effect_on_new_locks() {
    new_test_ext(1).execute_with(|| {
        let netuid = 1;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let lock_amount = 1_000_000;
        let new_lock_interval = 14400;

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);
        // Set new lock interval
        assert_ok!(SubtensorModule::sudo_set_lock_interval_blocks(
            RuntimeOrigin::root(),
            new_lock_interval
        ));

        // Add balance to coldkey account (more than needed to ensure sufficient funds)
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, lock_amount * 10);

        // Add stake to the hotkey (equal to lock_amount to ensure sufficient stake)
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            lock_amount
        ));
        // Create a new lock
        let current_block = SubtensorModule::get_current_block_as_u64();
        assert_ok!(SubtensorModule::lock_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            new_lock_interval,
            lock_amount
        ));

        // Verify that new lock uses the new interval
        let (stored_amount, stored_start, stored_end) =
            Locks::<Test>::get((netuid, hotkey, coldkey));
        assert_eq!(stored_amount, lock_amount);
        assert_eq!(stored_start, current_block);
        assert_eq!(stored_end, current_block + new_lock_interval);
    });
}

// // SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut --exact --nocapture
// #[test]
// fn test_distribute_owner_cut() {
//     new_test_ext(1).execute_with(|| {
//         let netuid = 1;
//         let coldkey1 = U256::from(1);
//         let hotkey1 = U256::from(2);
//         let coldkey2 = U256::from(3);
//         let hotkey2 = U256::from(4);

//         // Set up initial balances and stakes
//         SubtensorModule::add_balance_to_coldkey_account(&coldkey1, 1_000_000_000);
//         SubtensorModule::add_balance_to_coldkey_account(&coldkey2, 1_000_000_000);

//         // Register hotkeys
//         assert_ok!(SubtensorModule::register(RuntimeOrigin::signed(coldkey1), netuid, hotkey1));
//         assert_ok!(SubtensorModule::register(RuntimeOrigin::signed(coldkey2), netuid, hotkey2));

//         // Lock stakes
//         assert_ok!(SubtensorModule::increase_stake(RuntimeOrigin::signed(coldkey1), hotkey1, netuid, 500_000_000));
//         assert_ok!(SubtensorModule::increase_stake(RuntimeOrigin::signed(coldkey2), hotkey2, netuid, 250_000_000));

//         // Lock the stakes
//         assert_ok!(SubtensorModule::lock_stake(RuntimeOrigin::signed(coldkey1), hotkey1, netuid, 500_000_000, 7200 * 30)); // 30 days
//         assert_ok!(SubtensorModule::lock_stake(RuntimeOrigin::signed(coldkey2), hotkey2, netuid, 250_000_000, 7200 * 15)); // 15 days

//         // Distribute owner cut
//         let amount_to_distribute = 1_000_000;
//         let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

//         // Check that all funds were distributed
//         assert_eq!(remaining, 0);

//         // Check that the locks were updated correctly
//         let (lock1, _, _) = Locks::<Test>::get((netuid, hotkey1, coldkey1)).unwrap();
//         let (lock2, _, _) = Locks::<Test>::get((netuid, hotkey2, coldkey2)).unwrap();

//         // The exact distribution might vary, but hotkey1 should receive more than hotkey2
//         assert!(lock1 > 500_000_000);
//         assert!(lock2 > 250_000_000);
//         assert!(lock1 - 500_000_000 > lock2 - 250_000_000);

//         // Check that the total distributed amount matches the input amount
//         assert_eq!(lock1 - 500_000_000 + lock2 - 250_000_000, amount_to_distribute);
//     });
// }

// // SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_distribute_owner_cut_no_conviction --exact --nocapture
// #[test]
// fn test_distribute_owner_cut_no_conviction() {
//     new_test_ext(1).execute_with(|| {
//         let netuid = 1;
//         let amount_to_distribute = 1_000_000;

//         // Distribute owner cut when there are no stakes
//         let remaining = SubtensorModule::distribute_owner_cut(netuid, amount_to_distribute);

//         // Check that all funds were returned as there were no stakes to distribute to
//         assert_eq!(remaining, amount_to_distribute);
//     });
// }

// // SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_max_allowed_unstakable --exact --nocapture
// #[test]
// fn test_calculate_max_allowed_unstakable() {
//     new_test_ext(1).execute_with(|| {
//         let alpha_locked = 1_000_000;
//         let start_block = 1000;
//         let lock_interval_blocks = 7200 * 365; // One year in blocks

//         // Test immediately after locking
//         let current_block = start_block;
//         let max_unstakable = SubtensorModule::calculate_max_allowed_unstakable(alpha_locked, start_block, current_block);
//         assert_eq!(max_unstakable, 0, "Should not be able to unstake immediately after locking");

//         // Test after 25% of the lock period
//         let current_block = start_block + (lock_interval_blocks / 4);
//         let max_unstakable = SubtensorModule::calculate_max_allowed_unstakable(alpha_locked, start_block, current_block);
//         assert!(max_unstakable > 0 && max_unstakable < alpha_locked / 2, "Should be able to unstake some, but less than half after 25% of lock period");

//         // Test after 50% of the lock period
//         let current_block = start_block + (lock_interval_blocks / 2);
//         let max_unstakable = SubtensorModule::calculate_max_allowed_unstakable(alpha_locked, start_block, current_block);
//         assert!(max_unstakable > alpha_locked / 2 && max_unstakable < alpha_locked, "Should be able to unstake more than half, but not all after 50% of lock period");

//         // Test after full lock period
//         let current_block = start_block + lock_interval_blocks;
//         let max_unstakable = SubtensorModule::calculate_max_allowed_unstakable(alpha_locked, start_block, current_block);
//         assert_eq!(max_unstakable, alpha_locked, "Should be able to unstake all after full lock period");

//         // Test long after lock period
//         let current_block = start_block + (lock_interval_blocks * 2);
//         let max_unstakable = SubtensorModule::calculate_max_allowed_unstakable(alpha_locked, start_block, current_block);
//         assert_eq!(max_unstakable, alpha_locked, "Should still be able to unstake all long after lock period");
//     });
// }

// // SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_update_subnet_owner --exact --nocapture
// #[test]
// fn test_update_subnet_owner() {
//     new_test_ext(1).execute_with(|| {
//         let netuid = 1;
//         let coldkey1 = U256::from(1);
//         let hotkey1 = U256::from(2);
//         let coldkey2 = U256::from(3);
//         let hotkey2 = U256::from(4);

//         // Add balance and register hotkeys
//         SubtensorModule::add_balance_to_coldkey_account(&coldkey1, 1_000_000_000);
//         SubtensorModule::add_balance_to_coldkey_account(&coldkey2, 1_000_000_000);
//         assert_ok!(SubtensorModule::register(&hotkey1, &coldkey1, netuid));
//         assert_ok!(SubtensorModule::register(&hotkey2, &coldkey2, netuid));

//         // Create locks for both hotkeys
//         let current_block = SubtensorModule::get_current_block_as_u64();
//         let lock_amount1 = 500_000_000;
//         let lock_amount2 = 750_000_000;
//         let duration = 7200 * 365; // One year in blocks

//         Locks::<Test>::insert(
//             (netuid, hotkey1, coldkey1),
//             (lock_amount1, current_block, current_block + duration)
//         );
//         Locks::<Test>::insert(
//             (netuid, hotkey2, coldkey2),
//             (lock_amount2, current_block, current_block + duration)
//         );

//         // Update subnet owner
//         SubtensorModule::update_subnet_owner(netuid);

//         // Check that the subnet owner is set to coldkey2 (which has a higher lock amount)
//         assert_eq!(SubnetOwner::<Test>::get(netuid), Some(coldkey2));

//         // Advance blocks and update locks to change conviction scores
//         run_to_block(current_block + duration / 2);

//         // Update lock for hotkey1 to have higher conviction
//         Locks::<Test>::insert(
//             (netuid, hotkey1, coldkey1),
//             (lock_amount1 * 2, current_block, current_block + duration * 2)
//         );

//         // Update subnet owner again
//         SubtensorModule::update_subnet_owner(netuid);

//         // Check that the subnet owner is now set to coldkey1
//         assert_eq!(SubnetOwner::<Test>::get(netuid), Some(coldkey1));
//     });
// }

// // SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test lock -- test_calculate_conviction --exact --nocapture
// #[test]
// fn test_calculate_conviction() {
//     new_test_ext(1).execute_with(|| {
//         let lock_amount = 1_000_000;
//         let current_block = 1000;
//         let end_block = current_block + 7200 * 365; // One year from now

//         // Test conviction at the start of the lock
//         let conviction_start = SubtensorModule::calculate_conviction(lock_amount, end_block, current_block);
//         assert!(conviction_start > 0, "Conviction should be positive at the start of the lock");
//         assert!(conviction_start < lock_amount, "Conviction should be less than the lock amount");

//         // Test conviction at the middle of the lock period
//         let mid_block = current_block + (7200 * 365 / 2);
//         let conviction_mid = SubtensorModule::calculate_conviction(lock_amount, end_block, mid_block);
//         assert!(conviction_mid > conviction_start, "Conviction should increase over time");
//         assert!(conviction_mid < lock_amount, "Conviction should still be less than the lock amount");

//         // Test conviction near the end of the lock period
//         let near_end_block = end_block - 1000;
//         let conviction_near_end = SubtensorModule::calculate_conviction(lock_amount, end_block, near_end_block);
//         assert!(conviction_near_end > conviction_mid, "Conviction should be higher near the end");
//         assert!(conviction_near_end < lock_amount, "Conviction should still be less than the lock amount");

//         // Test conviction with different lock amounts
//         let larger_lock = lock_amount * 2;
//         let conviction_larger = SubtensorModule::calculate_conviction(larger_lock, end_block, current_block);
//         assert!(conviction_larger > conviction_start, "Larger lock should have higher conviction");

//         // Test conviction with very short lock duration
//         let short_end_block = current_block + 100;
//         let conviction_short = SubtensorModule::calculate_conviction(lock_amount, short_end_block, current_block);
//         assert!(conviction_short < conviction_start, "Short lock should have lower conviction");

//         // Test conviction at the exact end of the lock
//         let conviction_end = SubtensorModule::calculate_conviction(lock_amount, end_block, end_block);
//         assert_eq!(conviction_end, lock_amount, "Conviction should equal lock amount at the end");
//     });
// }
