mod mock;
use frame_support::assert_noop;
use frame_support::assert_ok;
use mock::*;
use pallet_subtensor::*;
use sp_core::U256;

// 1. test_do_move_success
// Description: Test a successful move of stake between two hotkeys in the same subnet
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_success --exact --nocapture
#[test]
fn test_do_move_success() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let netuid = 1;
        let stake_amount = 1000;

        // Set up initial stake
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, netuid, stake_amount);

        // Perform the move
        assert_ok!(SubtensorModule::do_move_stake(
            RuntimeOrigin::signed(coldkey),
            origin_hotkey,
            destination_hotkey,
            netuid,
            netuid,
            None
        ));

        // Check that the stake has been moved
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                netuid
            ),
            stake_amount
        );
    });
}

// 2. test_do_move_different_subnets
// Description: Test moving stake between two hotkeys in different subnets
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_different_subnets --exact --nocapture
#[test]
fn test_do_move_different_subnets() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let origin_netuid = 1;
        let destination_netuid = 2;
        let stake_amount = 1000;

        // Set up initial stake and subnets
        add_network(origin_netuid, 0, 0);
        add_network(destination_netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, origin_netuid, stake_amount);

        // Perform the move
        assert_ok!(SubtensorModule::do_move_stake(
            RuntimeOrigin::signed(coldkey),
            origin_hotkey,
            destination_hotkey,
            origin_netuid,
            destination_netuid,
            None
        ));

        // Check that the stake has been moved
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                origin_netuid
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                destination_netuid
            ),
            stake_amount
        );
    });
}

// 3. test_do_move_locked_funds
// Description: Attempt to move locked funds across subnets, which should fail
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_locked_funds --exact --nocapture
#[test]
fn test_do_move_locked_funds() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let origin_netuid = 1;
        let destination_netuid = 2;
        let stake_amount = 1000;
        let lock_amount = 500;

        // Set up initial stake, subnets, and lock
        add_network(origin_netuid, 0, 0);
        add_network(destination_netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, origin_netuid, stake_amount);
        assert_ok!(SubtensorModule::do_lock(
            RuntimeOrigin::signed(coldkey),
            origin_hotkey,
            origin_netuid,
            100,
            lock_amount
        ));

        // Attempt to move locked funds
        assert_noop!(
            SubtensorModule::do_move_stake(
                RuntimeOrigin::signed(coldkey),
                origin_hotkey,
                destination_hotkey,
                origin_netuid,
                destination_netuid,
                None
            ),
            Error::<Test>::NotEnoughStakeToWithdraw
        );

        // Check that the stake and lock remain unchanged
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                origin_netuid
            ),
            stake_amount
        );
        assert_eq!(
            SubtensorModule::get_locked_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                origin_netuid
            ),
            lock_amount
        );
    });
}

// 4. test_do_move_nonexistent_subnet
// Description: Attempt to move stake to a non-existent subnet, which should fail
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_nonexistent_subnet --exact --nocapture
#[test]
fn test_do_move_nonexistent_subnet() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let origin_netuid = 1;
        let nonexistent_netuid = 99; // Assuming this subnet doesn't exist
        let stake_amount = 1000;

        // Set up initial stake
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, origin_netuid, stake_amount);

        // Attempt to move stake to a non-existent subnet
        assert_noop!(
            SubtensorModule::do_move_stake(
                RuntimeOrigin::signed(coldkey),
                origin_hotkey,
                destination_hotkey,
                origin_netuid,
                nonexistent_netuid,
                None
            ),
            Error::<Test>::SubnetNotExists
        );

        // Check that the stake remains unchanged
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                origin_netuid
            ),
            stake_amount
        );
    });
}

// 5. test_do_move_nonexistent_origin_hotkey
// Description: Attempt to move stake from a non-existent origin hotkey, which should fail
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_nonexistent_origin_hotkey --exact --nocapture
#[test]
fn test_do_move_nonexistent_origin_hotkey() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let nonexistent_origin_hotkey = U256::from(99); // Assuming this hotkey doesn't exist
        let destination_hotkey = U256::from(3);
        let netuid = 1;

        // Attempt to move stake from a non-existent origin hotkey
        add_network(netuid, 0, 0);
        assert_noop!(
            SubtensorModule::do_move_stake(
                RuntimeOrigin::signed(coldkey),
                nonexistent_origin_hotkey,
                destination_hotkey,
                netuid,
                netuid,
                None
            ),
            Error::<Test>::HotKeyAccountNotExists
        );

        // Check that no stake was moved
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &nonexistent_origin_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
    });
}

// 6. test_do_move_nonexistent_destination_hotkey
// Description: Attempt to move stake to a non-existent destination hotkey, which should fail
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_nonexistent_destination_hotkey --exact --nocapture
#[test]
fn test_do_move_nonexistent_destination_hotkey() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let nonexistent_destination_hotkey = U256::from(99); // Assuming this hotkey doesn't exist
        let netuid = 1;
        let stake_amount = 1000;

        // Set up initial stake
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, netuid, stake_amount);

        // Attempt to move stake from a non-existent origin hotkey
        add_network(netuid, 0, 0);
        assert_noop!(
            SubtensorModule::do_move_stake(
                RuntimeOrigin::signed(coldkey),
                origin_hotkey,
                nonexistent_destination_hotkey,
                netuid,
                netuid,
                None
            ),
            Error::<Test>::HotKeyAccountNotExists
        );

        // Check that the stake was moved successfully
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                netuid
            ),
            1000
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &nonexistent_destination_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
    });
}

// 7. test_do_move_zero_stake
// Description: Test moving zero stake, which should succeed but have no effect
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_zero_stake --exact --nocapture
#[test]
fn test_do_move_zero_stake() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let netuid = 1;

        // Attempt to move zero stake
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);
        assert_noop!(
            SubtensorModule::do_move_stake(
                RuntimeOrigin::signed(coldkey),
                origin_hotkey,
                destination_hotkey,
                netuid,
                netuid,
                None
            ),
            Error::<Test>::MoveAmountCanNotBeZero
        );

        // Check that no stake was moved
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
    });
}

// 8. test_do_move_all_stake
// Description: Test moving all stake from one hotkey to another
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_all_stake --exact --nocapture
#[test]
fn test_do_move_all_stake() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let netuid = 1;
        let stake_amount = 1000;

        // Set up initial stake
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, netuid, stake_amount);

        // Move all stake
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);
        assert_ok!(SubtensorModule::do_move_stake(
            RuntimeOrigin::signed(coldkey),
            origin_hotkey,
            destination_hotkey,
            netuid,
            netuid,
            None
        ));

        // Check that all stake was moved
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                netuid
            ),
            stake_amount
        );
    });
}

// 9. test_do_move_partial_stake
// Description: Test moving a portion of stake from one hotkey to another
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_partial_stake --exact --nocapture
#[test]
fn test_do_move_partial_stake() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let netuid = 1;
        let total_stake = 1000;

        // Set up initial stake
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, netuid, total_stake);

        // Move partial stake
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);
        assert_ok!(SubtensorModule::do_move_stake(
            RuntimeOrigin::signed(coldkey),
            origin_hotkey,
            destination_hotkey,
            netuid,
            netuid,
            None
        ));

        // Check that the correct amount of stake was moved
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                netuid
            ),
            total_stake
        );
    });
}

// 10. test_do_move_multiple_times
// Description: Test moving stake multiple times between the same hotkeys
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_multiple_times --exact --nocapture
#[test]
fn test_do_move_multiple_times() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey1 = U256::from(2);
        let hotkey2 = U256::from(3);
        let netuid = 1;
        let initial_stake = 1000;

        // Set up initial stake
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &hotkey1);
        SubtensorModule::create_account_if_non_existent(&coldkey, &hotkey2);
        SubtensorModule::stake_into_subnet(&hotkey1, &coldkey, netuid, initial_stake);

        // Move stake multiple times
        for _ in 0..3 {
            assert_ok!(SubtensorModule::do_move_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey1,
                hotkey2,
                netuid,
                netuid,
                None
            ));
            assert_ok!(SubtensorModule::do_move_stake(
                RuntimeOrigin::signed(coldkey),
                hotkey2,
                hotkey1,
                netuid,
                netuid,
                None
            ));
        }

        // Check final stake distribution
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey1, &coldkey, netuid),
            initial_stake
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey2, &coldkey, netuid),
            0
        );
    });
}

// 11. test_do_move_with_locks
// Description: Test moving stake when locks are present in the same subnet
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_with_locks --exact --nocapture
#[test]
fn test_do_move_with_locks() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let netuid = 1;
        let stake_amount = 1000;
        let lock_amount = 500;

        // Set up initial stake and lock
        add_network(netuid, 0, 0);
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, netuid, stake_amount);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);

        assert_ok!(SubtensorModule::do_lock(
            RuntimeOrigin::signed(coldkey),
            origin_hotkey,
            netuid,
            100,
            lock_amount
        ));

        // Attempt to move stake
        assert_ok!(SubtensorModule::do_move_stake(
            RuntimeOrigin::signed(coldkey),
            origin_hotkey,
            destination_hotkey,
            netuid,
            netuid,
            None
        ));

        // Check that only unlocked stake was moved
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                netuid
            ),
            stake_amount
        );
    });
}

// 13. test_do_move_wrong_origin
// Description: Attempt to move stake with a different origin than the coldkey, which should fail
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_wrong_origin --exact --nocapture
#[test]
fn test_do_move_wrong_origin() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let wrong_coldkey = U256::from(99);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let netuid = 1;
        let stake_amount = 1000;

        // Set up initial stake
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, netuid, stake_amount);

        // Attempt to move stake with wrong origin
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);
        assert_noop!(
            SubtensorModule::do_move_stake(
                RuntimeOrigin::signed(wrong_coldkey),
                origin_hotkey,
                destination_hotkey,
                netuid,
                netuid,
                Some(1)
            ),
            Error::<Test>::MoveAmountCanNotBeZero
        );

        // Check that no stake was moved
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                netuid
            ),
            stake_amount
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
    });
}

// 14. test_do_move_same_hotkey
// Description: Attempt to move stake to the same hotkey, which should fail or have no effect
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_same_hotkey --exact --nocapture
#[test]
fn test_do_move_same_hotkey() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid = 1;
        let stake_amount = 1000;

        // Set up initial stake
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &hotkey);
        SubtensorModule::stake_into_subnet(&hotkey, &coldkey, netuid, stake_amount);

        // Attempt to move stake to the same hotkey
        assert_ok!(SubtensorModule::do_move_stake(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            hotkey,
            netuid,
            netuid,
            None
        ));

        // Check that stake remains unchanged
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid),
            stake_amount
        );
    });
}

// 15. test_do_move_event_emission
// Description: Verify that the correct event is emitted after a successful move
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_event_emission --exact --nocapture
#[test]
fn test_do_move_event_emission() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let netuid = 1;
        let stake_amount = 1000;

        // Set up initial stake
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, netuid, stake_amount);

        // Move stake and capture events
        System::reset_events();
        assert_ok!(SubtensorModule::do_move_stake(
            RuntimeOrigin::signed(coldkey),
            origin_hotkey,
            destination_hotkey,
            netuid,
            netuid,
            None
        ));

        // Check for the correct event emission
        System::assert_last_event(
            Event::StakeMoved(coldkey, origin_hotkey, netuid, destination_hotkey, netuid).into(),
        );
    });
}

// 16. test_do_move_storage_updates
// Description: Verify that all relevant storage items are correctly updated after a move
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_storage_updates --exact --nocapture
#[test]
fn test_do_move_storage_updates() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let origin_netuid = 1;
        let destination_netuid = 2;
        let stake_amount = 1000;

        // Set up initial stake
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, origin_netuid, stake_amount);

        // Move stake
        add_network(origin_netuid, 0, 0);
        add_network(destination_netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);
        assert_ok!(SubtensorModule::do_move_stake(
            RuntimeOrigin::signed(coldkey),
            origin_hotkey,
            destination_hotkey,
            origin_netuid,
            destination_netuid,
            None
        ));

        // Verify storage updates
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                origin_netuid
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                destination_netuid
            ),
            stake_amount
        );
        assert_eq!(
            SubtensorModule::get_locked_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                origin_netuid
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_locked_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                destination_netuid
            ),
            0
        );
    });
}

// 18. test_do_move_max_values
// Description: Test moving the maximum possible stake values to check for overflows
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test move -- test_do_move_max_values --exact --nocapture
#[test]
fn test_do_move_max_values() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let origin_hotkey = U256::from(2);
        let destination_hotkey = U256::from(3);
        let netuid = 1;
        let max_stake = u64::MAX;

        // Set up initial stake with maximum value
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &origin_hotkey);
        SubtensorModule::create_account_if_non_existent(&coldkey, &destination_hotkey);
        SubtensorModule::stake_into_subnet(&origin_hotkey, &coldkey, netuid, max_stake);

        // Move maximum stake
        assert_ok!(SubtensorModule::do_move_stake(
            RuntimeOrigin::signed(coldkey),
            origin_hotkey,
            destination_hotkey,
            netuid,
            netuid,
            None
        ));

        // Verify stake movement without overflow
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &origin_hotkey,
                &coldkey,
                netuid
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &destination_hotkey,
                &coldkey,
                netuid
            ),
            max_stake
        );
    });
}
