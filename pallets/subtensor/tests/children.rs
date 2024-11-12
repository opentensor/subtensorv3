#![allow(clippy::indexing_slicing)]
use crate::mock::*;
use frame_support::{assert_err, assert_noop, assert_ok};
mod mock;
use pallet_subtensor::{utils::rate_limiting::TransactionType, *};
use sp_core::U256;

// 1: Successful setting of a single child
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_child_singular_success --exact --nocapture
#[test]
fn test_do_set_child_singular_success() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child = U256::from(3);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set child
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion, child)]
        ));

        // Verify child assignment
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(children, vec![(proportion, child)]);
    });
}

// 2: Attempt to set child in non-existent network
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_child_singular_network_does_not_exist --exact --nocapture
#[test]
fn test_do_set_child_singular_network_does_not_exist() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child = U256::from(3);
        let netuid: u16 = 999; // Non-existent network
        let proportion: u64 = 1000;

        // Attempt to set child
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(proportion, child)]
            ),
            Error::<Test>::SubNetworkDoesNotExist
        );
    });
}

// 3: Attempt to set invalid child (same as hotkey)
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_child_singular_invalid_child --exact --nocapture
#[test]
fn test_do_set_child_singular_invalid_child() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Attempt to set child as the same hotkey
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![
                    (proportion, hotkey) // Invalid child
                ]
            ),
            Error::<Test>::InvalidChild
        );
    });
}

// 4: Attempt to set child with non-associated coldkey
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_child_singular_non_associated_coldkey --exact --nocapture
#[test]
fn test_do_set_child_singular_non_associated_coldkey() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child = U256::from(3);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey with a different coldkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, U256::from(999), 0);

        // Attempt to set child
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(proportion, child)]
            ),
            Error::<Test>::NonAssociatedColdKey
        );
    });
}

// 5: Attempt to set child in root network
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_child_singular_root_network --exact --nocapture
#[test]
fn test_do_set_child_singular_root_network() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child = U256::from(3);
        let netuid: u16 = SubtensorModule::get_root_netuid(); // Root network
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);

        // Attempt to set child
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(proportion, child)]
            ),
            Error::<Test>::RegistrationNotPermittedOnRootSubnet
        );
    });
}

// 6: Cleanup of old children when setting new ones
// This test verifies that when new children are set, the old ones are properly removed.
// It checks:
// - Setting an initial child
// - Replacing it with a new child
// - Ensuring the old child is no longer associated
// - Confirming the new child is correctly assigned
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_child_singular_old_children_cleanup --exact --nocapture
#[test]
fn test_do_set_child_singular_old_children_cleanup() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let old_child = U256::from(3);
        let new_child = U256::from(4);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set old child
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion, old_child)]
        ));

        // Set new child
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion, new_child)]
        ));

        // Verify old child is removed
        let old_child_parents = SubtensorModule::get_parents(&old_child, netuid);
        assert!(old_child_parents.is_empty());

        // Verify new child assignment
        let new_child_parents = SubtensorModule::get_parents(&new_child, netuid);
        assert_eq!(new_child_parents, vec![(proportion, hotkey)]);
    });
}

// 7: Verify new children assignment
// This test checks if new children are correctly assigned to a parent.
// It verifies:
// - Setting a child for a parent
// - Confirming the child is correctly listed under the parent
// - Ensuring the parent is correctly listed for the child
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_child_singular_new_children_assignment --exact --nocapture
#[test]
fn test_do_set_child_singular_new_children_assignment() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child = U256::from(3);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set child
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion, child)]
        ));

        // Verify child assignment
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(children, vec![(proportion, child)]);

        // Verify parent assignment
        let parents = SubtensorModule::get_parents(&child, netuid);
        assert_eq!(parents, vec![(proportion, hotkey)]);
    });
}

// 8: Test edge cases for proportion values
// This test verifies that the system correctly handles minimum and maximum proportion values.
// It checks:
// - Setting a child with the minimum possible proportion (0)
// - Setting a child with the maximum possible proportion (u64::MAX)
// - Confirming both assignments are processed correctly
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_child_singular_proportion_edge_cases --exact --nocapture
#[test]
fn test_do_set_child_singular_proportion_edge_cases() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child = U256::from(3);
        let netuid: u16 = 1;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set child with minimum proportion
        let min_proportion: u64 = 0;
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(min_proportion, child)]
        ));

        // Verify child assignment with minimum proportion
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(children, vec![(min_proportion, child)]);

        // Set child with maximum proportion
        let max_proportion: u64 = u64::MAX;
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(max_proportion, child)]
        ));

        // Verify child assignment with maximum proportion
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(children, vec![(max_proportion, child)]);
    });
}

// 9: Test setting multiple children
// This test verifies that when multiple children are set, only the last one remains.
// It checks:
// - Setting an initial child
// - Setting a second child
// - Confirming only the second child remains associated
// - Verifying the first child is no longer associated
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_child_singular_multiple_children --exact --nocapture
#[test]
fn test_do_set_child_singular_multiple_children() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let child2 = U256::from(4);
        let netuid: u16 = 1;
        let proportion1: u64 = 500;
        let proportion2: u64 = 500;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set first child
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion1, child1)]
        ));

        // Set second child
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion2, child2)]
        ));

        // Verify children assignment
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(children, vec![(proportion2, child2)]);

        // Verify parent assignment for both children
        let parents1 = SubtensorModule::get_parents(&child1, netuid);
        assert!(parents1.is_empty()); // Old child should be removed

        let parents2 = SubtensorModule::get_parents(&child2, netuid);
        assert_eq!(parents2, vec![(proportion2, hotkey)]);
    });
}

// 10: Test adding a singular child with various error conditions
// This test checks different scenarios when adding a child, including:
// - Attempting to set a child in a non-existent network
// - Trying to set a child with an unassociated coldkey
// - Setting an invalid child
// - Successfully setting a valid child
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_add_singular_child --exact --nocapture
#[test]
fn test_add_singular_child() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let child = U256::from(1);
        let hotkey = U256::from(1);
        let coldkey = U256::from(2);
        assert_eq!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(u64::MAX, child)]
            ),
            Err(Error::<Test>::SubNetworkDoesNotExist.into())
        );
        add_network(netuid, 0, 0);
        assert_eq!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(u64::MAX, child)]
            ),
            Err(Error::<Test>::NonAssociatedColdKey.into())
        );
        SubtensorModule::create_account_if_non_existent(&coldkey, &hotkey);
        assert_eq!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(u64::MAX, child)]
            ),
            Err(Error::<Test>::InvalidChild.into())
        );
        let child = U256::from(3);
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(u64::MAX, child)]
        ));
    })
}

// 11: Test getting stake for a hotkey on a subnet
// This test verifies the correct calculation of stake for a parent and child neuron:
// - Sets up a network with a parent and child neuron
// - Stakes tokens to both parent and child from different coldkeys
// - Establishes a parent-child relationship with 100% stake allocation
// - Checks that the parent's stake is correctly transferred to the child
// - Ensures the total stake is preserved in the system
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_get_stake_for_hotkey_on_subnet --exact --nocapture
#[test]
fn test_get_stake_for_hotkey_on_subnet() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let parent = U256::from(1);
        let child = U256::from(2);
        let coldkey1 = U256::from(3);
        let coldkey2 = U256::from(4);

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, parent, coldkey1, 0);
        register_ok_neuron(netuid, child, coldkey2, 0);

        // Stake 1000 to parent from coldkey1
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey1, &parent, 1000);
        // Stake 1000 to parent from coldkey2
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey2, &parent, 1000);
        // Stake 1000 to child from coldkey1
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey1, &child, 1000);
        // Stake 1000 to child from coldkey2
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey2, &child, 1000);

        // Set parent-child relationship with 100% stake allocation
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey1),
            parent,
            netuid,
            vec![(u64::MAX, child)]
        ));

        let parent_stake = SubtensorModule::get_stake_for_hotkey_on_subnet(&parent, netuid);
        let child_stake = SubtensorModule::get_stake_for_hotkey_on_subnet(&child, netuid);

        log::info!("Parent stake: {}", parent_stake);
        log::info!("Child stake: {}", child_stake);

        // The parent should have 0 stake as it's all allocated to the child
        assert_eq!(parent_stake, 0);
        // The child should have its original stake (2000) plus the parent's stake (2000)
        assert_eq!(child_stake, 4000);

        // Ensure total stake is preserved
        assert_eq!(parent_stake + child_stake, 4000);
    });
}

// 12: Test revoking a singular child successfully
// This test checks the process of revoking a child neuron:
// - Sets up a network with a parent and child neuron
// - Establishes a parent-child relationship
// - Revokes the child relationship
// - Verifies that the child is removed from the parent's children list
// - Ensures the parent is removed from the child's parents list
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_child_singular_success --exact --nocapture
#[test]
fn test_do_revoke_child_singular_success() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child = U256::from(3);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set child
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion, child)]
        ));

        // Verify child assignment
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(children, vec![(proportion, child)]);

        // Revoke child
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![]
        ));

        // Verify child removal
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert!(children.is_empty());

        // Verify parent removal
        let parents = SubtensorModule::get_parents(&child, netuid);
        assert!(parents.is_empty());
    });
}

// 13: Test revoking a child in a non-existent network
// This test verifies that attempting to revoke a child in a non-existent network results in an error:
// - Attempts to revoke a child in a network that doesn't exist
// - Checks that the appropriate error is returned
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_child_singular_network_does_not_exist --exact --nocapture
#[test]
fn test_do_revoke_child_singular_network_does_not_exist() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid: u16 = 999; // Non-existent network

        // Attempt to revoke child
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![]
            ),
            Error::<Test>::SubNetworkDoesNotExist
        );
    });
}

// 14: Test revoking a child with a non-associated coldkey
// This test ensures that attempting to revoke a child using an unassociated coldkey results in an error:
// - Sets up a network with a hotkey registered to a different coldkey
// - Attempts to revoke a child using an unassociated coldkey
// - Verifies that the appropriate error is returned
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_child_singular_non_associated_coldkey --exact --nocapture
#[test]
fn test_do_revoke_child_singular_non_associated_coldkey() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid: u16 = 1;

        // Add network and register hotkey with a different coldkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, U256::from(999), 0);

        // Attempt to revoke child
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![]
            ),
            Error::<Test>::NonAssociatedColdKey
        );
    });
}

// 15: Test revoking a non-associated child
// This test verifies that attempting to revoke a child that is not associated with the parent results in an error:
// - Sets up a network and registers a hotkey
// - Attempts to revoke a child that was never associated with the parent
// - Checks that the appropriate error is returned
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_child_singular_child_not_associated --exact --nocapture
#[test]
fn test_do_revoke_child_singular_child_not_associated() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child = U256::from(3);
        let netuid: u16 = 1;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        // Attempt to revoke child that is not associated
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(u64::MAX, child)]
            ),
            Error::<Test>::NonAssociatedColdKey
        );
    });
}

// 16: Test setting multiple children successfully
// This test verifies that multiple children can be set for a parent successfully:
// - Sets up a network and registers a hotkey
// - Sets multiple children with different proportions
// - Verifies that the children are correctly assigned to the parent
// - Checks that the parent is correctly assigned to each child
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_children_multiple_success --exact --nocapture
#[test]
fn test_do_set_children_multiple_success() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let child2 = U256::from(4);
        let netuid: u16 = 1;
        let proportion1: u64 = 1000;
        let proportion2: u64 = 2000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set multiple children
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion1, child1), (proportion2, child2)]
        ));

        // Verify children assignment
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(children, vec![(proportion1, child1), (proportion2, child2)]);

        // Verify parent assignment for both children
        let parents1 = SubtensorModule::get_parents(&child1, netuid);
        assert_eq!(parents1, vec![(proportion1, hotkey)]);

        let parents2 = SubtensorModule::get_parents(&child2, netuid);
        assert_eq!(parents2, vec![(proportion2, hotkey)]);
    });
}

// 17: Test setting multiple children in a non-existent network
// This test ensures that attempting to set multiple children in a non-existent network results in an error:
// - Attempts to set children in a network that doesn't exist
// - Verifies that the appropriate error is returned
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_children_multiple_network_does_not_exist --exact --nocapture
#[test]
fn test_do_set_children_multiple_network_does_not_exist() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let netuid: u16 = 999; // Non-existent network
        let proportion: u64 = 1000;

        // Attempt to set children
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(proportion, child1)]
            ),
            Error::<Test>::SubNetworkDoesNotExist
        );
    });
}

// 18: Test setting multiple children with an invalid child
// This test verifies that attempting to set multiple children with an invalid child (same as parent) results in an error:
// - Sets up a network and registers a hotkey
// - Attempts to set a child that is the same as the parent hotkey
// - Checks that the appropriate error is returned
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_children_multiple_invalid_child --exact --nocapture
#[test]
fn test_do_set_children_multiple_invalid_child() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Attempt to set child as the same hotkey
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(proportion, hotkey)]
            ),
            Error::<Test>::InvalidChild
        );
    });
}

// 19: Test setting multiple children with a non-associated coldkey
// This test ensures that attempting to set multiple children using an unassociated coldkey results in an error:
// - Sets up a network with a hotkey registered to a different coldkey
// - Attempts to set children using an unassociated coldkey
// - Verifies that the appropriate error is returned
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_children_multiple_non_associated_coldkey --exact --nocapture
#[test]
fn test_do_set_children_multiple_non_associated_coldkey() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child = U256::from(3);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey with a different coldkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, U256::from(999), 0);

        // Attempt to set children
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(proportion, child)]
            ),
            Error::<Test>::NonAssociatedColdKey
        );
    });
}

// 20: Test setting multiple children in root network
// This test verifies that attempting to set children in the root network results in an error:
// - Sets up the root network
// - Attempts to set children in the root network
// - Checks that the appropriate error is returned
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_children_multiple_root_network --exact --nocapture
#[test]
fn test_do_set_children_multiple_root_network() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child = U256::from(3);
        let netuid: u16 = SubtensorModule::get_root_netuid(); // Root network
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);

        // Attempt to set children
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(proportion, child)]
            ),
            Error::<Test>::RegistrationNotPermittedOnRootSubnet
        );
    });
}

// 21: Test cleanup of old children when setting multiple new ones
// This test ensures that when new children are set, the old ones are properly removed:
// - Sets up a network and registers a hotkey
// - Sets an initial child
// - Replaces it with multiple new children
// - Verifies that the old child is no longer associated
// - Confirms the new children are correctly assigned
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_children_multiple_old_children_cleanup --exact --nocapture
#[test]
fn test_do_set_children_multiple_old_children_cleanup() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let old_child = U256::from(3);
        let new_child1 = U256::from(4);
        let new_child2 = U256::from(5);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set old child
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion, old_child)]
        ));

        // Set new children
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion, new_child1), (proportion, new_child2)]
        ));

        // Verify old child is removed
        let old_child_parents = SubtensorModule::get_parents(&old_child, netuid);
        assert!(old_child_parents.is_empty());

        // Verify new children assignment
        let new_child1_parents = SubtensorModule::get_parents(&new_child1, netuid);
        assert_eq!(new_child1_parents, vec![(proportion, hotkey)]);

        let new_child2_parents = SubtensorModule::get_parents(&new_child2, netuid);
        assert_eq!(new_child2_parents, vec![(proportion, hotkey)]);
    });
}

// 22: Test setting multiple children with edge case proportions
// This test verifies the behavior when setting multiple children with minimum and maximum proportions:
// - Sets up a network and registers a hotkey
// - Sets two children with minimum and maximum proportions respectively
// - Verifies that the children are correctly assigned with their respective proportions
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_children_multiple_proportion_edge_cases --exact --nocapture
#[test]
fn test_do_set_children_multiple_proportion_edge_cases() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let child2 = U256::from(4);
        let netuid: u16 = 1;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set children with minimum and maximum proportions
        let min_proportion: u64 = 0;
        let max_proportion: u64 = u64::MAX;
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(min_proportion, child1), (max_proportion, child2)]
        ));

        // Verify children assignment
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(
            children,
            vec![(min_proportion, child1), (max_proportion, child2)]
        );
    });
}

// 23: Test overwriting existing children with new ones
// This test ensures that when new children are set, they correctly overwrite the existing ones:
// - Sets up a network and registers a hotkey
// - Sets initial children
// - Overwrites with new children
// - Verifies that the final children assignment is correct
// - Checks that old children are properly removed and new ones are correctly assigned
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_children_multiple_overwrite_existing --exact --nocapture
#[test]
fn test_do_set_children_multiple_overwrite_existing() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let child2 = U256::from(4);
        let child3 = U256::from(5);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set initial children
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion, child1), (proportion, child2)]
        ));

        // Overwrite with new children
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion * 2, child2), (proportion * 3, child3)]
        ));

        // Verify final children assignment
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(
            children,
            vec![(proportion * 2, child2), (proportion * 3, child3)]
        );

        // Verify parent assignment for all children
        let parents1 = SubtensorModule::get_parents(&child1, netuid);
        assert!(parents1.is_empty());

        let parents2 = SubtensorModule::get_parents(&child2, netuid);
        assert_eq!(parents2, vec![(proportion * 2, hotkey)]);

        let parents3 = SubtensorModule::get_parents(&child3, netuid);
        assert_eq!(parents3, vec![(proportion * 3, hotkey)]);
    });
}

// 24: Test childkey take functionality
// This test verifies the functionality of setting and getting childkey take:
// - Sets up a network and registers a hotkey
// - Checks default and maximum childkey take values
// - Sets a new childkey take value
// - Verifies the new take value is stored correctly
// - Attempts to set an invalid take value and checks for appropriate error
// - Tries to set take with a non-associated coldkey and verifies the error
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_childkey_take_functionality --exact --nocapture
#[test]
fn test_childkey_take_functionality() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid: u16 = 1;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Test default and max childkey take
        let default_take = SubtensorModule::get_default_childkey_take();
        let min_take = SubtensorModule::get_min_childkey_take();
        log::info!("Default take: {}, Max take: {}", default_take, min_take);

        // Check if default take and max take are the same
        assert_eq!(
            default_take, min_take,
            "Default take should be equal to max take"
        );

        // Log the actual value of MaxChildkeyTake
        log::info!(
            "MaxChildkeyTake value: {:?}",
            MaxChildkeyTake::<Test>::get()
        );

        // Test setting childkey take
        let new_take: u16 = SubtensorModule::get_max_childkey_take() / 2; // 50% of max_take
        assert_ok!(SubtensorModule::set_childkey_take(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            new_take
        ));

        // Verify childkey take was set correctly
        let stored_take = SubtensorModule::get_childkey_take(&hotkey, netuid);
        log::info!("Stored take: {}", stored_take);
        assert_eq!(stored_take, new_take);

        // Test setting childkey take outside of allowed range
        let invalid_take: u16 = SubtensorModule::get_max_childkey_take() + 1;
        assert_noop!(
            SubtensorModule::set_childkey_take(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                invalid_take
            ),
            Error::<Test>::InvalidChildkeyTake
        );

        // Test setting childkey take with non-associated coldkey
        let non_associated_coldkey = U256::from(999);
        assert_noop!(
            SubtensorModule::set_childkey_take(
                RuntimeOrigin::signed(non_associated_coldkey),
                hotkey,
                netuid,
                new_take
            ),
            Error::<Test>::NonAssociatedColdKey
        );
    });
}

// 25: Test childkey take rate limiting
// This test verifies the rate limiting functionality for setting childkey take:
// - Sets up a network and registers a hotkey
// - Sets a rate limit for childkey take changes
// - Performs multiple attempts to set childkey take
// - Verifies that rate limiting prevents frequent changes
// - Advances blocks to bypass rate limit and confirms successful change
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_childkey_take_rate_limiting --exact --nocapture
#[test]
fn test_childkey_take_rate_limiting() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid: u16 = 1;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set a rate limit for childkey take changes
        let rate_limit: u64 = 100;
        SubtensorModule::set_tx_childkey_take_rate_limit(rate_limit);

        log::info!(
            "Set TxChildkeyTakeRateLimit: {:?}",
            TxChildkeyTakeRateLimit::<Test>::get()
        );

        // Helper function to log rate limit information
        let log_rate_limit_info = || {
            let current_block = SubtensorModule::get_current_block_as_u64();
            let last_block = SubtensorModule::get_last_transaction_block(
                &hotkey,
                netuid,
                &TransactionType::SetChildkeyTake,
            );
            let passes = SubtensorModule::passes_rate_limit_on_subnet(
                &TransactionType::SetChildkeyTake,
                &hotkey,
                netuid,
            );
            let limit = SubtensorModule::get_rate_limit(&TransactionType::SetChildkeyTake);
            log::info!(
                "Rate limit info: current_block: {}, last_block: {}, limit: {}, passes: {}, diff: {}",
                current_block,
                last_block,
                limit,
                passes,
                current_block.saturating_sub(last_block)
            );
        };

        // First transaction (should succeed)
        log_rate_limit_info();
        assert_ok!(SubtensorModule::set_childkey_take(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            500
        ));
        log_rate_limit_info();

        // Second transaction (should fail due to rate limit)
        log_rate_limit_info();
        assert_noop!(
            SubtensorModule::set_childkey_take(RuntimeOrigin::signed(coldkey), hotkey, netuid, 600),
            Error::<Test>::TxChildkeyTakeRateLimitExceeded
        );
        log_rate_limit_info();

        // Advance the block number to just before the rate limit
        run_to_block(rate_limit - 1);

        // Third transaction (should still fail)
        log_rate_limit_info();
        assert_noop!(
            SubtensorModule::set_childkey_take(RuntimeOrigin::signed(coldkey), hotkey, netuid, 650),
            Error::<Test>::TxChildkeyTakeRateLimitExceeded
        );
        log_rate_limit_info();

        // Advance the block number to just after the rate limit
        run_to_block(rate_limit + 1);

        // Fourth transaction (should succeed)
        log_rate_limit_info();
        assert_ok!(SubtensorModule::set_childkey_take(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            700
        ));
        log_rate_limit_info();

        // Verify the final take was set
        let stored_take = SubtensorModule::get_childkey_take(&hotkey, netuid);
        assert_eq!(stored_take, 700);
    });
}

// 26: Test childkey take functionality across multiple networks
// This test verifies the childkey take functionality across multiple networks:
// - Creates multiple networks and sets up neurons
// - Sets unique childkey take values for each network
// - Verifies that each network has a different childkey take value
// - Attempts to set childkey take again (should fail due to rate limit)
// - Advances blocks to bypass rate limit and successfully updates take value
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_multiple_networks_childkey_take --exact --nocapture
#[test]
fn test_multiple_networks_childkey_take() {
    new_test_ext(1).execute_with(|| {
        const NUM_NETWORKS: u16 = 10;
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);

        // Create 10 networks and set up neurons (skip network 0)
        for netuid in 1..NUM_NETWORKS {
            // Add network
            add_network(netuid, 13, 0);

            // Register neuron
            register_ok_neuron(netuid, hotkey, coldkey, 0);

            // Set a unique childkey take value for each network
            let take_value = (netuid + 1) * 100; // Values will be 200, 300, ..., 1000
            assert_ok!(SubtensorModule::set_childkey_take(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                take_value
            ));

            // Verify the childkey take was set correctly
            let stored_take = SubtensorModule::get_childkey_take(&hotkey, netuid);
            assert_eq!(
                stored_take, take_value,
                "Childkey take not set correctly for network {}",
                netuid
            );

            // Log the set value
            log::info!("Network {}: Childkey take set to {}", netuid, take_value);
        }

        // Verify all networks have different childkey take values
        for i in 1..NUM_NETWORKS {
            for j in (i + 1)..NUM_NETWORKS {
                let take_i = SubtensorModule::get_childkey_take(&hotkey, i);
                let take_j = SubtensorModule::get_childkey_take(&hotkey, j);
                assert_ne!(
                    take_i, take_j,
                    "Childkey take values should be different for networks {} and {}",
                    i, j
                );
            }
        }

        // Attempt to set childkey take again (should fail due to rate limit)
        let result =
            SubtensorModule::set_childkey_take(RuntimeOrigin::signed(coldkey), hotkey, 1, 1100);
        assert_noop!(result, Error::<Test>::TxChildkeyTakeRateLimitExceeded);

        // Advance blocks to bypass rate limit
        run_to_block(SubtensorModule::get_tx_childkey_take_rate_limit() + 1);

        // Now setting childkey take should succeed
        assert_ok!(SubtensorModule::set_childkey_take(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            1,
            1100
        ));

        // Verify the new take value
        let new_take = SubtensorModule::get_childkey_take(&hotkey, 1);
        assert_eq!(new_take, 1100, "Childkey take not updated after rate limit");
    });
}

// 27: Test setting children with an empty list
// This test verifies the behavior of setting an empty children list:
// - Adds a network and registers a hotkey
// - Sets an empty children list for the hotkey
// - Verifies that the children assignment is empty
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_set_children_multiple_empty_list --exact --nocapture
#[test]
fn test_do_set_children_multiple_empty_list() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid: u16 = 1;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set empty children list
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![]
        ));

        // Verify children assignment is empty
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert!(children.is_empty());
    });
}

// 28: Test revoking multiple children successfully
// This test verifies the successful revocation of multiple children:
// - Adds a network and registers a hotkey
// - Sets multiple children for the hotkey
// - Revokes all children by setting an empty list
// - Verifies that the children list is empty
// - Verifies that the parent-child relationships are removed for both children
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_children_multiple_success --exact --nocapture
#[test]
fn test_do_revoke_children_multiple_success() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let child2 = U256::from(4);
        let netuid: u16 = 1;
        let proportion1: u64 = 1000;
        let proportion2: u64 = 2000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set multiple children
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion1, child1), (proportion2, child2)]
        ));

        // Revoke multiple children
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![]
        ));

        // Verify children removal
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert!(children.is_empty());

        // Verify parent removal for both children
        let parents1 = SubtensorModule::get_parents(&child1, netuid);
        assert!(parents1.is_empty());

        let parents2 = SubtensorModule::get_parents(&child2, netuid);
        assert!(parents2.is_empty());
    });
}

// 29: Test revoking children when network does not exist
// This test verifies the behavior when attempting to revoke children on a non-existent network:
// - Attempts to revoke children on a network that doesn't exist
// - Verifies that the operation fails with the correct error
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_children_multiple_network_does_not_exist --exact --nocapture
#[test]
fn test_do_revoke_children_multiple_network_does_not_exist() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let child2 = U256::from(4);
        let netuid: u16 = 999; // Non-existent network
                               // Attempt to revoke children
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(u64::MAX / 2, child1), (u64::MAX / 2, child2)]
            ),
            Error::<Test>::SubNetworkDoesNotExist
        );
    });
}

// 30: Test revoking children with non-associated coldkey
// This test verifies the behavior when attempting to revoke children using a non-associated coldkey:
// - Adds a network and registers a hotkey with a different coldkey
// - Attempts to revoke children using an unassociated coldkey
// - Verifies that the operation fails with the correct error
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_children_multiple_non_associated_coldkey --exact --nocapture
#[test]
fn test_do_revoke_children_multiple_non_associated_coldkey() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let child2 = U256::from(4);
        let netuid: u16 = 1;

        // Add network and register hotkey with a different coldkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, U256::from(999), 0);

        // Attempt to revoke children
        assert_err!(
            SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkey,
                netuid,
                vec![(u64::MAX / 2, child1), (u64::MAX / 2, child2)]
            ),
            Error::<Test>::NonAssociatedColdKey
        );
    });
}

// 31: Test partial revocation of children
// This test verifies the behavior when partially revoking children:
// - Adds a network and registers a hotkey
// - Sets multiple children for the hotkey
// - Revokes one of the children
// - Verifies that the correct children remain and the revoked child is removed
// - Checks the parent-child relationships after partial revocation
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_children_multiple_partial_revocation --exact --nocapture
#[test]
fn test_do_revoke_children_multiple_partial_revocation() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let child2 = U256::from(4);
        let child3 = U256::from(5);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set multiple children
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![
                (proportion, child1),
                (proportion, child2),
                (proportion, child3)
            ]
        ));

        // Revoke only child3
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion, child1), (proportion, child2)]
        ));

        // Verify children removal
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(children, vec![(proportion, child1), (proportion, child2)]);

        // Verify parents.
        let parents1 = SubtensorModule::get_parents(&child3, netuid);
        assert!(parents1.is_empty());
        let parents1 = SubtensorModule::get_parents(&child1, netuid);
        assert_eq!(parents1, vec![(proportion, hotkey)]);
        let parents2 = SubtensorModule::get_parents(&child2, netuid);
        assert_eq!(parents2, vec![(proportion, hotkey)]);
    });
}

// 32: Test revoking non-existent children
// This test verifies the behavior when attempting to revoke non-existent children:
// - Adds a network and registers a hotkey
// - Sets one child for the hotkey
// - Attempts to revoke all children (including non-existent ones)
// - Verifies that all children are removed, including the existing one
// - Checks that the parent-child relationship is properly updated
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_children_multiple_non_existent_children --exact --nocapture
#[test]
fn test_do_revoke_children_multiple_non_existent_children() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let netuid: u16 = 1;
        let proportion: u64 = 1000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set one child
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion, child1)]
        ));

        // Attempt to revoke existing and non-existent children
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![]
        ));

        // Verify all children are removed
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert!(children.is_empty());

        // Verify parent removal for the existing child
        let parents1 = SubtensorModule::get_parents(&child1, netuid);
        assert!(parents1.is_empty());
    });
}

// 33: Test revoking children with an empty list
// This test verifies the behavior when attempting to revoke children using an empty list:
// - Adds a network and registers a hotkey
// - Attempts to revoke children with an empty list
// - Verifies that no changes occur in the children list
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_children_multiple_empty_list --exact --nocapture
#[test]
fn test_do_revoke_children_multiple_empty_list() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid: u16 = 1;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Attempt to revoke with an empty list
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![]
        ));

        // Verify no changes in children
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert!(children.is_empty());
    });
}

// 34: Test complex scenario for revoking multiple children
// This test verifies a complex scenario involving setting and revoking multiple children:
// - Adds a network and registers a hotkey
// - Sets multiple children with different proportions
// - Revokes one child and verifies the remaining children
// - Revokes all remaining children
// - Verifies that all parent-child relationships are properly updated
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_do_revoke_children_multiple_complex_scenario --exact --nocapture
#[test]
fn test_do_revoke_children_multiple_complex_scenario() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let child2 = U256::from(4);
        let child3 = U256::from(5);
        let netuid: u16 = 1;
        let proportion1: u64 = 1000;
        let proportion2: u64 = 2000;
        let proportion3: u64 = 3000;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        // Set multiple children
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![
                (proportion1, child1),
                (proportion2, child2),
                (proportion3, child3)
            ]
        ));

        // Revoke child2
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![(proportion1, child1), (proportion3, child3)]
        ));

        // Verify remaining children
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert_eq!(children, vec![(proportion1, child1), (proportion3, child3)]);

        // Verify parent removal for child2
        let parents2 = SubtensorModule::get_parents(&child2, netuid);
        assert!(parents2.is_empty());

        // Revoke remaining children
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![]
        ));

        // Verify all children are removed
        let children = SubtensorModule::get_children(&hotkey, netuid);
        assert!(children.is_empty());

        // Verify parent removal for all children
        let parents1 = SubtensorModule::get_parents(&child1, netuid);
        assert!(parents1.is_empty());
        let parents3 = SubtensorModule::get_parents(&child3, netuid);
        assert!(parents3.is_empty());
    });
}

// 35: Test getting network max stake
// This test verifies the functionality of getting the network max stake:
// - Checks the default max stake value
// - Sets a new max stake value
// - Verifies that the new value is retrieved correctly
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_get_network_max_stake --exact --nocapture
#[test]
fn test_get_network_max_stake() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let default_max_stake = SubtensorModule::get_network_max_stake(netuid);

        // Check that the default value is set correctly
        assert_eq!(default_max_stake, u64::MAX);

        // Set a new max stake value
        let new_max_stake: u64 = 1_000_000;
        SubtensorModule::set_network_max_stake(netuid, new_max_stake);

        // Check that the new value is retrieved correctly
        assert_eq!(
            SubtensorModule::get_network_max_stake(netuid),
            new_max_stake
        );
    });
}

// 36: Test setting network max stake
// This test verifies the functionality of setting the network max stake:
// - Checks the initial max stake value
// - Sets a new max stake value
// - Verifies that the new value is set correctly
// - Checks that the appropriate event is emitted
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_set_network_max_stake --exact --nocapture
#[test]
fn test_set_network_max_stake() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let initial_max_stake = SubtensorModule::get_network_max_stake(netuid);

        // Set a new max stake value
        let new_max_stake: u64 = 500_000;
        SubtensorModule::set_network_max_stake(netuid, new_max_stake);

        // Check that the new value is set correctly
        assert_eq!(
            SubtensorModule::get_network_max_stake(netuid),
            new_max_stake
        );
        assert_ne!(
            SubtensorModule::get_network_max_stake(netuid),
            initial_max_stake
        );

        // Check that the event is emitted
        System::assert_last_event(Event::NetworkMaxStakeSet(netuid, new_max_stake).into());
    });
}

// 37: Test setting network max stake for multiple networks
// This test verifies the functionality of setting different max stake values for multiple networks:
// - Sets different max stake values for two networks
// - Verifies that the values are set correctly for each network
// - Checks that the values are different between networks
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_set_network_max_stake_multiple_networks --exact --nocapture
#[test]
fn test_set_network_max_stake_multiple_networks() {
    new_test_ext(1).execute_with(|| {
        let netuid1: u16 = 1;
        let netuid2: u16 = 2;

        // Set different max stake values for two networks
        let max_stake1: u64 = 1_000_000;
        let max_stake2: u64 = 2_000_000;
        SubtensorModule::set_network_max_stake(netuid1, max_stake1);
        SubtensorModule::set_network_max_stake(netuid2, max_stake2);

        // Check that the values are set correctly for each network
        assert_eq!(SubtensorModule::get_network_max_stake(netuid1), max_stake1);
        assert_eq!(SubtensorModule::get_network_max_stake(netuid2), max_stake2);
        assert_ne!(
            SubtensorModule::get_network_max_stake(netuid1),
            SubtensorModule::get_network_max_stake(netuid2)
        );
    });
}

// 38: Test updating network max stake
// This test verifies the functionality of updating an existing network max stake value:
// - Sets an initial max stake value
// - Updates the max stake value
// - Verifies that the value is updated correctly
// - Checks that the appropriate event is emitted for the update
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_set_network_max_stake_update --exact --nocapture
#[test]
fn test_set_network_max_stake_update() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;

        // Set an initial max stake value
        let initial_max_stake: u64 = 1_000_000;
        SubtensorModule::set_network_max_stake(netuid, initial_max_stake);

        // Update the max stake value
        let updated_max_stake: u64 = 1_500_000;
        SubtensorModule::set_network_max_stake(netuid, updated_max_stake);

        // Check that the value is updated correctly
        assert_eq!(
            SubtensorModule::get_network_max_stake(netuid),
            updated_max_stake
        );
        assert_ne!(
            SubtensorModule::get_network_max_stake(netuid),
            initial_max_stake
        );

        // Check that the event is emitted for the update
        System::assert_last_event(Event::NetworkMaxStakeSet(netuid, updated_max_stake).into());
    });
}

// 39: Test children stake values
// This test verifies the correct distribution of stake among parent and child neurons:
// - Sets up a network with a parent neuron and multiple child neurons
// - Assigns stake to the parent neuron
// - Sets child neurons with specific proportions
// - Verifies that the stake is correctly distributed among parent and child neurons
// - Checks that the total stake remains constant across all neurons
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_children_stake_values --exact --nocapture
#[test]
fn test_children_stake_values() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let child1 = U256::from(3);
        let child2 = U256::from(4);
        let child3 = U256::from(5);
        let netuid: u16 = 1;
        let proportion1: u64 = u64::MAX / 4;
        let proportion2: u64 = u64::MAX / 4;
        let proportion3: u64 = u64::MAX / 4;

        // Add network and register hotkey
        add_network(netuid, 13, 0);
        SubtensorModule::set_max_registrations_per_block(netuid, 4);
        SubtensorModule::set_target_registrations_per_interval(netuid, 4);
        register_ok_neuron(netuid, hotkey, coldkey, 0);
        register_ok_neuron(netuid, child1, coldkey, 0);
        register_ok_neuron(netuid, child2, coldkey, 0);
        register_ok_neuron(netuid, child3, coldkey, 0);
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey,
            &hotkey,
            100_000_000_000_000,
        );

        // Set multiple children with proportions.
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            hotkey,
            netuid,
            vec![
                (proportion1, child1),
                (proportion2, child2),
                (proportion3, child3)
            ]
        ));
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey, netuid),
            25_000_000_069_852
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child1, netuid),
            24_999_999_976_716
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child2, netuid),
            24_999_999_976_716
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child3, netuid),
            24_999_999_976_716
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child3, netuid)
                + SubtensorModule::get_stake_for_hotkey_on_subnet(&child2, netuid)
                + SubtensorModule::get_stake_for_hotkey_on_subnet(&child1, netuid)
                + SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey, netuid),
            100_000_000_000_000
        );
    });
}

// 40: Test getting parents chain
// This test verifies the correct implementation of parent-child relationships and the get_parents function:
// - Sets up a network with multiple neurons in a chain of parent-child relationships
// - Verifies that each neuron has the correct parent
// - Tests the root neuron has no parents
// - Tests a neuron with multiple parents
// - Verifies correct behavior when adding a new parent to an existing child
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test children -- test_get_parents_chain --exact --nocapture
#[test]
fn test_get_parents_chain() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let coldkey = U256::from(1);
        let num_keys: usize = 5;
        let proportion = u64::MAX / 2; // 50% stake allocation

        log::info!(
            "Test setup: netuid={}, coldkey={}, num_keys={}, proportion={}",
            netuid,
            coldkey,
            num_keys,
            proportion
        );

        // Create a vector of hotkeys
        let hotkeys: Vec<U256> = (0..num_keys).map(|i| U256::from(i as u64 + 2)).collect();
        log::info!("Created hotkeys: {:?}", hotkeys);

        // Add network
        add_network(netuid, 13, 0);
        SubtensorModule::set_max_registrations_per_block(netuid, 1000);
        SubtensorModule::set_target_registrations_per_interval(netuid, 1000);
        log::info!("Network added and parameters set: netuid={}", netuid);

        // Register all neurons
        for hotkey in &hotkeys {
            register_ok_neuron(netuid, *hotkey, coldkey, 0);
            log::info!(
                "Registered neuron: hotkey={}, coldkey={}, netuid={}",
                hotkey,
                coldkey,
                netuid
            );
        }

        // Set up parent-child relationships
        for i in 0..num_keys - 1 {
            assert_ok!(SubtensorModule::do_set_children(
                RuntimeOrigin::signed(coldkey),
                hotkeys[i],
                netuid,
                vec![(proportion, hotkeys[i + 1])]
            ));
            log::info!(
                "Set parent-child relationship: parent={}, child={}, proportion={}",
                hotkeys[i],
                hotkeys[i + 1],
                proportion
            );
        }

        // Test get_parents for each hotkey
        for i in 1..num_keys {
            let parents = SubtensorModule::get_parents(&hotkeys[i], netuid);
            log::info!(
                "Testing get_parents for hotkey {}: {:?}",
                hotkeys[i],
                parents
            );
            assert_eq!(
                parents.len(),
                1,
                "Hotkey {} should have exactly one parent",
                i
            );
            assert_eq!(
                parents[0],
                (proportion, hotkeys[i - 1]),
                "Incorrect parent for hotkey {}",
                i
            );
        }

        // Test get_parents for the root (should be empty)
        let root_parents = SubtensorModule::get_parents(&hotkeys[0], netuid);
        log::info!(
            "Testing get_parents for root hotkey {}: {:?}",
            hotkeys[0],
            root_parents
        );
        assert!(
            root_parents.is_empty(),
            "Root hotkey should have no parents"
        );

        // Test multiple parents
        let last_hotkey = hotkeys[num_keys - 1];
        let new_parent = U256::from(num_keys as u64 + 2);
        register_ok_neuron(netuid, new_parent, coldkey, 0);
        log::info!(
            "Registered new parent neuron: new_parent={}, coldkey={}, netuid={}",
            new_parent,
            coldkey,
            netuid
        );

        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            new_parent,
            netuid,
            vec![(proportion / 2, last_hotkey)]
        ));
        log::info!(
            "Set additional parent-child relationship: parent={}, child={}, proportion={}",
            new_parent,
            last_hotkey,
            proportion / 2
        );

        let last_hotkey_parents = SubtensorModule::get_parents(&last_hotkey, netuid);
        log::info!(
            "Testing get_parents for last hotkey {} with multiple parents: {:?}",
            last_hotkey,
            last_hotkey_parents
        );
        assert_eq!(
            last_hotkey_parents.len(),
            2,
            "Last hotkey should have two parents"
        );
        assert!(
            last_hotkey_parents.contains(&(proportion, hotkeys[num_keys - 2])),
            "Last hotkey should still have its original parent"
        );
        assert!(
            last_hotkey_parents.contains(&(proportion / 2, new_parent)),
            "Last hotkey should have the new parent"
        );
    });
}

// 41: Test emission distribution between a childkey and a single parent
// This test verifies the correct distribution of emissions between a child and a single parent:
// - Sets up a network with a parent, child, and weight setter
// - Establishes a parent-child relationship
// - Sets weights on the child
// - Runs an epoch with a hardcoded emission value
// - Checks the emission distribution among parent, child, and weight setter
// - Verifies that all parties received emissions and the weight setter received the most
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children test_childkey_single_parent_emission -- --nocapture
#[test]
fn test_childkey_single_parent_emission() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        add_network(netuid, 1, 0);

        // Define hotkeys
        let parent: U256 = U256::from(1);
        let child: U256 = U256::from(2);
        let weight_setter: U256 = U256::from(3);

        // Define coldkeys with more readable names
        let coldkey_parent: U256 = U256::from(100);
        let coldkey_child: U256 = U256::from(101);
        let coldkey_weight_setter: U256 = U256::from(102);

        // Register parent with minimal stake and child with high stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_parent, 1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_child, 109_999);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_weight_setter, 1_000_000);

        // Add neurons for parent, child and weight_setter
        register_ok_neuron(netuid, parent, coldkey_parent, 1);
        register_ok_neuron(netuid, child, coldkey_child, 1);
        register_ok_neuron(netuid, weight_setter, coldkey_weight_setter, 1);

        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey_parent,
            &parent,
            109_999,
        );
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey_weight_setter,
            &weight_setter,
            1_000_000,
        );

        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        // Set parent-child relationship
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey_parent),
            parent,
            netuid,
            vec![(u64::MAX, child)]
        ));
        step_block(7200 + 1);
        // Set weights on the child using the weight_setter account
        let origin = RuntimeOrigin::signed(weight_setter);
        let uids: Vec<u16> = vec![1]; // Only set weight for the child (UID 1)
        let values: Vec<u16> = vec![u16::MAX]; // Use maximum value for u16
        let version_key = SubtensorModule::get_weights_version_key(netuid);
        assert_ok!(SubtensorModule::set_weights(
            origin,
            netuid,
            uids,
            values,
            version_key
        ));

        // Run epoch with a hardcoded emission value
        let hardcoded_emission: u64 = 1_000_000_000; // 1 TAO
        let hotkey_emission: Vec<(U256, u64, u64)> =
            SubtensorModule::epoch(netuid, hardcoded_emission);

        // Process the hotkey emission results
        for (hotkey, mining_emission, validator_emission) in hotkey_emission {
            SubtensorModule::accumulate_hotkey_emission(
                &hotkey,
                netuid,
                validator_emission,
                mining_emission,
            );
            log::debug!(
                "Accumulated emissions on hotkey {:?} for netuid {:?}: mining {:?}, validator {:?}",
                hotkey,
                netuid,
                mining_emission,
                validator_emission
            );
        }
        step_block(7200 + 1);
        // Check emission distribution
        let parent_stake: u64 =
            SubtensorModule::get_stake_for_coldkey_and_hotkey(&coldkey_parent, &parent);
        let parent_stake_on_subnet: u64 =
            SubtensorModule::get_stake_for_hotkey_on_subnet(&parent, netuid);

        log::debug!(
            "Parent stake: {:?}, Parent stake on subnet: {:?}",
            parent_stake,
            parent_stake_on_subnet
        );

        let child_stake: u64 =
            SubtensorModule::get_stake_for_coldkey_and_hotkey(&coldkey_child, &child);
        let child_stake_on_subnet: u64 =
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child, netuid);

        log::debug!(
            "Child stake: {:?}, Child stake on subnet: {:?}",
            child_stake,
            child_stake_on_subnet
        );

        let weight_setter_stake: u64 = SubtensorModule::get_stake_for_coldkey_and_hotkey(
            &coldkey_weight_setter,
            &weight_setter,
        );
        let weight_setter_stake_on_subnet: u64 =
            SubtensorModule::get_stake_for_hotkey_on_subnet(&weight_setter, netuid);

        log::debug!(
            "Weight setter stake: {:?}, Weight setter stake on subnet: {:?}",
            weight_setter_stake,
            weight_setter_stake_on_subnet
        );

        assert!(parent_stake > 1, "Parent should have received emission");
        assert!(child_stake > 109_999, "Child should have received emission");
        assert!(
            weight_setter_stake > 1_000_000,
            "Weight setter should have received emission"
        );

        // Additional assertion to verify that the weight setter received the most emission
        assert!(
            weight_setter_stake > parent_stake && weight_setter_stake > child_stake,
            "Weight setter should have received the most emission"
        );
    });
}

// 43: Test emission distribution between a childkey and multiple parents
// This test verifies the correct distribution of emissions between a child and multiple parents:
// - Sets up a network with two parents, a child, and a weight setter
// - Establishes parent-child relationships with different stake proportions
// - Sets weights on the child and one parent
// - Runs an epoch with a hardcoded emission value
// - Checks the emission distribution among parents, child, and weight setter
// - Verifies that all parties received emissions and the total stake increased correctly
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test coinbase test_childkey_multiple_parents_emission -- --nocapture
#[test]
fn test_childkey_multiple_parents_emission() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        add_network(netuid, 1, 0);

        // Set registration parameters and emission tempo
        SubtensorModule::set_max_registrations_per_block(netuid, 1000);
        SubtensorModule::set_target_registrations_per_interval(netuid, 1000);
        SubtensorModule::set_hotkey_emission_tempo(10);

        // Define hotkeys and coldkeys
        let parent1: U256 = U256::from(1);
        let parent2: U256 = U256::from(2);
        let child: U256 = U256::from(3);
        let weight_setter: U256 = U256::from(4);
        let coldkey_parent1: U256 = U256::from(100);
        let coldkey_parent2: U256 = U256::from(101);
        let coldkey_child: U256 = U256::from(102);
        let coldkey_weight_setter: U256 = U256::from(103);

        // Register neurons and add initial stakes
        let initial_stakes: Vec<(U256, U256, u64)> = vec![
            (coldkey_parent1, parent1, 200_000),
            (coldkey_parent2, parent2, 150_000),
            (coldkey_child, child, 20_000),
            (coldkey_weight_setter, weight_setter, 100_000),
        ];

        for (coldkey, hotkey, stake) in initial_stakes.iter() {
            SubtensorModule::add_balance_to_coldkey_account(coldkey, *stake);
            register_ok_neuron(netuid, *hotkey, *coldkey, 0);
            SubtensorModule::increase_stake_on_coldkey_hotkey_account(coldkey, hotkey, *stake);
        }

        SubtensorModule::set_weights_set_rate_limit(netuid, 0);
        step_block(2);

        // Set parent-child relationships
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey_parent1),
            parent1,
            netuid,
            vec![(100_000, child)]
        ));
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey_parent2),
            parent2,
            netuid,
            vec![(75_000, child)]
        ));

        // Set weights
        let uids: Vec<u16> = vec![0, 1, 2];
        let values: Vec<u16> = vec![0, 65354, 65354];
        let version_key = SubtensorModule::get_weights_version_key(netuid);
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(weight_setter),
            netuid,
            uids,
            values,
            version_key
        ));

        // Run epoch with a hardcoded emission value
        let hardcoded_emission: u64 = 1_000_000_000; // 1 billion
        let hotkey_emission: Vec<(U256, u64, u64)> =
            SubtensorModule::epoch(netuid, hardcoded_emission);

        // Process the hotkey emission results
        for (hotkey, mining_emission, validator_emission) in hotkey_emission {
            SubtensorModule::accumulate_hotkey_emission(
                &hotkey,
                netuid,
                validator_emission,
                mining_emission,
            );
            log::debug!(
                "Accumulated emissions on hotkey {:?} for netuid {:?}: mining {:?}, validator {:?}",
                hotkey,
                netuid,
                mining_emission,
                validator_emission
            );
        }

        step_block(11);

        // Check emission distribution
        let stakes: Vec<(U256, U256, &str)> = vec![
            (coldkey_parent1, parent1, "Parent1"),
            (coldkey_parent2, parent2, "Parent2"),
            (coldkey_child, child, "Child"),
            (coldkey_weight_setter, weight_setter, "Weight setter"),
        ];

        for (coldkey, hotkey, name) in stakes.iter() {
            let stake = SubtensorModule::get_stake_for_coldkey_and_hotkey(coldkey, hotkey);
            let stake_on_subnet = SubtensorModule::get_stake_for_hotkey_on_subnet(hotkey, netuid);
            log::debug!(
                "{} stake: {:?}, {} stake on subnet: {:?}",
                name,
                stake,
                name,
                stake_on_subnet
            );
        }

        let parent1_stake =
            SubtensorModule::get_stake_for_coldkey_and_hotkey(&coldkey_parent1, &parent1);
        let parent2_stake =
            SubtensorModule::get_stake_for_coldkey_and_hotkey(&coldkey_parent2, &parent2);
        let child_stake = SubtensorModule::get_stake_for_coldkey_and_hotkey(&coldkey_child, &child);
        let weight_setter_stake = SubtensorModule::get_stake_for_coldkey_and_hotkey(
            &coldkey_weight_setter,
            &weight_setter,
        );

        assert!(
            parent1_stake > 200_000,
            "Parent1 should have received emission"
        );
        assert!(
            parent2_stake > 150_000,
            "Parent2 should have received emission"
        );
        assert!(child_stake > 20_000, "Child should have received emission");
        assert!(
            weight_setter_stake > 100_000,
            "Weight setter should have received emission"
        );

        // Check individual stake increases
        let parent1_stake_increase = parent1_stake - 200_000;
        let parent2_stake_increase = parent2_stake - 150_000;
        let child_stake_increase = child_stake - 20_000;

        log::debug!(
            "Stake increases - Parent1: {}, Parent2: {}, Child: {}",
            parent1_stake_increase,
            parent2_stake_increase,
            child_stake_increase
        );

        // Assert that all neurons received some emission
        assert!(
            parent1_stake_increase > 0,
            "Parent1 should have received some emission"
        );
        assert!(
            parent2_stake_increase > 0,
            "Parent2 should have received some emission"
        );
        assert!(
            child_stake_increase > 0,
            "Child should have received some emission"
        );

        // Check that the total stake has increased by the hardcoded emission amount
        let total_stake = parent1_stake + parent2_stake + child_stake + weight_setter_stake;
        let initial_total_stake: u64 = initial_stakes.iter().map(|(_, _, stake)| stake).sum();
        assert_eq!(
            total_stake,
            initial_total_stake + hardcoded_emission - 2, // U64::MAX normalization rounding error
            "Total stake should have increased by the hardcoded emission amount"
        );
    });
}

// 44: Test with a chain of parent-child relationships (e.g., A -> B -> C)
// This test verifies the correct distribution of emissions in a chain of parent-child relationships:
// - Sets up a network with three neurons A, B, and C in a chain (A -> B -> C)
// - Establishes parent-child relationships with different stake proportions
// - Sets weights for all neurons
// - Runs an epoch with a hardcoded emission value
// - Checks the emission distribution among A, B, and C
// - Verifies that all parties received emissions and the total stake increased correctly
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test coinbase test_parent_child_chain_emission -- --nocapture
#[test]
fn test_parent_child_chain_emission() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        add_network(netuid, 1, 0);

        // Define hotkeys and coldkeys
        let hotkey_a: U256 = U256::from(1);
        let hotkey_b: U256 = U256::from(2);
        let hotkey_c: U256 = U256::from(3);
        let coldkey_a: U256 = U256::from(100);
        let coldkey_b: U256 = U256::from(101);
        let coldkey_c: U256 = U256::from(102);

        // Register neurons with decreasing stakes
        register_ok_neuron(netuid, hotkey_a, coldkey_a, 0);
        register_ok_neuron(netuid, hotkey_b, coldkey_b, 0);
        register_ok_neuron(netuid, hotkey_c, coldkey_c, 0);

        // Add initial stakes
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_a, 300_000);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_b, 100_000);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_c, 50_000);

        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey_a, &hotkey_a, 300_000);
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey_b, &hotkey_b, 100_000);
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey_c, &hotkey_c, 50_000);

        // Set parent-child relationships
        // A -> B (50% of A's stake)
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey_a),
            hotkey_a,
            netuid,
            vec![(u64::MAX / 2, hotkey_b)]
        ));
        // B -> C (50% of B's stake)
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey_b),
            hotkey_b,
            netuid,
            vec![(u64::MAX / 2, hotkey_c)]
        ));

        step_block(2);

        // Set weights
        let origin = RuntimeOrigin::signed(hotkey_a);
        let uids: Vec<u16> = vec![0, 1, 2]; // UIDs for hotkey_a, hotkey_b, hotkey_c
        let values: Vec<u16> = vec![65535, 65535, 65535]; // Set equal weights for all hotkeys
        let version_key = SubtensorModule::get_weights_version_key(netuid);

        // Ensure we can set weights without rate limiting
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        assert_ok!(SubtensorModule::set_weights(
            origin,
            netuid,
            uids,
            values,
            version_key
        ));

        // Run epoch with a hardcoded emission value
        let hardcoded_emission: u64 = 1_000_000; // 1 million (adjust as needed)
        let hotkey_emission: Vec<(U256, u64, u64)> =
            SubtensorModule::epoch(netuid, hardcoded_emission);

        // Process the hotkey emission results
        for (hotkey, mining_emission, validator_emission) in hotkey_emission {
            SubtensorModule::accumulate_hotkey_emission(
                &hotkey,
                netuid,
                validator_emission,
                mining_emission,
            );
        }

        // Log PendingEmission Tuple for a, b, c
        let pending_emission_a = SubtensorModule::get_pending_hotkey_emission(&hotkey_a);
        let pending_emission_b = SubtensorModule::get_pending_hotkey_emission(&hotkey_b);
        let pending_emission_c = SubtensorModule::get_pending_hotkey_emission(&hotkey_c);

        log::info!("Pending Emission for A: {:?}", pending_emission_a);
        log::info!("Pending Emission for B: {:?}", pending_emission_b);
        log::info!("Pending Emission for C: {:?}", pending_emission_c);

        // Assert that pending emissions are non-zero
        // A's pending emission: 2/3 of total emission (due to having 2/3 of total stake)
        assert!(
            pending_emission_a == 666667,
            "A should have pending emission of 2/3 of total emission"
        );
        // B's pending emission: 2/9 of total emission (1/3 of A's emission + 1/3 of total emission)
        assert!(
            pending_emission_b == 222222,
            "B should have pending emission of 2/9 of total emission"
        );
        // C's pending emission: 1/9 of total emission (1/2 of B's emission)
        assert!(
            pending_emission_c == 111109,
            "C should have pending emission of 1/9 of total emission"
        );

        SubtensorModule::set_hotkey_emission_tempo(10);

        step_block(10 + 1);
        // Retrieve the current stake for each hotkey on the subnet
        let stake_a: u64 = SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey_a, netuid);
        let stake_b: u64 = SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey_b, netuid);
        let stake_c: u64 = SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey_c, netuid);

        // Log the current stakes for debugging purposes
        log::info!("Stake for hotkey A: {:?}", stake_a);
        log::info!("Stake for hotkey B: {:?}", stake_b);
        log::info!("Stake for hotkey C: {:?}", stake_c);

        // Assert that the stakes have been updated correctly after emission distribution
        assert_eq!(
            stake_a, 483334,
            "A's stake should be 483334 (initial 300_000 + 666667 emission - 483333 given to B)"
        );
        assert_eq!(
            stake_b, 644445,
            "B's stake should be 644445 (initial 100_000 + 222222 emission + 483333 from A - 161110 given to C)"
        );
        assert_eq!(
            stake_c, 322219,
            "C's stake should be 322219 (initial 50_000 + 111109 emission + 161110 from B)"
        );

        // Check that the total stake has increased by the hardcoded emission amount
        let total_stake = stake_a + stake_b + stake_c;
        let initial_total_stake = 300_000 + 100_000 + 50_000;
        let hardcoded_emission = 1_000_000; // Define the hardcoded emission value
        assert_eq!(
            total_stake,
            initial_total_stake + hardcoded_emission - 2, // U64::MAX normalization rounding error
            "Total stake should have increased by the hardcoded emission amount"
        );
    });
}

// 46: Test emission distribution when adding/removing parent-child relationships mid-epoch
// This test verifies the correct distribution of emissions when parent-child relationships change:
// - Sets up a network with three neurons: parent, child1, and child2
// - Establishes initial parent-child relationship between parent and child1
// - Runs first epoch and distributes emissions
// - Changes parent-child relationships to include both child1 and child2
// - Runs second epoch and distributes emissions
// - Checks final emission distribution and stake updates
// - Verifies correct parent-child relationships and stake proportions
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children -- test_dynamic_parent_child_relationships --exact --nocapture
#[test]
fn test_dynamic_parent_child_relationships() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        add_network(netuid, 1, 0);

        // Define hotkeys and coldkeys
        let parent: U256 = U256::from(1);
        let child1: U256 = U256::from(2);
        let child2: U256 = U256::from(3);
        let coldkey_parent: U256 = U256::from(100);
        let coldkey_child1: U256 = U256::from(101);
        let coldkey_child2: U256 = U256::from(102);

        // Register neurons with varying stakes
        register_ok_neuron(netuid, parent, coldkey_parent, 0);
        register_ok_neuron(netuid, child1, coldkey_child1, 0);
        register_ok_neuron(netuid, child2, coldkey_child2, 0);

        // Add initial stakes
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_parent, 500_000);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_child1, 50_000);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_child2, 30_000);

        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey_parent, &parent, 500_000);
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey_child1, &child1, 50_000);
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey_child2, &child2, 30_000);

        // Set initial parent-child relationship
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey_parent),
            parent,
            netuid,
            vec![(u64::MAX / 2, child1)]
        ));

        step_block(2);

        // Set weights
        let origin = RuntimeOrigin::signed(parent);
        let uids: Vec<u16> = vec![0, 1, 2]; // UIDs for parent, child1, child2
        let values: Vec<u16> = vec![65535, 65535, 65535]; // Set equal weights for all hotkeys
        let version_key = SubtensorModule::get_weights_version_key(netuid);

        // Ensure we can set weights without rate limiting
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        assert_ok!(SubtensorModule::set_weights(
            origin,
            netuid,
            uids,
            values,
            version_key
        ));

        // Set hotkey emission tempo
        SubtensorModule::set_hotkey_emission_tempo(10);

        // Run first epoch
        let hardcoded_emission: u64 = 1_000_000; // 1 million (adjust as needed)
        let hotkey_emission: Vec<(U256, u64, u64)> = SubtensorModule::epoch(netuid, hardcoded_emission);

        // Process the hotkey emission results
        for (hotkey, mining_emission, validator_emission) in hotkey_emission {
            SubtensorModule::accumulate_hotkey_emission(&hotkey, netuid, validator_emission, mining_emission);
        }

        // Step blocks to allow for emission distribution
        step_block(11);

        // Change parent-child relationships
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey_parent),
            parent,
            netuid,
            vec![(u64::MAX / 4, child1), (u64::MAX / 3, child2)]
        ));

        // Run second epoch
        let hotkey_emission: Vec<(U256, u64, u64)> = SubtensorModule::epoch(netuid, hardcoded_emission);

        // Process the hotkey emission results
        for (hotkey, mining_emission, validator_emission) in hotkey_emission {
            SubtensorModule::accumulate_hotkey_emission(&hotkey, netuid, validator_emission, mining_emission);
        }

        // Step blocks again to allow for emission distribution
        step_block(11);

        // Check final emission distribution
        let parent_stake: u64 = SubtensorModule::get_stake_for_hotkey_on_subnet(&parent, netuid);
        let child1_stake: u64 = SubtensorModule::get_stake_for_hotkey_on_subnet(&child1, netuid);
        let child2_stake: u64 = SubtensorModule::get_stake_for_hotkey_on_subnet(&child2, netuid);

        log::info!("Final stakes:");
        log::info!("Parent stake: {}", parent_stake);
        log::info!("Child1 stake: {}", child1_stake);
        log::info!("Child2 stake: {}", child2_stake);

        const TOLERANCE: u64 = 5; // Allow for a small discrepancy due to potential rounding

        // Precise assertions with tolerance
        assert!(
            (parent_stake as i64 - 926725).abs() <= TOLERANCE as i64,
            "Parent stake should be close to 926,725, but was {}",
            parent_stake
        );
        // Parent stake calculation:
        // Initial stake: 500,000
        // First epoch: ~862,500 (500,000 + 725,000 * 1/2)
        // Second epoch: ~926,725 (862,500 + 725,000 * 5/12)

        assert!(
            (child1_stake as i64 - 778446).abs() <= TOLERANCE as i64,
            "Child1 stake should be close to 778,446, but was {}",
            child1_stake
        );
        // Child1 stake calculation:
        // Initial stake: 50,000
        // First epoch: ~412,500 (50,000 + 725,000 * 1/2)
        // Second epoch: ~778,446 (412,500 + 725,000 * 1/2 * 1/4 + 137,500)

        assert!(
            (child2_stake as i64 - 874826).abs() <= TOLERANCE as i64,
            "Child2 stake should be close to 874,826, but was {}",
            child2_stake
        );
        // Child2 stake calculation:
        // Initial stake: 30,000
        // First epoch: ~167,500 (30,000 + 137,500)
        // Second epoch: ~874,826 (167,500 + 725,000 * 1/2 * 1/3 + 137,500)

        // Check that the total stake has increased by approximately twice the hardcoded emission amount
        let total_stake: u64 = parent_stake + child1_stake + child2_stake;
        let initial_total_stake: u64 = 500_000 + 50_000 + 30_000;
        let total_emission: u64 = 2 * hardcoded_emission;
        assert!(
            (total_stake as i64 - (initial_total_stake + total_emission) as i64).abs() <= TOLERANCE as i64,
            "Total stake should have increased by approximately twice the hardcoded emission amount"
        );
        // Total stake calculation:
        // Initial total stake: 500,000 + 50,000 + 30,000 = 580,000
        // Total emission: 2 * 1,000,000 = 2,000,000
        // Expected total stake: 580,000 + 2,000,000 = 2,580,000

        // Additional checks for parent-child relationships
        let parent_children: Vec<(u64, U256)> = SubtensorModule::get_children(&parent, netuid);
        assert_eq!(
            parent_children,
            vec![(u64::MAX / 4, child1), (u64::MAX / 3, child2)],
            "Parent should have both children with correct proportions"
        );
        // Parent-child relationship:
        // child1: 1/4 of parent's stake
        // child2: 1/3 of parent's stake

        let child1_parents: Vec<(u64, U256)> = SubtensorModule::get_parents(&child1, netuid);
        assert_eq!(
            child1_parents,
            vec![(u64::MAX / 4, parent)],
            "Child1 should have parent as its parent with correct proportion"
        );
        // Child1-parent relationship:
        // parent: 1/4 of child1's stake

        let child2_parents: Vec<(u64, U256)> = SubtensorModule::get_parents(&child2, netuid);
        assert_eq!(
            child2_parents,
            vec![(u64::MAX / 3, parent)],
            "Child2 should have parent as its parent with correct proportion"
        );
        // Child2-parent relationship:
        // parent: 1/3 of child2's stake

        // Check that child2 has received more stake than child1
        assert!(
            child2_stake > child1_stake,
            "Child2 should have received more emission than Child1 due to higher proportion"
        );
        // Child2 stake (874,826) > Child1 stake (778,446)

        // Check the approximate difference between child2 and child1 stakes
        let stake_difference: u64 = child2_stake - child1_stake;
        assert!(
            (stake_difference as i64 - 96_380).abs() <= TOLERANCE as i64,
            "The difference between Child2 and Child1 stakes should be close to 96,380, but was {}",
            stake_difference
        );
        // Stake difference calculation:
        // Child2 stake: 874,826
        // Child1 stake: 778,446
        // Difference: 874,826 - 778,446 = 96,380
    });
}

// 47: Test basic stake retrieval for a single hotkey on a subnet
/// This test verifies the basic functionality of retrieving stake for a single hotkey on a subnet:
/// - Sets up a network with one neuron
/// - Increases stake for the neuron
/// - Checks if the retrieved stake matches the increased amount
///     SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children -- test_get_stake_for_hotkey_on_subnet_basic --exact --nocapture
#[test]
fn test_get_stake_for_hotkey_on_subnet_basic() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey = U256::from(1);
        let coldkey = U256::from(2);

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);

        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey, &hotkey, 1000);

        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey, netuid),
            1000
        );
    });
}

// 48: Test stake retrieval for a hotkey with multiple coldkeys on a subnet
/// This test verifies the functionality of retrieving stake for a hotkey with multiple coldkeys on a subnet:
/// - Sets up a network with one neuron and two coldkeys
/// - Increases stake from both coldkeys
/// - Checks if the retrieved stake matches the total increased amount
///     SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children -- test_get_stake_for_hotkey_on_subnet_multiple_coldkeys --exact --nocapture
#[test]
fn test_get_stake_for_hotkey_on_subnet_multiple_coldkeys() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey = U256::from(1);
        let coldkey1 = U256::from(2);
        let coldkey2 = U256::from(3);

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey1, 0);

        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey1, &hotkey, 1000);
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey2, &hotkey, 2000);

        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey, netuid),
            3000
        );
    });
}

// 49: Test stake retrieval for a single parent-child relationship on a subnet
/// This test verifies the functionality of retrieving stake for a single parent-child relationship on a subnet:
/// - Sets up a network with a parent and child neuron
/// - Increases stake for the parent
/// - Sets the child as the parent's only child with 100% stake allocation
/// - Checks if the retrieved stake for both parent and child is correct
///     SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children -- test_get_stake_for_hotkey_on_subnet_single_parent_child --exact --nocapture
#[test]
fn test_get_stake_for_hotkey_on_subnet_single_parent_child() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let parent = U256::from(1);
        let child = U256::from(2);
        let coldkey = U256::from(3);

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, parent, coldkey, 0);
        register_ok_neuron(netuid, child, coldkey, 0);

        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey, &parent, 1000);

        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            parent,
            netuid,
            vec![(u64::MAX, child)]
        ));

        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&parent, netuid),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child, netuid),
            1000
        );
    });
}

// 50: Test stake retrieval for multiple parents and a single child on a subnet
/// This test verifies the functionality of retrieving stake for multiple parents and a single child on a subnet:
/// - Sets up a network with two parents and one child neuron
/// - Increases stake for both parents
/// - Sets the child as a 50% stake recipient for both parents
/// - Checks if the retrieved stake for parents and child is correct
///     SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children -- test_get_stake_for_hotkey_on_subnet_multiple_parents_single_child --exact --nocapture
#[test]
fn test_get_stake_for_hotkey_on_subnet_multiple_parents_single_child() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let parent1 = U256::from(1);
        let parent2 = U256::from(2);
        let child = U256::from(3);
        let coldkey = U256::from(4);

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, parent1, coldkey, 0);
        register_ok_neuron(netuid, parent2, coldkey, 0);
        register_ok_neuron(netuid, child, coldkey, 0);

        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey, &parent1, 1000);
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey, &parent2, 2000);

        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            parent1,
            netuid,
            vec![(u64::MAX / 2, child)]
        ));
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            parent2,
            netuid,
            vec![(u64::MAX / 2, child)]
        ));

        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&parent1, netuid),
            501
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&parent2, netuid),
            1001
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child, netuid),
            1498
        );
    });
}

// 51: Test stake retrieval for a single parent with multiple children on a subnet
/// This test verifies the functionality of retrieving stake for a single parent with multiple children on a subnet:
/// - Sets up a network with one parent and two child neurons
/// - Increases stake for the parent
/// - Sets both children as 1/3 stake recipients of the parent
/// - Checks if the retrieved stake for parent and children is correct and preserves total stake
///     SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children -- test_get_stake_for_hotkey_on_subnet_single_parent_multiple_children --exact --nocapture
#[test]
fn test_get_stake_for_hotkey_on_subnet_single_parent_multiple_children() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let parent = U256::from(1);
        let child1 = U256::from(2);
        let child2 = U256::from(3);
        let coldkey = U256::from(4);

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, parent, coldkey, 0);
        register_ok_neuron(netuid, child1, coldkey, 0);
        register_ok_neuron(netuid, child2, coldkey, 0);

        let total_stake = 3000;
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey, &parent, total_stake);

        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            parent,
            netuid,
            vec![(u64::MAX / 3, child1), (u64::MAX / 3, child2)]
        ));

        let parent_stake = SubtensorModule::get_stake_for_hotkey_on_subnet(&parent, netuid);
        let child1_stake = SubtensorModule::get_stake_for_hotkey_on_subnet(&child1, netuid);
        let child2_stake = SubtensorModule::get_stake_for_hotkey_on_subnet(&child2, netuid);

        // Check that the total stake is preserved
        assert_eq!(parent_stake + child1_stake + child2_stake, total_stake);

        // Check that the parent stake is slightly higher due to rounding
        assert_eq!(parent_stake, 1002);

        // Check that each child gets an equal share of the remaining stake
        assert_eq!(child1_stake, 999);
        assert_eq!(child2_stake, 999);

        // Log the actual stake values
        log::info!("Parent stake: {}", parent_stake);
        log::info!("Child1 stake: {}", child1_stake);
        log::info!("Child2 stake: {}", child2_stake);
    });
}

// 52: Test stake retrieval for edge cases on a subnet
/// This test verifies the functionality of retrieving stake for edge cases on a subnet:
/// - Sets up a network with one parent and two child neurons
/// - Increases stake to the network maximum
/// - Sets children with 0% and 100% stake allocation
/// - Checks if the retrieved stake for parent and children is correct and preserves total stake
///     SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children -- test_get_stake_for_hotkey_on_subnet_edge_cases --exact --nocapture
#[test]
fn test_get_stake_for_hotkey_on_subnet_edge_cases() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let parent = U256::from(1);
        let child1 = U256::from(2);
        let child2 = U256::from(3);
        let coldkey = U256::from(4);

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, parent, coldkey, 0);
        register_ok_neuron(netuid, child1, coldkey, 0);
        register_ok_neuron(netuid, child2, coldkey, 0);

        // Set network max stake
        let network_max_stake: u64 = 500_000_000_000_000; // 500_000 TAO
        SubtensorModule::set_network_max_stake(netuid, network_max_stake);

        // Increase stake to the network max
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey,
            &parent,
            network_max_stake,
        );

        // Test with 0% and 100% stake allocation
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey),
            parent,
            netuid,
            vec![(0, child1), (u64::MAX, child2)]
        ));

        let parent_stake = SubtensorModule::get_stake_for_hotkey_on_subnet(&parent, netuid);
        let child1_stake = SubtensorModule::get_stake_for_hotkey_on_subnet(&child1, netuid);
        let child2_stake = SubtensorModule::get_stake_for_hotkey_on_subnet(&child2, netuid);

        log::info!("Parent stake: {}", parent_stake);
        log::info!("Child1 stake: {}", child1_stake);
        log::info!("Child2 stake: {}", child2_stake);

        assert_eq!(parent_stake, 0, "Parent should have 0 stake");
        assert_eq!(child1_stake, 0, "Child1 should have 0 stake");
        assert_eq!(
            child2_stake, network_max_stake,
            "Child2 should have all the stake"
        );

        // Check that the total stake is preserved and equal to the network max stake
        assert_eq!(
            parent_stake + child1_stake + child2_stake,
            network_max_stake,
            "Total stake should equal the network max stake"
        );
    });
}

// 53: Test stake distribution in a complex hierarchy of parent-child relationships
// This test verifies the correct distribution of stake in a multi-level parent-child hierarchy:
// - Sets up a network with four neurons: parent, child1, child2, and grandchild
// - Establishes parent-child relationships between parent and its children, and child1 and grandchild
// - Adds initial stake to the parent
// - Checks stake distribution after setting up the first level of relationships
// - Checks stake distribution after setting up the second level of relationships
// - Verifies correct stake calculations, parent-child relationships, and preservation of total stake
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children -- test_get_stake_for_hotkey_on_subnet_complex_hierarchy --exact --nocapture

#[test]
fn test_get_stake_for_hotkey_on_subnet_complex_hierarchy() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let parent = U256::from(1);
        let child1 = U256::from(2);
        let child2 = U256::from(3);
        let grandchild = U256::from(4);
        let coldkey_parent = U256::from(5);
        let coldkey_child1 = U256::from(6);
        let coldkey_child2 = U256::from(7);
        let coldkey_grandchild = U256::from(8);

        add_network(netuid, 0, 0);
        SubtensorModule::set_max_registrations_per_block(netuid, 1000);
        SubtensorModule::set_target_registrations_per_interval(netuid, 1000);
        register_ok_neuron(netuid, parent, coldkey_parent, 0);
        register_ok_neuron(netuid, child1, coldkey_child1, 0);
        register_ok_neuron(netuid, child2, coldkey_child2, 0);
        register_ok_neuron(netuid, grandchild, coldkey_grandchild, 0);

        let total_stake = 1000;
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey_parent,
            &parent,
            total_stake,
        );

        log::info!("Initial stakes:");
        log::info!(
            "Parent stake: {}",
            SubtensorModule::get_stake_for_hotkey_on_subnet(&parent, netuid)
        );
        log::info!(
            "Child1 stake: {}",
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child1, netuid)
        );
        log::info!(
            "Child2 stake: {}",
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child2, netuid)
        );
        log::info!(
            "Grandchild stake: {}",
            SubtensorModule::get_stake_for_hotkey_on_subnet(&grandchild, netuid)
        );

        // Step 1: Set children for parent
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey_parent),
            parent,
            netuid,
            vec![(u64::MAX / 2, child1), (u64::MAX / 2, child2)]
        ));

        log::info!("After setting parent's children:");
        log::info!(
            "Parent's children: {:?}",
            SubtensorModule::get_children(&parent, netuid)
        );
        log::info!(
            "Child1's parents: {:?}",
            SubtensorModule::get_parents(&child1, netuid)
        );
        log::info!(
            "Child2's parents: {:?}",
            SubtensorModule::get_parents(&child2, netuid)
        );

        let parent_stake_1 = SubtensorModule::get_stake_for_hotkey_on_subnet(&parent, netuid);
        let child1_stake_1 = SubtensorModule::get_stake_for_hotkey_on_subnet(&child1, netuid);
        let child2_stake_1 = SubtensorModule::get_stake_for_hotkey_on_subnet(&child2, netuid);

        log::info!("Parent stake: {}", parent_stake_1);
        log::info!("Child1 stake: {}", child1_stake_1);
        log::info!("Child2 stake: {}", child2_stake_1);

        assert_eq!(
            parent_stake_1, 2,
            "Parent should have 2 stake due to rounding"
        );
        assert_eq!(child1_stake_1, 499, "Child1 should have 499 stake");
        assert_eq!(child2_stake_1, 499, "Child2 should have 499 stake");

        // Step 2: Set children for child1
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey_child1),
            child1,
            netuid,
            vec![(u64::MAX, grandchild)]
        ));

        log::info!("After setting child1's children:");
        log::info!(
            "Child1's children: {:?}",
            SubtensorModule::get_children(&child1, netuid)
        );
        log::info!(
            "Grandchild's parents: {:?}",
            SubtensorModule::get_parents(&grandchild, netuid)
        );

        let parent_stake_2 = SubtensorModule::get_stake_for_hotkey_on_subnet(&parent, netuid);
        let child1_stake_2 = SubtensorModule::get_stake_for_hotkey_on_subnet(&child1, netuid);
        let child2_stake_2 = SubtensorModule::get_stake_for_hotkey_on_subnet(&child2, netuid);
        let grandchild_stake = SubtensorModule::get_stake_for_hotkey_on_subnet(&grandchild, netuid);

        log::info!("Parent stake: {}", parent_stake_2);
        log::info!("Child1 stake: {}", child1_stake_2);
        log::info!("Child2 stake: {}", child2_stake_2);
        log::info!("Grandchild stake: {}", grandchild_stake);

        assert_eq!(parent_stake_2, 2, "Parent stake should remain 2");
        assert_eq!(
            child1_stake_2, 499,
            "Child1 stake should be be the same , as it doesnt have owned stake"
        );
        assert_eq!(child2_stake_2, 499, "Child2 should still have 499 stake");
        assert_eq!(
            grandchild_stake, 0,
            "Grandchild should have 0 , as child1 doesnt have any  owned stake"
        );

        // Check that the total stake is preserved
        assert_eq!(
            parent_stake_2 + child1_stake_2 + child2_stake_2 + grandchild_stake,
            total_stake,
            "Total stake should equal the initial stake"
        );

        // Additional checks
        log::info!("Final parent-child relationships:");
        log::info!(
            "Parent's children: {:?}",
            SubtensorModule::get_children(&parent, netuid)
        );
        log::info!(
            "Child1's parents: {:?}",
            SubtensorModule::get_parents(&child1, netuid)
        );
        log::info!(
            "Child2's parents: {:?}",
            SubtensorModule::get_parents(&child2, netuid)
        );
        log::info!(
            "Child1's children: {:?}",
            SubtensorModule::get_children(&child1, netuid)
        );
        log::info!(
            "Grandchild's parents: {:?}",
            SubtensorModule::get_parents(&grandchild, netuid)
        );

        // Check if the parent-child relationships are correct
        assert_eq!(
            SubtensorModule::get_children(&parent, netuid),
            vec![(u64::MAX / 2, child1), (u64::MAX / 2, child2)],
            "Parent should have both children"
        );
        assert_eq!(
            SubtensorModule::get_parents(&child1, netuid),
            vec![(u64::MAX / 2, parent)],
            "Child1 should have parent as its parent"
        );
        assert_eq!(
            SubtensorModule::get_parents(&child2, netuid),
            vec![(u64::MAX / 2, parent)],
            "Child2 should have parent as its parent"
        );
        assert_eq!(
            SubtensorModule::get_children(&child1, netuid),
            vec![(u64::MAX, grandchild)],
            "Child1 should have grandchild as its child"
        );
        assert_eq!(
            SubtensorModule::get_parents(&grandchild, netuid),
            vec![(u64::MAX, child1)],
            "Grandchild should have child1 as its parent"
        );
    });
}

// 54: Test stake distribution across multiple networks
// This test verifies the correct distribution of stake for a single neuron across multiple networks:
// - Sets up two networks with a single neuron registered on both
// - Adds initial stake to the neuron
// - Checks that the stake is correctly reflected on both networks
// - Verifies that changes in stake are consistently applied across all networks
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children -- test_get_stake_for_hotkey_on_subnet_multiple_networks --exact --nocapture

#[test]
fn test_get_stake_for_hotkey_on_subnet_multiple_networks() {
    new_test_ext(1).execute_with(|| {
        let netuid1: u16 = 1;
        let netuid2: u16 = 2;
        let hotkey = U256::from(1);
        let coldkey = U256::from(2);

        add_network(netuid1, 0, 0);
        add_network(netuid2, 0, 0);
        register_ok_neuron(netuid1, hotkey, coldkey, 0);
        register_ok_neuron(netuid2, hotkey, coldkey, 0);

        SubtensorModule::increase_stake_on_coldkey_hotkey_account(&coldkey, &hotkey, 1000);

        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey, netuid1),
            1000
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey, netuid2),
            1000
        );
    });
}

/// 55: Test rank, trust, and incentive calculation with parent-child relationships
///
/// This test verifies the correct calculation and distribution of rank, trust, incentive, and dividends
/// in a network with parent-child relationships:
/// - Sets up a network with validators (including a parent-child pair) and miners
/// - Establishes initial stakes and weights for all validators
/// - Runs a first epoch to establish baseline metrics
/// - Sets up a parent-child relationship
/// - Runs a second epoch to observe changes in metrics
/// - Verifies that the child's metrics improve relative to its initial state and other validators
///
/// # Test Steps:
/// 1. Initialize test environment with validators (including parent and child) and miners
/// 2. Set up network parameters and register all neurons
/// 3. Set initial stakes for validators
/// 4. Set initial weights for all validators
/// 5. Run first epoch and process emissions
/// 6. Record initial metrics for the child
/// 7. Establish parent-child relationship
/// 8. Run second epoch and process emissions
/// 9. Record final metrics for the child
/// 10. Compare child's initial and final metrics
/// 11. Compare child's final metrics with other validators
///
/// # Expected Results:
/// - Child's rank should improve (decrease)
/// - Child's trust should increase or remain the same
/// - Child's dividends should increase
/// - Child's final metrics should be better than or equal to other validators'
///
///     SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test children -- test_rank_trust_incentive_calculation_with_parent_child --exact --nocapture
#[test]
fn test_rank_trust_incentive_calculation_with_parent_child() {
    new_test_ext(1).execute_with(|| {
        // Initialize test environment
        let netuid: u16 = 1;
        let parent_hotkey: U256 = U256::from(1);
        let parent_coldkey: U256 = U256::from(101);
        let child_hotkey: U256 = U256::from(2);
        let child_coldkey: U256 = U256::from(102);
        let other_validators: Vec<(U256, U256)> = (3..6)
            .map(|i| (U256::from(i), U256::from(100 + i)))
            .collect();
        let miners: Vec<(U256, U256)> = (6..16)
            .map(|i| (U256::from(i), U256::from(100 + i)))
            .collect(); // 10 miners

        // Setup network and set registration parameters
        add_network(netuid, 1, 0);
        SubtensorModule::set_max_registrations_per_block(netuid, 1000);
        SubtensorModule::set_target_registrations_per_interval(netuid, 1000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);
        SubtensorModule::set_hotkey_emission_tempo(10);

        // Register neurons (validators and miners)
        register_ok_neuron(netuid, parent_hotkey, parent_coldkey, 0);
        register_ok_neuron(netuid, child_hotkey, child_coldkey, 0);
        for (hotkey, coldkey) in &other_validators {
            register_ok_neuron(netuid, *hotkey, *coldkey, 0);
        }
        for (hotkey, coldkey) in &miners {
            register_ok_neuron(netuid, *hotkey, *coldkey, 0);
        }

        step_block(2);

        // Set initial stakes for validators only
        let initial_stake: u64 = 1_000_000_000; // 1000 TAO
        SubtensorModule::add_balance_to_coldkey_account(&parent_coldkey, initial_stake);
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &parent_coldkey,
            &parent_hotkey,
            initial_stake,
        );
        SubtensorModule::add_balance_to_coldkey_account(&child_coldkey, initial_stake);
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &child_coldkey,
            &child_hotkey,
            initial_stake,
        );
        for (hotkey, coldkey) in &other_validators {
            SubtensorModule::add_balance_to_coldkey_account(coldkey, initial_stake);
            SubtensorModule::increase_stake_on_coldkey_hotkey_account(
                coldkey,
                hotkey,
                initial_stake,
            );
        }

        step_block(2);

        // Set initial weights for all validators
        let all_uids: Vec<u16> = (0..15).collect(); // 0-4 are validators, 5-14 are miners
        let validator_weights: Vec<u16> = vec![u16::MAX / 5; 5] // Equal weights for validators
            .into_iter()
            .chain(vec![u16::MAX / 10; 10]) // Equal weights for miners
            .collect();

        for hotkey in std::iter::once(&parent_hotkey)
            .chain(other_validators.iter().map(|(h, _)| h))
            .chain(std::iter::once(&child_hotkey))
        {
            assert_ok!(SubtensorModule::set_weights(
                RuntimeOrigin::signed(*hotkey),
                netuid,
                all_uids.clone(),
                validator_weights.clone(),
                0
            ));
        }

        step_block(10);

        // Run first epoch
        let rao_emission: u64 = 1_000_000_000;
        let initial_emission = SubtensorModule::epoch(netuid, rao_emission);

        // Process initial emission
        for (hotkey, mining_emission, validator_emission) in initial_emission {
            SubtensorModule::accumulate_hotkey_emission(
                &hotkey,
                netuid,
                validator_emission,
                mining_emission,
            );
        }

        step_block(11);

        // Get initial rank, trust, incentive, and dividends for the child
        let initial_child_rank: u16 = SubtensorModule::get_rank_for_uid(netuid, 1);
        let initial_child_trust: u16 = SubtensorModule::get_trust_for_uid(netuid, 1);
        let initial_child_incentive: u16 = SubtensorModule::get_incentive_for_uid(netuid, 1);
        let initial_child_dividends: u16 = SubtensorModule::get_dividends_for_uid(netuid, 1);

        log::debug!("Initial child rank: {:?}", initial_child_rank);
        log::debug!("Initial child trust: {:?}", initial_child_trust);
        log::debug!("Initial child incentive: {:?}", initial_child_incentive);
        log::debug!("Initial child dividends: {:?}", initial_child_dividends);

        // Parent sets the child with 100% of its weight
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(parent_coldkey),
            parent_hotkey,
            netuid,
            vec![(u64::MAX, child_hotkey)]
        ));

        // Child now sets weights as a validator
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(child_hotkey),
            netuid,
            all_uids.clone(),
            validator_weights.clone(),
            1
        ));

        step_block(10);

        // Run second epoch
        let final_emission = SubtensorModule::epoch(netuid, rao_emission);

        // Process final emission
        for (hotkey, mining_emission, validator_emission) in final_emission {
            SubtensorModule::accumulate_hotkey_emission(
                &hotkey,
                netuid,
                validator_emission,
                mining_emission,
            );
        }

        step_block(11);

        // Get final rank, trust, incentive, and dividends for the child
        let final_child_rank: u16 = SubtensorModule::get_rank_for_uid(netuid, 1);
        let final_child_trust: u16 = SubtensorModule::get_trust_for_uid(netuid, 1);
        let final_child_incentive: u16 = SubtensorModule::get_incentive_for_uid(netuid, 1);
        let final_child_dividends: u16 = SubtensorModule::get_dividends_for_uid(netuid, 1);

        log::debug!("Final child rank: {:?}", final_child_rank);
        log::debug!("Final child trust: {:?}", final_child_trust);
        log::debug!("Final child incentive: {:?}", final_child_incentive);
        log::debug!("Final child dividends: {:?}", final_child_dividends);

        // Print ranks for all validators
        for i in 0..5 {
            log::debug!(
                "Validator {} rank: {:?}",
                i,
                SubtensorModule::get_rank_for_uid(netuid, i)
            );
        }

        // Assert that rank has improved (decreased) for the child
        assert!(
            final_child_rank < initial_child_rank,
            "Child rank should have improved (decreased). Initial: {}, Final: {}",
            initial_child_rank,
            final_child_rank
        );

        // Assert that trust has increased or remained the same for the child
        assert!(
            final_child_trust >= initial_child_trust,
            "Child trust should have increased or remained the same. Initial: {}, Final: {}",
            initial_child_trust,
            final_child_trust
        );


        // Assert that dividends have increased for the child
        assert!(
            final_child_dividends > initial_child_dividends,
            "Child dividends should have increased. Initial: {}, Final: {}",
            initial_child_dividends,
            final_child_dividends
        );

        // Compare child's final values with other validators
        for i in 2..5 {
            let other_rank: u16 = SubtensorModule::get_rank_for_uid(netuid, i);
            let other_trust: u16 = SubtensorModule::get_trust_for_uid(netuid, i);
            let other_incentive: u16 = SubtensorModule::get_incentive_for_uid(netuid, i);
            let other_dividends: u16 = SubtensorModule::get_dividends_for_uid(netuid, i);

            log::debug!(
                "Validator {} - Rank: {}, Trust: {}, Incentive: {}, Dividends: {}",
                i, other_rank, other_trust, other_incentive, other_dividends
            );

            assert!(
                final_child_rank <= other_rank,
                "Child rank should be better than or equal to other validators. Child: {}, Other: {}",
                final_child_rank,
                other_rank
            );

            assert!(
                final_child_trust >= other_trust,
                "Child trust should be greater than or equal to other validators. Child: {}, Other: {}",
                final_child_trust,
                other_trust
            );

            assert!(
                final_child_dividends >= other_dividends,
                "Child dividends should be greater than or equal to other validators. Child: {}, Other: {}",
                final_child_dividends,
                other_dividends
            );
        }

    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --package pallet-subtensor --test children -- test_childkey_set_weights_single_parent --exact --nocapture
#[test]
fn test_childkey_set_weights_single_parent() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        add_network(netuid, 1, 0);

        // Define hotkeys
        let parent: U256 = U256::from(1);
        let child: U256 = U256::from(2);
        let weight_setter: U256 = U256::from(3);

        // Define coldkeys with more readable names
        let coldkey_parent: U256 = U256::from(100);
        let coldkey_child: U256 = U256::from(101);
        let coldkey_weight_setter: U256 = U256::from(102);

        let stake_to_give_child = 109_999;

        // Register parent with minimal stake and child with high stake
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_parent, 1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_child, stake_to_give_child + 10);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_weight_setter, 1_000_000);

        // Add neurons for parent, child and weight_setter
        register_ok_neuron(netuid, parent, coldkey_parent, 1);
        register_ok_neuron(netuid, child, coldkey_child, 1);
        register_ok_neuron(netuid, weight_setter, coldkey_weight_setter, 1);

        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey_parent,
            &parent,
            stake_to_give_child,
        );
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey_weight_setter,
            &weight_setter,
            1_000_000,
        );

        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        // Set parent-child relationship
        assert_ok!(SubtensorModule::do_set_children(
            RuntimeOrigin::signed(coldkey_parent),
            parent,
            netuid,
            vec![(u64::MAX, child)]
        ));
        step_block(7200 + 1);
        // Set weights on the child using the weight_setter account
        let origin = RuntimeOrigin::signed(weight_setter);
        let uids: Vec<u16> = vec![1]; // Only set weight for the child (UID 1)
        let values: Vec<u16> = vec![u16::MAX]; // Use maximum value for u16
        let version_key = SubtensorModule::get_weights_version_key(netuid);
        assert_ok!(SubtensorModule::set_weights(
            origin,
            netuid,
            uids.clone(),
            values.clone(),
            version_key
        ));

        // Set the min stake very high
        SubtensorModule::set_weights_min_stake(stake_to_give_child * 5);

        // Check the child has less stake than required
        assert!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child, netuid)
                < SubtensorModule::get_weights_min_stake()
        );

        // Check the child cannot set weights
        assert_noop!(
            SubtensorModule::set_weights(
                RuntimeOrigin::signed(child),
                netuid,
                uids.clone(),
                values.clone(),
                version_key
            ),
            Error::<Test>::NotEnoughStakeToSetWeights
        );

        assert!(!SubtensorModule::check_weights_min_stake(&child, netuid));

        // Set a minimum stake to set weights
        SubtensorModule::set_weights_min_stake(stake_to_give_child - 5);

        // Check if the stake for the child is above
        assert!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&child, netuid)
                >= SubtensorModule::get_weights_min_stake()
        );

        // Check the child can set weights
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(child),
            netuid,
            uids,
            values,
            version_key
        ));

        assert!(SubtensorModule::check_weights_min_stake(&child, netuid));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --package pallet-subtensor --test children -- test_set_weights_no_parent --exact --nocapture
#[test]
fn test_set_weights_no_parent() {
    // Verify that a regular key without a parent delegation is effected by the minimum stake requirements
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        add_network(netuid, 1, 0);

        let hotkey: U256 = U256::from(2);
        let spare_hk: U256 = U256::from(3);

        let coldkey: U256 = U256::from(101);
        let spare_ck = U256::from(102);

        let stake_to_give_child = 109_999;

        SubtensorModule::add_balance_to_coldkey_account(&coldkey, stake_to_give_child + 10);

        // Is registered
        register_ok_neuron(netuid, hotkey, coldkey, 1);
        // Register a spare key
        register_ok_neuron(netuid, spare_hk, spare_ck, 1);

        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey,
            &hotkey,
            stake_to_give_child,
        );

        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        // Has stake and no parent
        step_block(7200 + 1);

        let uids: Vec<u16> = vec![1]; // Set weights on the other hotkey
        let values: Vec<u16> = vec![u16::MAX]; // Use maximum value for u16
        let version_key = SubtensorModule::get_weights_version_key(netuid);

        // Set the min stake very high
        SubtensorModule::set_weights_min_stake(stake_to_give_child * 5);

        // Check the key has less stake than required
        assert!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey, netuid)
                < SubtensorModule::get_weights_min_stake()
        );

        // Check the hotkey cannot set weights
        assert_noop!(
            SubtensorModule::set_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                values.clone(),
                version_key
            ),
            Error::<Test>::NotEnoughStakeToSetWeights
        );

        assert!(!SubtensorModule::check_weights_min_stake(&hotkey, netuid));

        // Set a minimum stake to set weights
        SubtensorModule::set_weights_min_stake(stake_to_give_child - 5);

        // Check if the stake for the hotkey is above
        assert!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&hotkey, netuid)
                >= SubtensorModule::get_weights_min_stake()
        );

        // Check the hotkey can set weights
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids,
            values,
            version_key
        ));

        assert!(SubtensorModule::check_weights_min_stake(&hotkey, netuid));
    });
}
