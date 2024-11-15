#![allow(clippy::indexing_slicing, clippy::unwrap_used)]

use crate::mock::*;
use frame_support::{assert_err, assert_noop, assert_ok};
use frame_system::Config;
use frame_system::{EventRecord, Phase};
use pallet_subtensor::{
    migrations, Error, Event, SubnetIdentities, SubnetIdentity, SubnetIdentityOf,
};
use sp_core::{Get, H256, U256};
use sp_runtime::DispatchError;

mod mock;

#[allow(dead_code)]
fn record(event: RuntimeEvent) -> EventRecord<RuntimeEvent, H256> {
    EventRecord {
        phase: Phase::Initialization,
        event,
        topics: vec![],
    }
}

#[test]
fn test_root_register_network_exist() {
    new_test_ext(1).execute_with(|| {
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id = U256::from(667);
        assert_ok!(SubtensorModule::root_register(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
        ));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test root -- test_set_weights_not_root_error --exact --nocapture
#[test]
fn test_set_weights_not_root_error() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;

        let dests = vec![0];
        let weights = vec![1];
        let version_key: u64 = 0;
        let hotkey = U256::from(1);
        let coldkey = U256::from(2);

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 2143124);

        assert_err!(
            SubtensorModule::set_root_weights(
                RuntimeOrigin::signed(coldkey),
                netuid,
                hotkey,
                dests.clone(),
                weights.clone(),
                version_key,
            ),
            Error::<Test>::NotRootSubnet
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test root -- test_root_register_normal_on_root_fails --exact --nocapture
#[test]
fn test_root_register_normal_on_root_fails() {
    new_test_ext(1).execute_with(|| {
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();
        // Test fails because normal registrations are not allowed
        // on the root network.
        let root_netuid: u16 = 0;
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id = U256::from(667);

        // Burn registration fails.
        SubtensorModule::set_burn(root_netuid, 0);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1);
        assert_eq!(
            SubtensorModule::burned_register(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,
                hotkey_account_id
            ),
            Err(Error::<Test>::RegistrationNotPermittedOnRootSubnet.into())
        );
        // Pow registration fails.
        let block_number: u64 = SubtensorModule::get_current_block_as_u64();
        let (nonce, work): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            root_netuid,
            block_number,
            0,
            &hotkey_account_id,
        );
        assert_eq!(
            SubtensorModule::register(
                <<Test as frame_system::Config>::RuntimeOrigin>::signed(hotkey_account_id),
                root_netuid,
                block_number,
                nonce,
                work,
                hotkey_account_id,
                coldkey_account_id,
            ),
            Err(Error::<Test>::RegistrationNotPermittedOnRootSubnet.into())
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test root -- test_root_register_stake_based_pruning_works --exact --nocapture
#[test]
fn test_root_register_stake_based_pruning_works() {
    new_test_ext(1).execute_with(|| {
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();
        // Add two networks.
        let root_netuid: u16 = 0;
        let other_netuid: u16 = 1;
        add_network(other_netuid, 0, 0);

        // Set params to allow all registrations to subnet.
        SubtensorModule::set_burn(other_netuid, 0);
        SubtensorModule::set_max_registrations_per_block(other_netuid, 256);
        SubtensorModule::set_target_registrations_per_interval(other_netuid, 256);

        SubtensorModule::set_max_registrations_per_block(root_netuid, 1000);
        SubtensorModule::set_target_registrations_per_interval(root_netuid, 1000);

        // Register 128 accounts with stake to the other network.
        for i in 0..128 {
            let hot: U256 = U256::from(i);
            let cold: U256 = U256::from(i);
            // Add balance
            SubtensorModule::add_balance_to_coldkey_account(&cold, 1000 + (i as u64));
            // Register
            assert_ok!(SubtensorModule::burned_register(
                <<Test as Config>::RuntimeOrigin>::signed(cold),
                other_netuid,
                hot
            ));
            // Add stake on other network
            assert_ok!(SubtensorModule::add_stake(
                <<Test as Config>::RuntimeOrigin>::signed(cold),
                hot,
                1000 + (i as u64)
            ));
            // Check successful registration.
            assert!(SubtensorModule::get_uid_for_net_and_hotkey(other_netuid, &hot).is_ok());
            // Check that they are NOT all delegates
            assert!(!SubtensorModule::hotkey_is_delegate(&hot));
        }

        // Register the first 64 accounts with stake to the root network.
        for i in 0..64 {
            let hot: U256 = U256::from(i);
            let cold: U256 = U256::from(i);
            assert_ok!(SubtensorModule::root_register(
                <<Test as Config>::RuntimeOrigin>::signed(cold),
                hot,
            ));
            // Check successful registration.
            assert!(SubtensorModule::get_uid_for_net_and_hotkey(root_netuid, &hot).is_ok());
            // Check that they are all delegates
            assert!(SubtensorModule::hotkey_is_delegate(&hot));
        }

        // Register the second 64 accounts with stake to the root network.
        // Replaces the first 64
        for i in 64..128 {
            let hot: U256 = U256::from(i);
            let cold: U256 = U256::from(i);
            assert_ok!(SubtensorModule::root_register(
                <<Test as Config>::RuntimeOrigin>::signed(cold),
                hot,
            ));
            // Check successful registration.
            assert!(SubtensorModule::get_uid_for_net_and_hotkey(root_netuid, &hot).is_ok());
        }

        // Register the first 64 accounts again, this time failing because they
        // don't have enough stake.
        for i in 0..64 {
            let hot: U256 = U256::from(i);
            let cold: U256 = U256::from(i);
            assert_eq!(
                SubtensorModule::root_register(
                    <<Test as Config>::RuntimeOrigin>::signed(cold),
                    hot,
                ),
                Err(Error::<Test>::StakeTooLowForRoot.into())
            );
            // Check for unsuccessful registration.
            assert!(SubtensorModule::get_uid_for_net_and_hotkey(root_netuid, &hot).is_err());
            // Check that they are NOT senate members
            assert!(!SubtensorModule::is_senate_member(&hot));
        }
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test root -- test_root_set_weights --exact --nocapture
#[test]
fn test_root_set_weights() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(0);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let n: usize = 10;
        let root_netuid: u16 = 0;
        SubtensorModule::set_max_registrations_per_block(root_netuid, n as u16);
        SubtensorModule::set_target_registrations_per_interval(root_netuid, n as u16);
        SubtensorModule::set_max_allowed_uids(root_netuid, n as u16);
        for i in 0..n {
            let hotkey_account_id: U256 = U256::from(i);
            let coldkey_account_id: U256 = U256::from(i + 456);
            SubtensorModule::add_balance_to_coldkey_account(
                &coldkey_account_id,
                1_000_000_000_000_000,
            );
            assert_ok!(SubtensorModule::root_register(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                hotkey_account_id,
            ));
            assert_ok!(SubtensorModule::add_stake(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                hotkey_account_id,
                1000
            ));
        }

        log::info!("subnet limit: {:?}", SubtensorModule::get_max_subnets());
        log::info!(
            "current subnet count: {:?}",
            SubtensorModule::get_num_subnets()
        );

        // Lets create n networks
        for netuid in 1..n {
            log::debug!("Adding network with netuid: {}", netuid);
            assert_ok!(SubtensorModule::register_network(
                <<Test as Config>::RuntimeOrigin>::signed(U256::from(netuid + 456)),
            ));
        }

        // Test that signing with hotkey will fail.
        for i in 0..n {
            let hotkey = U256::from(i);
            let uids: Vec<u16> = vec![i as u16];
            let values: Vec<u16> = vec![1];
            assert_err!(
                SubtensorModule::set_root_weights(
                    <<Test as Config>::RuntimeOrigin>::signed(hotkey),
                    root_netuid,
                    hotkey,
                    uids,
                    values,
                    0,
                ),
                Error::<Test>::NonAssociatedColdKey
            );
        }

        // Test that signing an unassociated coldkey will fail.
        let unassociated_coldkey = U256::from(612);
        for i in 0..n {
            let hotkey = U256::from(i);
            let uids: Vec<u16> = vec![i as u16];
            let values: Vec<u16> = vec![1];
            assert_err!(
                SubtensorModule::set_root_weights(
                    <<Test as Config>::RuntimeOrigin>::signed(unassociated_coldkey),
                    root_netuid,
                    hotkey,
                    uids,
                    values,
                    0,
                ),
                Error::<Test>::NonAssociatedColdKey
            );
        }

        // Set weights into diagonal matrix.
        for i in 0..n {
            let hotkey = U256::from(i);
            let coldkey = U256::from(i + 456);
            let uids: Vec<u16> = vec![i as u16];
            let values: Vec<u16> = vec![1];
            assert_ok!(SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey),
                root_netuid,
                hotkey,
                uids,
                values,
                0,
            ));
        }
        // Run the root epoch
        log::debug!("Running Root epoch");
        SubtensorModule::set_tempo(root_netuid, 1);
        assert_ok!(SubtensorModule::root_epoch(1_000_000_000));
        // Check that the emission values have been set.
        for netuid in 1..n {
            log::debug!("check emission for netuid: {}", netuid);
            assert_eq!(
                SubtensorModule::get_subnet_emission_value(netuid as u16),
                99_999_999
            );
        }
        step_block(2);
        // Check that the pending emission values have been set.
        for netuid in 1..n {
            log::debug!(
                "check pending emission for netuid {} has pending {}",
                netuid,
                SubtensorModule::get_pending_emission(netuid as u16)
            );
            assert_eq!(
                SubtensorModule::get_pending_emission(netuid as u16),
                199_999_998
            );
        }
        step_block(1);
        for netuid in 1..n {
            log::debug!(
                "check pending emission for netuid {} has pending {}",
                netuid,
                SubtensorModule::get_pending_emission(netuid as u16)
            );
            assert_eq!(
                SubtensorModule::get_pending_emission(netuid as u16),
                299_999_997
            );
        }
        let step = SubtensorModule::blocks_until_next_epoch(
            10,
            1000,
            SubtensorModule::get_current_block_as_u64(),
        );
        step_block(step as u16);
        assert_eq!(SubtensorModule::get_pending_emission(10), 0);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test root -- test_root_set_weights --exact --nocapture
#[test]
fn test_root_set_weights_out_of_order_netuids() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(0);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let n: usize = 10;
        let root_netuid: u16 = 0;
        SubtensorModule::set_max_registrations_per_block(root_netuid, n as u16);
        SubtensorModule::set_target_registrations_per_interval(root_netuid, n as u16);
        SubtensorModule::set_max_allowed_uids(root_netuid, n as u16);
        for i in 0..n {
            let hotkey_account_id: U256 = U256::from(i);
            let coldkey_account_id: U256 = U256::from(i);
            SubtensorModule::add_balance_to_coldkey_account(
                &coldkey_account_id,
                1_000_000_000_000_000,
            );
            assert_ok!(SubtensorModule::root_register(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                hotkey_account_id,
            ));
            assert_ok!(SubtensorModule::add_stake(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                hotkey_account_id,
                1000
            ));
        }

        log::info!("subnet limit: {:?}", SubtensorModule::get_max_subnets());
        log::info!(
            "current subnet count: {:?}",
            SubtensorModule::get_num_subnets()
        );

        // Lets create n networks
        for netuid in 1..n {
            log::debug!("Adding network with netuid: {}", netuid);

            if netuid % 2 == 0 {
                assert_ok!(SubtensorModule::register_network(
                    <<Test as Config>::RuntimeOrigin>::signed(U256::from(netuid)),
                ));
            } else {
                add_network(netuid as u16 * 10, 1000, 0)
            }
        }

        log::info!("netuids: {:?}", SubtensorModule::get_all_subnet_netuids());
        log::info!(
            "root network count: {:?}",
            SubtensorModule::get_subnetwork_n(0)
        );

        let subnets = SubtensorModule::get_all_subnet_netuids();
        // Set weights into diagonal matrix.
        for (i, netuid) in subnets.iter().enumerate() {
            let uids: Vec<u16> = vec![*netuid];
            let values: Vec<u16> = vec![1];

            let coldkey = U256::from(i);
            let hotkey = U256::from(i);
            assert_ok!(SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey),
                root_netuid,
                hotkey,
                uids,
                values,
                0,
            ));
        }
        // Run the root epoch
        log::debug!("Running Root epoch");
        SubtensorModule::set_tempo(root_netuid, 1);
        assert_ok!(SubtensorModule::root_epoch(1_000_000_000));
        // Check that the emission values have been set.
        for netuid in subnets.iter() {
            log::debug!("check emission for netuid: {}", netuid);
            assert_eq!(
                SubtensorModule::get_subnet_emission_value(*netuid),
                99_999_999
            );
        }
        step_block(2);
        // Check that the pending emission values have been set.
        for netuid in subnets.iter() {
            if *netuid == 0 {
                continue;
            }

            log::debug!(
                "check pending emission for netuid {} has pending {}",
                netuid,
                SubtensorModule::get_pending_emission(*netuid)
            );
            assert_eq!(SubtensorModule::get_pending_emission(*netuid), 199_999_998);
        }
        step_block(1);
        for netuid in subnets.iter() {
            if *netuid == 0 {
                continue;
            }

            log::debug!(
                "check pending emission for netuid {} has pending {}",
                netuid,
                SubtensorModule::get_pending_emission(*netuid)
            );
            assert_eq!(SubtensorModule::get_pending_emission(*netuid), 299_999_997);
        }
        let step = SubtensorModule::blocks_until_next_epoch(
            9,
            1000,
            SubtensorModule::get_current_block_as_u64(),
        );
        step_block(step as u16);
        assert_eq!(SubtensorModule::get_pending_emission(9), 0);
    });
}

// Helper function to register a hotkey on a subnet
fn register_hotkey_on_subnet(netuid: u16, coldkey_account_id: U256, hotkey_account_id: U256) {
    // Enable registration on the subnet
    SubtensorModule::set_network_registration_allowed(netuid, true);
    SubtensorModule::set_network_pow_registration_allowed(netuid, true);

    // Set registration parameters
    SubtensorModule::set_max_registrations_per_block(netuid, 10);
    SubtensorModule::set_target_registrations_per_interval(netuid, 10);
    SubtensorModule::set_max_allowed_uids(netuid, 10);

    // Generate nonce and work for registration
    let block_number: u64 = SubtensorModule::get_current_block_as_u64();
    let (nonce, work): (u64, Vec<u8>) =
        SubtensorModule::create_work_for_block_number(netuid, block_number, 0, &hotkey_account_id);

    assert_ok!(SubtensorModule::register(
        <<Test as frame_system::Config>::RuntimeOrigin>::signed(hotkey_account_id),
        netuid,
        block_number,
        nonce,
        work,
        hotkey_account_id,
        coldkey_account_id,
    ));
}

// Test when the hotkey account does not exist.
#[test]
fn test_root_set_weights_hotkey_not_exists() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(0);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let root_netuid: u16 = 0;

        // Create a coldkey account and fund it
        let coldkey_account_id: U256 = U256::from(0);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);

        // Use a hotkey that does not exist
        let hotkey_account_id: U256 = U256::from(9999);

        // Try to set weights with non-existing hotkey
        let uids: Vec<u16> = vec![1];
        let values: Vec<u16> = vec![1];

        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,
                hotkey_account_id,
                uids,
                values,
                0,
            ),
            Error::<Test>::HotKeyAccountNotExists
        );
    });
}

// Test when the subnet does not exist.
#[test]
fn test_root_set_weights_subnet_does_not_exist() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(0);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let non_existent_netuid: u16 = 9999;

        // Create and register hotkey and coldkey
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);
        // Register the hotkey on root network
        assert_ok!(SubtensorModule::root_register(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
        ));

        let uids: Vec<u16> = vec![1];
        let values: Vec<u16> = vec![1];

        // Try to set weights on a non-existent subnet
        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                non_existent_netuid,
                hotkey_account_id,
                uids,
                values,
                0,
            ),
            Error::<Test>::SubNetworkDoesNotExist
        );
    });
}

// Test when using a netuid that is not the root network.
#[test]
fn test_root_set_weights_not_root_subnet() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(0);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let netuid: u16 = 1;

        // Create and register hotkey and coldkey
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);

        // Register other network
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
        ));

        // Register the hotkey on the other network
        register_hotkey_on_subnet(netuid, coldkey_account_id, hotkey_account_id);

        let uids: Vec<u16> = vec![netuid];
        let values: Vec<u16> = vec![1];

        // Try to set weights on a network that is not the root subnet
        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                netuid,
                hotkey_account_id,
                uids,
                values,
                0,
            ),
            Error::<Test>::NotRootSubnet
        );
    });
}

// Test when uids and values vectors have different lengths.
#[test]
fn test_root_set_weights_weight_vec_not_equal_size() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(0);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let root_netuid: u16 = 0;

        // Create and register hotkey and coldkey on root network
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);
        assert_ok!(SubtensorModule::root_register(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
        ));

        // Stake tokens
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
            1000
        ));

        let uids: Vec<u16> = vec![1, 2];
        let values: Vec<u16> = vec![1];

        // Try to set weights with mismatched uids and values length
        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,
                hotkey_account_id,
                uids,
                values,
                0,
            ),
            Error::<Test>::WeightVecNotEqualSize
        );
    });
}

// Test when uids contain invalid subnet netuids.
#[test]
fn test_root_set_weights_invalid_uids() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(0);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let root_netuid: u16 = 0;

        // Create and register hotkey and coldkey on root network
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);
        assert_ok!(SubtensorModule::root_register(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
        ));

        // Stake tokens
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
            1000
        ));

        // Register other networks
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
        )); // netuid 1

        // Valid netuids are 0 (root), 1 (registered above)
        let invalid_netuid: u16 = 9999; // invalid netuid

        let uids: Vec<u16> = vec![invalid_netuid];
        let values: Vec<u16> = vec![1];

        // Try to set weights with invalid uids
        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,
                hotkey_account_id,
                uids,
                values,
                0,
            ),
            Error::<Test>::UidVecContainInvalidOne
        );
    });
}

// Test when the hotkey is not registered on the root network.
#[test]
fn test_root_set_weights_hotkey_not_registered_in_subnet() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(0);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let root_netuid: u16 = 0;
        let other_netuid: u16 = 1;

        // Create and register coldkey
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);

        // Register other network
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
        ));

        // Register hotkey on other subnet
        register_hotkey_on_subnet(other_netuid, coldkey_account_id, hotkey_account_id);

        // Do NOT register the hotkey on the root network

        // Stake tokens
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
            1000
        ));

        let uids: Vec<u16> = vec![other_netuid]; // Use valid netuid
        let values: Vec<u16> = vec![1];

        // Try to set weights with hotkey not registered on root network
        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,
                hotkey_account_id,
                uids,
                values,
                0,
            ),
            Error::<Test>::HotKeyNotRegisteredInSubNet
        );
    });
}

// Test when the hotkey does not have enough stake.
#[test]
fn test_root_set_weights_not_enough_stake() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(1);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let root_netuid: u16 = 0;

        // Set the minimum stake required to set weights
        SubtensorModule::set_weights_min_stake(1000);

        // Create and register hotkey and coldkey on root network
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);
        assert_ok!(SubtensorModule::root_register(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
        ));

        // Do NOT stake any tokens

        let uids: Vec<u16> = vec![0];
        let values: Vec<u16> = vec![1];

        // Get the correct version_key
        let version_key = SubtensorModule::get_weights_version_key(root_netuid);

        // Try to set weights without enough stake
        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,
                hotkey_account_id,
                uids,
                values,
                version_key,
            ),
            Error::<Test>::NotEnoughStakeToSetWeights
        );
    });
}

// Test when providing an incorrect version_key.
#[test]
fn test_root_set_weights_incorrect_version_key() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(1);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let root_netuid: u16 = 0;

        // Set the weights set rate limit for the root network
        SubtensorModule::set_weights_set_rate_limit(root_netuid, 1);

        // Create and register hotkey and coldkey on the root network
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);
        assert_ok!(SubtensorModule::root_register(
            <<Test as frame_system::Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
        ));

        // Advance the block number to satisfy the rate limit
        System::set_block_number(System::block_number() + 1);

        // Stake tokens
        assert_ok!(SubtensorModule::add_stake(
            <<Test as frame_system::Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
            1000,
        ));

        // Manually set the WeightsVersionKey to a value greater than 0
        pallet_subtensor::WeightsVersionKey::<Test>::insert(root_netuid, 5);

        // Try to set weights with a version_key less than the network_version_key
        let incorrect_version_key = 4;
        let uids: Vec<u16> = vec![0];
        let values: Vec<u16> = vec![1];

        // Attempt to set weights and expect an error
        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as frame_system::Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,
                hotkey_account_id,
                uids,
                values,
                incorrect_version_key,
            ),
            Error::<Test>::IncorrectWeightVersionKey
        );
    });
}

// Test when setting weights faster than the weights_set_rate_limit.
#[test]
fn test_root_set_weights_too_fast() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(1);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let root_netuid: u16 = 0;

        // Set the weights set rate limit to 10 (weights can be set every 10 blocks)
        SubtensorModule::set_weights_set_rate_limit(root_netuid, 10);

        // Create and register hotkey and coldkey on root network
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);
        assert_ok!(SubtensorModule::root_register(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
        ));

        // Advance the block number to pass the initial rate limit
        System::set_block_number(System::block_number() + 10);

        // Stake tokens
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
            1000
        ));

        // Get the correct version_key
        let version_key = SubtensorModule::get_weights_version_key(root_netuid);

        let uids: Vec<u16> = vec![root_netuid];
        let values: Vec<u16> = vec![1];

        // Set weights for the first time
        assert_ok!(SubtensorModule::set_root_weights(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            root_netuid,
            hotkey_account_id,
            uids.clone(),
            values.clone(),
            version_key,
        ));

        // Attempt to set weights again without advancing the block number
        let new_version_key = SubtensorModule::get_weights_version_key(root_netuid);

        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,
                hotkey_account_id,
                uids,
                values,
                new_version_key,
            ),
            Error::<Test>::SettingWeightsTooFast
        );
    });
}

// Test when uids vector contains duplicates.
#[test]
fn test_root_set_weights_duplicate_uids() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(1);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let root_netuid: u16 = 0;

        // Set the weights set rate limit for the root network
        SubtensorModule::set_weights_set_rate_limit(root_netuid, 1);

        // Create and register hotkey and coldkey on root network
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);
        assert_ok!(SubtensorModule::root_register(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
        ));

        // Advance the block number to satisfy the rate limit
        System::set_block_number(System::block_number() + 1);

        // Stake tokens
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
            1000
        ));

        // Get the correct version_key
        let version_key = SubtensorModule::get_weights_version_key(root_netuid);

        // Try to set weights with duplicate uids
        let uids: Vec<u16> = vec![0, 0]; // Duplicate uids
        let values: Vec<u16> = vec![1, 1];

        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,
                hotkey_account_id,
                uids,
                values,
                version_key,
            ),
            Error::<Test>::DuplicateUids
        );
    });
}

// Test when weights vector length is too low.
#[test]
fn test_root_set_weights_length_too_low() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(1);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let root_netuid: u16 = 0;

        // Set the weights set rate limit for the root network
        SubtensorModule::set_weights_set_rate_limit(root_netuid, 1);

        // **Set the minimum allowed weights length to 1**
        SubtensorModule::set_min_allowed_weights(root_netuid, 1);

        // Create and register hotkey and coldkey on root network
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);
        assert_ok!(SubtensorModule::root_register(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
        ));

        // Advance the block number to satisfy the rate limit
        System::set_block_number(System::block_number() + 1);

        // Stake tokens
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
            1000
        ));

        // Try to set weights with length too low
        let uids: Vec<u16> = vec![]; // Empty vector
        let values: Vec<u16> = vec![];

        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,
                hotkey_account_id,
                uids,
                values,
                SubtensorModule::get_weights_version_key(root_netuid),
            ),
            Error::<Test>::WeightVecLengthIsLow
        );
    });
}

// Test when weights exceed the maximum weight limit.
#[test]
fn test_root_set_weights_max_weight_exceeded() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(1);

        // Migrate and create root network
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        let root_netuid: u16 = 0;
        let subnet_netuid: u16 = 1;

        // Set the weights set rate limit for the root network
        SubtensorModule::set_weights_set_rate_limit(root_netuid, 1);

        // Set max weight limit for testing (e.g., 1000)
        SubtensorModule::set_max_weight_limit(root_netuid, 1000);

        // Create and register hotkey and coldkey for the neuron on root network
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id: U256 = U256::from(1);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1_000_000_000_000_000);
        // Register neuron on root network
        assert_ok!(SubtensorModule::root_register(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
        ));

        // Register a new subnet with netuid `1`
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
        ));

        // Advance the block number to satisfy the rate limit after registration
        System::set_block_number(System::block_number() + 1);

        // Stake tokens for the neuron
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            hotkey_account_id,
            1000
        ));

        // Get the correct version_key
        let version_key = SubtensorModule::get_weights_version_key(root_netuid);

        // Neuron tries to set weight to subnet netuid `1`, exceeding the max weight limit
        let uids: Vec<u16> = vec![subnet_netuid]; // Setting weight to subnet netuid `1`
        let values: Vec<u16> = vec![u16::MAX]; // Exceeding max weight limit

        // Attempt to set weights and expect the `MaxWeightExceeded` error
        assert_err!(
            SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
                root_netuid,       // The root network
                hotkey_account_id, // Neuron's hotkey
                uids,
                values,
                version_key,
            ),
            Error::<Test>::MaxWeightExceeded
        );
    });
}

#[test]
fn test_root_subnet_creation_deletion() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(0);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();
        // Owner of subnets.
        let owner: U256 = U256::from(0);

        // Add a subnet.
        SubtensorModule::add_balance_to_coldkey_account(&owner, 1_000_000_000_000_000);
        // last_lock: 100000000000, min_lock: 100000000000, last_lock_block: 0, lock_reduction_interval: 2, current_block: 0, mult: 1 lock_cost: 100000000000
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        // last_lock: 100000000000, min_lock: 100000000000, last_lock_block: 0, lock_reduction_interval: 2, current_block: 0, mult: 1 lock_cost: 100000000000
        assert_eq!(SubtensorModule::get_network_lock_cost(), 100_000_000_000);
        step_block(1);
        // last_lock: 100000000000, min_lock: 100000000000, last_lock_block: 0, lock_reduction_interval: 2, current_block: 1, mult: 1 lock_cost: 100000000000
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        // last_lock: 100000000000, min_lock: 100000000000, last_lock_block: 1, lock_reduction_interval: 2, current_block: 1, mult: 2 lock_cost: 200000000000
        assert_eq!(SubtensorModule::get_network_lock_cost(), 200_000_000_000); // Doubles from previous subnet creation
        step_block(1);
        // last_lock: 100000000000, min_lock: 100000000000, last_lock_block: 1, lock_reduction_interval: 2, current_block: 2, mult: 2 lock_cost: 150000000000
        assert_eq!(SubtensorModule::get_network_lock_cost(), 150_000_000_000); // Reduced by 50%
        step_block(1);
        // last_lock: 100000000000, min_lock: 100000000000, last_lock_block: 1, lock_reduction_interval: 2, current_block: 3, mult: 2 lock_cost: 100000000000
        assert_eq!(SubtensorModule::get_network_lock_cost(), 100_000_000_000); // Reduced another 50%
        step_block(1);
        // last_lock: 100000000000, min_lock: 100000000000, last_lock_block: 1, lock_reduction_interval: 2, current_block: 4, mult: 2 lock_cost: 100000000000
        assert_eq!(SubtensorModule::get_network_lock_cost(), 100_000_000_000); // Reaches min value
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        // last_lock: 100000000000, min_lock: 100000000000, last_lock_block: 4, lock_reduction_interval: 2, current_block: 4, mult: 2 lock_cost: 200000000000
        assert_eq!(SubtensorModule::get_network_lock_cost(), 200_000_000_000); // Doubles from previous subnet creation
        step_block(1);
        // last_lock: 100000000000, min_lock: 100000000000, last_lock_block: 4, lock_reduction_interval: 2, current_block: 5, mult: 2 lock_cost: 150000000000
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        // last_lock: 150000000000, min_lock: 100000000000, last_lock_block: 5, lock_reduction_interval: 2, current_block: 5, mult: 2 lock_cost: 300000000000
        assert_eq!(SubtensorModule::get_network_lock_cost(), 300_000_000_000); // Doubles from previous subnet creation
        step_block(1);
        // last_lock: 150000000000, min_lock: 100000000000, last_lock_block: 5, lock_reduction_interval: 2, current_block: 6, mult: 2 lock_cost: 225000000000
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        // last_lock: 225000000000, min_lock: 100000000000, last_lock_block: 6, lock_reduction_interval: 2, current_block: 6, mult: 2 lock_cost: 450000000000
        assert_eq!(SubtensorModule::get_network_lock_cost(), 450_000_000_000); // Increasing
        step_block(1);
        // last_lock: 225000000000, min_lock: 100000000000, last_lock_block: 6, lock_reduction_interval: 2, current_block: 7, mult: 2 lock_cost: 337500000000
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        // last_lock: 337500000000, min_lock: 100000000000, last_lock_block: 7, lock_reduction_interval: 2, current_block: 7, mult: 2 lock_cost: 675000000000
        assert_eq!(SubtensorModule::get_network_lock_cost(), 675_000_000_000); // Increasing.
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        // last_lock: 337500000000, min_lock: 100000000000, last_lock_block: 7, lock_reduction_interval: 2, current_block: 7, mult: 2 lock_cost: 675000000000
        assert_eq!(SubtensorModule::get_network_lock_cost(), 1_350_000_000_000); // Double increasing.
        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        assert_eq!(SubtensorModule::get_network_lock_cost(), 2_700_000_000_000); // Double increasing again.

        // Now drop it like its hot to min again.
        step_block(1);
        assert_eq!(SubtensorModule::get_network_lock_cost(), 2_025_000_000_000); // 675_000_000_000 decreasing.
        step_block(1);
        assert_eq!(SubtensorModule::get_network_lock_cost(), 1_350_000_000_000); // 675_000_000_000 decreasing.
        step_block(1);
        assert_eq!(SubtensorModule::get_network_lock_cost(), 675_000_000_000); // 675_000_000_000 decreasing.
        step_block(1);
        assert_eq!(SubtensorModule::get_network_lock_cost(), 100_000_000_000); // 675_000_000_000 decreasing with 100000000000 min
    });
}

#[test]
fn test_network_pruning() {
    new_test_ext(1).execute_with(|| {
        System::set_block_number(0);
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        assert_eq!(SubtensorModule::get_total_issuance(), 0);

        let n: usize = 10;
        let root_netuid: u16 = 0;
        SubtensorModule::set_max_registrations_per_block(root_netuid, n as u16);
        SubtensorModule::set_target_registrations_per_interval(root_netuid, n as u16);
        SubtensorModule::set_max_allowed_uids(root_netuid, n as u16 + 1);
        SubtensorModule::set_tempo(root_netuid, 1);
        // No validators yet.
        assert_eq!(SubtensorModule::get_subnetwork_n(root_netuid), 0);

        for i in 0..n {
            let hot: U256 = U256::from(i);
            let cold: U256 = U256::from(i);
            let uids: Vec<u16> = (0..i as u16).collect();
            let values: Vec<u16> = vec![1; i];
            SubtensorModule::add_balance_to_coldkey_account(&cold, 1_000_000_000_000_000);
            assert_ok!(SubtensorModule::root_register(
                <<Test as Config>::RuntimeOrigin>::signed(cold),
                hot
            ));
            assert_ok!(SubtensorModule::add_stake(
                <<Test as Config>::RuntimeOrigin>::signed(cold),
                hot,
                1_000
            ));
            assert_ok!(SubtensorModule::register_network(
                <<Test as Config>::RuntimeOrigin>::signed(cold),
            ));
            log::debug!("Adding network with netuid: {}", (i as u16) + 1);
            assert!(SubtensorModule::if_subnet_exist((i as u16) + 1));
            assert!(SubtensorModule::is_hotkey_registered_on_network(
                root_netuid,
                &hot
            ));
            assert!(SubtensorModule::get_uid_for_net_and_hotkey(root_netuid, &hot).is_ok());
            assert_ok!(SubtensorModule::set_root_weights(
                <<Test as Config>::RuntimeOrigin>::signed(cold),
                root_netuid,
                hot,
                uids,
                values,
                0
            ));
            SubtensorModule::set_tempo((i as u16) + 1, 1);
            SubtensorModule::set_burn((i as u16) + 1, 0);
            assert_ok!(SubtensorModule::burned_register(
                <<Test as Config>::RuntimeOrigin>::signed(cold),
                (i as u16) + 1,
                hot
            ));
            assert_eq!(
                SubtensorModule::get_subnetwork_n(root_netuid),
                (i as u16) + 1
            );
        }
        // Stakes
        // 0 : 10_000
        // 1 : 9_000
        // 2 : 8_000
        // 3 : 7_000
        // 4 : 6_000
        // 5 : 5_000
        // 6 : 4_000
        // 7 : 3_000
        // 8 : 2_000
        // 9 : 1_000

        step_block(1);
        assert_ok!(SubtensorModule::root_epoch(1_000_000_000));
        assert_eq!(SubtensorModule::get_subnet_emission_value(0), 385_861_815);
        assert_eq!(SubtensorModule::get_subnet_emission_value(1), 249_435_914);
        assert_eq!(SubtensorModule::get_subnet_emission_value(2), 180_819_837);
        assert_eq!(SubtensorModule::get_subnet_emission_value(3), 129_362_980);
        assert_eq!(SubtensorModule::get_subnet_emission_value(4), 50_857_187);
        assert_eq!(SubtensorModule::get_subnet_emission_value(5), 3_530_356);
        step_block(1);
        assert_eq!(SubtensorModule::get_pending_emission(0), 0); // root network gets no pending emission.
        assert_eq!(SubtensorModule::get_pending_emission(1), 249_435_914);
        assert_eq!(SubtensorModule::get_pending_emission(2), 0); // This has been drained.
        assert_eq!(SubtensorModule::get_pending_emission(3), 129_362_980);
        assert_eq!(SubtensorModule::get_pending_emission(4), 0); // This network has been drained.
        assert_eq!(SubtensorModule::get_pending_emission(5), 3_530_356);
        step_block(1);
    });
}

#[test]
fn test_network_prune_results() {
    new_test_ext(1).execute_with(|| {
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        SubtensorModule::set_network_immunity_period(3);
        SubtensorModule::set_network_min_lock(0);
        SubtensorModule::set_network_rate_limit(0);

        let owner: U256 = U256::from(0);
        SubtensorModule::add_balance_to_coldkey_account(&owner, 1_000_000_000_000_000);

        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        step_block(3);

        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        step_block(3);

        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
        ));
        step_block(3);

        // lowest emission
        SubtensorModule::set_emission_values(&[1u16, 2u16, 3u16], vec![5u64, 4u64, 4u64]).unwrap();
        assert_eq!(SubtensorModule::get_subnet_to_prune(), 2u16);

        // equal emission, creation date
        SubtensorModule::set_emission_values(&[1u16, 2u16, 3u16], vec![5u64, 5u64, 4u64]).unwrap();
        assert_eq!(SubtensorModule::get_subnet_to_prune(), 3u16);

        // equal emission, creation date
        SubtensorModule::set_emission_values(&[1u16, 2u16, 3u16], vec![4u64, 5u64, 5u64]).unwrap();
        assert_eq!(SubtensorModule::get_subnet_to_prune(), 1u16);
    });
}

#[test]
fn test_weights_after_network_pruning() {
    new_test_ext(1).execute_with(|| {
        migrations::migrate_create_root_network::migrate_create_root_network::<Test>();

        assert_eq!(SubtensorModule::get_total_issuance(), 0);

        // Set up N subnets, with max N + 1 allowed UIDs
        let n: usize = 2;
        let root_netuid: u16 = 0;
        SubtensorModule::set_network_immunity_period(3);
        SubtensorModule::set_max_registrations_per_block(root_netuid, n as u16);
        SubtensorModule::set_max_subnets(n as u16);
        SubtensorModule::set_weights_set_rate_limit(root_netuid, 0_u64);

        // No validators yet.
        assert_eq!(SubtensorModule::get_subnetwork_n(root_netuid), 0);

        for i in 0..n {
            // Register a validator
            let cold: U256 = U256::from(i);

            SubtensorModule::add_balance_to_coldkey_account(&cold, 1_000_000_000_000);

            // Register a network
            assert_ok!(SubtensorModule::register_network(
                <<Test as Config>::RuntimeOrigin>::signed(cold),
            ));

            log::debug!("Adding network with netuid: {}", (i as u16) + 1);
            assert!(SubtensorModule::if_subnet_exist((i as u16) + 1));
            step_block(3);
        }

        // Register a validator in subnet 0
        let hot: U256 = U256::from((n as u64) - 1);
        let cold: U256 = U256::from((n as u64) - 1);

        assert_ok!(SubtensorModule::root_register(
            <<Test as Config>::RuntimeOrigin>::signed(cold),
            hot
        ));
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(cold),
            hot,
            1_000
        ));

        // Let's give these subnets some weights
        let uids: Vec<u16> = (0..(n as u16) + 1).collect();
        let values: Vec<u16> = vec![4u16, 2u16, 6u16];
        log::info!("uids set: {:?}", uids);
        log::info!("values set: {:?}", values);
        log::info!("In netuid: {:?}", root_netuid);
        assert_ok!(SubtensorModule::set_root_weights(
            <<Test as Config>::RuntimeOrigin>::signed(cold),
            root_netuid,
            hot,
            uids,
            values,
            0
        ));

        log::info!(
            "Root network weights before extra network registration: {:?}",
            SubtensorModule::get_root_weights()
        );
        log::info!("Max subnets: {:?}", SubtensorModule::get_max_subnets());
        let i = (n as u16) + 1;
        // let _hot: U256 = U256::from(i);
        let cold: U256 = U256::from(i);

        SubtensorModule::add_balance_to_coldkey_account(&cold, 1_000_000_000_000_000_000);
        let subnet_to_prune = SubtensorModule::get_subnet_to_prune();

        // Subnet 1 should be pruned here.
        assert_eq!(subnet_to_prune, 1);
        log::info!("Removing subnet: {:?}", subnet_to_prune);

        // Check that the weights have been set appropriately.
        let latest_weights = SubtensorModule::get_root_weights();
        log::info!("Weights before register network: {:?}", latest_weights);
        // We expect subnet 1 to be deregistered as it is oldest and has lowest emissions
        assert_eq!(latest_weights[0][1], 21845);

        assert_ok!(SubtensorModule::register_network(
            <<Test as Config>::RuntimeOrigin>::signed(cold),
        ));

        // Subnet should not exist, as it would replace a previous subnet.
        assert!(!SubtensorModule::if_subnet_exist(i + 1));

        log::info!(
            "Root network weights: {:?}",
            SubtensorModule::get_root_weights()
        );

        let latest_weights = SubtensorModule::get_root_weights();
        log::info!(
            "Weights after register network: {:?}",
            SubtensorModule::get_root_weights()
        );

        // Subnet 0 should be kicked, and thus its weight should be 0
        assert_eq!(latest_weights[0][1], 0);
    });
}

/// This test checks the halving mechanism of the emission schedule.
/// Run this test using the following command:
/// `cargo test --package pallet-subtensor --test root test_issance_bounds`
#[test]
fn test_issuance_bounds() {
    new_test_ext(1).execute_with(|| {
        // Simulate 100 halvings convergence to 21M. Note that the total issuance never reaches 21M because of rounding errors.
        // We converge to 20_999_999_989_500_000 (< 1 TAO away).
        let n_halvings: usize = 100;
        let mut total_issuance: u64 = 0;
        for _ in 0..n_halvings {
            let block_emission_10_500_000x: u64 =
                SubtensorModule::get_block_emission_for_issuance(total_issuance).unwrap()
                    * 10_500_000;
            total_issuance += block_emission_10_500_000x;
        }
        assert_eq!(total_issuance, 20_999_999_989_500_000);
    })
}

/// This test checks the halving mechanism of the emission schedule.
/// Run this test using the following command:
/// `cargo test --package pallet-subtensor --test root test_halving`
#[test]
fn test_halving() {
    new_test_ext(1).execute_with(|| {
        let expected_emissions: [(u64, u64); 43] = [
            (0, 1_000_000_000), // Testing at zero issuance.
            (1_776_000, 1_000_000_000),
            (1_776_000_000, 1_000_000_000),
            (1_776_000_000_000, 1_000_000_000),
            (10_500_000_000_000_000, 500_000_000), // First halving event
            (10_999_999_000_000_000, 500_000_000),
            (11_000_000_000_000_000, 500_000_000),
            (12_000_999_000_000_000, 500_000_000),
            (15_749_999_000_000_000, 500_000_000),
            (15_800_000_000_000_000, 250_000_000), // Second halving event
            (16_400_999_000_000_000, 250_000_000),
            (16_499_999_000_000_000, 250_000_000),
            (17_624_999_000_000_000, 250_000_000),
            (18_400_000_000_000_000, 125_000_000), // Third halving event
            (19_312_500_000_000_000, 125_000_000),
            (19_700_000_000_000_000, 62_500_000), // Fourth halving event
            (19_906_249_000_000_000, 62_500_000),
            (20_400_000_000_000_000, 31_250_000), // Fifth halving event
            (20_500_000_000_000_000, 31_250_000),
            (20_700_000_000_000_000, 15_625_000), // Sixth halving event
            (20_800_000_000_000_000, 15_625_000),
            (20_900_000_000_000_000, 7_812_500), // Seventh halving event
            (20_917_970_000_000_000, 3_906_250), // Eighth halving event
            (20_958_985_000_000_000, 1_953_125), // Ninth halving event
            (20_979_493_000_000_000, 976_562),   // Tenth halving event
            (20_989_747_000_000_000, 488_281),   // Eleventh halving event
            (20_994_874_000_000_000, 244_140),   // Twelfth halving event
            (20_997_437_000_000_000, 122_070),   // Thirteenth halving event
            (20_998_719_000_000_000, 61_035),    // Fourteenth halving event
            (20_999_360_000_000_000, 30_517),    // Fifteenth halving event
            (20_999_680_000_000_000, 15_258),    // Sixteenth halving event
            (20_999_840_000_000_000, 7_629),     // Seventeenth halving event
            (20_999_920_000_000_000, 3_814),     // Eighteenth halving event
            (20_999_960_000_000_000, 1_907),     // Nineteenth halving event
            (20_999_980_000_000_000, 953),       // Twentieth halving event
            (20_999_990_000_000_000, 476),       // Twenty-first halving event
            (20_999_990_500_000_000, 476),
            (20_999_995_000_000_000, 238), // Twenty-second halving event
            (20_999_998_000_000_000, 119), // Twenty-third halving event
            (20_999_999_000_000_000, 59),  // Twenty-fourth halving event
            (21_000_000_000_000_000, 0),   // Total supply reached, emissions stop
            (21_100_000_000_000_000, 0),   // Just for fun
            (u64::MAX, 0),                 // Testing bounds
        ];

        for (issuance, expected_emission) in expected_emissions.iter() {
            SubtensorModule::set_total_issuance(*issuance);
            step_block(1);

            let current_emission = SubtensorModule::get_block_emission().unwrap();
            assert_eq!(
                current_emission, *expected_emission,
                "Incorrect emission {} at total issuance {}",
                current_emission, issuance
            );
        }
    });
}

#[test]
fn test_get_emission_across_entire_issuance_range() {
    new_test_ext(1).execute_with(|| {
        let total_supply: u64 = pallet_subtensor::TotalSupply::<Test>::get();
        let original_emission: u64 = pallet_subtensor::DefaultBlockEmission::<Test>::get();
        let halving_issuance: u64 = total_supply / 2;

        let mut issuance = 0;

        // Issuance won't reach total supply.
        while issuance <= 20_900_000_000_000_000 {
            SubtensorModule::set_total_issuance(issuance);

            let issuance_f64 = issuance as f64;
            let h = f64::log2(1.0 / (1.0 - issuance_f64 / (2.0 * halving_issuance as f64)));
            let h = h.floor();
            let emission_percentage = f64::powf(2.0, -h);

            let expected_emission: u64 = if issuance < total_supply {
                (original_emission as f64 * emission_percentage) as u64
            } else {
                0
            };
            assert_eq!(
                SubtensorModule::get_block_emission().unwrap(),
                expected_emission,
                "Issuance: {}",
                issuance_f64
            );

            issuance += expected_emission;
        }
    });
}

#[test]
fn test_dissolve_network_ok() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 30;
        let hotkey = U256::from(1);

        add_network(netuid, 0, 0);
        let owner_coldkey = SubtensorModule::get_subnet_owner(netuid);
        register_ok_neuron(netuid, hotkey, owner_coldkey, 3);

        assert!(SubtensorModule::if_subnet_exist(netuid));
        assert_ok!(SubtensorModule::dissolve_network(
            RuntimeOrigin::root(),
            owner_coldkey,
            netuid
        ));
        assert!(!SubtensorModule::if_subnet_exist(netuid))
    });
}

#[test]
fn test_dissolve_network_refund_coldkey_ok() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 30;
        let hotkey = U256::from(1);
        let subnet_locked_balance = 1000;

        add_network(netuid, 0, 0);
        let owner_coldkey = SubtensorModule::get_subnet_owner(netuid);
        register_ok_neuron(netuid, hotkey, owner_coldkey, 3);

        SubtensorModule::set_subnet_locked_balance(netuid, subnet_locked_balance);
        let coldkey_balance = SubtensorModule::get_coldkey_balance(&owner_coldkey);

        assert!(SubtensorModule::if_subnet_exist(netuid));
        assert_ok!(SubtensorModule::dissolve_network(
            RuntimeOrigin::root(),
            owner_coldkey,
            netuid
        ));
        assert!(!SubtensorModule::if_subnet_exist(netuid));

        let coldkey_new_balance = SubtensorModule::get_coldkey_balance(&owner_coldkey);

        assert!(coldkey_new_balance > coldkey_balance);
        assert_eq!(coldkey_new_balance, coldkey_balance + subnet_locked_balance);
    });
}

#[test]
fn test_dissolve_network_not_owner_err() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 30;
        let hotkey = U256::from(1);
        let owner_coldkey = U256::from(2);
        let random_coldkey = U256::from(3);

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, owner_coldkey, 3);

        assert_err!(
            SubtensorModule::dissolve_network(RuntimeOrigin::root(), random_coldkey, netuid),
            Error::<Test>::NotSubnetOwner
        );
        assert!(!pallet_subtensor::SubnetOwner::<Test>::contains_key(netuid));
    });
}

#[test]
fn test_dissolve_network_does_not_exist_err() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 30;
        let coldkey = U256::from(2);

        assert_err!(
            SubtensorModule::dissolve_network(RuntimeOrigin::root(), coldkey, netuid),
            Error::<Test>::SubNetworkDoesNotExist
        );
    });
}

#[test]
fn test_user_add_network_with_identity_fields_ok() {
    new_test_ext(1).execute_with(|| {
        let coldkey_1 = U256::from(1);
        let coldkey_2 = U256::from(2);
        let balance_1 = SubtensorModule::get_network_lock_cost() + 10_000;

        let subnet_name_1: Vec<u8> = b"GenericSubnet1".to_vec();
        let github_repo_1: Vec<u8> = b"GenericSubnet1.com".to_vec();
        let subnet_contact_1: Vec<u8> = b"https://www.GenericSubnet1.co".to_vec();

        let identity_value_1: SubnetIdentity = SubnetIdentityOf {
            subnet_name: subnet_name_1.clone(),
            github_repo: github_repo_1.clone(),
            subnet_contact: subnet_contact_1.clone(),
        };

        let subnet_name_2: Vec<u8> = b"DistinctSubnet2".to_vec();
        let github_repo_2: Vec<u8> = b"https://github.com/DistinctRepo2".to_vec();
        let subnet_contact_2: Vec<u8> = b"https://contact2.example.com".to_vec();

        let identity_value_2: SubnetIdentity = SubnetIdentityOf {
            subnet_name: subnet_name_2.clone(),
            github_repo: github_repo_2.clone(),
            subnet_contact: subnet_contact_2.clone(),
        };

        SubtensorModule::add_balance_to_coldkey_account(&coldkey_1, balance_1);

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey_1),
            Some(identity_value_1.clone())
        ));

        let balance_2 = SubtensorModule::get_network_lock_cost() + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_2, balance_2);

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey_2),
            Some(identity_value_2.clone())
        ));

        let stored_identity_1: SubnetIdentity = SubnetIdentities::<Test>::get(1).unwrap();
        assert_eq!(stored_identity_1.subnet_name, subnet_name_1);
        assert_eq!(stored_identity_1.github_repo, github_repo_1);
        assert_eq!(stored_identity_1.subnet_contact, subnet_contact_1);

        let stored_identity_2: SubnetIdentity = SubnetIdentities::<Test>::get(2).unwrap();
        assert_eq!(stored_identity_2.subnet_name, subnet_name_2);
        assert_eq!(stored_identity_2.github_repo, github_repo_2);
        assert_eq!(stored_identity_2.subnet_contact, subnet_contact_2);

        // Now remove the first network.
        assert_ok!(SubtensorModule::user_remove_network(coldkey_1, 1));

        // Verify that the first network and identity have been removed.
        assert!(SubnetIdentities::<Test>::get(1).is_none());

        // Ensure the second network and identity are still intact.
        let stored_identity_2_after_removal: SubnetIdentity =
            SubnetIdentities::<Test>::get(2).unwrap();
        assert_eq!(stored_identity_2_after_removal.subnet_name, subnet_name_2);
        assert_eq!(stored_identity_2_after_removal.github_repo, github_repo_2);
        assert_eq!(
            stored_identity_2_after_removal.subnet_contact,
            subnet_contact_2
        );
    });
}

#[test]
fn test_user_add_network_not_signed_error() {
    new_test_ext(1).execute_with(|| {
        let identity_value: Option<SubnetIdentity> = None;

        let result = SubtensorModule::user_add_network(
            RuntimeOrigin::none(), // Unsigned origin
            identity_value,
        );

        assert_eq!(result, Err(DispatchError::BadOrigin));
    });
}

#[test]
fn test_user_add_network_rate_limit_exceeded() {
    new_test_ext(1).execute_with(|| {
        pallet_subtensor::NetworkRateLimit::<Test>::put(1);

        let coldkey = U256::from(1);

        // Capture the initial lock cost before adding the first network
        let initial_lock_cost = SubtensorModule::get_network_lock_cost();
        let balance = initial_lock_cost + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, balance);

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey),
            None
        ));

        SubtensorModule::add_balance_to_coldkey_account(
            &coldkey,
            SubtensorModule::get_network_lock_cost(),
        );

        // Attempt to add another network in the same block
        assert_noop!(
            SubtensorModule::user_add_network(RuntimeOrigin::signed(coldkey), None),
            Error::<Test>::NetworkTxRateLimitExceeded
        );

        System::set_block_number(System::block_number() + 1);

        // Now the rate limit should allow another network addition
        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey),
            None
        ));
    });
}

#[test]
fn test_user_add_network_insufficient_balance_error() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let balance = SubtensorModule::get_network_lock_cost() - 1; // Less than lock cost
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, balance);

        let result = SubtensorModule::user_add_network(RuntimeOrigin::signed(coldkey), None);

        // Assuming the error is InsufficientBalance
        assert_noop!(result, Error::<Test>::NotEnoughBalanceToStake);
    });
}

#[test]
fn test_user_add_network_sets_subnet_locked_balance() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let initial_lock_cost = SubtensorModule::get_network_lock_cost();
        let balance = initial_lock_cost + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, balance);

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey),
            None
        ));

        let locked_balance = SubtensorModule::get_subnet_locked_balance(1);

        assert_eq!(locked_balance, initial_lock_cost);
    });
}

#[test]
fn test_user_add_network_reuses_netuid_after_removal() {
    new_test_ext(1).execute_with(|| {
        SubtensorModule::set_max_subnets(2);

        let coldkey_1 = U256::from(1);
        let coldkey_2 = U256::from(2);
        let coldkey_3 = U256::from(3);

        // --- Add subnet 1 ---
        let lock_cost_1 = SubtensorModule::get_network_lock_cost();
        let balance_1 = lock_cost_1 + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_1, balance_1);
        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey_1),
            None
        ));
        assert!(SubtensorModule::if_subnet_exist(1));

        // --- Add subnet 2 ---
        let lock_cost_2 = SubtensorModule::get_network_lock_cost();
        let balance_2 = lock_cost_2 + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_2, balance_2);
        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey_2),
            None
        ));
        assert!(SubtensorModule::if_subnet_exist(2));

        // --- Remove subnet 1 ---
        assert_ok!(SubtensorModule::user_remove_network(coldkey_1, 1));
        assert!(!SubtensorModule::if_subnet_exist(1));

        // --- Add subnet 3 ---
        let lock_cost_3 = SubtensorModule::get_network_lock_cost();
        let balance_3 = lock_cost_3 + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_3, balance_3);
        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey_3),
            None
        ));

        // --- Verify that netuid 1 is reused ---
        let subnet_owner = SubtensorModule::get_subnet_owner(1);
        assert_eq!(subnet_owner, coldkey_3);
        assert!(pallet_subtensor::SubnetOwner::<Test>::contains_key(1));
        assert_eq!(pallet_subtensor::SubnetOwner::<Test>::get(1), coldkey_3);
        assert!(SubtensorModule::if_subnet_exist(1));
        assert!(!SubtensorModule::if_subnet_exist(3))
    });
}

#[test]
fn test_user_add_network_deducts_lock_cost_from_balance() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let lock_cost = SubtensorModule::get_network_lock_cost();
        let initial_balance = lock_cost + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_balance);

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey),
            None
        ));

        let expected_balance = initial_balance - lock_cost;
        let actual_balance = SubtensorModule::get_coldkey_balance(&coldkey);

        assert_eq!(actual_balance, expected_balance);
    });
}

#[test]
fn test_user_add_network_increases_lock_cost() {
    new_test_ext(1).execute_with(|| {
        let coldkey_1 = U256::from(1);
        let coldkey_2 = U256::from(2);

        let lock_cost_1 = SubtensorModule::get_network_lock_cost();
        let balance_1 = lock_cost_1 + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_1, balance_1);

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey_1),
            None
        ));

        // After adding a network, the lock cost should increase
        let lock_cost_2 = SubtensorModule::get_network_lock_cost();
        assert!(lock_cost_2 > lock_cost_1);

        let balance_2 = lock_cost_2 + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey_2, balance_2);

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey_2),
            None
        ));

        // Check that the lock cost increased again
        let lock_cost_3 = SubtensorModule::get_network_lock_cost();
        assert!(lock_cost_3 > lock_cost_2);
    });
}

#[test]
fn test_user_add_network_initializes_subnet_parameters() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let balance = SubtensorModule::get_network_lock_cost() + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, balance);

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey),
            None
        ));

        let netuid = 1;

        assert_eq!(pallet_subtensor::SubnetworkN::<Test>::get(netuid), 0);
        assert!(pallet_subtensor::NetworksAdded::<Test>::get(netuid));
        assert!(pallet_subtensor::Tempo::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::NetworkModality::<Test>::contains_key(
            netuid
        ));
        assert_eq!(pallet_subtensor::TotalNetworks::<Test>::get(), 1);
        assert!(SubtensorModule::get_network_registration_allowed(netuid));
        assert_eq!(SubtensorModule::get_max_allowed_uids(netuid), 256);
        assert_eq!(SubtensorModule::get_max_allowed_validators(netuid), 64);
        assert_eq!(SubtensorModule::get_min_allowed_weights(netuid), 1);
        assert_eq!(SubtensorModule::get_max_weight_limit(netuid), u16::MAX);
        assert_eq!(SubtensorModule::get_adjustment_interval(netuid), 360);
        assert_eq!(
            SubtensorModule::get_target_registrations_per_interval(netuid),
            1
        );
        assert_eq!(
            SubtensorModule::get_adjustment_alpha(netuid),
            17_893_341_751_498_265_066u64
        );
        assert_eq!(SubtensorModule::get_immunity_period(netuid), 5000);
        assert_eq!(SubtensorModule::get_min_burn_as_u64(netuid), 1);
        assert_eq!(SubtensorModule::get_min_difficulty(netuid), u64::MAX);
        assert_eq!(SubtensorModule::get_max_difficulty(netuid), u64::MAX);

        // Ensure that other parameters are initialized
        assert!(pallet_subtensor::Kappa::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::Difficulty::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::MaxAllowedUids::<Test>::contains_key(
            netuid
        ));
        assert!(pallet_subtensor::ImmunityPeriod::<Test>::contains_key(
            netuid
        ));
        assert!(pallet_subtensor::ActivityCutoff::<Test>::contains_key(
            netuid
        ));
        assert!(pallet_subtensor::EmissionValues::<Test>::contains_key(
            netuid
        ));
        assert!(pallet_subtensor::MaxWeightsLimit::<Test>::contains_key(
            netuid
        ));
        assert!(pallet_subtensor::MinAllowedWeights::<Test>::contains_key(
            netuid
        ));
        assert!(pallet_subtensor::RegistrationsThisInterval::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::POWRegistrationsThisInterval::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::BurnRegistrationsThisInterval::<Test>::contains_key(netuid));
    });
}

#[test]
fn test_user_add_network_initializes_subnet_owner() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let balance = SubtensorModule::get_network_lock_cost() + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, balance);

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey),
            None
        ));

        // Check that SubnetOwner is set correctly
        let owner = SubtensorModule::get_subnet_owner(1);
        assert_eq!(owner, coldkey);
    });
}

#[test]
fn test_user_add_network_sets_network_registered_at() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let balance = SubtensorModule::get_network_lock_cost() + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, balance);
        let current_block = System::block_number();

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey),
            None
        ));

        let netuid = 1;

        let registered_at = pallet_subtensor::NetworkRegisteredAt::<Test>::get(netuid);
        assert_eq!(registered_at, current_block);

        let last_registered = pallet_subtensor::NetworkLastRegistered::<Test>::get();
        assert_eq!(last_registered, netuid as u64);
    });
}

#[test]
fn test_user_add_network_emits_network_added_event() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let balance = SubtensorModule::get_network_lock_cost() + 10_000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, balance);
        System::reset_events();

        assert_ok!(SubtensorModule::user_add_network(
            RuntimeOrigin::signed(coldkey),
            None
        ));

        let events = System::events();
        let network_added_event_found = events.iter().any(|record| {
            matches!(
                record.event,
                RuntimeEvent::SubtensorModule(Event::NetworkAdded(added_netuid, 0))
                if added_netuid == 1
            )
        });

        assert!(network_added_event_found, "NetworkAdded event not found");
    });
}

#[test]
fn test_dissolve_network_not_subnet_owner_bad_origin_error() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 30;
        let subnet_owner = U256::from(1);
        let non_owner = U256::from(2);

        add_network(netuid, 0, 0);
        pallet_subtensor::SubnetOwner::<Test>::insert(netuid, subnet_owner);

        let result = SubtensorModule::dissolve_network(
            RuntimeOrigin::signed(non_owner), // must be root
            non_owner,
            netuid,
        );

        assert_eq!(result, Err(DispatchError::BadOrigin));
    });
}

#[test]
fn test_dissolve_network_emits_network_removed_event() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 30;
        let subnet_owner = U256::from(1);

        // Set up the network with subnet_owner as the owner
        add_network(netuid, 0, 0);
        pallet_subtensor::SubnetOwner::<Test>::insert(netuid, subnet_owner);

        // Dissolve the network
        assert_ok!(SubtensorModule::dissolve_network(
            RuntimeOrigin::root(),
            subnet_owner,
            netuid,
        ));

        // Check that the NetworkRemoved event was emitted
        let events = System::events();
        let network_removed_event_found = events.iter().any(|record| {
            matches!(
                record.event,
                RuntimeEvent::SubtensorModule(Event::NetworkRemoved(removed_netuid))
                if removed_netuid == netuid
            )
        });

        assert!(
            network_removed_event_found,
            "NetworkRemoved event not found"
        );
    });
}

#[test]
fn test_dissolve_network_removes_subnet_identity_and_emits_event() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 30;
        let subnet_owner = U256::from(1);

        // Set up the network with subnet_owner as the owner
        add_network(netuid, 0, 0);
        pallet_subtensor::SubnetOwner::<Test>::insert(netuid, subnet_owner);

        let subnet_name = b"Test Subnet".to_vec();
        let github_repo = b"https://github.com/test/subnet".to_vec();
        let subnet_contact = b"contact@testsubnet.com".to_vec();

        // Set subnet identity
        assert_ok!(SubtensorModule::do_set_subnet_identity(
            <<Test as Config>::RuntimeOrigin>::signed(subnet_owner),
            netuid,
            subnet_name.clone(),
            github_repo.clone(),
            subnet_contact.clone()
        ));

        // Ensure the subnet identity exists
        assert!(pallet_subtensor::SubnetIdentities::<Test>::contains_key(
            netuid
        ));

        // Dissolve the network
        assert_ok!(SubtensorModule::dissolve_network(
            RuntimeOrigin::root(),
            subnet_owner,
            netuid,
        ));

        // Check that the SubnetIdentityRemoved event was emitted
        let events = System::events();
        let subnet_identity_removed_event_found = events.iter().any(|record| {
            matches!(
                record.event,
                RuntimeEvent::SubtensorModule(Event::SubnetIdentityRemoved(removed_netuid))
                if removed_netuid == netuid
            )
        });

        assert!(
            subnet_identity_removed_event_found,
            "SubnetIdentityRemoved event not found"
        );

        // Ensure the subnet identity is removed
        assert!(!pallet_subtensor::SubnetIdentities::<Test>::contains_key(
            netuid
        ));
    });
}

#[test]
fn test_dissolve_network_all_weights_removed() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 30;
        let subnet_owner = U256::from(1);
        let uid_i: u16 = 0;
        let uid_j: u16 = 1;
        let weight_value: u16 = 50;

        // Set up the network with subnet_owner as the owner
        add_network(netuid, 0, 0);
        pallet_subtensor::SubnetOwner::<Test>::insert(netuid, subnet_owner);

        // Insert some weights for the netuid
        let weights = vec![(uid_j, weight_value)];
        pallet_subtensor::Weights::<Test>::insert(netuid, uid_i, weights);

        // Ensure weights exist
        assert!(pallet_subtensor::Weights::<Test>::contains_key(
            netuid, uid_i
        ));

        // Dissolve the network
        assert_ok!(SubtensorModule::dissolve_network(
            RuntimeOrigin::root(),
            subnet_owner,
            netuid,
        ));

        // Check that weights are removed
        assert!(!pallet_subtensor::Weights::<Test>::contains_key(
            netuid, uid_i
        ));
    });
}

#[test]
fn test_dissolve_network_removes_netuid_parameters() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 30;
        let subnet_owner = U256::from(1);

        // Set up the network with subnet_owner as the owner
        add_network(netuid, 0, 0);
        pallet_subtensor::SubnetOwner::<Test>::insert(netuid, subnet_owner);

        // Set some netuid related parameters with correct types
        pallet_subtensor::Rank::<Test>::insert(netuid, vec![0u16, 1, 2]);
        pallet_subtensor::Trust::<Test>::insert(netuid, vec![0u16, 1, 2]);
        pallet_subtensor::Active::<Test>::insert(netuid, vec![true, false, true]);
        pallet_subtensor::Emission::<Test>::insert(netuid, vec![100u64, 200, 300]);
        pallet_subtensor::Incentive::<Test>::insert(netuid, vec![10u16, 20, 30]);
        pallet_subtensor::Consensus::<Test>::insert(netuid, vec![0u16, 1, 2]);
        pallet_subtensor::Dividends::<Test>::insert(netuid, vec![5u16, 15, 25]);
        pallet_subtensor::PruningScores::<Test>::insert(netuid, vec![1u16, 2, 3]);
        pallet_subtensor::LastUpdate::<Test>::insert(netuid, vec![12345u64]);
        pallet_subtensor::ValidatorPermit::<Test>::insert(netuid, vec![true, true, false]);
        pallet_subtensor::ValidatorTrust::<Test>::insert(netuid, vec![5u16, 7, 9]);

        // Ensure parameters exist
        assert!(pallet_subtensor::Rank::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::Trust::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::Active::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::Emission::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::Incentive::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::Consensus::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::Dividends::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::PruningScores::<Test>::contains_key(
            netuid
        ));
        assert!(pallet_subtensor::LastUpdate::<Test>::contains_key(netuid));
        assert!(pallet_subtensor::ValidatorPermit::<Test>::contains_key(
            netuid
        ));
        assert!(pallet_subtensor::ValidatorTrust::<Test>::contains_key(
            netuid
        ));

        // Dissolve the network
        assert_ok!(SubtensorModule::dissolve_network(
            RuntimeOrigin::root(),
            subnet_owner,
            netuid,
        ));

        // Check that parameters are removed
        assert!(!pallet_subtensor::Rank::<Test>::contains_key(netuid));
        assert!(!pallet_subtensor::Trust::<Test>::contains_key(netuid));
        assert!(!pallet_subtensor::Active::<Test>::contains_key(netuid));
        assert!(!pallet_subtensor::Emission::<Test>::contains_key(netuid));
        assert!(!pallet_subtensor::Incentive::<Test>::contains_key(netuid));
        assert!(!pallet_subtensor::Consensus::<Test>::contains_key(netuid));
        assert!(!pallet_subtensor::Dividends::<Test>::contains_key(netuid));
        assert!(!pallet_subtensor::PruningScores::<Test>::contains_key(
            netuid
        ));
        assert!(!pallet_subtensor::LastUpdate::<Test>::contains_key(netuid));
        assert!(!pallet_subtensor::ValidatorPermit::<Test>::contains_key(
            netuid
        ));
        assert!(!pallet_subtensor::ValidatorTrust::<Test>::contains_key(
            netuid
        ));
    });
}

#[test]
fn test_dissolve_network_subnet_locked_balance_returned_and_cleared() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 30;
        let subnet_owner = U256::from(1);
        let reserved_amount: u64 = 1000;

        // Set up the network with subnet_owner as the owner
        add_network(netuid, 0, 0);
        pallet_subtensor::SubnetOwner::<Test>::insert(netuid, subnet_owner);

        // Set the subnet locked balance
        SubtensorModule::set_subnet_locked_balance(netuid, reserved_amount);

        // Give the subnet owner some initial balance
        SubtensorModule::add_balance_to_coldkey_account(&subnet_owner, 0);
        let initial_balance = SubtensorModule::get_coldkey_balance(&subnet_owner);

        // Dissolve the network
        assert_ok!(SubtensorModule::dissolve_network(
            RuntimeOrigin::root(),
            subnet_owner,
            netuid,
        ));

        // Check that the subnet owner's balance increased by reserved_amount
        let new_balance = SubtensorModule::get_coldkey_balance(&subnet_owner);
        assert_eq!(new_balance, initial_balance + reserved_amount);

        // Check that the subnet locked balance is cleared
        let locked_balance = SubtensorModule::get_subnet_locked_balance(netuid);
        assert_eq!(locked_balance, 0);
    });
}

#[test]
fn test_blocks_until_next_epoch() {
    // Special case: tempo = 0 (Network never runs)
    assert_eq!(
        SubtensorModule::blocks_until_next_epoch(0, 0, 0),
        u64::MAX,
        "Special case: tempo = 0"
    );

    // First epoch block, tempo = 1, netuid = 0
    assert_eq!(
        SubtensorModule::blocks_until_next_epoch(0, 1, 0),
        0,
        "First epoch block, tempo = 1, netuid = 0"
    );

    // First epoch block, tempo = 1, netuid = 1
    assert_eq!(
        SubtensorModule::blocks_until_next_epoch(1, 1, 0),
        1,
        "First epoch block, tempo = 1, netuid = 1"
    );

    // First epoch block, tempo = 2, netuid = 0
    assert_eq!(
        SubtensorModule::blocks_until_next_epoch(0, 2, 0),
        1,
        "First epoch block, tempo = 2, netuid = 0"
    );

    // First epoch block, tempo = 2, netuid = 1
    assert_eq!(
        SubtensorModule::blocks_until_next_epoch(1, 2, 0),
        0,
        "First epoch block, tempo = 2, netuid = 1"
    );

    // First epoch block, tempo = 100, netuid = 0
    assert_eq!(
        SubtensorModule::blocks_until_next_epoch(0, 100, 0),
        99,
        "First epoch block, tempo = 100, netuid = 0"
    );

    // First epoch block, tempo = 100, netuid = 1
    assert_eq!(
        SubtensorModule::blocks_until_next_epoch(1, 100, 0),
        98,
        "First epoch block, tempo = 100, netuid = 1"
    );

    // block_number > 0, tempo = 100, netuid = 1
    assert_eq!(
        SubtensorModule::blocks_until_next_epoch(1, 100, 101),
        98,
        "block_number > 0, tempo = 100, netuid = 1"
    );
}
