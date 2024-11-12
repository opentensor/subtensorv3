#![allow(clippy::indexing_slicing)]

mod mock;
use frame_support::{
    assert_err, assert_ok,
    dispatch::{DispatchClass, DispatchInfo, DispatchResult, GetDispatchInfo, Pays},
    pallet_prelude::{InvalidTransaction, TransactionValidityError},
};
use mock::*;
use pallet_subtensor::{Error, Owner};
use scale_info::prelude::collections::HashMap;
use sp_core::{H256, U256};
use sp_runtime::{
    traits::{BlakeTwo256, DispatchInfoOf, Hash, SignedExtension},
    DispatchError,
};
use sp_std::collections::vec_deque::VecDeque;
use substrate_fixed::types::I32F32;

/***************************
  pub fn set_weights() tests
*****************************/

// Test the call passes through the subtensor module.
#[test]
fn test_set_weights_dispatch_info_ok() {
    new_test_ext(0).execute_with(|| {
        let dests = vec![1, 1];
        let weights = vec![1, 1];
        let netuid: u16 = 1;
        let version_key: u64 = 0;
        let call = RuntimeCall::SubtensorModule(SubtensorCall::set_weights {
            netuid,
            dests,
            weights,
            version_key,
        });
        let dispatch_info = call.get_dispatch_info();

        assert_eq!(dispatch_info.class, DispatchClass::Normal);
        assert_eq!(dispatch_info.pays_fee, Pays::No);
    });
}
#[test]
fn test_set_rootweights_dispatch_info_ok() {
    new_test_ext(0).execute_with(|| {
        let dests = vec![1, 1];
        let weights = vec![1, 1];
        let netuid: u16 = 1;
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1); // Add the hotkey field
        let call = RuntimeCall::SubtensorModule(SubtensorCall::set_root_weights {
            netuid,
            dests,
            weights,
            version_key,
            hotkey, // Include the hotkey field
        });
        let dispatch_info = call.get_dispatch_info();

        assert_eq!(dispatch_info.class, DispatchClass::Normal);
        assert_eq!(dispatch_info.pays_fee, Pays::No);
    });
}

#[test]
fn test_set_rootweights_validate() {
    // Testing the signed extension validate function
    // correctly filters this transaction.

    new_test_ext(0).execute_with(|| {
        let dests = vec![1, 1];
        let weights = vec![1, 1];
        let netuid: u16 = 1;
        let version_key: u64 = 0;
        let coldkey = U256::from(0);
        let hotkey: U256 = U256::from(1); // Add the hotkey field
        assert_ne!(hotkey, coldkey); // Ensure hotkey is NOT the same as coldkey !!!

        let who = coldkey; // The coldkey signs this transaction

        let call = RuntimeCall::SubtensorModule(SubtensorCall::set_root_weights {
            netuid,
            dests,
            weights,
            version_key,
            hotkey, // Include the hotkey field
        });

        // Create netuid
        add_network(netuid, 0, 0);
        // Register the hotkey
        SubtensorModule::append_neuron(netuid, &hotkey, 0);
        Owner::<Test>::insert(hotkey, coldkey);

        let min_stake = 500_000_000_000;
        // Set the minimum stake
        SubtensorModule::set_weights_min_stake(min_stake);

        // Verify stake is less than minimum
        assert!(SubtensorModule::get_total_stake_for_hotkey(&hotkey) < min_stake);
        let info: DispatchInfo =
            DispatchInfoOf::<<Test as frame_system::Config>::RuntimeCall>::default();

        let extension = pallet_subtensor::SubtensorSignedExtension::<Test>::new();
        // Submit to the signed extension validate function
        let result_no_stake = extension.validate(&who, &call.clone(), &info, 10);
        // Should fail
        assert_err!(
            // Should get an invalid transaction error
            result_no_stake,
            TransactionValidityError::Invalid(InvalidTransaction::Custom(4))
        );

        // Increase the stake to be equal to the minimum
        SubtensorModule::increase_stake_on_hotkey_account(&hotkey, min_stake);

        // Verify stake is equal to minimum
        assert_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&hotkey),
            min_stake
        );

        // Submit to the signed extension validate function
        let result_min_stake = extension.validate(&who, &call.clone(), &info, 10);
        // Now the call should pass
        assert_ok!(result_min_stake);

        // Try with more stake than minimum
        SubtensorModule::increase_stake_on_hotkey_account(&hotkey, 1);

        // Verify stake is more than minimum
        assert!(SubtensorModule::get_total_stake_for_hotkey(&hotkey) > min_stake);

        let result_more_stake = extension.validate(&who, &call.clone(), &info, 10);
        // The call should still pass
        assert_ok!(result_more_stake);
    });
}

#[test]
fn test_commit_weights_dispatch_info_ok() {
    new_test_ext(0).execute_with(|| {
        let dests = vec![1, 1];
        let weights = vec![1, 1];
        let netuid: u16 = 1;
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        let commit_hash: H256 =
            BlakeTwo256::hash_of(&(hotkey, netuid, dests, weights, salt, version_key));

        let call = RuntimeCall::SubtensorModule(SubtensorCall::commit_weights {
            netuid,
            commit_hash,
        });
        let dispatch_info = call.get_dispatch_info();

        assert_eq!(dispatch_info.class, DispatchClass::Normal);
        assert_eq!(dispatch_info.pays_fee, Pays::No);
    });
}

#[test]
fn test_commit_weights_validate() {
    // Testing the signed extension validate function
    // correctly filters this transaction.

    new_test_ext(0).execute_with(|| {
        let dests = vec![1, 1];
        let weights = vec![1, 1];
        let netuid: u16 = 1;
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let coldkey = U256::from(0);
        let hotkey: U256 = U256::from(1); // Add the hotkey field
        assert_ne!(hotkey, coldkey); // Ensure hotkey is NOT the same as coldkey !!!

        let who = hotkey; // The hotkey signs this transaction

        let commit_hash: H256 =
            BlakeTwo256::hash_of(&(hotkey, netuid, dests, weights, salt, version_key));

        let call = RuntimeCall::SubtensorModule(SubtensorCall::commit_weights {
            netuid,
            commit_hash,
        });

        // Create netuid
        add_network(netuid, 0, 0);
        // Register the hotkey
        SubtensorModule::append_neuron(netuid, &hotkey, 0);
        Owner::<Test>::insert(hotkey, coldkey);

        let min_stake = 500_000_000_000;
        // Set the minimum stake
        SubtensorModule::set_weights_min_stake(min_stake);

        // Verify stake is less than minimum
        assert!(SubtensorModule::get_total_stake_for_hotkey(&hotkey) < min_stake);
        let info: DispatchInfo =
            DispatchInfoOf::<<Test as frame_system::Config>::RuntimeCall>::default();

        let extension = pallet_subtensor::SubtensorSignedExtension::<Test>::new();
        // Submit to the signed extension validate function
        let result_no_stake = extension.validate(&who, &call.clone(), &info, 10);
        // Should fail
        assert_err!(
            // Should get an invalid transaction error
            result_no_stake,
            TransactionValidityError::Invalid(InvalidTransaction::Custom(1))
        );

        // Increase the stake to be equal to the minimum
        SubtensorModule::increase_stake_on_hotkey_account(&hotkey, min_stake);

        // Verify stake is equal to minimum
        assert_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&hotkey),
            min_stake
        );

        // Submit to the signed extension validate function
        let result_min_stake = extension.validate(&who, &call.clone(), &info, 10);
        // Now the call should pass
        assert_ok!(result_min_stake);

        // Try with more stake than minimum
        SubtensorModule::increase_stake_on_hotkey_account(&hotkey, 1);

        // Verify stake is more than minimum
        assert!(SubtensorModule::get_total_stake_for_hotkey(&hotkey) > min_stake);

        let result_more_stake = extension.validate(&who, &call.clone(), &info, 10);
        // The call should still pass
        assert_ok!(result_more_stake);
    });
}

#[test]
fn test_reveal_weights_dispatch_info_ok() {
    new_test_ext(0).execute_with(|| {
        let dests = vec![1, 1];
        let weights = vec![1, 1];
        let netuid: u16 = 1;
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;

        let call = RuntimeCall::SubtensorModule(SubtensorCall::reveal_weights {
            netuid,
            uids: dests,
            values: weights,
            salt,
            version_key,
        });
        let dispatch_info = call.get_dispatch_info();

        assert_eq!(dispatch_info.class, DispatchClass::Normal);
        assert_eq!(dispatch_info.pays_fee, Pays::No);
    });
}

#[test]
fn test_set_weights_validate() {
    // Testing the signed extension validate function
    // correctly filters the `set_weights` transaction.

    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;
        let coldkey = U256::from(0);
        let hotkey: U256 = U256::from(1);
        assert_ne!(hotkey, coldkey);

        let who = hotkey; // The hotkey signs this transaction

        let call = RuntimeCall::SubtensorModule(SubtensorCall::set_weights {
            netuid,
            dests: vec![1, 1],
            weights: vec![1, 1],
            version_key: 0,
        });

        // Create netuid
        add_network(netuid, 0, 0);
        // Register the hotkey
        SubtensorModule::append_neuron(netuid, &hotkey, 0);
        Owner::<Test>::insert(hotkey, coldkey);

        let min_stake = 500_000_000_000;
        // Set the minimum stake
        SubtensorModule::set_weights_min_stake(min_stake);

        // Verify stake is less than minimum
        assert!(SubtensorModule::get_total_stake_for_hotkey(&hotkey) < min_stake);
        let info: DispatchInfo =
            DispatchInfoOf::<<Test as frame_system::Config>::RuntimeCall>::default();

        let extension = pallet_subtensor::SubtensorSignedExtension::<Test>::new();
        // Submit to the signed extension validate function
        let result_no_stake = extension.validate(&who, &call.clone(), &info, 10);
        // Should fail due to insufficient stake
        assert_err!(
            result_no_stake,
            TransactionValidityError::Invalid(InvalidTransaction::Custom(3))
        );

        // Increase the stake to be equal to the minimum
        SubtensorModule::increase_stake_on_hotkey_account(&hotkey, min_stake);

        // Verify stake is equal to minimum
        assert_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&hotkey),
            min_stake
        );

        // Submit to the signed extension validate function
        let result_min_stake = extension.validate(&who, &call.clone(), &info, 10);
        // Now the call should pass
        assert_ok!(result_min_stake);
    });
}

#[test]
fn test_reveal_weights_validate() {
    // Testing the signed extension validate function
    // correctly filters this transaction.

    new_test_ext(0).execute_with(|| {
        let dests = vec![1, 1];
        let weights = vec![1, 1];
        let netuid: u16 = 1;
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let coldkey = U256::from(0);
        let hotkey: U256 = U256::from(1); // Add the hotkey field
        assert_ne!(hotkey, coldkey); // Ensure hotkey is NOT the same as coldkey !!!

        let who = hotkey; // The hotkey signs this transaction

        let call = RuntimeCall::SubtensorModule(SubtensorCall::reveal_weights {
            netuid,
            uids: dests,
            values: weights,
            salt,
            version_key,
        });

        // Create netuid
        add_network(netuid, 0, 0);
        // Register the hotkey
        SubtensorModule::append_neuron(netuid, &hotkey, 0);
        Owner::<Test>::insert(hotkey, coldkey);

        let min_stake = 500_000_000_000;
        // Set the minimum stake
        SubtensorModule::set_weights_min_stake(min_stake);

        // Verify stake is less than minimum
        assert!(SubtensorModule::get_total_stake_for_hotkey(&hotkey) < min_stake);
        let info: DispatchInfo =
            DispatchInfoOf::<<Test as frame_system::Config>::RuntimeCall>::default();

        let extension = pallet_subtensor::SubtensorSignedExtension::<Test>::new();
        // Submit to the signed extension validate function
        let result_no_stake = extension.validate(&who, &call.clone(), &info, 10);
        // Should fail
        assert_err!(
            // Should get an invalid transaction error
            result_no_stake,
            TransactionValidityError::Invalid(InvalidTransaction::Custom(2))
        );

        // Increase the stake to be equal to the minimum
        SubtensorModule::increase_stake_on_hotkey_account(&hotkey, min_stake);

        // Verify stake is equal to minimum
        assert_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&hotkey),
            min_stake
        );

        // Submit to the signed extension validate function
        let result_min_stake = extension.validate(&who, &call.clone(), &info, 10);
        // Now the call should pass
        assert_ok!(result_min_stake);

        // Try with more stake than minimum
        SubtensorModule::increase_stake_on_hotkey_account(&hotkey, 1);

        // Verify stake is more than minimum
        assert!(SubtensorModule::get_total_stake_for_hotkey(&hotkey) > min_stake);

        let result_more_stake = extension.validate(&who, &call.clone(), &info, 10);
        // The call should still pass
        assert_ok!(result_more_stake);
    });
}

#[test]
fn test_set_weights_is_root_error() {
    new_test_ext(0).execute_with(|| {
        let root_netuid: u16 = 0;

        let uids = vec![0];
        let weights = vec![1];
        let version_key: u64 = 0;
        let hotkey = U256::from(1);

        assert_err!(
            SubtensorModule::set_weights(
                RuntimeOrigin::signed(hotkey),
                root_netuid,
                uids.clone(),
                weights.clone(),
                version_key,
            ),
            Error::<Test>::CanNotSetRootNetworkWeights
        );
    });
}

// Test ensures that uid has validator permit to set non-self weights.
#[test]
fn test_weights_err_no_validator_permit() {
    new_test_ext(0).execute_with(|| {
        let hotkey_account_id = U256::from(55);
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        add_network(netuid, tempo, 0);
        SubtensorModule::set_min_allowed_weights(netuid, 0);
        SubtensorModule::set_max_allowed_uids(netuid, 3);
        SubtensorModule::set_max_weight_limit(netuid, u16::MAX);
        register_ok_neuron(netuid, hotkey_account_id, U256::from(66), 0);
        register_ok_neuron(netuid, U256::from(1), U256::from(1), 65555);
        register_ok_neuron(netuid, U256::from(2), U256::from(2), 75555);

        let weights_keys: Vec<u16> = vec![1, 2];
        let weight_values: Vec<u16> = vec![1, 2];

        let result = SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey_account_id),
            netuid,
            weights_keys,
            weight_values,
            0,
        );
        assert_eq!(result, Err(Error::<Test>::NeuronNoValidatorPermit.into()));

        let weights_keys: Vec<u16> = vec![1, 2];
        let weight_values: Vec<u16> = vec![1, 2];
        let neuron_uid: u16 =
            SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey_account_id)
                .expect("Not registered.");
        SubtensorModule::set_validator_permit_for_uid(netuid, neuron_uid, true);
        let result = SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey_account_id),
            netuid,
            weights_keys,
            weight_values,
            0,
        );
        assert_ok!(result);
    });
}

// To execute this test: cargo test --package pallet-subtensor --test weights test_set_weights_min_stake_failed -- --nocapture`
#[test]
fn test_set_weights_min_stake_failed() {
    new_test_ext(0).execute_with(|| {
        let dests = vec![0];
        let weights = vec![1];
        let netuid: u16 = 1;
        let version_key: u64 = 0;
        let hotkey = U256::from(0);
        let coldkey = U256::from(0);

        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 2143124);
        SubtensorModule::set_weights_min_stake(20_000_000_000_000);

        // Check the signed extension function.
        assert_eq!(SubtensorModule::get_weights_min_stake(), 20_000_000_000_000);
        assert!(!SubtensorModule::check_weights_min_stake(&hotkey, netuid));
        SubtensorModule::increase_stake_on_hotkey_account(&hotkey, 19_000_000_000_000);
        assert!(!SubtensorModule::check_weights_min_stake(&hotkey, netuid));
        SubtensorModule::increase_stake_on_hotkey_account(&hotkey, 20_000_000_000_000);
        assert!(SubtensorModule::check_weights_min_stake(&hotkey, netuid));

        // Check that it fails at the pallet level.
        SubtensorModule::set_weights_min_stake(100_000_000_000_000);
        assert_eq!(
            SubtensorModule::set_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                dests.clone(),
                weights.clone(),
                version_key,
            ),
            Err(Error::<Test>::NotEnoughStakeToSetWeights.into())
        );
        // Now passes
        SubtensorModule::increase_stake_on_hotkey_account(&hotkey, 100_000_000_000_000);
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            dests.clone(),
            weights.clone(),
            version_key
        ));
    });
}

// Test ensures that a uid can only set weights if it has the valid weights set version key.
#[test]
fn test_weights_version_key() {
    new_test_ext(0).execute_with(|| {
        let hotkey = U256::from(55);
        let coldkey = U256::from(66);
        let netuid0: u16 = 1;
        let netuid1: u16 = 2;

        add_network(netuid0, 0, 0);
        add_network(netuid1, 0, 0);
        register_ok_neuron(netuid0, hotkey, coldkey, 2143124);
        register_ok_neuron(netuid1, hotkey, coldkey, 3124124);

        let weights_keys: Vec<u16> = vec![0];
        let weight_values: Vec<u16> = vec![1];
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid0,
            weights_keys.clone(),
            weight_values.clone(),
            0
        ));
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid1,
            weights_keys.clone(),
            weight_values.clone(),
            0
        ));

        // Set version keys.
        let key0: u64 = 12312;
        let key1: u64 = 20313;
        SubtensorModule::set_weights_version_key(netuid0, key0);
        SubtensorModule::set_weights_version_key(netuid1, key1);

        // Setting works with version key.
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid0,
            weights_keys.clone(),
            weight_values.clone(),
            key0
        ));
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid1,
            weights_keys.clone(),
            weight_values.clone(),
            key1
        ));

        // validator:20313 >= network:12312 (accepted: validator newer)
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid0,
            weights_keys.clone(),
            weight_values.clone(),
            key1
        ));

        // Setting fails with incorrect keys.
        // validator:12312 < network:20313 (rejected: validator not updated)
        assert_eq!(
            SubtensorModule::set_weights(
                RuntimeOrigin::signed(hotkey),
                netuid1,
                weights_keys.clone(),
                weight_values.clone(),
                key0
            ),
            Err(Error::<Test>::IncorrectWeightVersionKey.into())
        );
    });
}

// Test ensures that uid has validator permit to set non-self weights.
#[test]
fn test_weights_err_setting_weights_too_fast() {
    new_test_ext(0).execute_with(|| {
        let hotkey_account_id = U256::from(55);
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        add_network(netuid, tempo, 0);
        SubtensorModule::set_min_allowed_weights(netuid, 0);
        SubtensorModule::set_max_allowed_uids(netuid, 3);
        SubtensorModule::set_max_weight_limit(netuid, u16::MAX);
        register_ok_neuron(netuid, hotkey_account_id, U256::from(66), 0);
        register_ok_neuron(netuid, U256::from(1), U256::from(1), 65555);
        register_ok_neuron(netuid, U256::from(2), U256::from(2), 75555);

        let neuron_uid: u16 =
            SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey_account_id)
                .expect("Not registered.");
        SubtensorModule::set_validator_permit_for_uid(netuid, neuron_uid, true);
        SubtensorModule::set_weights_set_rate_limit(netuid, 10);
        assert_eq!(SubtensorModule::get_weights_set_rate_limit(netuid), 10);

        let weights_keys: Vec<u16> = vec![1, 2];
        let weight_values: Vec<u16> = vec![1, 2];

        // Note that LastUpdate has default 0 for new uids, but if they have actually set weights on block 0
        // then they are allowed to set weights again once more without a wait restriction, to accommodate the default.
        let result = SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey_account_id),
            netuid,
            weights_keys.clone(),
            weight_values.clone(),
            0,
        );
        assert_ok!(result);
        run_to_block(1);

        for i in 1..100 {
            let result = SubtensorModule::set_weights(
                RuntimeOrigin::signed(hotkey_account_id),
                netuid,
                weights_keys.clone(),
                weight_values.clone(),
                0,
            );
            if i % 10 == 1 {
                assert_ok!(result);
            } else {
                assert_eq!(result, Err(Error::<Test>::SettingWeightsTooFast.into()));
            }
            run_to_block(i + 1);
        }
    });
}

// Test ensures that uids -- weights must have the same size.
#[test]
fn test_weights_err_weights_vec_not_equal_size() {
    new_test_ext(0).execute_with(|| {
        let hotkey_account_id = U256::from(55);
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        add_network(netuid, tempo, 0);
        register_ok_neuron(1, hotkey_account_id, U256::from(66), 0);
        let neuron_uid: u16 =
            SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey_account_id)
                .expect("Not registered.");
        SubtensorModule::set_validator_permit_for_uid(netuid, neuron_uid, true);
        let weights_keys: Vec<u16> = vec![1, 2, 3, 4, 5, 6];
        let weight_values: Vec<u16> = vec![1, 2, 3, 4, 5]; // Uneven sizes
        let result = commit_reveal_set_weights(
            hotkey_account_id,
            1,
            weights_keys.clone(),
            weight_values.clone(),
            salt.clone(),
            0,
        );
        assert_eq!(result, Err(Error::<Test>::WeightVecNotEqualSize.into()));
    });
}

// Test ensures that uids can have not duplicates
#[test]
fn test_weights_err_has_duplicate_ids() {
    new_test_ext(0).execute_with(|| {
        let hotkey_account_id = U256::from(666);
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        add_network(netuid, tempo, 0);

        SubtensorModule::set_max_allowed_uids(netuid, 100); // Allow many registrations per block.
        SubtensorModule::set_max_registrations_per_block(netuid, 100); // Allow many registrations per block.
        SubtensorModule::set_target_registrations_per_interval(netuid, 100); // Allow many registrations per block.
                                                                             // uid 0
        register_ok_neuron(netuid, hotkey_account_id, U256::from(77), 0);
        let neuron_uid: u16 =
            SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey_account_id)
                .expect("Not registered.");
        SubtensorModule::set_validator_permit_for_uid(netuid, neuron_uid, true);

        // uid 1
        register_ok_neuron(netuid, U256::from(1), U256::from(1), 100_000);
        SubtensorModule::get_uid_for_net_and_hotkey(netuid, &U256::from(1))
            .expect("Not registered.");

        // uid 2
        register_ok_neuron(netuid, U256::from(2), U256::from(1), 200_000);
        SubtensorModule::get_uid_for_net_and_hotkey(netuid, &U256::from(2))
            .expect("Not registered.");

        // uid 3
        register_ok_neuron(netuid, U256::from(3), U256::from(1), 300_000);
        SubtensorModule::get_uid_for_net_and_hotkey(netuid, &U256::from(3))
            .expect("Not registered.");

        assert_eq!(SubtensorModule::get_subnetwork_n(netuid), 4);

        let weights_keys: Vec<u16> = vec![1, 1, 1]; // Contains duplicates
        let weight_values: Vec<u16> = vec![1, 2, 3];
        let result = commit_reveal_set_weights(
            hotkey_account_id,
            netuid,
            weights_keys.clone(),
            weight_values.clone(),
            salt.clone(),
            0,
        );
        assert_eq!(result, Err(Error::<Test>::DuplicateUids.into()));
    });
}

// Test ensures weights cannot exceed max weight limit.
#[test]
fn test_weights_err_max_weight_limit() {
    //TO DO SAM: uncomment when we implement run_to_block fn
    new_test_ext(0).execute_with(|| {
        // Add network.
        let netuid: u16 = 1;
        let tempo: u16 = 100;
        add_network(netuid, tempo, 0);

        // Set params.
        SubtensorModule::set_max_allowed_uids(netuid, 5);
        SubtensorModule::set_target_registrations_per_interval(netuid, 5);
        SubtensorModule::set_max_weight_limit(netuid, u16::MAX / 5);
        SubtensorModule::set_min_allowed_weights(netuid, 0);

        // Add 5 accounts.
        println!("+Registering: net:{:?}, cold:{:?}, hot:{:?}", netuid, 0, 0);
        register_ok_neuron(netuid, U256::from(0), U256::from(0), 55555);
        let neuron_uid: u16 = SubtensorModule::get_uid_for_net_and_hotkey(netuid, &U256::from(0))
            .expect("Not registered.");
        SubtensorModule::set_validator_permit_for_uid(netuid, neuron_uid, true);
        assert_eq!(SubtensorModule::get_subnetwork_n(netuid), 1);
        assert!(SubtensorModule::is_hotkey_registered_on_network(
            netuid,
            &U256::from(0)
        ));
        step_block(1);

        println!("+Registering: net:{:?}, cold:{:?}, hot:{:?}", netuid, 1, 1);
        register_ok_neuron(netuid, U256::from(1), U256::from(1), 65555);
        assert!(SubtensorModule::is_hotkey_registered_on_network(
            netuid,
            &U256::from(1)
        ));
        assert_eq!(SubtensorModule::get_subnetwork_n(netuid), 2);
        step_block(1);

        println!("+Registering: net:{:?}, cold:{:?}, hot:{:?}", netuid, 2, 2);
        register_ok_neuron(netuid, U256::from(2), U256::from(2), 75555);
        assert!(SubtensorModule::is_hotkey_registered_on_network(
            netuid,
            &U256::from(2)
        ));
        assert_eq!(SubtensorModule::get_subnetwork_n(netuid), 3);
        step_block(1);

        println!("+Registering: net:{:?}, cold:{:?}, hot:{:?}", netuid, 3, 3);
        register_ok_neuron(netuid, U256::from(3), U256::from(3), 95555);
        assert!(SubtensorModule::is_hotkey_registered_on_network(
            netuid,
            &U256::from(3)
        ));
        assert_eq!(SubtensorModule::get_subnetwork_n(netuid), 4);
        step_block(1);

        println!("+Registering: net:{:?}, cold:{:?}, hot:{:?}", netuid, 4, 4);
        register_ok_neuron(netuid, U256::from(4), U256::from(4), 35555);
        assert!(SubtensorModule::is_hotkey_registered_on_network(
            netuid,
            &U256::from(4)
        ));
        assert_eq!(SubtensorModule::get_subnetwork_n(netuid), 5);
        step_block(1);

        // Non self-weight fails.
        let uids: Vec<u16> = vec![1, 2, 3, 4];
        let values: Vec<u16> = vec![u16::MAX / 4, u16::MAX / 4, u16::MAX / 54, u16::MAX / 4];
        let result =
            SubtensorModule::set_weights(RuntimeOrigin::signed(U256::from(0)), 1, uids, values, 0);
        assert_eq!(result, Err(Error::<Test>::MaxWeightExceeded.into()));

        // Self-weight is a success.
        let uids: Vec<u16> = vec![0]; // Self.
        let values: Vec<u16> = vec![u16::MAX]; // normalizes to u32::MAX
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(U256::from(0)),
            1,
            uids,
            values,
            0
        ));
    });
}

// Tests the call requires a valid origin.
#[test]
fn test_no_signature() {
    new_test_ext(0).execute_with(|| {
        let uids: Vec<u16> = vec![];
        let values: Vec<u16> = vec![];
        let result = SubtensorModule::set_weights(RuntimeOrigin::none(), 1, uids, values, 0);
        assert_eq!(result, Err(DispatchError::BadOrigin));
    });
}

// Tests that weights cannot be set BY non-registered hotkeys.
#[test]
fn test_set_weights_err_not_active() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        add_network(netuid, tempo, 0);

        // Register one neuron. Should have uid 0
        register_ok_neuron(1, U256::from(666), U256::from(2), 100000);
        SubtensorModule::get_uid_for_net_and_hotkey(netuid, &U256::from(666))
            .expect("Not registered.");

        let weights_keys: Vec<u16> = vec![0]; // Uid 0 is valid.
        let weight_values: Vec<u16> = vec![1];
        // This hotkey is NOT registered.
        let result =
            commit_reveal_set_weights(U256::from(1), 1, weights_keys, weight_values, salt, 0);
        assert_eq!(
            result,
            Err(Error::<Test>::HotKeyNotRegisteredInSubNet.into())
        );
    });
}

// Tests that set weights fails if you pass invalid uids.
#[test]
fn test_set_weights_err_invalid_uid() {
    new_test_ext(0).execute_with(|| {
        let hotkey_account_id = U256::from(55);
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        add_network(netuid, tempo, 0);
        register_ok_neuron(1, hotkey_account_id, U256::from(66), 0);
        let neuron_uid: u16 =
            SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey_account_id)
                .expect("Not registered.");
        SubtensorModule::set_validator_permit_for_uid(netuid, neuron_uid, true);
        let weight_keys: Vec<u16> = vec![9999]; // Does not exist
        let weight_values: Vec<u16> = vec![88]; // random value
        let result =
            commit_reveal_set_weights(hotkey_account_id, 1, weight_keys, weight_values, salt, 0);
        assert_eq!(result, Err(Error::<Test>::UidVecContainInvalidOne.into()));
    });
}

// Tests that set weights fails if you don't pass enough values.
#[test]
fn test_set_weight_not_enough_values() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let account_id = U256::from(1);
        add_network(netuid, tempo, 0);

        register_ok_neuron(1, account_id, U256::from(2), 100000);
        let neuron_uid: u16 = SubtensorModule::get_uid_for_net_and_hotkey(netuid, &U256::from(1))
            .expect("Not registered.");
        SubtensorModule::set_validator_permit_for_uid(netuid, neuron_uid, true);
        SubtensorModule::set_max_weight_limit(netuid, u16::MAX);

        register_ok_neuron(1, U256::from(3), U256::from(4), 300000);
        SubtensorModule::set_min_allowed_weights(netuid, 2);

        // Should fail because we are only setting a single value and its not the self weight.
        let weight_keys: Vec<u16> = vec![1]; // not weight.
        let weight_values: Vec<u16> = vec![88]; // random value.
        let result = SubtensorModule::set_weights(
            RuntimeOrigin::signed(account_id),
            1,
            weight_keys,
            weight_values,
            0,
        );
        assert_eq!(result, Err(Error::<Test>::WeightVecLengthIsLow.into()));

        // Shouldnt fail because we setting a single value but it is the self weight.
        let weight_keys: Vec<u16> = vec![0]; // self weight.
        let weight_values: Vec<u16> = vec![88]; // random value.
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(account_id),
            1,
            weight_keys,
            weight_values,
            0
        ));

        // Should pass because we are setting enough values.
        let weight_keys: Vec<u16> = vec![0, 1]; // self weight.
        let weight_values: Vec<u16> = vec![10, 10]; // random value.
        SubtensorModule::set_min_allowed_weights(netuid, 1);
        assert_ok!(commit_reveal_set_weights(
            account_id,
            1,
            weight_keys,
            weight_values,
            salt,
            0
        ));
    });
}

// Tests that the weights set fails if you pass too many uids for the subnet
#[test]
fn test_set_weight_too_many_uids() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        add_network(netuid, tempo, 0);

        register_ok_neuron(1, U256::from(1), U256::from(2), 100_000);
        let neuron_uid: u16 = SubtensorModule::get_uid_for_net_and_hotkey(netuid, &U256::from(1))
            .expect("Not registered.");
        SubtensorModule::set_validator_permit_for_uid(netuid, neuron_uid, true);

        register_ok_neuron(1, U256::from(3), U256::from(4), 300_000);
        SubtensorModule::set_min_allowed_weights(1, 2);
        SubtensorModule::set_max_weight_limit(netuid, u16::MAX);

        // Should fail because we are setting more weights than there are neurons.
        let weight_keys: Vec<u16> = vec![0, 1, 2, 3, 4]; // more uids than neurons in subnet.
        let weight_values: Vec<u16> = vec![88, 102, 303, 1212, 11]; // random value.
        let result = SubtensorModule::set_weights(
            RuntimeOrigin::signed(U256::from(1)),
            1,
            weight_keys,
            weight_values,
            0,
        );
        assert_eq!(
            result,
            Err(Error::<Test>::UidsLengthExceedUidsInSubNet.into())
        );

        // Shouldnt fail because we are setting less weights than there are neurons.
        let weight_keys: Vec<u16> = vec![0, 1]; // Only on neurons that exist.
        let weight_values: Vec<u16> = vec![10, 10]; // random value.
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(U256::from(1)),
            1,
            weight_keys,
            weight_values,
            0
        ));
    });
}

// Tests that the weights set doesn't panic if you pass weights that sum to larger than u16 max.
#[test]
fn test_set_weights_sum_larger_than_u16_max() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        add_network(netuid, tempo, 0);

        register_ok_neuron(1, U256::from(1), U256::from(2), 100_000);
        let neuron_uid: u16 = SubtensorModule::get_uid_for_net_and_hotkey(netuid, &U256::from(1))
            .expect("Not registered.");
        SubtensorModule::set_validator_permit_for_uid(netuid, neuron_uid, true);
        SubtensorModule::set_max_weight_limit(netuid, u16::MAX);

        register_ok_neuron(1, U256::from(3), U256::from(4), 300_000);
        SubtensorModule::set_min_allowed_weights(1, 2);

        // Shouldn't fail because we are setting the right number of weights.
        let weight_keys: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![u16::MAX, u16::MAX];
        // sum of weights is larger than u16 max.
        assert!(weight_values.iter().map(|x| *x as u64).sum::<u64>() > (u16::MAX as u64));

        let result =
            commit_reveal_set_weights(U256::from(1), 1, weight_keys, weight_values, salt, 0);
        assert_ok!(result);

        // Get max-upscaled unnormalized weights.
        let all_weights: Vec<Vec<I32F32>> = SubtensorModule::get_weights(netuid);
        let weights_set: &[I32F32] = &all_weights[neuron_uid as usize];
        assert_eq!(weights_set[0], I32F32::from_num(u16::MAX));
        assert_eq!(weights_set[1], I32F32::from_num(u16::MAX));
    });
}

/// Check _truthy_ path for self weight
#[test]
fn test_check_length_allows_singleton() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;

        let max_allowed: u16 = 1;
        let min_allowed_weights = max_allowed;

        SubtensorModule::set_min_allowed_weights(netuid, min_allowed_weights);

        let uids: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));
        let uid: u16 = uids[0];
        let weights: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));

        let expected = true;
        let result = SubtensorModule::check_length(netuid, uid, &uids, &weights);

        assert_eq!(expected, result, "Failed get expected result");
    });
}

/// Check _truthy_ path for weights within allowed range
#[test]
fn test_check_length_weights_length_exceeds_min_allowed() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;

        let max_allowed: u16 = 3;
        let min_allowed_weights = max_allowed;

        SubtensorModule::set_min_allowed_weights(netuid, min_allowed_weights);

        let uids: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));
        let uid: u16 = uids[0];
        let weights: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));

        let expected = true;
        let result = SubtensorModule::check_length(netuid, uid, &uids, &weights);

        assert_eq!(expected, result, "Failed get expected result");
    });
}

/// Check _falsey_ path for weights outside allowed range
#[test]
fn test_check_length_to_few_weights() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;

        let min_allowed_weights = 3;

        add_network(netuid, 1, 0);
        SubtensorModule::set_target_registrations_per_interval(netuid, 100);
        SubtensorModule::set_max_registrations_per_block(netuid, 100);
        // register morw than min allowed
        register_ok_neuron(1, U256::from(1), U256::from(1), 300_000);
        register_ok_neuron(1, U256::from(2), U256::from(2), 300_001);
        register_ok_neuron(1, U256::from(3), U256::from(3), 300_002);
        register_ok_neuron(1, U256::from(4), U256::from(4), 300_003);
        register_ok_neuron(1, U256::from(5), U256::from(5), 300_004);
        register_ok_neuron(1, U256::from(6), U256::from(6), 300_005);
        register_ok_neuron(1, U256::from(7), U256::from(7), 300_006);
        SubtensorModule::set_min_allowed_weights(netuid, min_allowed_weights);

        let uids: Vec<u16> = Vec::from_iter((0..2).map(|id| id + 1));
        let weights: Vec<u16> = Vec::from_iter((0..2).map(|id| id + 1));
        let uid: u16 = uids[0];

        let expected = false;
        let result = SubtensorModule::check_length(netuid, uid, &uids, &weights);

        assert_eq!(expected, result, "Failed get expected result");
    });
}

/// Check do nothing path
#[test]
fn test_normalize_weights_does_not_mutate_when_sum_is_zero() {
    new_test_ext(0).execute_with(|| {
        let max_allowed: u16 = 3;

        let weights: Vec<u16> = Vec::from_iter((0..max_allowed).map(|_| 0));

        let expected = weights.clone();
        let result = SubtensorModule::normalize_weights(weights);

        assert_eq!(
            expected, result,
            "Failed get expected result when everything _should_ be fine"
        );
    });
}

/// Check do something path
#[test]
fn test_normalize_weights_does_not_mutate_when_sum_not_zero() {
    new_test_ext(0).execute_with(|| {
        let max_allowed: u16 = 3;

        let weights: Vec<u16> = Vec::from_iter(0..max_allowed);

        let expected = weights.clone();
        let result = SubtensorModule::normalize_weights(weights);

        assert_eq!(expected.len(), result.len(), "Length of weights changed?!");
    });
}

/// Check _truthy_ path for weights length
#[test]
fn test_max_weight_limited_allow_self_weights_to_exceed_max_weight_limit() {
    new_test_ext(0).execute_with(|| {
        let max_allowed: u16 = 1;

        let netuid: u16 = 1;
        let uids: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));
        let uid: u16 = uids[0];
        let weights: Vec<u16> = vec![0];

        let expected = true;
        let result = SubtensorModule::max_weight_limited(netuid, uid, &uids, &weights);

        assert_eq!(
            expected, result,
            "Failed get expected result when everything _should_ be fine"
        );
    });
}

/// Check _truthy_ path for max weight limit
#[test]
fn test_max_weight_limited_when_weight_limit_is_u16_max() {
    new_test_ext(0).execute_with(|| {
        let max_allowed: u16 = 3;

        let netuid: u16 = 1;
        let uids: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));
        let uid: u16 = uids[0];
        let weights: Vec<u16> = Vec::from_iter((0..max_allowed).map(|_id| u16::MAX));

        let expected = true;
        let result = SubtensorModule::max_weight_limited(netuid, uid, &uids, &weights);

        assert_eq!(
            expected, result,
            "Failed get expected result when everything _should_ be fine"
        );
    });
}

/// Check _truthy_ path for max weight limit
#[test]
fn test_max_weight_limited_when_max_weight_is_within_limit() {
    new_test_ext(0).execute_with(|| {
        let max_allowed: u16 = 1;
        let max_weight_limit = u16::MAX / 5;

        let netuid: u16 = 1;
        let uids: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));
        let uid: u16 = uids[0];
        let weights: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| max_weight_limit - id));

        SubtensorModule::set_max_weight_limit(netuid, max_weight_limit);

        let expected = true;
        let result = SubtensorModule::max_weight_limited(netuid, uid, &uids, &weights);

        assert_eq!(
            expected, result,
            "Failed get expected result when everything _should_ be fine"
        );
    });
}

/// Check _falsey_ path
#[test]
fn test_max_weight_limited_when_guard_checks_are_not_triggered() {
    new_test_ext(0).execute_with(|| {
        let max_allowed: u16 = 3;
        let max_weight_limit = u16::MAX / 5;

        let netuid: u16 = 1;
        let uids: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));
        let uid: u16 = uids[0];
        let weights: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| max_weight_limit + id));

        SubtensorModule::set_max_weight_limit(netuid, max_weight_limit);

        let expected = false;
        let result = SubtensorModule::max_weight_limited(netuid, uid, &uids, &weights);

        assert_eq!(
            expected, result,
            "Failed get expected result when guard-checks were not triggered"
        );
    });
}

/// Check _falsey_ path for weights length
#[test]
fn test_is_self_weight_weights_length_not_one() {
    new_test_ext(0).execute_with(|| {
        let max_allowed: u16 = 3;

        let uids: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));
        let uid: u16 = uids[0];
        let weights: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));

        let expected = false;
        let result = SubtensorModule::is_self_weight(uid, &uids, &weights);

        assert_eq!(
            expected, result,
            "Failed get expected result when `weights.len() != 1`"
        );
    });
}

/// Check _falsey_ path for uid vs uids[0]
#[test]
fn test_is_self_weight_uid_not_in_uids() {
    new_test_ext(0).execute_with(|| {
        let max_allowed: u16 = 3;

        let uids: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));
        let uid: u16 = uids[1];
        let weights: Vec<u16> = vec![0];

        let expected = false;
        let result = SubtensorModule::is_self_weight(uid, &uids, &weights);

        assert_eq!(
            expected, result,
            "Failed get expected result when `uid != uids[0]`"
        );
    });
}

/// Check _truthy_ path
/// @TODO: double-check if this really be desired behavior
#[test]
fn test_is_self_weight_uid_in_uids() {
    new_test_ext(0).execute_with(|| {
        let max_allowed: u16 = 1;

        let uids: Vec<u16> = Vec::from_iter((0..max_allowed).map(|id| id + 1));
        let uid: u16 = uids[0];
        let weights: Vec<u16> = vec![0];

        let expected = true;
        let result = SubtensorModule::is_self_weight(uid, &uids, &weights);

        assert_eq!(
            expected, result,
            "Failed get expected result when everything _should_ be fine"
        );
    });
}

/// Check _truthy_ path
#[test]
fn test_check_len_uids_within_allowed_within_network_pool() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;

        let tempo: u16 = 13;
        let modality: u16 = 0;

        let max_registrations_per_block: u16 = 100;

        add_network(netuid, tempo, modality);

        /* @TODO: use a loop maybe */
        register_ok_neuron(netuid, U256::from(1), U256::from(1), 0);
        register_ok_neuron(netuid, U256::from(3), U256::from(3), 65555);
        register_ok_neuron(netuid, U256::from(5), U256::from(5), 75555);
        let max_allowed: u16 = SubtensorModule::get_subnetwork_n(netuid);

        SubtensorModule::set_max_allowed_uids(netuid, max_allowed);
        SubtensorModule::set_max_registrations_per_block(netuid, max_registrations_per_block);

        let uids: Vec<u16> = Vec::from_iter(0..max_allowed);

        let expected = true;
        let result = SubtensorModule::check_len_uids_within_allowed(netuid, &uids);
        assert_eq!(
            expected, result,
            "netuid network length and uids length incompatible"
        );
    });
}

/// Check _falsey_ path
#[test]
fn test_check_len_uids_within_allowed_not_within_network_pool() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;

        let tempo: u16 = 13;
        let modality: u16 = 0;

        let max_registrations_per_block: u16 = 100;

        add_network(netuid, tempo, modality);

        /* @TODO: use a loop maybe */
        register_ok_neuron(netuid, U256::from(1), U256::from(1), 0);
        register_ok_neuron(netuid, U256::from(3), U256::from(3), 65555);
        register_ok_neuron(netuid, U256::from(5), U256::from(5), 75555);
        let max_allowed: u16 = SubtensorModule::get_subnetwork_n(netuid);

        SubtensorModule::set_max_allowed_uids(netuid, max_allowed);
        SubtensorModule::set_max_registrations_per_block(netuid, max_registrations_per_block);

        let uids: Vec<u16> = Vec::from_iter(0..(max_allowed + 1));

        let expected = false;
        let result = SubtensorModule::check_len_uids_within_allowed(netuid, &uids);
        assert_eq!(
            expected, result,
            "Failed to detect incompatible uids for network"
        );
    });
}

#[test]
fn test_set_weights_commit_reveal_enabled_error() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;
        add_network(netuid, 0, 0);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 10);

        let uids = vec![0];
        let weights = vec![1];
        let version_key: u64 = 0;
        let hotkey = U256::from(1);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        assert_err!(
            SubtensorModule::set_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weights.clone(),
                version_key
            ),
            Error::<Test>::CommitRevealEnabled
        );

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, false);

        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids,
            weights,
            version_key
        ));
    });
}

#[test]
fn test_reveal_weights_when_commit_reveal_disabled() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        let commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        System::set_block_number(0);

        let tempo: u16 = 5;
        add_network(netuid, tempo, 0);

        // Register neurons and set up configurations
        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 5);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        // Enable commit-reveal and commit
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));

        step_epochs(1, netuid);

        // Disable commit-reveal before reveal
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, false);

        // Attempt to reveal, should fail with CommitRevealDisabled
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids,
                weight_values,
                salt,
                version_key,
            ),
            Error::<Test>::CommitRevealDisabled
        );
    });
}

#[test]
fn test_commit_reveal_weights_ok() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        let commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        System::set_block_number(0);

        let tempo: u16 = 5;
        add_network(netuid, tempo, 0);

        // Register neurons and set up configurations
        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 5);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        // Commit at block 0
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));

        step_epochs(1, netuid);

        // Reveal in the next epoch
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids,
            weight_values,
            salt,
            version_key,
        ));
    });
}

#[test]
fn test_commit_reveal_tempo_interval() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        let commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        System::set_block_number(0);

        let tempo: u16 = 100;
        add_network(netuid, tempo, 0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 5);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        // Commit at block 0
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));

        // Attempt to reveal in the same epoch, should fail
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ),
            Error::<Test>::RevealTooEarly
        );

        step_epochs(1, netuid);

        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        step_block(6);

        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ),
            Error::<Test>::NoWeightsCommitFound
        );

        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));

        // step two epochs
        step_epochs(2, netuid);

        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ),
            Error::<Test>::ExpiredWeightCommit
        );

        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));

        step_block(50);

        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ),
            Error::<Test>::RevealTooEarly
        );

        step_epochs(1, netuid);

        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids,
            weight_values,
            salt,
            version_key,
        ));
    });
}

#[test]
fn test_commit_reveal_hash() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let bad_salt: Vec<u16> = vec![0, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        add_network(netuid, 5, 0);
        System::set_block_number(0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 5);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        let commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));

        step_epochs(1, netuid);

        // Attempt to reveal with incorrect data, should fail
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                vec![0, 2],
                weight_values.clone(),
                salt.clone(),
                version_key
            ),
            Error::<Test>::InvalidRevealCommitHashNotMatch
        );

        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                bad_salt.clone(),
                version_key,
            ),
            Error::<Test>::InvalidRevealCommitHashNotMatch
        );

        // Correct reveal, should succeed
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids,
            weight_values,
            salt,
            version_key,
        ));
    });
}

#[test]
fn test_commit_reveal_disabled_or_enabled() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        let commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        add_network(netuid, 5, 0);
        System::set_block_number(0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 5);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        // Disable commit/reveal
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, false);

        // Attempt to commit, should fail
        assert_err!(
            SubtensorModule::commit_weights(RuntimeOrigin::signed(hotkey), netuid, commit_hash),
            Error::<Test>::CommitRevealDisabled
        );

        // Enable commit/reveal
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        // Commit should now succeed
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));

        step_epochs(1, netuid);

        // Reveal should succeed
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids,
            weight_values,
            salt,
            version_key,
        ));
    });
}

#[test]
fn test_toggle_commit_reveal_weights_and_set_weights() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        let commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        add_network(netuid, 5, 0);
        System::set_block_number(0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);
        SubtensorModule::set_weights_set_rate_limit(netuid, 5);

        // Enable commit/reveal
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        // Commit at block 0
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));

        step_epochs(1, netuid);

        // Reveal in the next epoch
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        // Disable commit/reveal
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, false);

        // Advance to allow setting weights (due to rate limit)
        step_block(5);

        // Set weights directly
        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids,
            weight_values,
            version_key,
        ));
    });
}

#[test]
fn test_tempo_change_during_commit_reveal_process() {
    new_test_ext(0).execute_with(|| {
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        let commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        System::set_block_number(0);

        let tempo: u16 = 100;
        add_network(netuid, tempo, 0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 5);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));
        log::info!(
            "Commit successful at block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        step_block(9);
        log::info!(
            "Advanced to block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        let tempo_before_next_reveal: u16 = 200;
        log::info!("Changing tempo to {}", tempo_before_next_reveal);
        SubtensorModule::set_tempo(netuid, tempo_before_next_reveal);

        step_epochs(1, netuid);
        log::info!(
            "Advanced to block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));
        log::info!(
            "Revealed at block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));
        log::info!(
            "Commit successful at block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        let tempo: u16 = 150;
        log::info!("Changing tempo to {}", tempo);
        SubtensorModule::set_tempo(netuid, tempo);

        step_epochs(1, netuid);
        log::info!(
            "Advanced to block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));
        log::info!(
            "Revealed at block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        let tempo: u16 = 1050;
        log::info!("Changing tempo to {}", tempo);
        SubtensorModule::set_tempo(netuid, tempo);

        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));
        log::info!(
            "Commit successful at block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        let tempo: u16 = 805;
        log::info!("Changing tempo to {}", tempo);
        SubtensorModule::set_tempo(netuid, tempo);

        step_epochs(1, netuid);
        log::info!(
            "Advanced to block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));
        log::info!(
            "Revealed at block {}",
            SubtensorModule::get_current_block_as_u64()
        );
    });
}

#[test]
fn test_commit_reveal_multiple_commits() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        System::set_block_number(0);

        let tempo: u16 = 7200;
        add_network(netuid, tempo, 0);

        // Setup the network and neurons
        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        // 1. Commit 10 times successfully
        let mut commit_info = Vec::new();
        for i in 0..10 {
            let salt_i: Vec<u16> = vec![i; 8]; // Unique salt for each commit
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt_i.clone(),
                version_key,
            ));
            commit_info.push((commit_hash, salt_i));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                commit_hash
            ));
        }

        // 2. Attempt to commit an 11th time, should fail
        let salt_11: Vec<u16> = vec![11; 8];
        let commit_hash_11: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_11.clone(),
            version_key,
        ));
        assert_err!(
            SubtensorModule::commit_weights(RuntimeOrigin::signed(hotkey), netuid, commit_hash_11),
            Error::<Test>::TooManyUnrevealedCommits
        );

        // 3. Attempt to reveal out of order (reveal the second commit first)
        // Advance to the next epoch for reveals to be valid
        step_epochs(1, netuid);

        // Try to reveal the second commit first
        let (_commit_hash_2, salt_2) = &commit_info[1];
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_2.clone(),
            version_key,
        ));

        // Check that commits before the revealed one are removed
        let remaining_commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey)
            .expect("expected 8 remaining commits");
        assert_eq!(remaining_commits.len(), 8); // 10 commits - 2 removed (index 0 and 1)

        // 4. Reveal the last commit next
        let (_commit_hash_10, salt_10) = &commit_info[9];
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_10.clone(),
            version_key,
        ));

        // Remaining commits should have removed up to index 9
        let remaining_commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey);
        assert!(remaining_commits.is_none()); // All commits removed

        // After revealing all commits, attempt to commit again should now succeed
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash_11
        ));

        // 5. Test expired commits are removed and do not block reveals
        // Commit again and let the commit expire
        let salt_12: Vec<u16> = vec![12; 8];
        let commit_hash_12: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_12.clone(),
            version_key,
        ));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash_12
        ));

        // Advance two epochs so the commit expires
        step_epochs(2, netuid);

        // Attempt to reveal the expired commit, should fail
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt_12.clone(),
                version_key,
            ),
            Error::<Test>::ExpiredWeightCommit
        );

        // Commit again and reveal after advancing to next epoch
        let salt_13: Vec<u16> = vec![13; 8];
        let commit_hash_13: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_13.clone(),
            version_key,
        ));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash_13
        ));

        step_epochs(1, netuid);

        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_13.clone(),
            version_key,
        ));

        // 6. Ensure that attempting to reveal after the valid reveal period fails
        // Commit again
        let salt_14: Vec<u16> = vec![14; 8];
        let commit_hash_14: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_14.clone(),
            version_key,
        ));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash_14
        ));

        // Advance beyond the valid reveal period (more than one epoch)
        step_epochs(2, netuid);

        // Attempt to reveal, should fail
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt_14.clone(),
                version_key,
            ),
            Error::<Test>::ExpiredWeightCommit
        );

        // 7. Attempt to reveal a commit that is not ready yet (before the reveal period)
        // Commit again
        let salt_15: Vec<u16> = vec![15; 8];
        let commit_hash_15: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_15.clone(),
            version_key,
        ));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash_15
        ));

        // Attempt to reveal immediately, should fail
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt_15.clone(),
                version_key,
            ),
            Error::<Test>::RevealTooEarly
        );

        step_epochs(1, netuid);

        // Now reveal should succeed
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_15.clone(),
            version_key,
        ));

        // 8. Test that revealing with incorrect data (salt) fails
        // Commit again
        let salt_16: Vec<u16> = vec![16; 8];
        let commit_hash_16: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_16.clone(),
            version_key,
        ));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash_16
        ));

        step_epochs(1, netuid);

        // Attempt to reveal with incorrect salt
        let wrong_salt: Vec<u16> = vec![99; 8];
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                wrong_salt.clone(),
                version_key,
            ),
            Error::<Test>::InvalidRevealCommitHashNotMatch
        );

        // Reveal with correct data should succeed
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_16.clone(),
            version_key,
        ));

        // 9. Test that attempting to reveal when there are no commits fails
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt_16.clone(),
                version_key,
            ),
            Error::<Test>::NoWeightsCommitFound
        );

        // 10. Commit twice and attempt to reveal out of sequence (which is now allowed)
        let salt_a: Vec<u16> = vec![21; 8];
        let commit_hash_a: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_a.clone(),
            version_key,
        ));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash_a
        ));

        let salt_b: Vec<u16> = vec![22; 8];
        let commit_hash_b: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_b.clone(),
            version_key,
        ));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash_b
        ));

        step_epochs(1, netuid);

        // Reveal the second commit first, should now succeed
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_b.clone(),
            version_key,
        ));

        // Check that the first commit has been removed
        let remaining_commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey);
        assert!(remaining_commits.is_none());

        // Attempting to reveal the first commit should fail as it was removed
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids,
                weight_values,
                salt_a,
                version_key,
            ),
            Error::<Test>::NoWeightsCommitFound
        );
    });
}

fn commit_reveal_set_weights(
    hotkey: U256,
    netuid: u16,
    uids: Vec<u16>,
    weights: Vec<u16>,
    salt: Vec<u16>,
    version_key: u64,
) -> DispatchResult {
    SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

    let commit_hash: H256 = BlakeTwo256::hash_of(&(
        hotkey,
        netuid,
        uids.clone(),
        weights.clone(),
        salt.clone(),
        version_key,
    ));

    SubtensorModule::commit_weights(RuntimeOrigin::signed(hotkey), netuid, commit_hash)?;

    step_epochs(1, netuid);

    SubtensorModule::reveal_weights(
        RuntimeOrigin::signed(hotkey),
        netuid,
        uids,
        weights,
        salt,
        version_key,
    )?;

    Ok(())
}

#[test]
fn test_expired_commits_handling_in_commit_and_reveal() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey: <Test as frame_system::Config>::AccountId = U256::from(1);
        let version_key: u64 = 0;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        // Register neurons
        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        // 1. Commit 5 times in epoch 0
        let mut commit_info = Vec::new();
        for i in 0..5 {
            let salt: Vec<u16> = vec![i; 8];
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ));
            commit_info.push((commit_hash, salt));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                commit_hash
            ));
        }

        // Advance to epoch 1
        step_epochs(1, netuid);

        // 2. Commit another 5 times in epoch 1
        for i in 5..10 {
            let salt: Vec<u16> = vec![i; 8];
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ));
            commit_info.push((commit_hash, salt));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                commit_hash
            ));
        }

        // 3. Attempt to commit an 11th time, should fail with TooManyUnrevealedCommits
        let salt_11: Vec<u16> = vec![11; 8];
        let commit_hash_11: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_11.clone(),
            version_key,
        ));
        assert_err!(
            SubtensorModule::commit_weights(RuntimeOrigin::signed(hotkey), netuid, commit_hash_11),
            Error::<Test>::TooManyUnrevealedCommits
        );

        // 4. Advance to epoch 2 to expire the commits from epoch 0
        step_epochs(1, netuid); // Now at epoch 2

        // 5. Attempt to commit again; should succeed after expired commits are removed
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash_11
        ));

        // 6. Verify that the number of unrevealed, non-expired commits is now 6
        let commits: VecDeque<(H256, u64, u64, u64)> =
            pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey)
                .expect("Expected a commit");
        assert_eq!(commits.len(), 6); // 5 non-expired commits from epoch 1 + new commit

        // 7. Attempt to reveal an expired commit (from epoch 0)
        // Previous commit removed expired commits
        let (_, expired_salt) = &commit_info[0];
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                expired_salt.clone(),
                version_key,
            ),
            Error::<Test>::InvalidRevealCommitHashNotMatch
        );

        // 8. Reveal commits from epoch 1 at current_epoch = 2
        for (_, salt) in commit_info.iter().skip(5).take(5) {
            let salt = salt.clone();

            assert_ok!(SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ));
        }

        // 9. Advance to epoch 3 to reveal the new commit
        step_epochs(1, netuid);

        // 10. Reveal the new commit from epoch 2
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_11.clone(),
            version_key,
        ));

        // 10. Verify that all commits have been revealed and the queue is empty
        let commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey);
        assert!(commits.is_none());

        // 11. Attempt to reveal again, should fail with NoWeightsCommitFound
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt_11.clone(),
                version_key,
            ),
            Error::<Test>::NoWeightsCommitFound
        );

        // 12. Commit again to ensure we can continue after previous commits
        let salt_12: Vec<u16> = vec![12; 8];
        let commit_hash_12: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_12.clone(),
            version_key,
        ));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash_12
        ));

        // Advance to next epoch (epoch 4) and reveal
        step_epochs(1, netuid);

        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids,
            weight_values,
            salt_12,
            version_key,
        ));
    });
}

#[test]
fn test_reveal_at_exact_epoch() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey: <Test as frame_system::Config>::AccountId = U256::from(1);
        let version_key: u64 = 0;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        let reveal_periods: Vec<u64> = vec![0, 1, 2, 7, 40, 86, 100];

        for &reveal_period in &reveal_periods {
            SubtensorModule::set_reveal_period(netuid, reveal_period);

            let salt: Vec<u16> = vec![42; 8];
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                commit_hash
            ));

            // Retrieve commit information
            let commit_block = SubtensorModule::get_current_block_as_u64();
            let commit_epoch = SubtensorModule::get_epoch_index(netuid, commit_block);
            let reveal_epoch = commit_epoch.saturating_add(reveal_period);

            // Attempt to reveal before the allowed epoch
            if reveal_period > 0 {
                // Advance to epoch before the reveal epoch
                if reveal_period >= 1 {
                    step_epochs((reveal_period - 1) as u16, netuid);
                }

                // Attempt to reveal too early
                assert_err!(
                    SubtensorModule::reveal_weights(
                        RuntimeOrigin::signed(hotkey),
                        netuid,
                        uids.clone(),
                        weight_values.clone(),
                        salt.clone(),
                        version_key,
                    ),
                    Error::<Test>::RevealTooEarly
                );
            }

            // Advance to the exact reveal epoch
            let current_epoch = SubtensorModule::get_epoch_index(
                netuid,
                SubtensorModule::get_current_block_as_u64(),
            );
            if current_epoch < reveal_epoch {
                step_epochs((reveal_epoch - current_epoch) as u16, netuid);
            }

            // Reveal at the exact allowed epoch
            assert_ok!(SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ));

            assert_err!(
                SubtensorModule::reveal_weights(
                    RuntimeOrigin::signed(hotkey),
                    netuid,
                    uids.clone(),
                    weight_values.clone(),
                    salt.clone(),
                    version_key,
                ),
                Error::<Test>::NoWeightsCommitFound
            );

            let new_salt: Vec<u16> = vec![43; 8];
            let new_commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids.clone(),
                weight_values.clone(),
                new_salt.clone(),
                version_key,
            ));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                new_commit_hash
            ));

            // Advance past the reveal epoch to ensure commit expiration
            step_epochs((reveal_period + 1) as u16, netuid);

            // Attempt to reveal after the allowed epoch
            assert_err!(
                SubtensorModule::reveal_weights(
                    RuntimeOrigin::signed(hotkey),
                    netuid,
                    uids.clone(),
                    weight_values.clone(),
                    new_salt.clone(),
                    version_key,
                ),
                Error::<Test>::ExpiredWeightCommit
            );

            pallet_subtensor::WeightCommits::<Test>::remove(netuid, hotkey);
        }
    });
}

#[test]
fn test_tempo_and_reveal_period_change_during_commit_reveal_process() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let salt: Vec<u16> = vec![42; 8];
        let version_key: u64 = 0;
        let hotkey: <Test as frame_system::Config>::AccountId = U256::from(1);

        // Compute initial commit hash
        let commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        System::set_block_number(0);

        let initial_tempo: u16 = 100;
        let initial_reveal_period: u64 = 1;
        add_network(netuid, initial_tempo, 0);
        SubtensorModule::set_reveal_period(netuid, initial_reveal_period);
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        // Step 1: Commit weights
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));
        log::info!(
            "Commit successful at block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        // Retrieve commit block and epoch
        let commit_block = SubtensorModule::get_current_block_as_u64();
        let commit_epoch = SubtensorModule::get_epoch_index(netuid, commit_block);

        // Step 2: Change tempo and reveal period after commit
        let new_tempo: u16 = 50;
        let new_reveal_period: u64 = 2;
        SubtensorModule::set_tempo(netuid, new_tempo);
        SubtensorModule::set_reveal_period(netuid, new_reveal_period);
        log::info!(
            "Changed tempo to {} and reveal period to {}",
            new_tempo,
            new_reveal_period
        );

        // Step 3: Advance blocks to reach the reveal epoch according to new tempo and reveal period
        let current_block = SubtensorModule::get_current_block_as_u64();
        let current_epoch = SubtensorModule::get_epoch_index(netuid, current_block);
        let reveal_epoch = commit_epoch.saturating_add(new_reveal_period);

        // Advance to one epoch before reveal epoch
        if current_epoch < reveal_epoch {
            let epochs_to_advance = reveal_epoch - current_epoch - 1;
            step_epochs(epochs_to_advance as u16, netuid);
        }

        // Attempt to reveal too early
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key
            ),
            Error::<Test>::RevealTooEarly
        );
        log::info!(
            "Attempted to reveal too early at block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        // Advance to reveal epoch
        step_epochs(1, netuid);

        // Attempt to reveal at the correct epoch
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key
        ));
        log::info!(
            "Revealed weights at block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        // Step 4: Change tempo and reveal period again after reveal
        let new_tempo_after_reveal: u16 = 200;
        let new_reveal_period_after_reveal: u64 = 1;
        SubtensorModule::set_tempo(netuid, new_tempo_after_reveal);
        SubtensorModule::set_reveal_period(netuid, new_reveal_period_after_reveal);
        log::info!(
            "Changed tempo to {} and reveal period to {} after reveal",
            new_tempo_after_reveal,
            new_reveal_period_after_reveal
        );

        // Step 5: Commit again
        let new_salt: Vec<u16> = vec![43; 8];
        let new_commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            new_salt.clone(),
            version_key,
        ));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            new_commit_hash
        ));
        log::info!(
            "Commit successful at block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        // Retrieve new commit block and epoch
        let new_commit_block = SubtensorModule::get_current_block_as_u64();
        let new_commit_epoch = SubtensorModule::get_epoch_index(netuid, new_commit_block);
        let new_reveal_epoch = new_commit_epoch.saturating_add(new_reveal_period_after_reveal);

        // Advance to reveal epoch
        let current_block = SubtensorModule::get_current_block_as_u64();
        let current_epoch = SubtensorModule::get_epoch_index(netuid, current_block);
        if current_epoch < new_reveal_epoch {
            let epochs_to_advance = new_reveal_epoch - current_epoch;
            step_epochs(epochs_to_advance as u16, netuid);
        }

        // Attempt to reveal at the correct epoch
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            new_salt.clone(),
            version_key
        ));
        log::info!(
            "Revealed weights at block {}",
            SubtensorModule::get_current_block_as_u64()
        );

        // Step 6: Attempt to reveal after the allowed epoch (commit expires)
        // Advance past the reveal epoch
        let expiration_epochs = 1;
        step_epochs(expiration_epochs as u16, netuid);

        // Attempt to reveal again (should fail due to expired commit)
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                new_salt.clone(),
                version_key
            ),
            Error::<Test>::NoWeightsCommitFound
        );
        log::info!(
            "Attempted to reveal after expiration at block {}",
            SubtensorModule::get_current_block_as_u64()
        );
    });
}

#[test]
fn test_commit_reveal_order_enforcement() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey: <Test as frame_system::Config>::AccountId = U256::from(1);
        let version_key: u64 = 0;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        // Commit three times: A, B, C
        let mut commit_info = Vec::new();
        for i in 0..3 {
            let salt: Vec<u16> = vec![i; 8];
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ));
            commit_info.push((commit_hash, salt));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                commit_hash
            ));
        }

        step_epochs(1, netuid);

        // Attempt to reveal B first (index 1), should now succeed
        let (_commit_hash_b, salt_b) = &commit_info[1];
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_b.clone(),
            version_key,
        ));

        // Check that commits A and B are removed
        let remaining_commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey)
            .expect("expected 1 remaining commit");
        assert_eq!(remaining_commits.len(), 1); // Only commit C should remain

        // Attempt to reveal C (index 2), should succeed
        let (_commit_hash_c, salt_c) = &commit_info[2];
        assert_ok!(SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt_c.clone(),
            version_key,
        ));

        // Attempting to reveal A (index 0) should fail as it's been removed
        let (_commit_hash_a, salt_a) = &commit_info[0];
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids,
                weight_values,
                salt_a.clone(),
                version_key,
            ),
            Error::<Test>::NoWeightsCommitFound
        );
    });
}

#[test]
fn test_reveal_at_exact_block() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey: <Test as frame_system::Config>::AccountId = U256::from(1);
        let version_key: u64 = 0;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let tempo: u16 = 360;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        let reveal_periods: Vec<u64> = vec![
            0,
            1,
            2,
            5,
            19,
            21,
            30,
            77,
            104,
            833,
            1999,
            36398,
            u32::MAX as u64,
        ];

        for &reveal_period in &reveal_periods {
            SubtensorModule::set_reveal_period(netuid, reveal_period);

            // Step 1: Commit weights
            let salt: Vec<u16> = vec![42 + (reveal_period % 100) as u16; 8];
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key,
            ));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                commit_hash
            ));

            let commit_block = SubtensorModule::get_current_block_as_u64();
            let commit_epoch = SubtensorModule::get_epoch_index(netuid, commit_block);
            let reveal_epoch = commit_epoch.saturating_add(reveal_period);

            // Calculate the block number where the reveal epoch starts
            let tempo_plus_one = (tempo as u64).saturating_add(1);
            let netuid_plus_one = (netuid as u64).saturating_add(1);
            let reveal_epoch_start_block = reveal_epoch
                .saturating_mul(tempo_plus_one)
                .saturating_sub(netuid_plus_one);

            // Attempt to reveal before the reveal epoch starts
            let current_block = SubtensorModule::get_current_block_as_u64();
            if current_block < reveal_epoch_start_block {
                // Advance to one block before the reveal epoch starts
                let blocks_to_advance = reveal_epoch_start_block.saturating_sub(current_block);
                if blocks_to_advance > 1 {
                    // Advance to one block before the reveal epoch
                    let new_block_number = current_block + blocks_to_advance - 1;
                    System::set_block_number(new_block_number);
                }

                // Attempt to reveal too early
                assert_err!(
                    SubtensorModule::reveal_weights(
                        RuntimeOrigin::signed(hotkey),
                        netuid,
                        uids.clone(),
                        weight_values.clone(),
                        salt.clone(),
                        version_key
                    ),
                    Error::<Test>::RevealTooEarly
                );

                // Advance one more block to reach the exact reveal epoch start block
                System::set_block_number(reveal_epoch_start_block);
            } else {
                // If we're already at or past the reveal epoch start block
                System::set_block_number(reveal_epoch_start_block);
            }

            // Reveal at the exact allowed block
            assert_ok!(SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                uids.clone(),
                weight_values.clone(),
                salt.clone(),
                version_key
            ));

            // Attempt to reveal again; should fail with NoWeightsCommitFound
            assert_err!(
                SubtensorModule::reveal_weights(
                    RuntimeOrigin::signed(hotkey),
                    netuid,
                    uids.clone(),
                    weight_values.clone(),
                    salt.clone(),
                    version_key
                ),
                Error::<Test>::NoWeightsCommitFound
            );

            // Commit again with new salt
            let new_salt: Vec<u16> = vec![43 + (reveal_period % 100) as u16; 8];
            let new_commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids.clone(),
                weight_values.clone(),
                new_salt.clone(),
                version_key,
            ));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                new_commit_hash
            ));

            // Advance blocks to after the commit expires
            let commit_block = SubtensorModule::get_current_block_as_u64();
            let commit_epoch = SubtensorModule::get_epoch_index(netuid, commit_block);
            let reveal_epoch = commit_epoch.saturating_add(reveal_period);
            let expiration_epoch = reveal_epoch.saturating_add(1);
            let expiration_epoch_start_block = expiration_epoch
                .saturating_mul(tempo_plus_one)
                .saturating_sub(netuid_plus_one);

            let current_block = SubtensorModule::get_current_block_as_u64();
            if current_block < expiration_epoch_start_block {
                // Advance to the block where the commit expires
                System::set_block_number(expiration_epoch_start_block);
            }

            // Attempt to reveal after the commit has expired
            assert_err!(
                SubtensorModule::reveal_weights(
                    RuntimeOrigin::signed(hotkey),
                    netuid,
                    uids.clone(),
                    weight_values.clone(),
                    new_salt.clone(),
                    version_key
                ),
                Error::<Test>::ExpiredWeightCommit
            );

            // Clean up for next iteration
            pallet_subtensor::WeightCommits::<Test>::remove(netuid, hotkey);
        }
    });
}

#[test]
fn test_successful_batch_reveal() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey = U256::from(1);
        let version_keys: Vec<u64> = vec![0, 0, 0];
        let uids_list: Vec<Vec<u16>> = vec![vec![0, 1], vec![1, 0], vec![0, 1]];
        let weight_values_list: Vec<Vec<u16>> = vec![vec![10, 20], vec![30, 40], vec![50, 60]];
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, hotkey, U256::from(2), 100_000);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        // 1. Commit multiple times
        let mut commit_info = Vec::new();
        for i in 0..3 {
            let salt: Vec<u16> = vec![i as u16; 8];
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids_list[i].clone(),
                weight_values_list[i].clone(),
                salt.clone(),
                version_keys[i],
            ));
            commit_info.push((commit_hash, salt));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                commit_hash
            ));
        }

        step_epochs(1, netuid);

        // 2. Prepare batch reveal parameters
        let salts_list: Vec<Vec<u16>> = commit_info.iter().map(|(_, salt)| salt.clone()).collect();

        // 3. Perform batch reveal
        assert_ok!(SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list.clone(),
            weight_values_list.clone(),
            salts_list.clone(),
            version_keys.clone(),
        ));

        // 4. Ensure all commits are removed
        let commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey);
        assert!(commits.is_none());
    });
}

#[test]
fn test_batch_reveal_with_expired_commits() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey = U256::from(1);
        let version_keys: Vec<u64> = vec![0, 0, 0];
        let uids_list: Vec<Vec<u16>> = vec![vec![0, 1], vec![1, 0], vec![0, 1]];
        let weight_values_list: Vec<Vec<u16>> = vec![vec![10, 20], vec![30, 40], vec![50, 60]];
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, hotkey, U256::from(2), 100_000);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        let mut commit_info = Vec::new();

        // 1. Commit the first weight in epoch 0
        let salt0: Vec<u16> = vec![0u16; 8];
        let commit_hash0: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids_list[0].clone(),
            weight_values_list[0].clone(),
            salt0.clone(),
            version_keys[0],
        ));
        commit_info.push((commit_hash0, salt0));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash0
        ));

        // Advance to epoch 1
        step_epochs(1, netuid);

        // 2. Commit the next two weights in epoch 1
        for i in 1..3 {
            let salt: Vec<u16> = vec![i as u16; 8];
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids_list[i].clone(),
                weight_values_list[i].clone(),
                salt.clone(),
                version_keys[i],
            ));
            commit_info.push((commit_hash, salt));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                commit_hash
            ));
        }

        // Advance to epoch 2 (after reveal period for first commit)
        step_epochs(1, netuid);

        // 3. Prepare batch reveal parameters
        let salts_list: Vec<Vec<u16>> = commit_info.iter().map(|(_, salt)| salt.clone()).collect();

        // 4. Perform batch reveal
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list.clone(),
            weight_values_list.clone(),
            salts_list.clone(),
            version_keys.clone(),
        );
        assert_err!(result, Error::<Test>::ExpiredWeightCommit);

        // 5. Expired commit is not removed until a successful call
        let commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey)
            .expect("Expected remaining commits");
        assert_eq!(commits.len(), 3);

        // 6. Try revealing the remaining commits
        let valid_uids_list = uids_list[1..].to_vec();
        let valid_weight_values_list = weight_values_list[1..].to_vec();
        let valid_salts_list = salts_list[1..].to_vec();
        let valid_version_keys = version_keys[1..].to_vec();

        assert_ok!(SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            valid_uids_list,
            valid_weight_values_list,
            valid_salts_list,
            valid_version_keys,
        ));

        // 7. Ensure all commits are removed
        let commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey);
        assert!(commits.is_none());
    });
}

#[test]
fn test_batch_reveal_with_invalid_input_lengths() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey = U256::from(1);
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        // Base data for valid inputs
        let uids_list: Vec<Vec<u16>> = vec![vec![0, 1], vec![1, 0]];
        let weight_values_list: Vec<Vec<u16>> = vec![vec![10, 20], vec![30, 40]];
        let salts_list: Vec<Vec<u16>> = vec![vec![0u16; 8], vec![1u16; 8]];
        let version_keys: Vec<u64> = vec![0, 0];

        // Test cases with mismatched input lengths

        // Case 1: uids_list has an extra element
        let uids_list_case = vec![vec![0, 1], vec![1, 0], vec![2, 3]];
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list_case.clone(),
            weight_values_list.clone(),
            salts_list.clone(),
            version_keys.clone(),
        );
        assert_err!(result, Error::<Test>::InputLengthsUnequal);

        // Case 2: weight_values_list has an extra element
        let weight_values_list_case = vec![vec![10, 20], vec![30, 40], vec![50, 60]];
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list.clone(),
            weight_values_list_case.clone(),
            salts_list.clone(),
            version_keys.clone(),
        );
        assert_err!(result, Error::<Test>::InputLengthsUnequal);

        // Case 3: salts_list has an extra element
        let salts_list_case = vec![vec![0u16; 8], vec![1u16; 8], vec![2u16; 8]];
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list.clone(),
            weight_values_list.clone(),
            salts_list_case.clone(),
            version_keys.clone(),
        );
        assert_err!(result, Error::<Test>::InputLengthsUnequal);

        // Case 4: version_keys has an extra element
        let version_keys_case = vec![0, 0, 0];
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list.clone(),
            weight_values_list.clone(),
            salts_list.clone(),
            version_keys_case.clone(),
        );
        assert_err!(result, Error::<Test>::InputLengthsUnequal);

        // Case 5: All input vectors have mismatched lengths
        let uids_list_case = vec![vec![0, 1]];
        let weight_values_list_case = vec![vec![10, 20], vec![30, 40]];
        let salts_list_case = vec![vec![0u16; 8]];
        let version_keys_case = vec![0, 0, 0];
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list_case,
            weight_values_list_case,
            salts_list_case,
            version_keys_case,
        );
        assert_err!(result, Error::<Test>::InputLengthsUnequal);

        // Case 6: Valid input lengths (should not return an error)
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list.clone(),
            weight_values_list.clone(),
            salts_list.clone(),
            version_keys.clone(),
        );
        // We expect an error because no commits have been made, but it should not be InputLengthsUnequal
        assert_err!(result, Error::<Test>::NoWeightsCommitFound);
    });
}

#[test]
fn test_batch_reveal_with_no_commits() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey = U256::from(1);
        let version_keys: Vec<u64> = vec![0];
        let uids_list: Vec<Vec<u16>> = vec![vec![0, 1]];
        let weight_values_list: Vec<Vec<u16>> = vec![vec![10, 20]];
        let salts_list: Vec<Vec<u16>> = vec![vec![0u16; 8]];
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        // 1. Attempt to perform batch reveal without any commits
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list,
            weight_values_list,
            salts_list,
            version_keys,
        );
        assert_err!(result, Error::<Test>::NoWeightsCommitFound);
    });
}

#[test]
fn test_batch_reveal_before_reveal_period() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey = U256::from(1);
        let version_keys: Vec<u64> = vec![0, 0];
        let uids_list: Vec<Vec<u16>> = vec![vec![0, 1], vec![1, 0]];
        let weight_values_list: Vec<Vec<u16>> = vec![vec![10, 20], vec![30, 40]];
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, hotkey, U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        // 1. Commit multiple times in the same epoch
        let mut commit_info = Vec::new();
        for i in 0..2 {
            let salt: Vec<u16> = vec![i as u16; 8];
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids_list[i].clone(),
                weight_values_list[i].clone(),
                salt.clone(),
                version_keys[i],
            ));
            commit_info.push((commit_hash, salt));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                commit_hash
            ));
        }

        // 2. Prepare batch reveal parameters
        let salts_list: Vec<Vec<u16>> = commit_info.iter().map(|(_, salt)| salt.clone()).collect();

        // 3. Attempt to reveal before reveal period
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list.clone(),
            weight_values_list.clone(),
            salts_list.clone(),
            version_keys.clone(),
        );
        assert_err!(result, Error::<Test>::RevealTooEarly);
    });
}

#[test]
fn test_batch_reveal_after_commits_expired() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey = U256::from(1);
        let version_keys: Vec<u64> = vec![0, 0];
        let uids_list: Vec<Vec<u16>> = vec![vec![0, 1], vec![1, 0]];
        let weight_values_list: Vec<Vec<u16>> = vec![vec![10, 20], vec![30, 40]];
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, hotkey, U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        let mut commit_info = Vec::new();

        // 1. Commit the first weight in epoch 0
        let salt0: Vec<u16> = vec![0u16; 8];
        let commit_hash0: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids_list[0].clone(),
            weight_values_list[0].clone(),
            salt0.clone(),
            version_keys[0],
        ));
        commit_info.push((commit_hash0, salt0));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash0
        ));

        // Advance to epoch 1
        step_epochs(1, netuid);

        // 2. Commit the second weight in epoch 1
        let salt1: Vec<u16> = vec![1u16; 8];
        let commit_hash1: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids_list[1].clone(),
            weight_values_list[1].clone(),
            salt1.clone(),
            version_keys[1],
        ));
        commit_info.push((commit_hash1, salt1));
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash1
        ));

        // Advance to epoch 4 to ensure both commits have expired (assuming reveal_period is 1)
        step_epochs(3, netuid);

        // 3. Prepare batch reveal parameters
        let salts_list: Vec<Vec<u16>> = commit_info.iter().map(|(_, salt)| salt.clone()).collect();

        // 4. Attempt to reveal after commits have expired
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list.clone(),
            weight_values_list.clone(),
            salts_list,
            version_keys.clone(),
        );
        assert_err!(result, Error::<Test>::ExpiredWeightCommit);
    });
}

#[test]
fn test_batch_reveal_when_commit_reveal_disabled() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey = U256::from(1);
        let version_keys: Vec<u64> = vec![0];
        let uids_list: Vec<Vec<u16>> = vec![vec![0, 1]];
        let weight_values_list: Vec<Vec<u16>> = vec![vec![10, 20]];
        let salts_list: Vec<Vec<u16>> = vec![vec![0u16; 8]];
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, false);

        // 1. Attempt to perform batch reveal when commit-reveal is disabled
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list,
            weight_values_list,
            salts_list,
            version_keys,
        );
        assert_err!(result, Error::<Test>::CommitRevealDisabled);
    });
}

#[test]
fn test_batch_reveal_with_out_of_order_commits() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let hotkey = U256::from(1);
        let version_keys: Vec<u64> = vec![0, 0, 0];
        let uids_list: Vec<Vec<u16>> = vec![vec![0, 1], vec![1, 0], vec![0, 1]];
        let weight_values_list: Vec<Vec<u16>> = vec![vec![10, 20], vec![30, 40], vec![50, 60]];
        let tempo: u16 = 100;

        System::set_block_number(0);
        add_network(netuid, tempo, 0);

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, hotkey, U256::from(2), 100_000);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);

        // 1. Commit multiple times (A, B, C)
        let mut commit_info = Vec::new();
        for i in 0..3 {
            let salt: Vec<u16> = vec![i as u16; 8];
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                hotkey,
                netuid,
                uids_list[i].clone(),
                weight_values_list[i].clone(),
                salt.clone(),
                version_keys[i],
            ));
            commit_info.push((commit_hash, salt));
            assert_ok!(SubtensorModule::commit_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                commit_hash
            ));
        }

        step_epochs(1, netuid);

        // 2. Prepare batch reveal parameters for commits A and C (out of order)
        let salts_list: Vec<Vec<u16>> = vec![
            commit_info[2].1.clone(), // Third commit (C)
            commit_info[0].1.clone(), // First commit (A)
        ];
        let uids_list_out_of_order = vec![
            uids_list[2].clone(), // C
            uids_list[0].clone(), // A
        ];
        let weight_values_list_out_of_order = vec![
            weight_values_list[2].clone(), // C
            weight_values_list[0].clone(), // A
        ];
        let version_keys_out_of_order = vec![
            version_keys[2], // C
            version_keys[0], // A
        ];

        // 3. Attempt batch reveal of A and C out of order
        let result = SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids_list_out_of_order,
            weight_values_list_out_of_order,
            salts_list,
            version_keys_out_of_order,
        );

        // 4. Ensure the batch reveal succeeds
        assert_ok!(result);

        // 5. Prepare and reveal the remaining commit (B)
        let remaining_salt = commit_info[1].1.clone();
        let remaining_uids = uids_list[1].clone();
        let remaining_weights = weight_values_list[1].clone();
        let remaining_version_key = version_keys[1];

        assert_ok!(SubtensorModule::do_batch_reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            vec![remaining_uids],
            vec![remaining_weights],
            vec![remaining_salt],
            vec![remaining_version_key],
        ));

        // 6. Ensure all commits are removed
        let commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey);
        assert!(commits.is_none());
    });
}

#[test]
fn test_highly_concurrent_commits_and_reveals_with_multiple_hotkeys() {
    new_test_ext(1).execute_with(|| {
        // ==== Test Configuration ====
        let netuid: u16 = 1;
        let num_hotkeys: usize = 10;
        let max_unrevealed_commits: usize = 10;
        let commits_per_hotkey: usize = 20;
        let initial_reveal_period: u64 = 5;
        let initial_tempo: u16 = 100;

        // ==== Setup Network ====
        add_network(netuid, initial_tempo, 0);
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);
        SubtensorModule::set_weights_set_rate_limit(netuid, 0);
        SubtensorModule::set_reveal_period(netuid, initial_reveal_period);
        SubtensorModule::set_max_registrations_per_block(netuid, u16::MAX);
        SubtensorModule::set_target_registrations_per_interval(netuid, u16::MAX);

        // ==== Register Validators ====
        for uid in 0..5 {
            let validator_id = U256::from(100 + uid as u64);
            register_ok_neuron(netuid, validator_id, U256::from(200 + uid as u64), 300_000);
            SubtensorModule::set_validator_permit_for_uid(netuid, uid, true);
        }

        // ==== Register Hotkeys ====
        let mut hotkeys: Vec<<Test as frame_system::Config>::AccountId> = Vec::new();
        for i in 0..num_hotkeys {
            let hotkey_id = U256::from(1000 + i as u64);
            register_ok_neuron(netuid, hotkey_id, U256::from(2000 + i as u64), 100_000);
            hotkeys.push(hotkey_id);
        }

        // ==== Initialize Commit Information ====
        let mut commit_info_map: HashMap<
            <Test as frame_system::Config>::AccountId,
            Vec<(H256, Vec<u16>, Vec<u16>, Vec<u16>, u64)>,
        > = HashMap::new();

        // Initialize the map
        for hotkey in &hotkeys {
            commit_info_map.insert(*hotkey, Vec::new());
        }

        // ==== Function to Generate Unique Data ====
        fn generate_unique_data(index: usize) -> (Vec<u16>, Vec<u16>, Vec<u16>, u64) {
            let uids = vec![index as u16, (index + 1) as u16];
            let values = vec![(index * 10) as u16, ((index + 1) * 10) as u16];
            let salt = vec![(index % 100) as u16; 8];
            let version_key = index as u64;
            (uids, values, salt, version_key)
        }

        // ==== Simulate Concurrent Commits and Reveals ====
        for i in 0..commits_per_hotkey {
            for hotkey in &hotkeys {

                let current_commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey)
                    .unwrap_or_default();
                if current_commits.len() >= max_unrevealed_commits {
                    continue;
                }

                let (uids, values, salt, version_key) = generate_unique_data(i);
                let commit_hash: H256 = BlakeTwo256::hash_of(&(
                    *hotkey,
                    netuid,
                    uids.clone(),
                    values.clone(),
                    salt.clone(),
                    version_key,
                ));

                if let Some(commits) = commit_info_map.get_mut(hotkey) {
                    commits.push((commit_hash, salt.clone(), uids.clone(), values.clone(), version_key));
                }

                assert_ok!(SubtensorModule::commit_weights(
                    RuntimeOrigin::signed(*hotkey),
                    netuid,
                    commit_hash
                ));
            }

            // ==== Reveal Phase ====
            for hotkey in &hotkeys {
                if let Some(commits) = commit_info_map.get_mut(hotkey) {
                    if commits.is_empty() {
                        continue; // No commits to reveal
                    }

                    let (_commit_hash, salt, uids, values, version_key) = commits.first().expect("expected a value");

                    let reveal_result = SubtensorModule::reveal_weights(
                        RuntimeOrigin::signed(*hotkey),
                        netuid,
                        uids.clone(),
                        values.clone(),
                        salt.clone(),
                        *version_key,
                    );

                    match reveal_result {
                        Ok(_) => {
                            commits.remove(0);
                        }
                        Err(e) => {
                            if e == Error::<Test>::RevealTooEarly.into()
                                || e == Error::<Test>::ExpiredWeightCommit.into()
                                || e == Error::<Test>::InvalidRevealCommitHashNotMatch.into()
                            {
                                log::info!("Expected error during reveal after epoch advancement: {:?}", e);
                            } else {
                                panic!(
                                    "Unexpected error during reveal: {:?}, expected RevealTooEarly, ExpiredWeightCommit, or InvalidRevealCommitHashNotMatch",
                                    e
                                );
                            }
                        }
                    }
                }
            }
        }

        // ==== Modify Network Parameters During Commits ====
        SubtensorModule::set_tempo(netuid, 150);
        SubtensorModule::set_reveal_period(netuid, 7);
        log::info!("Changed tempo to 150 and reveal_period to 7 during commits.");

        step_epochs(3, netuid);

        // ==== Continue Reveals After Epoch Advancement ====
        for hotkey in &hotkeys {
            if let Some(commits) = commit_info_map.get_mut(hotkey) {
                while !commits.is_empty() {
                    let (_commit_hash, salt, uids, values, version_key) = &commits[0];

                    // Attempt to reveal
                    let reveal_result = SubtensorModule::reveal_weights(
                        RuntimeOrigin::signed(*hotkey),
                        netuid,
                        uids.clone(),
                        values.clone(),
                        salt.clone(),
                        *version_key,
                    );

                    match reveal_result {
                        Ok(_) => {
                            commits.remove(0);
                        }
                        Err(e) => {
                            // Check if the error is due to reveal being too early or commit expired
                            if e == Error::<Test>::RevealTooEarly.into()
                                || e == Error::<Test>::ExpiredWeightCommit.into()
                                || e == Error::<Test>::InvalidRevealCommitHashNotMatch.into()
                            {
                                log::info!("Expected error during reveal after epoch advancement: {:?}", e);
                                break;
                            } else {
                                panic!(
                                    "Unexpected error during reveal after epoch advancement: {:?}, expected RevealTooEarly, ExpiredWeightCommit, or InvalidRevealCommitHashNotMatch",
                                    e
                                );
                            }
                        }
                    }
                }
            }
        }

        // ==== Change Network Parameters Again ====
        SubtensorModule::set_tempo(netuid, 200);
        SubtensorModule::set_reveal_period(netuid, 10);
        log::info!("Changed tempo to 200 and reveal_period to 10 after initial reveals.");

        step_epochs(10, netuid);

        // ==== Final Reveal Attempts ====
        for (hotkey, commits) in commit_info_map.iter_mut() {
            for (_commit_hash, salt, uids, values, version_key) in commits.iter() {
                let reveal_result = SubtensorModule::reveal_weights(
                    RuntimeOrigin::signed(*hotkey),
                    netuid,
                    uids.clone(),
                    values.clone(),
                    salt.clone(),
                    *version_key,
                );

                assert_eq!(
                    reveal_result,
                    Err(Error::<Test>::ExpiredWeightCommit.into()),
                    "Expected ExpiredWeightCommit error, got {:?}",
                    reveal_result
                );
            }
        }

        for hotkey in &hotkeys {
            commit_info_map.insert(*hotkey, Vec::new());

            for i in 0..max_unrevealed_commits {
                let (uids, values, salt, version_key) = generate_unique_data(i + commits_per_hotkey);
                let commit_hash: H256 = BlakeTwo256::hash_of(&(
                    *hotkey,
                    netuid,
                    uids.clone(),
                    values.clone(),
                    salt.clone(),
                    version_key,
                ));

                assert_ok!(SubtensorModule::commit_weights(
                    RuntimeOrigin::signed(*hotkey),
                    netuid,
                    commit_hash
                ));
            }

            let (uids, values, salt, version_key) = generate_unique_data(max_unrevealed_commits + commits_per_hotkey);
            let commit_hash: H256 = BlakeTwo256::hash_of(&(
                *hotkey,
                netuid,
                uids.clone(),
                values.clone(),
                salt.clone(),
                version_key,
            ));

            assert_err!(
                SubtensorModule::commit_weights(
                    RuntimeOrigin::signed(*hotkey),
                    netuid,
                    commit_hash
                ),
                Error::<Test>::TooManyUnrevealedCommits
            );
        }

        // Attempt unauthorized reveal
        let unauthorized_hotkey = hotkeys[0];
        let target_hotkey = hotkeys[1];
        if let Some(commits) = commit_info_map.get(&target_hotkey) {
            if let Some((_commit_hash, salt, uids, values, version_key)) = commits.first() {
                assert_err!(
                    SubtensorModule::reveal_weights(
                        RuntimeOrigin::signed(unauthorized_hotkey),
                        netuid,
                        uids.clone(),
                        values.clone(),
                        salt.clone(),
                        *version_key,
                    ),
                    Error::<Test>::InvalidRevealCommitHashNotMatch
                );
            }
        }

        let non_committing_hotkey: <Test as frame_system::Config>::AccountId = U256::from(9999);
        assert_err!(
            SubtensorModule::reveal_weights(
                RuntimeOrigin::signed(non_committing_hotkey),
                netuid,
                vec![0, 1],
                vec![10, 20],
                vec![0; 8],
                0,
            ),
            Error::<Test>::NoWeightsCommitFound
        );

        assert_eq!(SubtensorModule::get_reveal_period(netuid), 10);
        assert_eq!(SubtensorModule::get_tempo(netuid), 200);
    })
}

#[test]
fn test_get_reveal_blocks() {
    new_test_ext(1).execute_with(|| {
        // **1. Define Test Parameters**
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        // **2. Generate the Commit Hash**
        let commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));

        // **3. Initialize the Block Number to 0**
        System::set_block_number(0);

        // **4. Define Network Parameters**
        let tempo: u16 = 5;
        add_network(netuid, tempo, 0);

        // **5. Register Neurons and Configure the Network**
        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 5);
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        // **6. Commit Weights at Block 0**
        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));

        // **7. Retrieve the Reveal Blocks Using `get_reveal_blocks`**
        let (first_reveal_block, last_reveal_block) = SubtensorModule::get_reveal_blocks(netuid, 0);

        // **8. Assert Correct Calculation of Reveal Blocks**
        // With tempo=5, netuid=1, reveal_period=1:
        // commit_epoch = (0 + 2) / 6 = 0
        // reveal_epoch = 0 + 1 = 1
        // first_reveal_block = 1 * 6 - 2 = 4
        // last_reveal_block = 4 + 5 = 9
        assert_eq!(first_reveal_block, 4);
        assert_eq!(last_reveal_block, 9);

        // **9. Attempt to Reveal Before `first_reveal_block` (Block 3)**
        step_block(3); // Advance to block 3
        let result = SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        );
        assert_err!(result, Error::<Test>::RevealTooEarly);

        // **10. Advance to `first_reveal_block` (Block 4)**
        step_block(1); // Advance to block 4
        let result = SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        );
        assert_ok!(result);

        // **11. Attempt to Reveal Again at Block 4 (Should Fail)**
        let result = SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        );
        assert_err!(result, Error::<Test>::NoWeightsCommitFound);

        // **12. Advance to After `last_reveal_block` (Block 10)**
        step_block(6); // Advance from block 4 to block 10

        // **13. Attempt to Reveal at Block 10 (Should Fail)**
        let result = SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        );
        assert_err!(result, Error::<Test>::NoWeightsCommitFound);

        // **14. Attempt to Reveal Outside of Any Reveal Window (No Commit)**
        let result = SubtensorModule::reveal_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        );
        assert_err!(result, Error::<Test>::NoWeightsCommitFound);

        // **15. Verify that All Commits Have Been Removed from Storage**
        let commits = pallet_subtensor::WeightCommits::<Test>::get(netuid, hotkey);
        assert!(
            commits.is_none(),
            "Commits should be cleared after successful reveal"
        );
    })
}

#[test]
fn test_commit_weights_rate_limit() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let uids: Vec<u16> = vec![0, 1];
        let weight_values: Vec<u16> = vec![10, 10];
        let salt: Vec<u16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let version_key: u64 = 0;
        let hotkey: U256 = U256::from(1);

        let commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            salt.clone(),
            version_key,
        ));
        System::set_block_number(11);

        let tempo: u16 = 5;
        add_network(netuid, tempo, 0);

        register_ok_neuron(netuid, U256::from(3), U256::from(4), 300_000);
        register_ok_neuron(netuid, U256::from(1), U256::from(2), 100_000);
        SubtensorModule::set_weights_set_rate_limit(netuid, 10); // Rate limit is 10 blocks
        SubtensorModule::set_validator_permit_for_uid(netuid, 0, true);
        SubtensorModule::set_validator_permit_for_uid(netuid, 1, true);
        SubtensorModule::set_commit_reveal_weights_enabled(netuid, true);

        let neuron_uid =
            SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey).expect("expected uid");
        SubtensorModule::set_last_update_for_uid(netuid, neuron_uid, 0);

        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            commit_hash
        ));

        let new_salt: Vec<u16> = vec![9; 8];
        let new_commit_hash: H256 = BlakeTwo256::hash_of(&(
            hotkey,
            netuid,
            uids.clone(),
            weight_values.clone(),
            new_salt.clone(),
            version_key,
        ));
        assert_err!(
            SubtensorModule::commit_weights(RuntimeOrigin::signed(hotkey), netuid, new_commit_hash),
            Error::<Test>::CommittingWeightsTooFast
        );

        step_block(5);
        assert_err!(
            SubtensorModule::commit_weights(RuntimeOrigin::signed(hotkey), netuid, new_commit_hash),
            Error::<Test>::CommittingWeightsTooFast
        );

        step_block(5); // Current block is now 21

        assert_ok!(SubtensorModule::commit_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            new_commit_hash
        ));

        SubtensorModule::set_commit_reveal_weights_enabled(netuid, false);
        let weights_keys: Vec<u16> = vec![0];
        let weight_values: Vec<u16> = vec![1];

        assert_err!(
            SubtensorModule::set_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                weights_keys.clone(),
                weight_values.clone(),
                0
            ),
            Error::<Test>::SettingWeightsTooFast
        );

        step_block(10);

        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            weights_keys.clone(),
            weight_values.clone(),
            0
        ));

        assert_err!(
            SubtensorModule::set_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                weights_keys.clone(),
                weight_values.clone(),
                0
            ),
            Error::<Test>::SettingWeightsTooFast
        );

        step_block(5);

        assert_err!(
            SubtensorModule::set_weights(
                RuntimeOrigin::signed(hotkey),
                netuid,
                weights_keys.clone(),
                weight_values.clone(),
                0
            ),
            Error::<Test>::SettingWeightsTooFast
        );

        step_block(5);

        assert_ok!(SubtensorModule::set_weights(
            RuntimeOrigin::signed(hotkey),
            netuid,
            weights_keys.clone(),
            weight_values.clone(),
            0
        ));
    });
}
