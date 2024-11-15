#![allow(clippy::unwrap_used)]

use crate::mock::*;
use frame_support::{assert_err, assert_ok};
use frame_system::Config;
use pallet_subtensor::*;
use sp_core::U256;

mod mock;

/********************************************
    tests for uids.rs file
*********************************************/

/********************************************
    tests uids::replace_neuron()
*********************************************/

#[test]
fn test_replace_neuron() {
    new_test_ext(1).execute_with(|| {
        let block_number: u64 = 0;
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        let hotkey_account_id = U256::from(1);
        let (nonce, work): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            netuid,
            block_number,
            111111,
            &hotkey_account_id,
        );
        let coldkey_account_id = U256::from(1234);

        let new_hotkey_account_id = U256::from(2);
        let _new_colkey_account_id = U256::from(12345);
        let certificate = NeuronCertificate::try_from(vec![1, 2, 3]).unwrap();

        //add network
        add_network(netuid, tempo, 0);

        // Register a neuron.
        assert_ok!(SubtensorModule::register(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid,
            block_number,
            nonce,
            work,
            hotkey_account_id,
            coldkey_account_id
        ));

        // Get UID
        let neuron_uid = SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey_account_id);
        assert_ok!(neuron_uid);
        let neuron_uid = neuron_uid.unwrap();

        // set non-default values
        Trust::<Test>::mutate(netuid, |v| {
            SubtensorModule::set_element_at(v, neuron_uid as usize, 5u16)
        });
        Emission::<Test>::mutate(netuid, |v| {
            SubtensorModule::set_element_at(v, neuron_uid as usize, 5u64)
        });
        Consensus::<Test>::mutate(netuid, |v| {
            SubtensorModule::set_element_at(v, neuron_uid as usize, 5u16)
        });
        Incentive::<Test>::mutate(netuid, |v| {
            SubtensorModule::set_element_at(v, neuron_uid as usize, 5u16)
        });
        Dividends::<Test>::mutate(netuid, |v| {
            SubtensorModule::set_element_at(v, neuron_uid as usize, 5u16)
        });

        // serve axon mock address
        let ip: u128 = 1676056785;
        let port: u16 = 9999;
        let ip_type: u8 = 4;
        let hotkey = SubtensorModule::get_hotkey_for_net_and_uid(netuid, neuron_uid).unwrap();
        assert!(SubtensorModule::serve_axon(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid,
            0,
            ip,
            port,
            ip_type,
            0,
            0,
            0
        )
        .is_ok());

        // Set a neuron certificate for it
        NeuronCertificates::<Test>::insert(netuid, hotkey_account_id, certificate);

        // Replace the neuron.
        SubtensorModule::replace_neuron(netuid, neuron_uid, &new_hotkey_account_id, block_number);

        assert!(!SubtensorModule::has_axon_info(netuid, &hotkey));

        // Check old hotkey is not registered on any network.
        assert!(SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey_account_id).is_err());
        assert!(!SubtensorModule::is_hotkey_registered_on_any_network(
            &hotkey_account_id
        ));

        let curr_hotkey = SubtensorModule::get_hotkey_for_net_and_uid(netuid, neuron_uid);
        assert_ok!(curr_hotkey);
        assert_ne!(curr_hotkey.unwrap(), hotkey_account_id);

        // Check new hotkey is registered on the network.
        assert!(
            SubtensorModule::get_uid_for_net_and_hotkey(netuid, &new_hotkey_account_id).is_ok()
        );
        assert!(SubtensorModule::is_hotkey_registered_on_any_network(
            &new_hotkey_account_id
        ));
        assert_eq!(curr_hotkey.unwrap(), new_hotkey_account_id);

        // Check neuron certificate was reset
        let certificate = NeuronCertificates::<Test>::get(netuid, hotkey_account_id);
        assert_eq!(certificate, None);

        // Check trust, emission, consensus, incentive, dividends have been reset to 0.
        assert_eq!(SubtensorModule::get_trust_for_uid(netuid, neuron_uid), 0);
        assert_eq!(SubtensorModule::get_emission_for_uid(netuid, neuron_uid), 0);
        assert_eq!(
            SubtensorModule::get_consensus_for_uid(netuid, neuron_uid),
            0
        );
        assert_eq!(
            SubtensorModule::get_incentive_for_uid(netuid, neuron_uid),
            0
        );
        assert_eq!(
            SubtensorModule::get_dividends_for_uid(netuid, neuron_uid),
            0
        );

        assert!(!SubtensorModule::has_axon_info(
            netuid,
            &new_hotkey_account_id
        ));

        // Check axon info is reset.
        let axon_info = SubtensorModule::get_axon_info(netuid, &curr_hotkey.unwrap());
        assert_eq!(axon_info.ip, 0);
        assert_eq!(axon_info.port, 0);
        assert_eq!(axon_info.ip_type, 0);
    });
}

#[test]
fn test_replace_neuron_multiple_subnets() {
    new_test_ext(1).execute_with(|| {
        let block_number: u64 = 0;
        let netuid: u16 = 1;
        let netuid1: u16 = 2;
        let tempo: u16 = 13;
        let hotkey_account_id = U256::from(1);
        let new_hotkey_account_id = U256::from(2);

        let (nonce, work): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            netuid,
            block_number,
            111111,
            &hotkey_account_id,
        );
        let (nonce1, work1): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            netuid1,
            block_number,
            111111 * 5,
            &hotkey_account_id,
        );

        let coldkey_account_id = U256::from(1234);

        let _new_colkey_account_id = U256::from(12345);

        //add network
        add_network(netuid, tempo, 0);
        add_network(netuid1, tempo, 0);

        // Register a neuron on both networks.
        assert_ok!(SubtensorModule::register(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid,
            block_number,
            nonce,
            work,
            hotkey_account_id,
            coldkey_account_id
        ));
        assert_ok!(SubtensorModule::register(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid1,
            block_number,
            nonce1,
            work1,
            hotkey_account_id,
            coldkey_account_id
        ));

        // Get UID
        let neuron_uid = SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey_account_id);
        assert_ok!(neuron_uid);

        // Verify neuron is registered on both networks.
        assert!(SubtensorModule::is_hotkey_registered_on_network(
            netuid,
            &hotkey_account_id
        ));
        assert!(SubtensorModule::is_hotkey_registered_on_network(
            netuid1,
            &hotkey_account_id
        ));
        assert!(SubtensorModule::is_hotkey_registered_on_any_network(
            &hotkey_account_id
        ));

        // Replace the neuron.
        // Only replace on ONE network.
        SubtensorModule::replace_neuron(
            netuid,
            neuron_uid.unwrap(),
            &new_hotkey_account_id,
            block_number,
        );

        // Check old hotkey is not registered on netuid network.
        assert!(SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey_account_id).is_err());

        // Verify still registered on netuid1 network.
        assert!(SubtensorModule::is_hotkey_registered_on_any_network(
            &hotkey_account_id
        ));
        assert!(SubtensorModule::is_hotkey_registered_on_network(
            netuid1,
            &hotkey_account_id
        ));
    });
}

#[test]
fn test_replace_neuron_multiple_subnets_unstake_all() {
    new_test_ext(1).execute_with(|| {
        let block_number: u64 = 0;
        let netuid: u16 = 1;
        let netuid1: u16 = 2;
        let tempo: u16 = 13;

        let hotkey_account_id = U256::from(1);
        let new_hotkey_account_id = U256::from(2);

        let (nonce, work): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            netuid,
            block_number,
            111111,
            &hotkey_account_id,
        );
        let (nonce1, work1): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            netuid1,
            block_number,
            111111 * 5,
            &hotkey_account_id,
        );

        let coldkey_account_id = U256::from(1234);
        let coldkey_account1_id = U256::from(1235);
        let coldkey_account2_id = U256::from(1236);

        let stake_amount = 1000;

        //add network
        add_network(netuid, tempo, 0);
        add_network(netuid1, tempo, 0);

        // Register a neuron on both networks.
        assert_ok!(SubtensorModule::register(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid,
            block_number,
            nonce,
            work,
            hotkey_account_id,
            coldkey_account_id
        ));
        assert_ok!(SubtensorModule::register(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid1,
            block_number,
            nonce1,
            work1,
            hotkey_account_id,
            coldkey_account_id
        ));

        // Get UID
        let neuron_uid = SubtensorModule::get_uid_for_net_and_hotkey(netuid, &hotkey_account_id);
        assert_ok!(neuron_uid);

        // Stake on neuron with multiple coldkeys.
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey_account_id,
            &hotkey_account_id,
            stake_amount,
        );
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey_account1_id,
            &hotkey_account_id,
            stake_amount + 1,
        );
        SubtensorModule::increase_stake_on_coldkey_hotkey_account(
            &coldkey_account2_id,
            &hotkey_account_id,
            stake_amount + 2,
        );

        // Check stake on neuron
        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_and_hotkey(
                &coldkey_account_id,
                &hotkey_account_id
            ),
            stake_amount
        );
        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_and_hotkey(
                &coldkey_account1_id,
                &hotkey_account_id
            ),
            stake_amount + 1
        );
        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_and_hotkey(
                &coldkey_account2_id,
                &hotkey_account_id
            ),
            stake_amount + 2
        );

        // Check total stake on neuron
        assert_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&hotkey_account_id),
            (stake_amount * 3) + (1 + 2)
        );

        // Replace the neuron.
        SubtensorModule::replace_neuron(
            netuid,
            neuron_uid.unwrap(),
            &new_hotkey_account_id,
            block_number,
        );

        // The stakes should still be on the neuron. It is still registered on one network.
        assert!(SubtensorModule::is_hotkey_registered_on_any_network(
            &hotkey_account_id
        ));

        // Check the stake is still on the coldkey accounts.
        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_and_hotkey(
                &coldkey_account_id,
                &hotkey_account_id
            ),
            stake_amount
        );
        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_and_hotkey(
                &coldkey_account1_id,
                &hotkey_account_id
            ),
            stake_amount + 1
        );
        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_and_hotkey(
                &coldkey_account2_id,
                &hotkey_account_id
            ),
            stake_amount + 2
        );

        // Check total stake on neuron
        assert_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&hotkey_account_id),
            (stake_amount * 3) + (1 + 2)
        );

        // replace on second network
        SubtensorModule::replace_neuron(
            netuid1,
            neuron_uid.unwrap(),
            &new_hotkey_account_id,
            block_number,
        );

        // The neuron should be unregistered now.
        assert!(!SubtensorModule::is_hotkey_registered_on_any_network(
            &hotkey_account_id
        ));

        // Check the stake is now on the free balance of the coldkey accounts.
        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_and_hotkey(
                &coldkey_account_id,
                &hotkey_account_id
            ),
            0
        );
        assert_eq!(Balances::free_balance(coldkey_account_id), stake_amount);

        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_and_hotkey(
                &coldkey_account1_id,
                &hotkey_account_id
            ),
            0
        );
        assert_eq!(
            Balances::free_balance(coldkey_account1_id),
            stake_amount + 1
        );

        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_and_hotkey(
                &coldkey_account2_id,
                &hotkey_account_id
            ),
            0
        );
        assert_eq!(
            Balances::free_balance(coldkey_account2_id),
            stake_amount + 2
        );

        // Check total stake on neuron
        assert_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&hotkey_account_id),
            0
        );
    });
}

#[test]
fn test_neuron_certificate() {
    new_test_ext(1).execute_with(|| {
        // 512 bits key
        let mut data = [0; 65].to_vec();
        assert_ok!(NeuronCertificate::try_from(data));

        // 256 bits key
        data = [1; 33].to_vec();
        assert_ok!(NeuronCertificate::try_from(data));

        // too much data
        data = [8; 88].to_vec();
        assert_err!(NeuronCertificate::try_from(data), ());

        // no data
        data = vec![];
        assert_err!(NeuronCertificate::try_from(data), ());
    });
}
