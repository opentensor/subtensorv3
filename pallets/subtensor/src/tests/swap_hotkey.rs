#![allow(unused, clippy::indexing_slicing, clippy::panic, clippy::unwrap_used)]

use approx::assert_abs_diff_eq;
use codec::Encode;
use frame_support::weights::Weight;
use frame_support::{assert_err, assert_noop, assert_ok};
use frame_system::{Config, RawOrigin};

use super::mock::*;
use crate::*;
use sp_core::{Get, H256, U256};
use sp_runtime::SaturatedConversion;
use substrate_fixed::types::U64F64;

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_owner --exact --nocapture
#[test]
fn test_swap_owner() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();

        Owner::<Test>::insert(old_hotkey, coldkey);
        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(!Owner::<Test>::contains_key(old_hotkey));
        assert_eq!(Owner::<Test>::get(new_hotkey), coldkey);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_owned_hotkeys --exact --nocapture
#[test]
fn test_swap_owned_hotkeys() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();

        OwnedHotkeys::<Test>::insert(coldkey, vec![old_hotkey]);
        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        let hotkeys = OwnedHotkeys::<Test>::get(coldkey);
        assert!(!hotkeys.contains(&old_hotkey));
        assert!(hotkeys.contains(&new_hotkey));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_total_hotkey_stake --exact --nocapture
#[test]
fn test_swap_total_hotkey_stake() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let amount = 10_000;
        let mut weight = Weight::zero();

        //add network
        let netuid: u16 = add_dynamic_network(&old_hotkey, &coldkey);

        // Give it some $$$ in his coldkey balance
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, amount);

        // Add stake
        assert_ok!(SubtensorModule::add_stake(
            RuntimeOrigin::signed(coldkey),
            old_hotkey,
            netuid,
            amount
        ));

        // Check if stake has increased
        assert_abs_diff_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&old_hotkey),
            amount,
            epsilon = 1,
        );
        assert_abs_diff_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&new_hotkey),
            0,
            epsilon = 1,
        );

        // Swap hotkey
        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        // Verify that total hotkey stake swapped
        assert_abs_diff_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&old_hotkey),
            0,
            epsilon = 1,
        );
        assert_abs_diff_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&new_hotkey),
            amount,
            epsilon = 1,
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_last_tx_block --exact --nocapture
#[test]
fn test_swap_last_tx_block() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();

        LastTxBlock::<Test>::insert(old_hotkey, 1000);
        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(!LastTxBlock::<Test>::contains_key(old_hotkey));
        assert_eq!(
            LastTxBlock::<Test>::get(new_hotkey),
            SubtensorModule::get_current_block_as_u64()
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_last_tx_block_delegate_take --exact --nocapture
#[test]
fn test_swap_last_tx_block_delegate_take() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();

        crate::LastTxBlockDelegateTake::<Test>::insert(old_hotkey, 1000);
        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(!LastTxBlockDelegateTake::<Test>::contains_key(old_hotkey));
        assert_eq!(
            LastTxBlockDelegateTake::<Test>::get(new_hotkey),
            SubtensorModule::get_current_block_as_u64()
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_senate_members --exact --nocapture
#[test]
fn test_swap_senate_members() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();

        // Assuming there's a way to add a member to the senate
        // SenateMembers::add_member(&old_hotkey);
        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        // Assert that the old_hotkey is no longer a member and new_hotkey is now a member
        // assert!(!SenateMembers::is_member(&old_hotkey));
        // assert!(SenateMembers::is_member(&new_hotkey));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_delegates --exact --nocapture
#[test]
fn test_swap_delegates() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();

        Delegates::<Test>::insert(old_hotkey, 100);
        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(!Delegates::<Test>::contains_key(old_hotkey));
        assert_eq!(Delegates::<Test>::get(new_hotkey), 100);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_subnet_membership --exact --nocapture
#[test]
fn test_swap_subnet_membership() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid = 0u16;
        let mut weight = Weight::zero();

        add_network(netuid, 0, 1);
        IsNetworkMember::<Test>::insert(old_hotkey, netuid, true);
        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(!IsNetworkMember::<Test>::contains_key(old_hotkey, netuid));
        assert!(IsNetworkMember::<Test>::get(new_hotkey, netuid));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_uids_and_keys --exact --nocapture
#[test]
fn test_swap_uids_and_keys() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid = 0u16;
        let uid = 5u16;
        let mut weight = Weight::zero();

        add_network(netuid, 0, 1);
        IsNetworkMember::<Test>::insert(old_hotkey, netuid, true);
        Uids::<Test>::insert(netuid, old_hotkey, uid);
        Keys::<Test>::insert(netuid, uid, old_hotkey);

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert_eq!(Uids::<Test>::get(netuid, old_hotkey), None);
        assert_eq!(Uids::<Test>::get(netuid, new_hotkey), Some(uid));
        assert_eq!(Keys::<Test>::get(netuid, uid), new_hotkey);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_prometheus --exact --nocapture
#[test]
fn test_swap_prometheus() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid = 0u16;
        let prometheus_info = PrometheusInfo::default();
        let mut weight = Weight::zero();

        add_network(netuid, 0, 1);
        IsNetworkMember::<Test>::insert(old_hotkey, netuid, true);
        Prometheus::<Test>::insert(netuid, old_hotkey, prometheus_info.clone());

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(!Prometheus::<Test>::contains_key(netuid, old_hotkey));
        assert_eq!(
            Prometheus::<Test>::get(netuid, new_hotkey),
            Some(prometheus_info)
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_axons --exact --nocapture
#[test]
fn test_swap_axons() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid = 0u16;
        let axon_info = AxonInfo::default();
        let mut weight = Weight::zero();

        add_network(netuid, 0, 1);
        IsNetworkMember::<Test>::insert(old_hotkey, netuid, true);
        Axons::<Test>::insert(netuid, old_hotkey, axon_info.clone());

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(!Axons::<Test>::contains_key(netuid, old_hotkey));
        assert_eq!(Axons::<Test>::get(netuid, new_hotkey), Some(axon_info));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_certificates --exact --nocapture
#[test]
fn test_swap_certificates() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid = 0u16;
        let certificate = NeuronCertificate::try_from(vec![1, 2, 3]).unwrap();
        let mut weight = Weight::zero();

        add_network(netuid, 0, 1);
        IsNetworkMember::<Test>::insert(old_hotkey, netuid, true);
        NeuronCertificates::<Test>::insert(netuid, old_hotkey, certificate.clone());

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(!NeuronCertificates::<Test>::contains_key(
            netuid, old_hotkey
        ));
        assert_eq!(
            NeuronCertificates::<Test>::get(netuid, new_hotkey),
            Some(certificate)
        );
    });
}
use sp_std::collections::vec_deque::VecDeque;
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_weight_commits --exact --nocapture
#[test]
fn test_swap_weight_commits() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid = 0u16;
        let mut weight_commits: VecDeque<(H256, u64, u64, u64)> = VecDeque::new();
        weight_commits.push_back((H256::from_low_u64_be(100), 200, 1, 1));
        let mut weight = Weight::zero();

        add_network(netuid, 0, 1);
        IsNetworkMember::<Test>::insert(old_hotkey, netuid, true);
        WeightCommits::<Test>::insert(netuid, old_hotkey, weight_commits.clone());

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(!WeightCommits::<Test>::contains_key(netuid, old_hotkey));
        assert_eq!(
            WeightCommits::<Test>::get(netuid, new_hotkey),
            Some(weight_commits)
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_loaded_emission --exact --nocapture
#[test]
fn test_swap_loaded_emission() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid = 0u16;
        let server_emission = 1000u64;
        let validator_emission = 1000u64;
        let mut weight = Weight::zero();

        add_network(netuid, 0, 1);
        IsNetworkMember::<Test>::insert(old_hotkey, netuid, true);
        LoadedEmission::<Test>::insert(
            netuid,
            vec![(old_hotkey, server_emission, validator_emission)],
        );

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        let new_loaded_emission = LoadedEmission::<Test>::get(netuid);
        assert_eq!(
            new_loaded_emission,
            Some(vec![(new_hotkey, server_emission, validator_emission)])
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_stake --exact --nocapture
#[test]
fn test_swap_stake() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let stake_amount = 100u64;
        let mut weight = Weight::zero();

        Stake::<Test>::insert(old_hotkey, coldkey, stake_amount);

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(!Stake::<Test>::contains_key(old_hotkey, coldkey));
        assert_eq!(Stake::<Test>::get(new_hotkey, coldkey), stake_amount);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_staking_hotkeys --exact --nocapture
#[test]
fn test_swap_staking_hotkeys() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();
        let netuid = 1;

        Stake::<Test>::insert(old_hotkey, coldkey, 100);
        StakingHotkeys::<Test>::insert(coldkey, vec![old_hotkey]);
        Alpha::<Test>::insert((old_hotkey, coldkey, netuid), U64F64::from_num(100));

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        let staking_hotkeys = StakingHotkeys::<Test>::get(coldkey);
        assert!(!staking_hotkeys.contains(&old_hotkey));
        assert!(staking_hotkeys.contains(&new_hotkey));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_hotkey_with_multiple_coldkeys --exact --nocapture
#[test]
fn test_swap_hotkey_with_multiple_coldkeys() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey1 = U256::from(3);
        let coldkey2 = U256::from(4);
        let mut weight = Weight::zero();
        let netuid = 1;

        Stake::<Test>::insert(old_hotkey, coldkey1, 100);
        Stake::<Test>::insert(old_hotkey, coldkey2, 200);
        StakingHotkeys::<Test>::insert(coldkey1, vec![old_hotkey]);
        StakingHotkeys::<Test>::insert(coldkey2, vec![old_hotkey]);

        Alpha::<Test>::insert((old_hotkey, coldkey1, netuid), U64F64::from_num(100));
        Alpha::<Test>::insert((old_hotkey, coldkey2, netuid), U64F64::from_num(200));

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey1,
            &mut weight
        ));

        assert_eq!(Stake::<Test>::get(new_hotkey, coldkey1), 100);
        assert_eq!(Stake::<Test>::get(new_hotkey, coldkey2), 200);
        assert!(StakingHotkeys::<Test>::get(coldkey1).contains(&new_hotkey));
        assert!(StakingHotkeys::<Test>::get(coldkey2).contains(&new_hotkey));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_hotkey_with_existing_stake --exact --nocapture
#[test]
fn test_swap_hotkey_with_existing_stake() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();

        Stake::<Test>::insert(old_hotkey, coldkey, 100);
        Stake::<Test>::insert(new_hotkey, coldkey, 50);

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert_eq!(Stake::<Test>::get(new_hotkey, coldkey), 150);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_hotkey_with_multiple_subnets --exact --nocapture
#[test]
fn test_swap_hotkey_with_multiple_subnets() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid1 = 0;
        let netuid2 = 1;
        let mut weight = Weight::zero();

        add_network(netuid1, 0, 1);
        add_network(netuid2, 0, 1);
        IsNetworkMember::<Test>::insert(old_hotkey, netuid1, true);
        IsNetworkMember::<Test>::insert(old_hotkey, netuid2, true);

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        assert!(IsNetworkMember::<Test>::get(new_hotkey, netuid1));
        assert!(IsNetworkMember::<Test>::get(new_hotkey, netuid2));
        assert!(!IsNetworkMember::<Test>::get(old_hotkey, netuid1));
        assert!(!IsNetworkMember::<Test>::get(old_hotkey, netuid2));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_staking_hotkeys_multiple_coldkeys --exact --nocapture
#[test]
fn test_swap_staking_hotkeys_multiple_coldkeys() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey1 = U256::from(3);
        let coldkey2 = U256::from(4);
        let mut weight = Weight::zero();
        let netuid = 1;

        // Set up initial state
        Stake::<Test>::insert(old_hotkey, coldkey1, 100);
        Stake::<Test>::insert(old_hotkey, coldkey2, 200);

        Alpha::<Test>::insert((old_hotkey, coldkey1, netuid), U64F64::from_num(100));
        Alpha::<Test>::insert((old_hotkey, coldkey2, netuid), U64F64::from_num(200));

        StakingHotkeys::<Test>::insert(coldkey1, vec![old_hotkey]);
        StakingHotkeys::<Test>::insert(coldkey2, vec![old_hotkey, U256::from(5)]);

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey1,
            &mut weight
        ));

        // Check if new_hotkey replaced old_hotkey in StakingHotkeys
        assert!(StakingHotkeys::<Test>::get(coldkey1).contains(&new_hotkey));
        assert!(!StakingHotkeys::<Test>::get(coldkey1).contains(&old_hotkey));

        // Check if new_hotkey replaced old_hotkey for coldkey2 as well
        assert!(StakingHotkeys::<Test>::get(coldkey2).contains(&new_hotkey));
        assert!(!StakingHotkeys::<Test>::get(coldkey2).contains(&old_hotkey));
        assert!(StakingHotkeys::<Test>::get(coldkey2).contains(&U256::from(5)));
        // Other hotkeys should remain
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_hotkey_with_no_stake --exact --nocapture
#[test]
fn test_swap_hotkey_with_no_stake() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();

        // Set up initial state with no stake
        Owner::<Test>::insert(old_hotkey, coldkey);

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey,
            &mut weight
        ));

        // Check if ownership transferred
        assert!(!Owner::<Test>::contains_key(old_hotkey));
        assert_eq!(Owner::<Test>::get(new_hotkey), coldkey);

        // Ensure no unexpected changes in Stake
        assert!(!Stake::<Test>::contains_key(old_hotkey, coldkey));
        assert!(!Stake::<Test>::contains_key(new_hotkey, coldkey));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --package pallet-subtensor --lib -- tests::swap_hotkey::test_swap_hotkey_with_multiple_coldkeys_and_subnets --exact --show-output
#[test]
fn test_swap_hotkey_with_multiple_coldkeys_and_subnets() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey1 = U256::from(3);
        let coldkey2 = U256::from(4);
        let netuid1 = 1;
        let netuid2 = 2;
        let stake = 1_000_000_u64;
        let mut weight = Weight::zero();

        // Set up initial state
        add_network(netuid1, 0, 1);
        add_network(netuid2, 0, 1);
        register_ok_neuron(netuid1, old_hotkey, coldkey1, 1234);
        register_ok_neuron(netuid2, old_hotkey, coldkey1, 1234);

        // Add balance to both coldkeys
        SubtensorModule::add_balance_to_coldkey_account(&coldkey1, stake + 1_000);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey2, stake + 1_000);

        // Stake with coldkey1
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey1),
            old_hotkey,
            netuid1,
            stake
        ));

        // Stake with coldkey2 also
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey2),
            old_hotkey,
            netuid2,
            stake
        ));

        let ck1_stake = SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
            &old_hotkey,
            &coldkey1,
            netuid1,
        );
        let ck2_stake = SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
            &old_hotkey,
            &coldkey2,
            netuid2,
        );
        assert!(ck1_stake > 0);
        assert!(ck2_stake > 0);
        let total_hk_stake = SubtensorModule::get_total_stake_for_hotkey(&old_hotkey);
        assert!(total_hk_stake > 0);

        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &old_hotkey,
            &new_hotkey,
            &coldkey1,
            &mut weight
        ));

        // Check ownership transfer
        assert_eq!(
            SubtensorModule::get_owning_coldkey_for_hotkey(&new_hotkey),
            coldkey1
        );
        assert!(!SubtensorModule::get_owned_hotkeys(&coldkey2).contains(&new_hotkey));

        // Check stake transfer
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &new_hotkey,
                &coldkey1,
                netuid1
            ),
            ck1_stake
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &new_hotkey,
                &coldkey2,
                netuid2
            ),
            ck2_stake
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &old_hotkey,
                &coldkey1,
                netuid1
            ),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_and_coldkey_on_subnet(
                &old_hotkey,
                &coldkey2,
                netuid2
            ),
            0
        );

        // Check subnet membership transfer
        assert!(SubtensorModule::is_hotkey_registered_on_network(
            netuid1,
            &new_hotkey
        ));
        assert!(SubtensorModule::is_hotkey_registered_on_network(
            netuid2,
            &new_hotkey
        ));
        assert!(!SubtensorModule::is_hotkey_registered_on_network(
            netuid1,
            &old_hotkey
        ));
        assert!(!SubtensorModule::is_hotkey_registered_on_network(
            netuid2,
            &old_hotkey
        ));

        // Check total stake transfer
        assert_eq!(
            SubtensorModule::get_total_stake_for_hotkey(&new_hotkey),
            total_hk_stake
        );
        assert_eq!(SubtensorModule::get_total_stake_for_hotkey(&old_hotkey), 0);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_hotkey_tx_rate_limit_exceeded --exact --nocapture
#[test]
fn test_swap_hotkey_tx_rate_limit_exceeded() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        let old_hotkey = U256::from(1);
        let new_hotkey_1 = U256::from(2);
        let new_hotkey_2 = U256::from(4);
        let coldkey = U256::from(3);
        let swap_cost = 1_000_000_000u64 * 2;

        let tx_rate_limit = 1;

        // Get the current transaction rate limit
        let current_tx_rate_limit = SubtensorModule::get_tx_rate_limit();
        log::info!("current_tx_rate_limit: {:?}", current_tx_rate_limit);

        // Set the transaction rate limit
        SubtensorModule::set_tx_rate_limit(tx_rate_limit);
        // assert the rate limit is set to 1000 blocks
        assert_eq!(SubtensorModule::get_tx_rate_limit(), tx_rate_limit);

        // Setup initial state
        add_network(netuid, tempo, 0);
        register_ok_neuron(netuid, old_hotkey, coldkey, 0);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, swap_cost);

        // Perform the first swap
        assert_ok!(SubtensorModule::do_swap_hotkey(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey),
            &old_hotkey,
            &new_hotkey_1
        ));

        // Attempt to perform another swap immediately, which should fail due to rate limit
        assert_err!(
            SubtensorModule::do_swap_hotkey(
                <<Test as Config>::RuntimeOrigin>::signed(coldkey),
                &new_hotkey_1,
                &new_hotkey_2
            ),
            Error::<Test>::HotKeySetTxRateLimitExceeded
        );

        // move in time past the rate limit
        step_block(1001);
        assert_ok!(SubtensorModule::do_swap_hotkey(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey),
            &new_hotkey_1,
            &new_hotkey_2
        ));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_do_swap_hotkey_err_not_owner --exact --nocapture
#[test]
fn test_do_swap_hotkey_err_not_owner() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let tempo: u16 = 13;
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let not_owner_coldkey = U256::from(4);
        let swap_cost = 1_000_000_000u64;

        // Setup initial state
        add_network(netuid, tempo, 0);
        register_ok_neuron(netuid, old_hotkey, coldkey, 0);
        SubtensorModule::add_balance_to_coldkey_account(&not_owner_coldkey, swap_cost);

        // Attempt the swap with a non-owner coldkey
        assert_err!(
            SubtensorModule::do_swap_hotkey(
                <<Test as Config>::RuntimeOrigin>::signed(not_owner_coldkey),
                &old_hotkey,
                &new_hotkey
            ),
            Error::<Test>::NonAssociatedColdKey
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_owner_success --exact --nocapture
#[test]
fn test_swap_owner_success() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();

        // Initialize Owner for old_hotkey
        Owner::<Test>::insert(old_hotkey, coldkey);

        // Perform the swap
        SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

        // Verify the swap
        assert_eq!(Owner::<Test>::get(new_hotkey), coldkey);
        assert!(!Owner::<Test>::contains_key(old_hotkey));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_owner_old_hotkey_not_exist --exact --nocapture
#[test]
fn test_swap_owner_old_hotkey_not_exist() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let mut weight = Weight::zero();

        // Ensure old_hotkey does not exist
        assert!(!Owner::<Test>::contains_key(old_hotkey));

        // Perform the swap
        SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

        // Verify the swap
        assert_eq!(Owner::<Test>::get(new_hotkey), coldkey);
        assert!(!Owner::<Test>::contains_key(old_hotkey));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_owner_new_hotkey_already_exists --exact --nocapture
#[test]
fn test_swap_owner_new_hotkey_already_exists() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let another_coldkey = U256::from(4);
        let mut weight = Weight::zero();

        // Initialize Owner for old_hotkey and new_hotkey
        Owner::<Test>::insert(old_hotkey, coldkey);
        Owner::<Test>::insert(new_hotkey, another_coldkey);

        // Perform the swap
        SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

        // Verify the swap
        assert_eq!(Owner::<Test>::get(new_hotkey), coldkey);
        assert!(!Owner::<Test>::contains_key(old_hotkey));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --package pallet-subtensor --lib -- tests::swap_hotkey::test_swap_delegates_success --exact --show-output
#[test]
fn test_swap_delegates_success() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let delegate_take = 10u16;
        let mut weight = Weight::zero();

        // Initialize Delegates for old_hotkey
        Delegates::<Test>::insert(old_hotkey, delegate_take);

        // Perform the swap
        SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

        // Verify the swap
        assert_eq!(Delegates::<Test>::get(new_hotkey), delegate_take);
        assert!(!Delegates::<Test>::contains_key(old_hotkey));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_stake_success --exact --nocapture
#[test]
fn test_swap_stake_success() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let subnet_owner_coldkey = U256::from(1001);
        let subnet_owner_hotkey = U256::from(1002);
        let netuid = add_dynamic_network(&subnet_owner_hotkey, &subnet_owner_coldkey);
        let amount = 10_000;
        let shares = U64F64::from_num(123456);
        let mut weight = Weight::zero();

        // Initialize staking variables for old_hotkey
        Stake::<Test>::insert(old_hotkey, coldkey, 0);
        TotalHotkeyAlpha::<Test>::insert(old_hotkey, netuid, amount);
        TotalHotkeyShares::<Test>::insert(old_hotkey, netuid, U64F64::from_num(shares));
        Alpha::<Test>::insert((old_hotkey, coldkey, netuid), U64F64::from_num(amount));

        // Perform the swap
        SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

        // Verify the swap
        assert_eq!(TotalHotkeyAlpha::<Test>::get(old_hotkey, netuid), 0);
        assert_eq!(TotalHotkeyAlpha::<Test>::get(new_hotkey, netuid), amount);
        assert_eq!(
            TotalHotkeyShares::<Test>::get(old_hotkey, netuid),
            U64F64::from_num(0)
        );
        assert_eq!(
            TotalHotkeyShares::<Test>::get(new_hotkey, netuid),
            U64F64::from_num(shares)
        );
        assert_eq!(
            Alpha::<Test>::get((old_hotkey, coldkey, netuid)),
            U64F64::from_num(0)
        );
        assert_eq!(
            Alpha::<Test>::get((new_hotkey, coldkey, netuid)),
            U64F64::from_num(amount)
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_stake_old_hotkey_not_exist --exact --nocapture
#[test]
fn test_swap_stake_old_hotkey_not_exist() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let stake_amount = 1000u64;
        let mut weight = Weight::zero();

        // Initialize Stake for old_hotkey
        Stake::<Test>::insert(old_hotkey, coldkey, stake_amount);

        // Ensure old_hotkey has a stake
        assert!(Stake::<Test>::contains_key(old_hotkey, coldkey));

        // Perform the swap
        SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

        // Verify that new_hotkey has the stake and old_hotkey does not
        assert!(Stake::<Test>::contains_key(new_hotkey, coldkey));
        assert!(!Stake::<Test>::contains_key(old_hotkey, coldkey));
    });
}

// // SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_total_hotkey_coldkey_stakes_this_interval_success --exact --nocapture
// #[test]
// fn test_swap_total_hotkey_coldkey_stakes_this_interval_success() {
//     new_test_ext(1).execute_with(|| {
//         let old_hotkey = U256::from(1);
//         let new_hotkey = U256::from(2);
//         let coldkey = U256::from(3);
//         let stake = (1000u64, 42u64); // Example tuple value
//         let mut weight = Weight::zero();

//         // Initialize TotalHotkeyColdkeyStakesThisInterval for old_hotkey
//         TotalHotkeyColdkeyStakesThisInterval::<Test>::insert(old_hotkey, coldkey, stake);

//         // Perform the swap
//         SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

//         // Verify the swap
//         assert_eq!(
//             TotalHotkeyColdkeyStakesThisInterval::<Test>::get(new_hotkey, coldkey),
//             stake
//         );
//         assert!(!TotalHotkeyColdkeyStakesThisInterval::<Test>::contains_key(
//             old_hotkey, coldkey
//         ));
//     });
// }

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_hotkey_error_cases --exact --nocapture
#[test]
fn test_swap_hotkey_error_cases() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let wrong_coldkey = U256::from(4);

        // Set up initial state
        Owner::<Test>::insert(old_hotkey, coldkey);
        TotalNetworks::<Test>::put(1);
        LastTxBlock::<Test>::insert(coldkey, 0);

        // Test not enough balance
        let swap_cost = SubtensorModule::get_key_swap_cost();
        assert_noop!(
            SubtensorModule::do_swap_hotkey(
                RuntimeOrigin::signed(coldkey),
                &old_hotkey,
                &new_hotkey
            ),
            Error::<Test>::NotEnoughBalanceToPaySwapHotKey
        );

        let initial_balance = SubtensorModule::get_key_swap_cost() + 1000;
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, initial_balance);

        // Test new hotkey same as old
        assert_noop!(
            SubtensorModule::do_swap_hotkey(
                RuntimeOrigin::signed(coldkey),
                &old_hotkey,
                &old_hotkey
            ),
            Error::<Test>::NewHotKeyIsSameWithOld
        );

        // Test new hotkey already registered
        IsNetworkMember::<Test>::insert(new_hotkey, 0, true);
        assert_noop!(
            SubtensorModule::do_swap_hotkey(
                RuntimeOrigin::signed(coldkey),
                &old_hotkey,
                &new_hotkey
            ),
            Error::<Test>::HotKeyAlreadyRegisteredInSubNet
        );
        IsNetworkMember::<Test>::remove(new_hotkey, 0);

        // Test non-associated coldkey
        assert_noop!(
            SubtensorModule::do_swap_hotkey(
                RuntimeOrigin::signed(wrong_coldkey),
                &old_hotkey,
                &new_hotkey
            ),
            Error::<Test>::NonAssociatedColdKey
        );

        // Run the successful swap
        assert_ok!(SubtensorModule::do_swap_hotkey(
            RuntimeOrigin::signed(coldkey),
            &old_hotkey,
            &new_hotkey
        ));

        // Check balance after swap
        assert_eq!(Balances::free_balance(coldkey), initial_balance - swap_cost);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_child_keys --exact --nocapture
#[test]
fn test_swap_child_keys() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid = 0u16;
        let children = vec![(100u64, U256::from(4)), (200u64, U256::from(5))];
        let mut weight = Weight::zero();

        // Initialize ChildKeys for old_hotkey
        add_network(netuid, 1, 0);
        ChildKeys::<Test>::insert(old_hotkey, netuid, children.clone());

        // Perform the swap
        SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

        // Verify the swap
        assert_eq!(ChildKeys::<Test>::get(new_hotkey, netuid), children);
        assert!(ChildKeys::<Test>::get(old_hotkey, netuid).is_empty());
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_parent_keys --exact --nocapture
#[test]
fn test_swap_parent_keys() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid = 0u16;
        let parents = vec![(100u64, U256::from(4)), (200u64, U256::from(5))];
        let mut weight = Weight::zero();

        // Initialize ParentKeys for old_hotkey
        add_network(netuid, 1, 0);
        ParentKeys::<Test>::insert(old_hotkey, netuid, parents.clone());

        // Initialize ChildKeys for parent
        ChildKeys::<Test>::insert(U256::from(4), netuid, vec![(100u64, old_hotkey)]);
        ChildKeys::<Test>::insert(U256::from(5), netuid, vec![(200u64, old_hotkey)]);

        // Perform the swap
        SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

        // Verify ParentKeys swap
        assert_eq!(ParentKeys::<Test>::get(new_hotkey, netuid), parents);
        assert!(ParentKeys::<Test>::get(old_hotkey, netuid).is_empty());

        // Verify ChildKeys update for parents
        assert_eq!(
            ChildKeys::<Test>::get(U256::from(4), netuid),
            vec![(100u64, new_hotkey)]
        );
        assert_eq!(
            ChildKeys::<Test>::get(U256::from(5), netuid),
            vec![(200u64, new_hotkey)]
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_multiple_subnets --exact --nocapture
#[test]
fn test_swap_multiple_subnets() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid1 = 0u16;
        let netuid2 = 1u16;
        let children1 = vec![(100u64, U256::from(4)), (200u64, U256::from(5))];
        let children2 = vec![(300u64, U256::from(6))];
        let mut weight = Weight::zero();

        add_network(netuid1, 1, 0);
        add_network(netuid2, 1, 0);

        // Initialize ChildKeys for old_hotkey in multiple subnets
        ChildKeys::<Test>::insert(old_hotkey, netuid1, children1.clone());
        ChildKeys::<Test>::insert(old_hotkey, netuid2, children2.clone());

        // Perform the swap
        SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

        // Verify the swap for both subnets
        assert_eq!(ChildKeys::<Test>::get(new_hotkey, netuid1), children1);
        assert_eq!(ChildKeys::<Test>::get(new_hotkey, netuid2), children2);
        assert!(ChildKeys::<Test>::get(old_hotkey, netuid1).is_empty());
        assert!(ChildKeys::<Test>::get(old_hotkey, netuid2).is_empty());
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_hotkey -- test_swap_complex_parent_child_structure --exact --nocapture
#[test]
fn test_swap_complex_parent_child_structure() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let coldkey = U256::from(3);
        let netuid = 0u16;
        let parent1 = U256::from(4);
        let parent2 = U256::from(5);
        let child1 = U256::from(6);
        let child2 = U256::from(7);
        let mut weight = Weight::zero();

        add_network(netuid, 1, 0);

        // Set up complex parent-child structure
        ParentKeys::<Test>::insert(
            old_hotkey,
            netuid,
            vec![(100u64, parent1), (200u64, parent2)],
        );
        ChildKeys::<Test>::insert(old_hotkey, netuid, vec![(300u64, child1), (400u64, child2)]);
        ChildKeys::<Test>::insert(
            parent1,
            netuid,
            vec![(100u64, old_hotkey), (500u64, U256::from(8))],
        );
        ChildKeys::<Test>::insert(
            parent2,
            netuid,
            vec![(200u64, old_hotkey), (600u64, U256::from(9))],
        );

        // Perform the swap
        SubtensorModule::perform_hotkey_swap(&old_hotkey, &new_hotkey, &coldkey, &mut weight);

        // Verify ParentKeys swap
        assert_eq!(
            ParentKeys::<Test>::get(new_hotkey, netuid),
            vec![(100u64, parent1), (200u64, parent2)]
        );
        assert!(ParentKeys::<Test>::get(old_hotkey, netuid).is_empty());

        // Verify ChildKeys swap
        assert_eq!(
            ChildKeys::<Test>::get(new_hotkey, netuid),
            vec![(300u64, child1), (400u64, child2)]
        );
        assert!(ChildKeys::<Test>::get(old_hotkey, netuid).is_empty());

        // Verify parent's ChildKeys update
        assert_eq!(
            ChildKeys::<Test>::get(parent1, netuid),
            vec![(100u64, new_hotkey), (500u64, U256::from(8))]
        );
        assert_eq!(
            ChildKeys::<Test>::get(parent2, netuid),
            vec![(200u64, new_hotkey), (600u64, U256::from(9))]
        );
    });
}

#[test]
fn test_swap_parent_hotkey_childkey_maps() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let parent_old = U256::from(1);
        let coldkey = U256::from(2);
        let child = U256::from(3);
        let parent_new = U256::from(4);
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &parent_old);

        // Set child and verify state maps
        mock_set_children(&coldkey, &parent_old, netuid, &[(u64::MAX, child)]);
        assert_eq!(
            ParentKeys::<Test>::get(child, netuid),
            vec![(u64::MAX, parent_old)]
        );
        assert_eq!(
            ChildKeys::<Test>::get(parent_old, netuid),
            vec![(u64::MAX, child)]
        );

        // Swap
        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &parent_old,
            &parent_new,
            &coldkey,
            &mut weight
        ));

        // Verify parent and child keys updates
        assert_eq!(
            ParentKeys::<Test>::get(child, netuid),
            vec![(u64::MAX, parent_new)]
        );
        assert_eq!(
            ChildKeys::<Test>::get(parent_new, netuid),
            vec![(u64::MAX, child)]
        );
    })
}

#[test]
fn test_swap_child_hotkey_childkey_maps() {
    new_test_ext(1).execute_with(|| {
        let netuid: u16 = 1;
        let parent = U256::from(1);
        let coldkey = U256::from(2);
        let child_old = U256::from(3);
        let child_new = U256::from(4);
        add_network(netuid, 0, 0);
        SubtensorModule::create_account_if_non_existent(&coldkey, &child_old);
        SubtensorModule::create_account_if_non_existent(&coldkey, &parent);

        // Set child and verify state maps
        mock_set_children(&coldkey, &parent, netuid, &[(u64::MAX, child_old)]);

        assert_eq!(
            ParentKeys::<Test>::get(child_old, netuid),
            vec![(u64::MAX, parent)]
        );
        assert_eq!(
            ChildKeys::<Test>::get(parent, netuid),
            vec![(u64::MAX, child_old)]
        );

        // Swap
        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_hotkey_swap(
            &child_old,
            &child_new,
            &coldkey,
            &mut weight
        ));

        // Verify parent and child keys updates
        assert_eq!(
            ParentKeys::<Test>::get(child_new, netuid),
            vec![(u64::MAX, parent)]
        );
        assert_eq!(
            ChildKeys::<Test>::get(parent, netuid),
            vec![(u64::MAX, child_new)]
        );
    })
}
