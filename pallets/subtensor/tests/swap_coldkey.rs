#![allow(unused, clippy::indexing_slicing, clippy::panic, clippy::unwrap_used)]
use codec::Encode;
use frame_support::weights::Weight;
use frame_support::{assert_err, assert_noop, assert_ok};
use frame_system::{Config, RawOrigin};
mod mock;
use frame_support::error::BadOrigin;
use frame_support::traits::schedule::v3::Named as ScheduleNamed;
use frame_support::traits::schedule::DispatchTime;
use frame_support::traits::OnInitialize;
use mock::*;
use pallet_subtensor::*;
use pallet_subtensor::{Call, ColdkeySwapScheduleDuration, Error};
use sp_core::H256;
use sp_core::U256;
use sp_runtime::DispatchError;

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_total_hotkey_coldkey_stakes_this_interval --exact --nocapture
#[test]
fn test_swap_total_hotkey_coldkey_stakes_this_interval() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey = U256::from(3);
        let stake = 100;
        let block = 42;

        OwnedHotkeys::<Test>::insert(old_coldkey, vec![hotkey]);
        TotalHotkeyColdkeyStakesThisInterval::<Test>::insert(hotkey, old_coldkey, (stake, block));

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert!(!TotalHotkeyColdkeyStakesThisInterval::<Test>::contains_key(
            hotkey,
            old_coldkey
        ));
        assert_eq!(
            TotalHotkeyColdkeyStakesThisInterval::<Test>::get(hotkey, new_coldkey),
            (stake, block)
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_subnet_owner --exact --nocapture
#[test]
fn test_swap_subnet_owner() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let netuid = 1u16;

        add_network(netuid, 1, 0);
        SubnetOwner::<Test>::insert(netuid, old_coldkey);

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert_eq!(SubnetOwner::<Test>::get(netuid), new_coldkey);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_stake --exact --nocapture
#[test]
fn test_swap_stake() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey = U256::from(3);
        let stake = 100;

        StakingHotkeys::<Test>::insert(old_coldkey, vec![hotkey]);
        Stake::<Test>::insert(hotkey, old_coldkey, stake);
        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert!(!Stake::<Test>::contains_key(hotkey, old_coldkey));
        assert_eq!(Stake::<Test>::get(hotkey, new_coldkey), stake);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_staking_hotkeys --exact --nocapture
#[test]
fn test_swap_staking_hotkeys() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey = U256::from(3);

        StakingHotkeys::<Test>::insert(old_coldkey, vec![hotkey]);

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert!(StakingHotkeys::<Test>::get(old_coldkey).is_empty());
        assert_eq!(StakingHotkeys::<Test>::get(new_coldkey), vec![hotkey]);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_hotkey_owners --exact --nocapture
#[test]
fn test_swap_hotkey_owners() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey = U256::from(3);

        Owner::<Test>::insert(hotkey, old_coldkey);
        OwnedHotkeys::<Test>::insert(old_coldkey, vec![hotkey]);

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert_eq!(Owner::<Test>::get(hotkey), new_coldkey);
        assert!(OwnedHotkeys::<Test>::get(old_coldkey).is_empty());
        assert_eq!(OwnedHotkeys::<Test>::get(new_coldkey), vec![hotkey]);
    });
}
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_transfer_remaining_balance --exact --nocapture
#[test]
fn test_transfer_remaining_balance() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let balance = 100;

        SubtensorModule::add_balance_to_coldkey_account(&old_coldkey, balance);

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert_eq!(SubtensorModule::get_coldkey_balance(&old_coldkey), 0);
        assert_eq!(SubtensorModule::get_coldkey_balance(&new_coldkey), balance);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_with_multiple_hotkeys --exact --nocapture
#[test]
fn test_swap_with_multiple_hotkeys() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey1 = U256::from(3);
        let hotkey2 = U256::from(4);

        OwnedHotkeys::<Test>::insert(old_coldkey, vec![hotkey1, hotkey2]);

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert!(OwnedHotkeys::<Test>::get(old_coldkey).is_empty());
        assert_eq!(
            OwnedHotkeys::<Test>::get(new_coldkey),
            vec![hotkey1, hotkey2]
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_with_multiple_subnets --exact --nocapture
#[test]
fn test_swap_with_multiple_subnets() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let netuid1 = 1u16;
        let netuid2 = 2u16;

        add_network(netuid1, 1, 0);
        add_network(netuid2, 1, 0);
        SubnetOwner::<Test>::insert(netuid1, old_coldkey);
        SubnetOwner::<Test>::insert(netuid2, old_coldkey);

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert_eq!(SubnetOwner::<Test>::get(netuid1), new_coldkey);
        assert_eq!(SubnetOwner::<Test>::get(netuid2), new_coldkey);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_with_zero_balance --exact --nocapture
#[test]
fn test_swap_with_zero_balance() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert_eq!(Balances::free_balance(old_coldkey), 0);
        assert_eq!(Balances::free_balance(new_coldkey), 0);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_with_max_hotkeys --exact --nocapture
#[test]
fn test_swap_with_max_hotkeys() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let max_hotkeys = 1000;
        let hotkeys: Vec<U256> = (0..max_hotkeys).map(U256::from).collect();

        OwnedHotkeys::<Test>::insert(old_coldkey, hotkeys.clone());

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert!(OwnedHotkeys::<Test>::get(old_coldkey).is_empty());
        assert_eq!(OwnedHotkeys::<Test>::get(new_coldkey), hotkeys);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_effect_on_delegated_stake --exact --nocapture
#[test]
fn test_swap_effect_on_delegated_stake() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let delegator = U256::from(3);
        let hotkey = U256::from(4);
        let stake = 100;

        StakingHotkeys::<Test>::insert(old_coldkey, vec![hotkey]);
        StakingHotkeys::<Test>::insert(delegator, vec![hotkey]);
        Stake::<Test>::insert(hotkey, old_coldkey, stake);
        Stake::<Test>::insert(hotkey, delegator, stake);

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert_eq!(Stake::<Test>::get(hotkey, new_coldkey), stake);
        assert_eq!(Stake::<Test>::get(hotkey, delegator), stake);
        assert_eq!(Stake::<Test>::get(hotkey, old_coldkey), 0);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_concurrent_modifications --exact --nocapture
#[test]
fn test_swap_concurrent_modifications() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey = U256::from(3);
        let netuid: u16 = 1;
        let initial_stake = 100;
        let additional_stake = 50;

        StakingHotkeys::<Test>::insert(old_coldkey, vec![hotkey]);
        Stake::<Test>::insert(hotkey, old_coldkey, initial_stake);

        // Simulate concurrent stake addition
        add_network(netuid, 1, 1);
        SubtensorModule::add_balance_to_coldkey_account(&new_coldkey, additional_stake);
        register_ok_neuron(netuid, hotkey, new_coldkey, 1001000);
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(new_coldkey),
            hotkey,
            netuid,
            additional_stake
        ));

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        assert_eq!(
            Stake::<Test>::get(hotkey, new_coldkey),
            initial_stake + additional_stake - 1
        );
        assert!(!Stake::<Test>::contains_key(hotkey, old_coldkey));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_with_invalid_subnet_ownership --exact --nocapture
#[test]
fn test_swap_with_invalid_subnet_ownership() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let netuid = 1u16;

        SubnetOwner::<Test>::insert(netuid, old_coldkey);

        // Simulate an invalid state where the subnet owner doesn't match the old_coldkey
        SubnetOwner::<Test>::insert(netuid, U256::from(3));

        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
            &mut weight
        ));

        // The swap should not affect the mismatched subnet ownership
        assert_eq!(SubnetOwner::<Test>::get(netuid), U256::from(3));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_do_swap_coldkey_success --exact --nocapture
#[test]
fn test_do_swap_coldkey_success() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey1 = U256::from(3);
        let hotkey2 = U256::from(4);
        let netuid = 1u16;
        let stake_amount1 = 1000u64;
        let stake_amount2 = 2000u64;
        let swap_cost = SubtensorModule::get_key_swap_cost();
        let free_balance_old = 12345u64 + swap_cost;

        // Setup initial state
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey1, old_coldkey, 0);
        register_ok_neuron(netuid, hotkey2, old_coldkey, 0);

        // Add balance to old coldkey
        SubtensorModule::add_balance_to_coldkey_account(
            &old_coldkey,
            stake_amount1 + stake_amount2 + free_balance_old,
        );

        // Log initial state
        log::info!(
            "Initial total stake: {}",
            SubtensorModule::get_total_stake()
        );
        log::info!(
            "Initial old coldkey stake: {}",
            SubtensorModule::get_stake_for_coldkey_on_subnet(&old_coldkey, netuid)
        );
        log::info!(
            "Initial new coldkey stake: {}",
            SubtensorModule::get_stake_for_coldkey_on_subnet(&new_coldkey, netuid)
        );

        // Add stake to the neurons
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            hotkey1,
            netuid,
            stake_amount1
        ));
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            hotkey2,
            netuid,
            stake_amount2
        ));

        // Insert an Identity
        let name: Vec<u8> = b"The fourth Coolest Identity".to_vec();
        let identity: ChainIdentity = ChainIdentity {
            name: name.clone(),
            url: vec![],
            image: vec![],
            discord: vec![],
            description: vec![],
            additional: vec![],
        };

        Identities::<Test>::insert(old_coldkey, identity.clone());

        assert!(Identities::<Test>::get(old_coldkey).is_some());
        assert!(Identities::<Test>::get(new_coldkey).is_none());

        // Log state after adding stake
        log::info!(
            "Total stake after adding: {}",
            SubtensorModule::get_total_stake()
        );
        log::info!(
            "Old coldkey stake after adding: {}",
            SubtensorModule::get_stake_for_coldkey_on_subnet(&old_coldkey, netuid)
        );
        log::info!(
            "New coldkey stake after adding: {}",
            SubtensorModule::get_stake_for_coldkey_on_subnet(&new_coldkey, netuid)
        );

        // Record total stake before swap
        let total_stake_before_swap = SubtensorModule::get_total_stake();

        // Perform the swap
        assert_ok!(SubtensorModule::do_swap_coldkey(
            // <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            &old_coldkey,
            &new_coldkey
        ));

        // Log state after swap
        log::info!(
            "Total stake after swap: {}",
            SubtensorModule::get_total_stake()
        );
        log::info!(
            "Old coldkey stake after swap: {}",
            SubtensorModule::get_stake_for_coldkey_on_subnet(&old_coldkey, netuid)
        );
        log::info!(
            "New coldkey stake after swap: {}",
            SubtensorModule::get_stake_for_coldkey_on_subnet(&new_coldkey, netuid)
        );

        // Verify the swap
        assert_eq!(Owner::<Test>::get(hotkey1), new_coldkey);
        assert_eq!(Owner::<Test>::get(hotkey2), new_coldkey);
        assert_eq!(Stake::<Test>::get(hotkey1, new_coldkey), stake_amount1);
        assert_eq!(Stake::<Test>::get(hotkey2, new_coldkey), stake_amount2);
        assert!(!Stake::<Test>::contains_key(hotkey1, old_coldkey));
        assert!(!Stake::<Test>::contains_key(hotkey2, old_coldkey));

        // Verify OwnedHotkeys
        let new_owned_hotkeys = OwnedHotkeys::<Test>::get(new_coldkey);
        assert!(new_owned_hotkeys.contains(&hotkey1));
        assert!(new_owned_hotkeys.contains(&hotkey2));
        assert_eq!(new_owned_hotkeys.len(), 2);
        assert!(!OwnedHotkeys::<Test>::contains_key(old_coldkey));

        // Verify balance transfer
        assert_eq!(
            SubtensorModule::get_coldkey_balance(&new_coldkey),
            free_balance_old - swap_cost
        );
        assert_eq!(SubtensorModule::get_coldkey_balance(&old_coldkey), 0);

        // Verify total stake remains unchanged
        assert_eq!(
            SubtensorModule::get_total_stake(),
            total_stake_before_swap,
            "Total stake changed unexpectedly"
        );

        // Verify identities were swapped
        assert!(Identities::<Test>::get(old_coldkey).is_none());
        assert!(Identities::<Test>::get(new_coldkey).is_some());
        assert_eq!(
            Identities::<Test>::get(new_coldkey).expect("Expected an Identity"),
            identity
        );

        // Verify event emission
        System::assert_last_event(
            Event::ColdkeySwapped {
                old_coldkey,
                new_coldkey,
            }
            .into(),
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap -- test_swap_stake_for_coldkey --exact --nocaptur
#[test]
fn test_swap_stake_for_coldkey() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey1 = U256::from(3);
        let hotkey2 = U256::from(4);
        let stake_amount1 = 1000u64;
        let stake_amount2 = 2000u64;
        let stake_amount3 = 3000u64;
        let total_stake = stake_amount1 + stake_amount2;
        let mut weight = Weight::zero();

        // Setup initial state
        OwnedHotkeys::<Test>::insert(old_coldkey, vec![hotkey1, hotkey2]);
        StakingHotkeys::<Test>::insert(old_coldkey, vec![hotkey1, hotkey2]);
        Stake::<Test>::insert(hotkey1, old_coldkey, stake_amount1);
        Stake::<Test>::insert(hotkey2, old_coldkey, stake_amount2);
        assert_eq!(Stake::<Test>::get(hotkey1, old_coldkey), stake_amount1);
        assert_eq!(Stake::<Test>::get(hotkey1, old_coldkey), stake_amount1);

        // Insert existing for same hotkey1
        Stake::<Test>::insert(hotkey1, new_coldkey, stake_amount3);
        StakingHotkeys::<Test>::insert(new_coldkey, vec![hotkey1]);

        // Set up total issuance
        TotalIssuance::<Test>::put(total_stake);
        TotalStake::<Test>::put(total_stake);

        // Record initial values
        let initial_total_issuance = SubtensorModule::get_total_issuance();
        let initial_total_stake = SubtensorModule::get_total_stake();

        // Perform the swap
        SubtensorModule::perform_swap_coldkey(&old_coldkey, &new_coldkey, &mut weight);

        // Verify stake is additive, not replaced
        assert_eq!(
            Stake::<Test>::get(hotkey1, new_coldkey),
            stake_amount1 + stake_amount3
        );

        // Verify ownership transfer
        assert_eq!(
            SubtensorModule::get_owned_hotkeys(&new_coldkey),
            vec![hotkey1, hotkey2]
        );
        assert_eq!(SubtensorModule::get_owned_hotkeys(&old_coldkey), vec![]);

        // Verify stake transfer
        assert_eq!(Stake::<Test>::get(hotkey2, new_coldkey), stake_amount2);
        assert_eq!(Stake::<Test>::get(hotkey1, old_coldkey), 0);
        assert_eq!(Stake::<Test>::get(hotkey2, old_coldkey), 0);

        // Verify total stake and issuance remain unchanged
        assert_eq!(
            SubtensorModule::get_total_stake(),
            initial_total_stake,
            "Total stake changed unexpectedly"
        );
        assert_eq!(
            SubtensorModule::get_total_issuance(),
            initial_total_issuance,
            "Total issuance changed unexpectedly"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_swap_staking_hotkeys_for_coldkey --exact --nocapture
#[test]
fn test_swap_staking_hotkeys_for_coldkey() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey1 = U256::from(3);
        let hotkey2 = U256::from(4);
        let stake_amount1 = 1000u64;
        let stake_amount2 = 2000u64;
        let total_stake = stake_amount1 + stake_amount2;
        let mut weight = Weight::zero();

        // Setup initial state
        OwnedHotkeys::<Test>::insert(old_coldkey, vec![hotkey1, hotkey2]);
        Stake::<Test>::insert(hotkey1, old_coldkey, stake_amount1);
        Stake::<Test>::insert(hotkey2, old_coldkey, stake_amount2);
        StakingHotkeys::<Test>::insert(old_coldkey, vec![hotkey1, hotkey2]);

        // Set up total issuance
        TotalIssuance::<Test>::put(total_stake);
        TotalStake::<Test>::put(total_stake);

        // Perform the swap
        SubtensorModule::perform_swap_coldkey(&old_coldkey, &new_coldkey, &mut weight);

        // Verify StakingHotkeys transfer
        assert_eq!(
            StakingHotkeys::<Test>::get(new_coldkey),
            vec![hotkey1, hotkey2]
        );
        assert_eq!(StakingHotkeys::<Test>::get(old_coldkey), vec![]);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_swap_delegated_stake_for_coldkey --exact --nocapture
#[test]
fn test_swap_delegated_stake_for_coldkey() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey1 = U256::from(3);
        let hotkey2 = U256::from(4);
        let stake_amount1 = 1000u64;
        let stake_amount2 = 2000u64;
        let total_stake = stake_amount1 + stake_amount2;
        let mut weight = Weight::zero();

        // Notice hotkey1 and hotkey2 are not in OwnedHotkeys
        // coldkey therefore delegates stake to them

        // Setup initial state
        StakingHotkeys::<Test>::insert(old_coldkey, vec![hotkey1, hotkey2]);
        Stake::<Test>::insert(hotkey1, old_coldkey, stake_amount1);
        Stake::<Test>::insert(hotkey2, old_coldkey, stake_amount2);

        // Set up total issuance
        TotalIssuance::<Test>::put(total_stake);
        TotalStake::<Test>::put(total_stake);

        // Record initial values
        let initial_total_issuance = SubtensorModule::get_total_issuance();
        let initial_total_stake = SubtensorModule::get_total_stake();

        // Perform the swap
        SubtensorModule::perform_swap_coldkey(&old_coldkey, &new_coldkey, &mut weight);

        // Verify stake transfer
        assert_eq!(Stake::<Test>::get(hotkey1, new_coldkey), stake_amount1);
        assert_eq!(Stake::<Test>::get(hotkey2, new_coldkey), stake_amount2);
        assert_eq!(Stake::<Test>::get(hotkey1, old_coldkey), 0);
        assert_eq!(Stake::<Test>::get(hotkey2, old_coldkey), 0);

        // Verify total stake and issuance remain unchanged
        assert_eq!(
            SubtensorModule::get_total_stake(),
            initial_total_stake,
            "Total stake changed unexpectedly"
        );
        assert_eq!(
            SubtensorModule::get_total_issuance(),
            initial_total_issuance,
            "Total issuance changed unexpectedly"
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_swap_total_hotkey_coldkey_stakes_this_interval_for_coldkey --exact --nocapture
#[test]
fn test_swap_total_hotkey_coldkey_stakes_this_interval_for_coldkey() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey1 = U256::from(3);
        let hotkey2 = U256::from(4);
        let stake1 = (1000u64, 100u64);
        let stake2 = (2000u64, 200u64);
        let mut weight = Weight::zero();

        // Initialize TotalHotkeyColdkeyStakesThisInterval for old_coldkey
        TotalHotkeyColdkeyStakesThisInterval::<Test>::insert(hotkey1, old_coldkey, stake1);
        TotalHotkeyColdkeyStakesThisInterval::<Test>::insert(hotkey2, old_coldkey, stake2);

        // Populate OwnedHotkeys map
        OwnedHotkeys::<Test>::insert(old_coldkey, vec![hotkey1, hotkey2]);

        // Perform the swap
        SubtensorModule::perform_swap_coldkey(&old_coldkey, &new_coldkey, &mut weight);

        // Verify the swap
        assert_eq!(
            TotalHotkeyColdkeyStakesThisInterval::<Test>::get(hotkey1, new_coldkey),
            stake1
        );
        assert_eq!(
            TotalHotkeyColdkeyStakesThisInterval::<Test>::get(hotkey2, new_coldkey),
            stake2
        );
        assert!(!TotalHotkeyColdkeyStakesThisInterval::<Test>::contains_key(
            old_coldkey,
            hotkey1
        ));
        assert!(!TotalHotkeyColdkeyStakesThisInterval::<Test>::contains_key(
            old_coldkey,
            hotkey2
        ));
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_swap_subnet_owner_for_coldkey --exact --nocapture
#[test]
fn test_swap_subnet_owner_for_coldkey() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let netuid1 = 1u16;
        let netuid2 = 2u16;
        let mut weight = Weight::zero();

        // Initialize SubnetOwner for old_coldkey
        add_network(netuid1, 13, 0);
        add_network(netuid2, 14, 0);
        SubnetOwner::<Test>::insert(netuid1, old_coldkey);
        SubnetOwner::<Test>::insert(netuid2, old_coldkey);

        // Set up TotalNetworks
        TotalNetworks::<Test>::put(3);

        // Perform the swap
        SubtensorModule::perform_swap_coldkey(&old_coldkey, &new_coldkey, &mut weight);

        // Verify the swap
        assert_eq!(SubnetOwner::<Test>::get(netuid1), new_coldkey);
        assert_eq!(SubnetOwner::<Test>::get(netuid2), new_coldkey);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_do_swap_coldkey_with_subnet_ownership --exact --nocapture
#[test]
fn test_do_swap_coldkey_with_subnet_ownership() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey = U256::from(3);
        let netuid = 1u16;
        let stake_amount: u64 = 1000u64;
        let swap_cost = SubtensorModule::get_key_swap_cost();

        // Setup initial state
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, old_coldkey, 0);

        // Set TotalNetworks because swap relies on it
        pallet_subtensor::TotalNetworks::<Test>::set(1);

        SubtensorModule::add_balance_to_coldkey_account(&old_coldkey, stake_amount + swap_cost);
        SubnetOwner::<Test>::insert(netuid, old_coldkey);

        // Populate OwnedHotkeys map
        OwnedHotkeys::<Test>::insert(old_coldkey, vec![hotkey]);

        // Perform the swap
        assert_ok!(SubtensorModule::do_swap_coldkey(&old_coldkey, &new_coldkey));

        // Verify subnet ownership transfer
        assert_eq!(SubnetOwner::<Test>::get(netuid), new_coldkey);
    });
}
// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_coldkey_has_associated_hotkeys --exact --nocapture
#[test]
fn test_coldkey_has_associated_hotkeys() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid = 1u16;

        // Setup initial state
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, coldkey, 0);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, 1000);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_swap_senate_member --exact --nocapture
#[test]
fn test_swap_senate_member() {
    new_test_ext(1).execute_with(|| {
        let old_hotkey = U256::from(1);
        let new_hotkey = U256::from(2);
        let non_member_hotkey = U256::from(3);
        let mut weight = Weight::zero();

        // Setup: Add old_hotkey as a Senate member
        assert_ok!(SenateMembers::add_member(
            RawOrigin::Root.into(),
            old_hotkey
        ));

        // Test 1: Successful swap
        assert_ok!(SubtensorModule::swap_senate_member(
            &old_hotkey,
            &new_hotkey,
            &mut weight
        ));
        assert!(Senate::is_member(&new_hotkey));
        assert!(!Senate::is_member(&old_hotkey));

        // Verify weight update
        let expected_weight = <Test as frame_system::Config>::DbWeight::get().reads_writes(2, 2);
        assert_eq!(weight, expected_weight);

        // Reset weight for next test
        weight = Weight::zero();

        // Test 2: Swap with non-member (should not change anything)
        assert_ok!(SubtensorModule::swap_senate_member(
            &non_member_hotkey,
            &new_hotkey,
            &mut weight
        ));
        assert!(Senate::is_member(&new_hotkey));
        assert!(!Senate::is_member(&non_member_hotkey));

        // Verify weight update (should only have read operations)
        let expected_weight = <Test as frame_system::Config>::DbWeight::get().reads(1);
        assert_eq!(weight, expected_weight);
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_coldkey_delegations --exact --nocapture
#[test]
fn test_coldkey_delegations() {
    new_test_ext(1).execute_with(|| {
        let new_coldkey = U256::from(0);
        let owner = U256::from(1);
        let coldkey = U256::from(4);
        let delegate = U256::from(2);
        let netuid = 1u16;
        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, delegate, owner, 0);
        SubtensorModule::add_balance_to_coldkey_account(&coldkey, 1000);
        assert_ok!(SubtensorModule::do_become_delegate(
            <<Test as Config>::RuntimeOrigin>::signed(owner),
            delegate,
            u16::MAX / 10
        ));
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey),
            delegate,
            netuid,
            100
        ));
        let mut weight = Weight::zero();
        assert_ok!(SubtensorModule::perform_swap_coldkey(
            &coldkey,
            &new_coldkey,
            &mut weight
        ));
        assert_eq!(
            SubtensorModule::get_stake_for_hotkey_on_subnet(&delegate, netuid),
            100
        );
        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_on_subnet(&coldkey, netuid),
            0
        );
        assert_eq!(
            SubtensorModule::get_stake_for_coldkey_on_subnet(&new_coldkey, netuid),
            100
        );
        assert_eq!(Stake::<Test>::get(delegate, new_coldkey), 100);
        assert_eq!(Stake::<Test>::get(delegate, coldkey), 0);
    });
}

#[test]
fn test_schedule_swap_coldkey_success() {
    new_test_ext(1).execute_with(|| {
        // Initialize test accounts
        let old_coldkey: U256 = U256::from(1);
        let new_coldkey: U256 = U256::from(2);

        // Add balance to the old coldkey account
        SubtensorModule::add_balance_to_coldkey_account(&old_coldkey, 1000);

        // Schedule the coldkey swap
        assert_ok!(SubtensorModule::schedule_swap_coldkey(
            <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            new_coldkey
        ));

        // Get the current block number
        let current_block: u64 = System::block_number();

        // Calculate the expected execution block (5 days from now)
        let expected_execution_block: u64 = current_block + 5 * 24 * 60 * 60 / 12;

        // Check for the SwapScheduled event
        System::assert_last_event(
            Event::ColdkeySwapScheduled {
                old_coldkey,
                new_coldkey,
                execution_block: expected_execution_block,
            }
            .into(),
        );

        // TODO: Add additional checks to ensure the swap is correctly scheduled in the system
        // For example, verify that the swap is present in the appropriate storage or scheduler
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_schedule_swap_coldkey_duplicate --exact --nocapture
#[test]
fn test_schedule_swap_coldkey_duplicate() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);

        SubtensorModule::add_balance_to_coldkey_account(&old_coldkey, 2000);

        assert_ok!(SubtensorModule::schedule_swap_coldkey(
            <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            new_coldkey
        ));

        // Attempt to schedule again
        assert_noop!(
            SubtensorModule::schedule_swap_coldkey(
                <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
                new_coldkey
            ),
            Error::<Test>::SwapAlreadyScheduled
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_schedule_swap_coldkey_execution --exact --nocapture
#[test]
fn test_schedule_swap_coldkey_execution() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey = U256::from(3);
        let netuid = 1u16;
        let stake_amount = 100;

        add_network(netuid, 13, 0);
        register_ok_neuron(netuid, hotkey, old_coldkey, 0);
        SubtensorModule::add_balance_to_coldkey_account(&old_coldkey, 1000000000000000);
        assert_ok!(SubtensorModule::add_stake(
            <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            hotkey,
            netuid,
            stake_amount,
        ));

        // Check initial ownership
        assert_eq!(
            Owner::<Test>::get(hotkey),
            old_coldkey,
            "Initial ownership check failed"
        );

        // Schedule the swap
        assert_ok!(SubtensorModule::schedule_swap_coldkey(
            <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            new_coldkey
        ));

        // Get the scheduled execution block
        let current_block = System::block_number();
        let execution_block = current_block + ColdkeySwapScheduleDuration::<Test>::get();

        System::assert_last_event(
            Event::ColdkeySwapScheduled {
                old_coldkey,
                new_coldkey,
                execution_block,
            }
            .into(),
        );

        run_to_block(execution_block);

        // Run on_initialize for the execution block
        SubtensorModule::on_initialize(execution_block);

        // Also run Scheduler's on_initialize
        <pallet_scheduler::Pallet<Test> as OnInitialize<BlockNumber>>::on_initialize(
            execution_block,
        );

        // Check if the swap has occurred
        let new_owner = Owner::<Test>::get(hotkey);
        assert_eq!(
            new_owner, new_coldkey,
            "Ownership was not updated as expected"
        );

        assert_eq!(
            Stake::<Test>::get(hotkey, new_coldkey),
            stake_amount,
            "Stake was not transferred to new coldkey"
        );
        assert_eq!(
            Stake::<Test>::get(hotkey, old_coldkey),
            0,
            "Old coldkey still has stake"
        );

        // Check for the SwapExecuted event
        System::assert_has_event(
            Event::ColdkeySwapped {
                old_coldkey,
                new_coldkey,
            }
            .into(),
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_direct_swap_coldkey_call_fails --exact --nocapture
#[test]
fn test_direct_swap_coldkey_call_fails() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);

        assert_noop!(
            SubtensorModule::swap_coldkey(
                <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
                old_coldkey,
                new_coldkey
            ),
            BadOrigin
        );
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=info cargo test --test swap_coldkey -- test_schedule_swap_coldkey_with_pending_swap --exact --nocapture
#[test]
fn test_schedule_swap_coldkey_with_pending_swap() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey1 = U256::from(2);
        let new_coldkey2 = U256::from(3);

        SubtensorModule::add_balance_to_coldkey_account(&old_coldkey, 2000);

        assert_ok!(SubtensorModule::schedule_swap_coldkey(
            <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            new_coldkey1
        ));

        // Attempt to schedule another swap before the first one executes
        assert_noop!(
            SubtensorModule::schedule_swap_coldkey(
                <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
                new_coldkey2
            ),
            Error::<Test>::SwapAlreadyScheduled
        );
    });
}

#[test]
fn test_coldkey_swap_delegate_identity_updated() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);

        let netuid = 1;
        let burn_cost = 10;
        let tempo = 1;

        SubtensorModule::set_burn(netuid, burn_cost);
        add_network(netuid, tempo, 0);

        SubtensorModule::add_balance_to_coldkey_account(&old_coldkey, 100_000_000_000);

        assert_ok!(SubtensorModule::burned_register(
            <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            netuid,
            old_coldkey
        ));

        let name: Vec<u8> = b"The Third Coolest Identity".to_vec();
        let identity: ChainIdentity = ChainIdentity {
            name: name.clone(),
            url: vec![],
            image: vec![],
            discord: vec![],
            description: vec![],
            additional: vec![],
        };

        Identities::<Test>::insert(old_coldkey, identity.clone());

        assert!(Identities::<Test>::get(old_coldkey).is_some());
        assert!(Identities::<Test>::get(new_coldkey).is_none());

        assert_ok!(SubtensorModule::do_swap_coldkey(&old_coldkey, &new_coldkey));

        assert!(Identities::<Test>::get(old_coldkey).is_none());
        assert!(Identities::<Test>::get(new_coldkey).is_some());
        assert_eq!(
            Identities::<Test>::get(new_coldkey).expect("Expected an Identity"),
            identity
        );
    });
}

#[test]
fn test_coldkey_swap_no_identity_no_changes() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);

        let netuid = 1;
        let burn_cost = 10;
        let tempo = 1;

        SubtensorModule::set_burn(netuid, burn_cost);
        add_network(netuid, tempo, 0);

        SubtensorModule::add_balance_to_coldkey_account(&old_coldkey, 100_000_000_000);

        assert_ok!(SubtensorModule::burned_register(
            <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            netuid,
            old_coldkey
        ));

        // Ensure the old coldkey does not have an identity before the swap
        assert!(Identities::<Test>::get(old_coldkey).is_none());

        // Perform the coldkey swap
        assert_ok!(SubtensorModule::do_swap_coldkey(&old_coldkey, &new_coldkey));

        // Ensure no identities have been changed
        assert!(Identities::<Test>::get(old_coldkey).is_none());
        assert!(Identities::<Test>::get(new_coldkey).is_none());
    });
}

#[test]
fn test_coldkey_swap_no_identity_no_changes_newcoldkey_exists() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(3);
        let new_coldkey = U256::from(4);

        let netuid = 1;
        let burn_cost = 10;
        let tempo = 1;

        SubtensorModule::set_burn(netuid, burn_cost);
        add_network(netuid, tempo, 0);
        SubtensorModule::add_balance_to_coldkey_account(&old_coldkey, 100_000_000_000);

        assert_ok!(SubtensorModule::burned_register(
            <<Test as Config>::RuntimeOrigin>::signed(old_coldkey),
            netuid,
            old_coldkey
        ));

        let name: Vec<u8> = b"The Coolest Identity".to_vec();
        let identity: ChainIdentity = ChainIdentity {
            name: name.clone(),
            url: vec![],
            image: vec![],
            discord: vec![],
            description: vec![],
            additional: vec![],
        };

        Identities::<Test>::insert(new_coldkey, identity.clone());
        // Ensure the new coldkey does have an identity before the swap
        assert!(Identities::<Test>::get(new_coldkey).is_some());
        assert!(Identities::<Test>::get(old_coldkey).is_none());

        // Perform the coldkey swap
        assert_ok!(SubtensorModule::do_swap_coldkey(&old_coldkey, &new_coldkey));

        // Ensure no identities have been changed
        assert!(Identities::<Test>::get(old_coldkey).is_none());
        assert!(Identities::<Test>::get(new_coldkey).is_some());
    });
}

// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test swap_coldkey -- test_swap_coldkey_locks --exact --nocapture
#[test]
fn test_swap_coldkey_locks() {
    new_test_ext(1).execute_with(|| {
        let old_coldkey = U256::from(1);
        let new_coldkey = U256::from(2);
        let hotkey = U256::from(3);
        let netuid = 1;
        let lock_amount = 1000;
        let start_block = 100;
        let end_block = 200;

        // Set up initial locks
        Locks::<Test>::insert(
            (netuid, hotkey, old_coldkey),
            (lock_amount, start_block, end_block),
        );

        // Ensure the old coldkey has enough balance for the swap
        SubtensorModule::add_balance_to_coldkey_account(&old_coldkey, 100_000_000_000);

        // Perform the coldkey swap
        assert_ok!(SubtensorModule::do_swap_coldkey(
            &old_coldkey,
            &new_coldkey,
        ));

        // Verify that the lock has been transferred to the new coldkey
        assert!(Locks::<Test>::contains_key((netuid, hotkey, new_coldkey)));
        assert_eq!(
            Locks::<Test>::get((netuid, hotkey, new_coldkey)),
            (lock_amount, start_block, end_block)
        );

        // Verify that the lock has been removed from the old coldkey
        assert!(!Locks::<Test>::contains_key((netuid, hotkey, old_coldkey)));
    });
}
