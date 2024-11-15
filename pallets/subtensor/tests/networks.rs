use crate::mock::*;
use frame_support::assert_ok;
use frame_system::Config;
use pallet_subtensor::{ColdkeySwapScheduleDuration, DissolveNetworkScheduleDuration, Event};
use sp_core::U256;

mod mock;

#[test]
fn test_registration_ok() {
    new_test_ext(1).execute_with(|| {
        let block_number: u64 = 0;
        let netuid: u16 = 2;
        let tempo: u16 = 13;
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id = U256::from(0); // Neighbour of the beast, har har
        let (nonce, work): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            netuid,
            block_number,
            129123813,
            &hotkey_account_id,
        );

        //add network
        add_network(netuid, tempo, 0);

        assert_ok!(SubtensorModule::register(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid,
            block_number,
            nonce,
            work.clone(),
            hotkey_account_id,
            coldkey_account_id
        ));

        assert_ok!(SubtensorModule::user_remove_network(
            coldkey_account_id,
            netuid
        ));

        assert!(!SubtensorModule::if_subnet_exist(netuid))
    })
}

#[test]
fn test_schedule_dissolve_network_execution() {
    new_test_ext(1).execute_with(|| {
        let block_number: u64 = 0;
        let netuid: u16 = 2;
        let tempo: u16 = 13;
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id = U256::from(0); // Neighbour of the beast, har har
        let (nonce, work): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            netuid,
            block_number,
            129123813,
            &hotkey_account_id,
        );

        //add network
        add_network(netuid, tempo, 0);

        assert_ok!(SubtensorModule::register(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid,
            block_number,
            nonce,
            work.clone(),
            hotkey_account_id,
            coldkey_account_id
        ));

        assert!(SubtensorModule::if_subnet_exist(netuid));

        assert_ok!(SubtensorModule::schedule_dissolve_network(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            netuid
        ));

        let current_block = System::block_number();
        let execution_block = current_block + DissolveNetworkScheduleDuration::<Test>::get();

        System::assert_last_event(
            Event::DissolveNetworkScheduled {
                account: coldkey_account_id,
                netuid,
                execution_block,
            }
            .into(),
        );

        run_to_block(execution_block);
        assert!(!SubtensorModule::if_subnet_exist(netuid));
    })
}

#[test]
fn test_non_owner_schedule_dissolve_network_execution() {
    new_test_ext(1).execute_with(|| {
        let block_number: u64 = 0;
        let netuid: u16 = 2;
        let tempo: u16 = 13;
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id = U256::from(0); // Neighbour of the beast, har har
        let non_network_owner_account_id = U256::from(2); //
        let (nonce, work): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            netuid,
            block_number,
            129123813,
            &hotkey_account_id,
        );

        //add network
        add_network(netuid, tempo, 0);

        assert_ok!(SubtensorModule::register(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid,
            block_number,
            nonce,
            work.clone(),
            hotkey_account_id,
            coldkey_account_id
        ));

        assert!(SubtensorModule::if_subnet_exist(netuid));

        assert_ok!(SubtensorModule::schedule_dissolve_network(
            <<Test as Config>::RuntimeOrigin>::signed(non_network_owner_account_id),
            netuid
        ));

        let current_block = System::block_number();
        let execution_block = current_block + DissolveNetworkScheduleDuration::<Test>::get();

        System::assert_last_event(
            Event::DissolveNetworkScheduled {
                account: non_network_owner_account_id,
                netuid,
                execution_block,
            }
            .into(),
        );

        run_to_block(execution_block);
        // network exists since the caller is no the network owner
        assert!(SubtensorModule::if_subnet_exist(netuid));
    })
}

#[test]
fn test_new_owner_schedule_dissolve_network_execution() {
    new_test_ext(1).execute_with(|| {
        let block_number: u64 = 0;
        let netuid: u16 = 2;
        let tempo: u16 = 13;
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id = U256::from(0); // Neighbour of the beast, har har
        let new_network_owner_account_id = U256::from(2); //
        let (nonce, work): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            netuid,
            block_number,
            129123813,
            &hotkey_account_id,
        );

        //add network
        add_network(netuid, tempo, 0);

        assert_ok!(SubtensorModule::register(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid,
            block_number,
            nonce,
            work.clone(),
            hotkey_account_id,
            coldkey_account_id
        ));

        assert!(SubtensorModule::if_subnet_exist(netuid));

        // the account is not network owner when schedule the call
        assert_ok!(SubtensorModule::schedule_dissolve_network(
            <<Test as Config>::RuntimeOrigin>::signed(new_network_owner_account_id),
            netuid
        ));

        let current_block = System::block_number();
        let execution_block = current_block + DissolveNetworkScheduleDuration::<Test>::get();

        System::assert_last_event(
            Event::DissolveNetworkScheduled {
                account: new_network_owner_account_id,
                netuid,
                execution_block,
            }
            .into(),
        );
        run_to_block(current_block + 1);
        // become network owner after call scheduled
        pallet_subtensor::SubnetOwner::<Test>::insert(netuid, new_network_owner_account_id);

        run_to_block(execution_block);
        // network exists since the caller is no the network owner
        assert!(!SubtensorModule::if_subnet_exist(netuid));
    })
}

#[test]
fn test_schedule_dissolve_network_execution_with_coldkey_swap() {
    new_test_ext(1).execute_with(|| {
        let block_number: u64 = 0;
        let netuid: u16 = 2;
        let tempo: u16 = 13;
        let hotkey_account_id: U256 = U256::from(1);
        let coldkey_account_id = U256::from(0); // Neighbour of the beast, har har
        let new_network_owner_account_id = U256::from(2); //

        SubtensorModule::add_balance_to_coldkey_account(&coldkey_account_id, 1000000000000000);

        let (nonce, work): (u64, Vec<u8>) = SubtensorModule::create_work_for_block_number(
            netuid,
            block_number,
            129123813,
            &hotkey_account_id,
        );

        //add network
        add_network(netuid, tempo, 0);

        assert_ok!(SubtensorModule::register(
            <<Test as Config>::RuntimeOrigin>::signed(hotkey_account_id),
            netuid,
            block_number,
            nonce,
            work.clone(),
            hotkey_account_id,
            coldkey_account_id
        ));

        assert!(SubtensorModule::if_subnet_exist(netuid));

        // the account is not network owner when schedule the call
        assert_ok!(SubtensorModule::schedule_swap_coldkey(
            <<Test as Config>::RuntimeOrigin>::signed(coldkey_account_id),
            new_network_owner_account_id
        ));

        let current_block = System::block_number();
        let execution_block = current_block + ColdkeySwapScheduleDuration::<Test>::get();

        run_to_block(execution_block - 1);

        // the account is not network owner when schedule the call
        assert_ok!(SubtensorModule::schedule_dissolve_network(
            <<Test as Config>::RuntimeOrigin>::signed(new_network_owner_account_id),
            netuid
        ));

        System::assert_last_event(
            Event::DissolveNetworkScheduled {
                account: new_network_owner_account_id,
                netuid,
                execution_block: DissolveNetworkScheduleDuration::<Test>::get() + execution_block
                    - 1,
            }
            .into(),
        );

        run_to_block(execution_block);
        assert_eq!(
            pallet_subtensor::SubnetOwner::<Test>::get(netuid),
            new_network_owner_account_id
        );

        let current_block = System::block_number();
        let execution_block = current_block + DissolveNetworkScheduleDuration::<Test>::get();

        run_to_block(execution_block);
        // network exists since the caller is no the network owner
        assert!(!SubtensorModule::if_subnet_exist(netuid));
    })
}

#[test]
fn test_get_network_lock_cost_last_lock_block_zero() {
    new_test_ext(1).execute_with(|| {
        let last_lock = 0u64;
        let min_lock = 1000u64;
        let last_lock_block = 0u64;
        let current_block = 1u64;
        let lock_reduction_interval = 10u64;

        pallet_subtensor::NetworkLastLockCost::<Test>::put(last_lock);
        pallet_subtensor::NetworkMinLockCost::<Test>::put(min_lock);
        pallet_subtensor::NetworkLastRegistered::<Test>::put(last_lock_block);
        System::set_block_number(current_block);
        pallet_subtensor::NetworkLockReductionInterval::<Test>::put(lock_reduction_interval);

        // Expected values
        let mult = if last_lock_block == 0 { 1 } else { 2 };
        assert_eq!(mult, 1);

        let mut lock_cost = last_lock.saturating_mul(mult).saturating_sub(
            last_lock
                .saturating_div(lock_reduction_interval)
                .saturating_mul(current_block.saturating_sub(last_lock_block)),
        );

        if lock_cost < min_lock {
            lock_cost = min_lock;
        }

        let lock_cost_from_fn = SubtensorModule::get_network_lock_cost();

        assert_eq!(lock_cost_from_fn, lock_cost);
        assert_eq!(lock_cost_from_fn, min_lock);
    });
}

#[test]
fn test_get_network_lock_cost_last_lock_block_nonzero_no_time_passed() {
    new_test_ext(1).execute_with(|| {
        let last_lock = 2000u64;
        let min_lock = 1000u64;
        let last_lock_block = 1u64;
        let current_block = 1u64;
        let lock_reduction_interval = 10u64;

        pallet_subtensor::NetworkLastLockCost::<Test>::put(last_lock);
        pallet_subtensor::NetworkMinLockCost::<Test>::put(min_lock);
        pallet_subtensor::NetworkLastRegistered::<Test>::put(last_lock_block);
        System::set_block_number(current_block);
        pallet_subtensor::NetworkLockReductionInterval::<Test>::put(lock_reduction_interval);

        // Expected values
        let mult = if last_lock_block == 0 { 1 } else { 2 };
        assert_eq!(mult, 2);

        let mut lock_cost = last_lock.saturating_mul(mult).saturating_sub(
            last_lock
                .saturating_div(lock_reduction_interval)
                .saturating_mul(current_block.saturating_sub(last_lock_block)),
        );

        if lock_cost < min_lock {
            lock_cost = min_lock;
        }

        let lock_cost_from_fn = SubtensorModule::get_network_lock_cost();

        assert_eq!(lock_cost_from_fn, lock_cost);
        assert_eq!(lock_cost_from_fn, 4000u64);
    });
}

#[test]
fn test_get_network_lock_cost_time_passed_reduction() {
    new_test_ext(1).execute_with(|| {
        let last_lock = 2000u64;
        let min_lock = 1000u64;
        let last_lock_block = 1u64;
        let current_block = 11u64; // 10 blocks later
        let lock_reduction_interval = 10u64;

        pallet_subtensor::NetworkLastLockCost::<Test>::put(last_lock);
        pallet_subtensor::NetworkMinLockCost::<Test>::put(min_lock);
        pallet_subtensor::NetworkLastRegistered::<Test>::put(last_lock_block);
        System::set_block_number(current_block);
        pallet_subtensor::NetworkLockReductionInterval::<Test>::put(lock_reduction_interval);

        // Expected values
        let mult = if last_lock_block == 0 { 1 } else { 2 };
        assert_eq!(mult, 2);

        let reduction = last_lock
            .saturating_div(lock_reduction_interval)
            .saturating_mul(current_block.saturating_sub(last_lock_block));

        let mut lock_cost = last_lock.saturating_mul(mult).saturating_sub(reduction);

        if lock_cost < min_lock {
            lock_cost = min_lock;
        }

        let lock_cost_from_fn = SubtensorModule::get_network_lock_cost();

        assert_eq!(lock_cost_from_fn, lock_cost);
        assert_eq!(lock_cost_from_fn, 2000u64);
    });
}

#[test]
fn test_get_network_lock_cost_lock_cost_below_min_lock() {
    new_test_ext(1).execute_with(|| {
        let last_lock = 2000u64;
        let min_lock = 1000u64;
        let last_lock_block = 1u64;
        let current_block = 21u64; // 20 blocks later
        let lock_reduction_interval = 10u64;

        pallet_subtensor::NetworkLastLockCost::<Test>::put(last_lock);
        pallet_subtensor::NetworkMinLockCost::<Test>::put(min_lock);
        pallet_subtensor::NetworkLastRegistered::<Test>::put(last_lock_block);
        System::set_block_number(current_block);
        pallet_subtensor::NetworkLockReductionInterval::<Test>::put(lock_reduction_interval);

        // Expected values
        let mult = if last_lock_block == 0 { 1 } else { 2 };
        assert_eq!(mult, 2);

        let reduction = last_lock
            .saturating_div(lock_reduction_interval)
            .saturating_mul(current_block.saturating_sub(last_lock_block));

        let mut lock_cost = last_lock.saturating_mul(mult).saturating_sub(reduction);

        if lock_cost < min_lock {
            lock_cost = min_lock;
        }

        let lock_cost_from_fn = SubtensorModule::get_network_lock_cost();

        assert_eq!(lock_cost_from_fn, lock_cost);
        assert_eq!(lock_cost_from_fn, min_lock);
    });
}

#[test]
fn test_get_network_lock_cost_large_lock_reduction_interval() {
    new_test_ext(1).execute_with(|| {
        let last_lock = 2000u64;
        let min_lock = 1000u64;
        let last_lock_block = 1u64;
        let current_block = 100u64;
        let lock_reduction_interval = u64::MAX;

        pallet_subtensor::NetworkLastLockCost::<Test>::put(last_lock);
        pallet_subtensor::NetworkMinLockCost::<Test>::put(min_lock);
        pallet_subtensor::NetworkLastRegistered::<Test>::put(last_lock_block);
        System::set_block_number(current_block);
        pallet_subtensor::NetworkLockReductionInterval::<Test>::put(lock_reduction_interval);

        // Expected values
        let mult = if last_lock_block == 0 { 1 } else { 2 };
        assert_eq!(mult, 2);

        let reduction = last_lock
            .saturating_div(lock_reduction_interval)
            .saturating_mul(current_block.saturating_sub(last_lock_block));

        let mut lock_cost = last_lock.saturating_mul(mult).saturating_sub(reduction);

        if lock_cost < min_lock {
            lock_cost = min_lock;
        }

        let lock_cost_from_fn = SubtensorModule::get_network_lock_cost();

        assert_eq!(reduction, 0);
        assert_eq!(lock_cost_from_fn, lock_cost);
        assert_eq!(lock_cost_from_fn, 4000u64);
    });
}

#[test]
fn test_get_network_lock_cost_small_lock_reduction_interval() {
    new_test_ext(1).execute_with(|| {
        let last_lock = 2000u64;
        let min_lock = 1000u64;
        let last_lock_block = 1u64;
        let current_block = 3u64;
        let lock_reduction_interval = 1u64;

        pallet_subtensor::NetworkLastLockCost::<Test>::put(last_lock);
        pallet_subtensor::NetworkMinLockCost::<Test>::put(min_lock);
        pallet_subtensor::NetworkLastRegistered::<Test>::put(last_lock_block);
        System::set_block_number(current_block);
        pallet_subtensor::NetworkLockReductionInterval::<Test>::put(lock_reduction_interval);

        // Expected values
        let mult = if last_lock_block == 0 { 1 } else { 2 };
        assert_eq!(mult, 2);

        let reduction = last_lock
            .saturating_div(lock_reduction_interval)
            .saturating_mul(current_block.saturating_sub(last_lock_block));

        let mut lock_cost = last_lock.saturating_mul(mult).saturating_sub(reduction);

        if lock_cost < min_lock {
            lock_cost = min_lock;
        }

        let lock_cost_from_fn = SubtensorModule::get_network_lock_cost();

        assert_eq!(reduction, 2000 * 2);
        assert_eq!(lock_cost_from_fn, lock_cost);
        assert_eq!(lock_cost_from_fn, min_lock);
    });
}

#[test]
fn test_get_network_lock_cost_last_lock_zero_min_lock_zero() {
    new_test_ext(1).execute_with(|| {
        let last_lock = 0u64;
        let min_lock = 0u64;
        let last_lock_block = 0u64;
        let current_block = 1u64;
        let lock_reduction_interval = 10u64;

        pallet_subtensor::NetworkLastLockCost::<Test>::put(last_lock);
        pallet_subtensor::NetworkMinLockCost::<Test>::put(min_lock);
        pallet_subtensor::NetworkLastRegistered::<Test>::put(last_lock_block);
        System::set_block_number(current_block);
        pallet_subtensor::NetworkLockReductionInterval::<Test>::put(lock_reduction_interval);

        // Expected values
        let mult = if last_lock_block == 0 { 1 } else { 2 };
        assert_eq!(mult, 1);

        let reduction = last_lock
            .saturating_div(lock_reduction_interval)
            .saturating_mul(current_block.saturating_sub(last_lock_block));

        let mut lock_cost = last_lock.saturating_mul(mult).saturating_sub(reduction);

        if lock_cost < min_lock {
            lock_cost = min_lock;
        }

        let lock_cost_from_fn = SubtensorModule::get_network_lock_cost();

        assert_eq!(lock_cost_from_fn, lock_cost);
        assert_eq!(lock_cost_from_fn, 0u64);
    });
}

#[test]
fn test_get_network_lock_cost_large_last_lock_saturating_arithmetic() {
    new_test_ext(1).execute_with(|| {
        let last_lock = u64::MAX / 2;
        let min_lock = 1000u64;
        let last_lock_block = 0;
        let current_block = 3u64;
        let lock_reduction_interval = 1u64;

        pallet_subtensor::NetworkLastLockCost::<Test>::put(last_lock);
        pallet_subtensor::NetworkMinLockCost::<Test>::put(min_lock);
        pallet_subtensor::NetworkLastRegistered::<Test>::put(last_lock_block);
        System::set_block_number(current_block);
        pallet_subtensor::NetworkLockReductionInterval::<Test>::put(lock_reduction_interval);

        // Expected reduction calculation
        let reduction = last_lock
            .saturating_div(lock_reduction_interval)
            .saturating_mul(current_block.saturating_sub(last_lock_block));

        // Expected lock_cost calculation
        let mut lock_cost = last_lock.saturating_mul(2).saturating_sub(reduction);

        if lock_cost < min_lock {
            lock_cost = min_lock;
        }

        let lock_cost_from_fn = SubtensorModule::get_network_lock_cost();

        assert_eq!(reduction, u64::MAX);
        assert_eq!(lock_cost_from_fn, lock_cost);
        assert_eq!(lock_cost_from_fn, min_lock);
    });
}
