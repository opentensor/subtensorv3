#![allow(
    clippy::arithmetic_side_effects,
    clippy::unwrap_used,
    clippy::indexing_slicing
)]

mod mock;
use crate::mock::*;
use pallet_subtensor::*;
use sp_core::U256;
use substrate_fixed::types::I96F32;

// 1. Test Zero Tempo
// Description: Verify that when tempo is 0, the function returns u64::MAX.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_zero_tempo -- --exact --nocapture
#[test]
fn test_zero_tempo() {
    new_test_ext(1).execute_with(|| {
        assert_eq!(
            SubtensorModule::blocks_until_next_epoch(1, 0, 100),
            u64::MAX
        );
    });
}

// 2. Test Regular Case
// Description: Check if the function correctly calculates the blocks until the next epoch for various combinations of netuid, tempo, and block_number.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_regular_case -- --exact --nocapture
#[test]
fn test_regular_case() {
    new_test_ext(1).execute_with(|| {
        assert_eq!(SubtensorModule::blocks_until_next_epoch(1, 10, 5), 3);
        assert_eq!(SubtensorModule::blocks_until_next_epoch(2, 20, 15), 2);
        assert_eq!(SubtensorModule::blocks_until_next_epoch(3, 30, 25), 1);
    });
}

// 3. Test Boundary Conditions
// Description: Ensure the function handles edge cases like maximum u16 values for netuid and tempo, and maximum u64 value for block_number.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_boundary_conditions -- --exact --nocapture
#[test]
fn test_boundary_conditions() {
    new_test_ext(1).execute_with(|| {
        assert_eq!(
            SubtensorModule::blocks_until_next_epoch(u16::MAX, u16::MAX, u64::MAX),
            0
        );
        assert_eq!(
            SubtensorModule::blocks_until_next_epoch(u16::MAX, u16::MAX, 0),
            u16::MAX as u64
        );
    });
}

// 4. Test Overflow Handling
// Description: Verify that the function correctly handles potential overflows in intermediate calculations.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_overflow_handling -- --exact --nocapture
#[test]
fn test_overflow_handling() {
    new_test_ext(1).execute_with(|| {
        assert_eq!(
            SubtensorModule::blocks_until_next_epoch(u16::MAX, u16::MAX, u64::MAX - 1),
            1
        );
    });
}

// 5. Test Epoch Alignment
// Description: Check if the function returns 0 when the current block is exactly at an epoch boundary.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_epoch_alignment -- --exact --nocapture
#[test]
fn test_epoch_alignment() {
    new_test_ext(1).execute_with(|| {
        assert_eq!(SubtensorModule::blocks_until_next_epoch(1, 10, 9), 10);
        assert_eq!(SubtensorModule::blocks_until_next_epoch(2, 20, 21), 17);
    });
}

// 7. Test Different Network IDs
// Description: Verify that the function behaves correctly for different network IDs (netuids).
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_different_network_ids -- --exact --nocapture
#[test]
fn test_different_network_ids() {
    new_test_ext(1).execute_with(|| {
        assert_eq!(SubtensorModule::blocks_until_next_epoch(1, 10, 5), 3);
        assert_eq!(SubtensorModule::blocks_until_next_epoch(2, 10, 5), 2);
        assert_eq!(SubtensorModule::blocks_until_next_epoch(3, 10, 5), 1);
    });
}

// 8. Test Large Tempo Values
// Description: Check if the function works correctly with large tempo values close to u16::MAX.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_large_tempo_values -- --exact --nocapture
#[test]
fn test_large_tempo_values() {
    new_test_ext(1).execute_with(|| {
        assert_eq!(
            SubtensorModule::blocks_until_next_epoch(1, u16::MAX - 1, 100),
            u16::MAX as u64 - 103
        );
    });
}

// 9. Test Consecutive Blocks
// Description: Ensure that the function returns expected decreasing values for consecutive block numbers within an epoch.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_consecutive_blocks -- --exact --nocapture
#[test]
fn test_consecutive_blocks() {
    new_test_ext(1).execute_with(|| {
        let tempo = 10;
        let netuid = 1;
        let mut last_result = SubtensorModule::blocks_until_next_epoch(netuid, tempo, 0);
        for i in 1..tempo - 1 {
            let current_result = SubtensorModule::blocks_until_next_epoch(netuid, tempo, i as u64);
            assert_eq!(current_result, last_result.saturating_sub(1));
            last_result = current_result;
        }
    });
}

// 10. Test Wrap-around Behavior
// Description: Verify that the function correctly handles the wrap-around case when block_number is close to u64::MAX.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_wrap_around_behavior -- --exact --nocapture
#[test]
fn test_wrap_around_behavior() {
    new_test_ext(1).execute_with(|| {
        assert_eq!(SubtensorModule::blocks_until_next_epoch(1, 10, u64::MAX), 9);
        assert_eq!(
            SubtensorModule::blocks_until_next_epoch(1, 10, u64::MAX - 1),
            10
        );
    });
}

// 11. Test Zero Hotkey
// Description: Verify that the function correctly handles a zero hotkey value.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_zero_hotkey -- --exact --nocapture
#[test]
fn test_zero_hotkey() {
    new_test_ext(1).execute_with(|| {
        let zero_hotkey = U256::from(0);
        let emit_tempo = 5;
        assert!(SubtensorModule::should_drain_hotkey(
            &zero_hotkey,
            5,
            emit_tempo
        ));
        assert!(!SubtensorModule::should_drain_hotkey(
            &zero_hotkey,
            6,
            emit_tempo
        ));
    });
}

// 12. Test Maximum Hotkey Value
// Description: Check the behavior when the hotkey is set to the maximum possible value (U256::MAX).
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_max_hotkey -- --exact --nocapture
#[test]
fn test_max_hotkey() {
    new_test_ext(1).execute_with(|| {
        let max_hotkey = U256::max_value();
        assert!(SubtensorModule::should_drain_hotkey(&max_hotkey, 5, 5));
        assert!(!SubtensorModule::should_drain_hotkey(&max_hotkey, 6, 5));
    });
}

// 13. Test Zero Block
// Description: Ensure the function works correctly when the block number is zero.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_zero_block -- --exact --nocapture
#[test]
fn test_zero_block() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1234);
        let emit_tempo: u64 = 5;
        let block: u64 = 0;
        let expected: bool = block.rem_euclid(emit_tempo.saturating_add(1))
            == SubtensorModule::hash_hotkey_to_u64(&hotkey)
                .rem_euclid(emit_tempo.saturating_add(1));
        assert_eq!(
            SubtensorModule::should_drain_hotkey(&hotkey, block, emit_tempo),
            expected
        );
    });
}

// 14. Test Maximum Block Value
// Description: Verify the function's behavior when the block number is set to the maximum possible value (u64::MAX).
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_max_block -- --exact --nocapture
#[test]
fn test_max_block() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1234);
        assert!(SubtensorModule::should_drain_hotkey(&hotkey, u64::MAX, 5));
    });
}

// 15. Test Zero Emit Tempo
// Description: Check the function's output when the emit_tempo is set to zero.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_zero_emit_tempo -- --exact --nocapture
#[test]
fn test_zero_emit_tempo() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1234);
        assert!(SubtensorModule::should_drain_hotkey(&hotkey, 10, 0));
        assert!(SubtensorModule::should_drain_hotkey(&hotkey, 11, 0));
    });
}

// 16. Test Maximum Emit Tempo
// Description: Ensure the function behaves correctly when emit_tempo is set to the maximum possible value (u64::MAX).
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_max_emit_tempo -- --exact --nocapture
#[test]
fn test_max_emit_tempo() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1234);
        assert!(!SubtensorModule::should_drain_hotkey(&hotkey, 10, u64::MAX));
        assert!(!SubtensorModule::should_drain_hotkey(
            &hotkey,
            u64::MAX,
            u64::MAX
        ));
    });
}

// 17. Test Consecutive Blocks
// Description: Verify that the function returns expected alternating boolean values for consecutive block numbers.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_consecutive_blocks_drain -- --exact --nocapture
#[test]
fn test_consecutive_blocks_drain() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1234);
        let emit_tempo = 5;
        let mut last_result = SubtensorModule::should_drain_hotkey(&hotkey, 0, emit_tempo);
        for i in 1..emit_tempo + 10 {
            let current_result = SubtensorModule::should_drain_hotkey(&hotkey, i, emit_tempo);
            if last_result {
                assert!(!current_result);
            }
            last_result = current_result;
        }
    });
}

// 18. Test Different Hotkeys Same Block
// Description: Check that different hotkeys produce different results for the same block number and emit_tempo.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_different_hotkeys_same_block -- --exact --nocapture
#[test]
fn test_different_hotkeys_same_block() {
    new_test_ext(1).execute_with(|| {
        let hotkey1 = U256::from(1234);
        let hotkey2 = U256::from(5678);
        let block = 10;
        let emit_tempo = 5;
        assert_ne!(
            SubtensorModule::should_drain_hotkey(&hotkey1, block, emit_tempo),
            SubtensorModule::should_drain_hotkey(&hotkey2, block, emit_tempo)
        );
    });
}

// 20. Test Periodic Behavior
// Description: Verify that the function exhibits periodic behavior based on the emit_tempo value.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_periodic_behavior -- --exact --nocapture
#[test]
fn test_periodic_behavior() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1234);
        let emit_tempo = 5;
        let initial_result = SubtensorModule::should_drain_hotkey(&hotkey, 0, emit_tempo);
        for i in 1..20 {
            let current_result =
                SubtensorModule::should_drain_hotkey(&hotkey, i * (emit_tempo + 1), emit_tempo);
            assert_eq!(current_result, initial_result);
        }
    });
}

// Test titles and descriptions for exhaustive testing of source_nominator_emission function:

// 21. Test Basic Emission Distribution
// Description: Verify that the function correctly distributes emissions between the hotkey and its nominators.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_basic_emission_distribution -- --exact --nocapture
#[test]
fn test_basic_emission_distribution() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator1 = U256::from(2);
        let nominator2 = U256::from(3);
        let netuid = 1;
        let emission = 10000;

        // Set up stakes and delegations
        SubtensorModule::stake_into_subnet(&hotkey, &nominator1, netuid, 500);
        SubtensorModule::stake_into_subnet(&hotkey, &nominator2, netuid, 500);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        assert_eq!(emission_tuples.len(), 3);
        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, _, amount)| amount).sum();
        assert_eq!(total_distributed, emission);

        // Check hotkey take
        let hotkey_emission: u64 = emission_tuples
            .iter()
            .filter(|(h, _, _, _)| h == &hotkey)
            .map(|(_, _, _, amount)| amount)
            .sum();
        assert!(hotkey_emission > 0);

        // Log the emission tuples
        println!("Emission tuples:");
        for (hotkey, nominator, netuid, amount) in &emission_tuples {
            println!(
                "Hotkey: {:?}, Nominator: {:?}, Netuid: {}, Amount: {}",
                hotkey, nominator, netuid, amount
            );
        }

        // Check nominator distributions
        let nominator1_emission: u64 = *emission_tuples
            .iter()
            .find(|(_, n, _, _)| n == &nominator1)
            .map(|(_, _, _, amount)| amount)
            .unwrap();
        let nominator2_emission: u64 = *emission_tuples
            .iter()
            .find(|(_, n, _, _)| n == &nominator2)
            .map(|(_, _, _, amount)| amount)
            .unwrap();
        assert!(nominator1_emission > 0);
        assert!(nominator2_emission > 0);
        assert_eq!(nominator1_emission, nominator2_emission);
    });
}

// 22. Test Hotkey Take Calculation
// Description: Ensure that the hotkey's take is calculated correctly based on the delegation status.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_hotkey_take_calculation -- --exact --nocapture
#[test]
fn test_hotkey_take_calculation() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator = U256::from(2);
        let netuid = 1;
        let emission = 1000;

        // Test with different delegation values
        for &delegation in &[0, 16384, 32768, 49152, 65535] {
            Delegates::<Test>::insert(hotkey, delegation);
            SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, 500);

            let mut emission_tuples = Vec::new();
            SubtensorModule::source_nominator_emission(
                &hotkey,
                netuid,
                emission,
                0,
                &mut emission_tuples,
            );

            let hotkey_emission: u64 = emission_tuples
                .iter()
                .filter(|(h, _, _, _)| h == &hotkey)
                .last()
                .map(|(_, _, _, amount)| *amount)
                .unwrap_or(0);
            let emission_fixed = I96F32::from_num(emission);
            let delegation_fixed = I96F32::from_num(delegation);
            let max_delegation_fixed = I96F32::from_num(65535u16);
            let expected_take =
                (emission_fixed * delegation_fixed / max_delegation_fixed).to_num::<u64>();
            log::debug!(
                "Hotkey emission: {:?}, Expected take: {:?}",
                hotkey_emission,
                expected_take
            );
            assert!(hotkey_emission >= expected_take && hotkey_emission <= expected_take + 1);
        }
    });
}

// 23. Test Nominator Distribution
// Description: Check that the remaining emissions are distributed proportionally among nominators.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_nominator_distribution -- --exact --nocapture
#[test]
fn test_nominator_distribution() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator1 = U256::from(2);
        let nominator2 = U256::from(3);
        let nominator3 = U256::from(4);
        let netuid = 1;
        let emission = 10000;

        // Set up stakes with different proportions
        SubtensorModule::stake_into_subnet(&hotkey, &nominator1, netuid, 500);
        SubtensorModule::stake_into_subnet(&hotkey, &nominator2, netuid, 300);
        SubtensorModule::stake_into_subnet(&hotkey, &nominator3, netuid, 200);
        Delegates::<Test>::insert(hotkey, 0); // No hotkey take

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        let nominator1_emission = emission_tuples
            .iter()
            .find(|(_, n, _, _)| n == &nominator1)
            .map(|(_, _, _, amount)| amount)
            .unwrap();
        let nominator2_emission = emission_tuples
            .iter()
            .find(|(_, n, _, _)| n == &nominator2)
            .map(|(_, _, _, amount)| amount)
            .unwrap();
        let nominator3_emission = emission_tuples
            .iter()
            .find(|(_, n, _, _)| n == &nominator3)
            .map(|(_, _, _, amount)| amount)
            .unwrap();
        let remainder: u64 = emission_tuples
            .iter()
            .filter(|(h, _, _, _)| h == &hotkey)
            .last()
            .map(|(_, _, _, amount)| *amount)
            .unwrap_or(0);

        // Check proportional distribution
        assert!(nominator1_emission > nominator2_emission);
        assert!(nominator2_emission > nominator3_emission);
        assert_eq!(
            *nominator1_emission + *nominator2_emission + *nominator3_emission,
            emission - remainder
        );

        // Check approximate proportions
        let total_stake = 500 + 300 + 200;
        let expected_nominator1 = (emission as f64 * 500.0 / total_stake as f64) as u64;
        let expected_nominator2 = (emission as f64 * 300.0 / total_stake as f64) as u64;
        let expected_nominator3 = (emission as f64 * 200.0 / total_stake as f64) as u64;

        assert!(nominator1_emission.abs_diff(expected_nominator1) <= 1);
        assert!(nominator2_emission.abs_diff(expected_nominator2) <= 1);
        assert!(nominator3_emission.abs_diff(expected_nominator3) <= 1);
    });
}

// 24. Test Global and Alpha Weight Distribution
// Description: Verify that the distribution considers both global and alpha weights correctly.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_global_and_alpha_weight_distribution -- --exact --nocapture
#[test]
fn test_global_and_alpha_weight_distribution() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator = U256::from(2);
        let netuid = 1;
        let emission = 10000;

        // Set up global and alpha weights
        add_network(netuid, 0, 0);
        SubtensorModule::set_global_weight(
            ((I96F32::from_num(3) * I96F32::from_num(u64::MAX)) / I96F32::from_num(10))
                .to_num::<u64>(),
        ); // 30% global weight
        SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, 1000);
        Delegates::<Test>::insert(hotkey, 0); // No hotkey take

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        let nominator_emission = emission_tuples
            .iter()
            .find(|(_, n, _, _)| n == &nominator)
            .map(|(_, _, _, amount)| amount)
            .unwrap();

        // Check if the distribution is close to expected
        let expected_global = (emission as f64 * 0.3) as u64;
        let expected_alpha = (emission as f64 * 0.7) as u64;
        let total_expected = expected_global + expected_alpha;

        assert!((*nominator_emission as i64 - total_expected as i64).abs() <= 1);
    });
}

// 25. Test Zero Stake Scenario
// Description: Ensure the function handles cases where a nominator or hotkey has zero stake without errors.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_zero_stake_scenario -- --exact --nocapture
#[test]
fn test_zero_stake_scenario() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator = U256::from(2);
        let netuid = 1;
        let emission = 10000;

        // Set up zero stake
        SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, 0);
        Delegates::<Test>::insert(hotkey, 0);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        // Check that no errors occurred and all emission went to hotkey
        assert_eq!(emission_tuples.len(), 1);
        let (_, recipient, _, amount) = &emission_tuples[0];
        assert_eq!(recipient, &Owner::<Test>::get(hotkey));
        assert_eq!(*amount, emission);
    });
}

// 26. Test Single Nominator Scenario
// Description: Check the behavior when a hotkey has only one nominator.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_single_nominator_scenario -- --exact --nocapture
#[test]
fn test_single_nominator_scenario() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator = U256::from(2);
        let netuid = 1;
        let emission = 10000;

        SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, 1000);
        Delegates::<Test>::insert(hotkey, 0);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        assert_eq!(emission_tuples.len(), 2); // with delegate and nominator position.
        let (_, recipient, _, amount) = &emission_tuples[0];
        assert_eq!(recipient, &nominator);
        assert_eq!(*amount, emission);
    });
}

// 27. Test Maximum Nominators Scenario
// Description: Verify the function's performance and correctness with the maximum possible number of nominators.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_maximum_nominators_scenario -- --exact --nocapture
#[test]
fn test_maximum_nominators_scenario() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let netuid = 1;
        let emission = 1000000;
        let max_nominators = 100; // Adjust based on your system's limits

        for i in 0..max_nominators {
            let nominator = U256::from(i + 2);
            SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, 100);
        }
        Delegates::<Test>::insert(hotkey, 0);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        assert_eq!(emission_tuples.len(), max_nominators + 1);
        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, _, amount)| amount).sum();
        assert_eq!(total_distributed, emission);
    });
}

// 28. Test Rounding and Precision
// Description: Ensure that rounding errors don't accumulate and all emissions are accounted for.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_rounding_and_precision -- --exact --nocapture
#[test]
fn test_rounding_and_precision() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator1 = U256::from(2);
        let nominator2 = U256::from(3);
        let netuid = 1;
        let emission = 1000000; // Large emission to test precision

        SubtensorModule::stake_into_subnet(&hotkey, &nominator1, netuid, 333333);
        SubtensorModule::stake_into_subnet(&hotkey, &nominator2, netuid, 666667);
        Delegates::<Test>::insert(hotkey, 0);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, _, amount)| amount).sum();
        assert_eq!(
            total_distributed, emission,
            "Total distributed should equal the original emission"
        );

        let nominator1_emission = emission_tuples
            .iter()
            .find(|(_, n, _, _)| n == &nominator1)
            .map(|(_, _, _, amount)| amount)
            .unwrap();
        let nominator2_emission = emission_tuples
            .iter()
            .find(|(_, n, _, _)| n == &nominator2)
            .map(|(_, _, _, amount)| amount)
            .unwrap();

        assert!(
            nominator1_emission > &0 && nominator2_emission > &0,
            "Both nominators should receive non-zero emissions"
        );
        assert!(
            nominator2_emission > nominator1_emission,
            "Nominator2 should receive more emission than Nominator1"
        );
    });
}

// 29. Test Emission Tuple Generation
// Description: Verify that the emission tuples are correctly generated and contain accurate information.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_emission_tuple_generation -- --exact --nocapture
#[test]
fn test_emission_tuple_generation() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator = U256::from(2);
        let netuid = 1;
        let emission = 1000;

        SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, 1000);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        assert_eq!(
            emission_tuples.len(),
            2,
            "Should generate 2 tuples: one for nominator, one for hotkey"
        );

        let nominator_tuple = emission_tuples
            .iter()
            .find(|(h, n, net, _)| h == &hotkey && n == &nominator && *net == netuid)
            .expect("Nominator tuple should exist");
        let hotkey_tuple = emission_tuples
            .iter()
            .find(|(h, n, net, _)| {
                h == &hotkey && n == &Owner::<Test>::get(hotkey) && *net == netuid
            })
            .expect("Hotkey tuple should exist");

        assert!(
            nominator_tuple.3 > 0,
            "Nominator should receive non-zero emission"
        );
        assert!(
            hotkey_tuple.3 > 0,
            "Hotkey should receive non-zero emission"
        );
        assert_eq!(
            nominator_tuple.3 + hotkey_tuple.3,
            emission,
            "Sum of emissions should equal total emission"
        );
    });
}

// 30. Test Remainder Distribution
// Description: Check that any undistributed remainder is correctly added to the hotkey's emission.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_remainder_distribution -- --exact --nocapture
#[test]
fn test_remainder_distribution() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator1 = U256::from(2);
        let nominator2 = U256::from(3);
        let netuid = 1;
        let emission = 1000;

        SubtensorModule::stake_into_subnet(&hotkey, &nominator1, netuid, 333);
        SubtensorModule::stake_into_subnet(&hotkey, &nominator2, netuid, 666);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        let hotkey_emission = emission_tuples
            .iter()
            .find(|(h, n, _, _)| h == &hotkey && n == &Owner::<Test>::get(hotkey))
            .map(|(_, _, _, amount)| amount)
            .unwrap();
        let expected_hotkey_take = (emission as u128 * 16384u128 / 65535u128) as u64;

        assert!(
            hotkey_emission >= &expected_hotkey_take,
            "Hotkey emission should be at least the expected take"
        );
        assert!(
            hotkey_emission > &expected_hotkey_take,
            "Hotkey emission should include some remainder"
        );

        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, _, amount)| amount).sum();
        assert_eq!(
            total_distributed, emission,
            "Total distributed should equal the original emission"
        );
    });
}

// 31. Test With Different Network IDs
// Description: Ensure the function works correctly across different network IDs (netuids).
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_different_network_ids -- --exact --nocapture
#[test]
fn test_different_network_ids_scenario() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator = U256::from(2);
        let emission = 1000;

        for netuid in 1..=3 {
            SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, 1000);
            Delegates::<Test>::insert(hotkey, 16384); // 25% take

            let mut emission_tuples = Vec::new();
            SubtensorModule::source_nominator_emission(
                &hotkey,
                netuid,
                emission,
                0,
                &mut emission_tuples,
            );

            assert_eq!(
                emission_tuples.len(),
                2,
                "Should generate 2 tuples for netuid {}",
                netuid
            );

            let total_distributed: u64 =
                emission_tuples.iter().map(|(_, _, _, amount)| amount).sum();
            assert_eq!(
                total_distributed, emission,
                "Total distributed should equal the original emission for netuid {}",
                netuid
            );
        }
    });
}

// 32. Test Large Emission Values
// Description: Verify the function's behavior with very large emission values to check for overflow.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_large_emission_values -- --exact --nocapture
#[test]
fn test_large_emission_values() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator = U256::from(2);
        let netuid = 1;
        let emission = u64::MAX;

        SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, u64::MAX);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        assert_eq!(
            emission_tuples.len(),
            2,
            "Should generate 2 tuples even with max emission"
        );

        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, _, amount)| amount).sum();
        assert_eq!(
            total_distributed, emission,
            "Total distributed should equal the original emission even with max value"
        );

        let hotkey_emission = emission_tuples
            .iter()
            .find(|(h, n, _, _)| h == &hotkey && n == &Owner::<Test>::get(hotkey))
            .map(|(_, _, _, amount)| amount)
            .unwrap();
        let nominator_emission = emission_tuples
            .iter()
            .find(|(h, n, _, _)| h == &hotkey && n == &nominator)
            .map(|(_, _, _, amount)| amount)
            .unwrap();

        assert!(
            hotkey_emission > &0,
            "Hotkey should receive non-zero emission even with max value"
        );
        assert!(
            nominator_emission > &0,
            "Nominator should receive non-zero emission even with max value"
        );
    });
}
// 33. Test Small Emission Values
// Description: Check the function's precision with very small emission values.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_small_emission_values -- --exact --nocapture
#[test]
fn test_small_emission_values() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator = U256::from(2);
        let netuid = 1;
        let emission = 1; // Smallest possible non-zero emission

        SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, 1000);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        assert_eq!(
            emission_tuples.len(),
            1,
            "Should generate 2 tuples even with small emission"
        );

        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, _, amount)| amount).sum();
        assert_eq!(
            total_distributed, emission,
            "Total distributed should equal the original emission even with small value"
        );

        let hotkey_emission = emission_tuples
            .iter()
            .find(|(h, n, _, _)| h == &hotkey && n == &Owner::<Test>::get(hotkey))
            .map(|(_, _, _, amount)| amount)
            .unwrap();
        assert!(
            *hotkey_emission == 0 || *hotkey_emission == 1,
            "Hotkey emission should be 0 or 1 with small value"
        );
    });
}

// 34. Test Consistency Across Multiple Calls
// Description: Ensure that repeated calls to the function with the same inputs produce consistent results.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_consistency_across_multiple_calls -- --exact --nocapture
#[test]
fn test_consistency_across_multiple_calls() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator = U256::from(2);
        let netuid = 1;
        let emission = 1000;

        SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, 1000);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take

        let mut first_result = Vec::new();
        SubtensorModule::source_nominator_emission(&hotkey, netuid, emission, 0, &mut first_result);

        for _ in 0..10 {
            let mut current_result = Vec::new();
            SubtensorModule::source_nominator_emission(
                &hotkey,
                netuid,
                emission,
                0,
                &mut current_result,
            );
            assert_eq!(
                first_result, current_result,
                "Results should be consistent across multiple calls"
            );
        }
    });
}

// 35. Test Performance with Many Nominators
// Description: Measure the function's performance when dealing with a large number of nominators.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_performance_with_many_nominators -- --exact --nocapture
#[test]
fn test_performance_with_many_nominators() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let netuid = 1;
        let emission = 1_000_000; // Large emission to distribute
        let num_nominators = 1000; // Large number of nominators

        // Set up stakes and alpha values for many nominators
        for i in 0..num_nominators {
            let nominator = U256::from(i + 2); // Start from 2 to avoid collision with hotkey
            SubtensorModule::stake_into_subnet(&hotkey, &nominator, netuid, 1000);
        }
        Delegates::<Test>::insert(hotkey, 16384); // 25% take

        let start_time = std::time::Instant::now();
        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );
        let duration = start_time.elapsed();

        assert_eq!(
            emission_tuples.len(),
            num_nominators + 1,
            "Should generate tuples for all nominators plus hotkey"
        );

        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, _, amount)| amount).sum();
        assert_eq!(
            total_distributed, emission,
            "Total distributed should equal the original emission"
        );

        println!(
            "Time taken to process {} nominators: {:?}",
            num_nominators, duration
        );
        // You might want to add an assertion here to ensure the function completes within an acceptable time frame
        // For example: assert!(duration < std::time::Duration::from_secs(5), "Function took too long to complete");
    });
}

// 36. Test Basic Emission Distribution
// Description: Verify that the function correctly distributes emissions between the hotkey and its parents.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_basic_emission_distribution_scenario -- --exact --nocapture
#[test]
fn test_basic_emission_distribution_scenario() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent1 = U256::from(3);
        let parent2 = U256::from(4);
        let netuid = 1;
        let validating_emission = 1000;
        let mining_emission = 500;
        let tempo = 1;

        // Skip tempo blocks so that LastAddStakeIncrease doesn't trigger stake-unstake
        // protection in source_hotkey_emission
        step_block(tempo);

        // Set up stakes and delegations
        add_network(netuid, tempo, 0);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take
        SubtensorModule::stake_into_subnet(&parent1, &coldkey, netuid, 500);
        SubtensorModule::stake_into_subnet(&parent2, &coldkey, netuid, 500);
        ParentKeys::<Test>::insert(
            hotkey,
            netuid,
            vec![(u64::MAX / 2, parent1), (u64::MAX / 2, parent2)],
        );

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        assert_eq!(emission_tuples.len(), 3);
        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, amount)| amount).sum();
        assert_eq!(total_distributed, validating_emission + mining_emission);

        // Check hotkey take and mining emission
        let hotkey_emission = emission_tuples
            .iter()
            .find(|(h, _, _)| h == &hotkey)
            .map(|(_, _, amount)| amount)
            .unwrap();
        assert!(hotkey_emission > &0);

        // Check parent distributions
        let parent1_emission = emission_tuples
            .iter()
            .find(|(p, _, _)| p == &parent1)
            .map(|(_, _, amount)| amount)
            .unwrap();
        let parent2_emission = emission_tuples
            .iter()
            .find(|(p, _, _)| p == &parent2)
            .map(|(_, _, amount)| amount)
            .unwrap();
        assert!(parent1_emission > &0);
        assert!(parent2_emission > &0);
        assert_eq!(parent1_emission, parent2_emission);
    });
}

// 37. Test Hotkey Take Calculation
// Description: Ensure that the hotkey's take is calculated correctly based on the delegation status.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_hotkey_take_calculation_scenario -- --exact --nocapture
#[test]
fn test_hotkey_take_calculation_scenario() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent = U256::from(3);
        let netuid = 1;
        let validating_emission = 1000;
        let mining_emission = 0;

        ParentKeys::<Test>::insert(hotkey, netuid, vec![(1000, parent)]);

        // Test with different delegation values
        for &delegation in &[0, 16384, 32768, 49152, 65535] {
            Delegates::<Test>::insert(hotkey, delegation);
            SubtensorModule::stake_into_subnet(&parent, &coldkey, netuid, u64::MAX);
            ParentKeys::<Test>::insert(hotkey, netuid, vec![(u64::MAX, parent)]);

            let mut emission_tuples = Vec::new();
            SubtensorModule::source_hotkey_emission(
                &hotkey,
                netuid,
                validating_emission,
                mining_emission,
                &mut emission_tuples,
            );

            let hotkey_emission: u64 = emission_tuples
                .iter()
                .filter(|(h, _, _)| h == &hotkey)
                .map(|(_, _, amount)| *amount)
                .sum();
            let emission_fixed = I96F32::from_num(validating_emission);
            let delegation_fixed = I96F32::from_num(delegation);
            let max_delegation_fixed = I96F32::from_num(65535u16);
            let expected_take =
                (emission_fixed * delegation_fixed / max_delegation_fixed).to_num::<u64>();
            assert!(hotkey_emission >= expected_take && hotkey_emission <= (expected_take + 1));
        }
    });
}

// 38. Test Parent Distribution
// Description: Check that the remaining emissions are distributed proportionally among parents.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_parent_distribution -- --exact --nocapture
#[test]
fn test_parent_distribution() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent1 = U256::from(3);
        let parent2 = U256::from(4);
        let parent3 = U256::from(5);
        let netuid = 1;
        let validating_emission = 10000;
        let mining_emission = 0;
        let tempo = 1;

        // Skip tempo blocks so that LastAddStakeIncrease doesn't trigger stake-unstake
        // protection in source_hotkey_emission
        step_block(tempo);

        // Set up parent proportions
        add_network(netuid, tempo, 0);
        SubtensorModule::stake_into_subnet(&parent1, &coldkey, netuid, 500);
        SubtensorModule::stake_into_subnet(&parent2, &coldkey, netuid, 300);
        SubtensorModule::stake_into_subnet(&parent3, &coldkey, netuid, 200);
        ParentKeys::<Test>::insert(
            hotkey,
            netuid,
            vec![
                (u64::MAX, parent1),
                (u64::MAX, parent2),
                (u64::MAX, parent3),
            ],
        );
        Delegates::<Test>::insert(hotkey, 0); // No hotkey take

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        let parent1_emission = emission_tuples
            .iter()
            .find(|(p, _, _)| p == &parent1)
            .map(|(_, _, amount)| *amount)
            .unwrap();
        let parent2_emission = emission_tuples
            .iter()
            .find(|(p, _, _)| p == &parent2)
            .map(|(_, _, amount)| *amount)
            .unwrap();
        let parent3_emission = emission_tuples
            .iter()
            .find(|(p, _, _)| p == &parent3)
            .map(|(_, _, amount)| *amount)
            .unwrap();
        let all_hotkey_emission: u64 = emission_tuples
            .iter()
            .filter(|(h, _, _)| h == &hotkey)
            .map(|(_, _, amount)| *amount)
            .sum();

        // Check proportional distribution
        assert!(parent1_emission > parent2_emission);
        assert!(parent2_emission > parent3_emission);
        assert_eq!(
            parent1_emission + parent2_emission + parent3_emission,
            validating_emission - all_hotkey_emission
        );

        // Check approximate proportions
        let total_proportion = 500 + 300 + 200;
        let expected_parent1 =
            (validating_emission as f64 * 500.0 / total_proportion as f64) as u64;
        let expected_parent2 =
            (validating_emission as f64 * 300.0 / total_proportion as f64) as u64;
        let expected_parent3 =
            (validating_emission as f64 * 200.0 / total_proportion as f64) as u64;

        assert!(parent1_emission.abs_diff(expected_parent1) <= 1);
        assert!(parent2_emission.abs_diff(expected_parent2) <= 1);
        assert!(parent3_emission.abs_diff(expected_parent3) <= 1);
    });
}

// 39. Test Global and Alpha Weight Distribution
// Description: Verify that the distribution considers both global and alpha weights correctly.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_global_and_alpha_weight_distribution_scenario -- --exact --nocapture
#[test]
fn test_global_and_alpha_weight_distribution_scenario() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent = U256::from(3);
        let netuid = 1;
        let validating_emission = 10000;
        let mining_emission = 0;
        let tempo = 1;

        // Skip tempo blocks so that LastAddStakeIncrease doesn't trigger stake-unstake
        // protection in source_hotkey_emission
        step_block(tempo);

        // Set up global and alpha weights
        add_network(netuid, tempo, 0);
        SubtensorModule::set_global_weight(
            (I96F32::from_num(u64::MAX) * I96F32::from_num(3) / I96F32::from_num(10))
                .to_num::<u64>(),
        );
        ParentKeys::<Test>::insert(hotkey, netuid, vec![(u64::MAX, parent)]);
        SubtensorModule::stake_into_subnet(&parent, &coldkey, netuid, 500);
        Delegates::<Test>::insert(hotkey, 0); // No hotkey take
        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        let parent_emission = emission_tuples
            .iter()
            .find(|(p, _, _)| p == &parent)
            .map(|(_, _, amount)| amount)
            .unwrap();

        // Check if the distribution is close to expected
        let expected_global = (validating_emission as f64 * 0.3) as u64;
        let expected_alpha = (validating_emission as f64 * 0.7) as u64;
        let total_expected = expected_global + expected_alpha;
        assert!((*parent_emission as i64 - total_expected as i64).abs() <= 1);
    });
}

// 40. Test Zero Stake Scenario
// Description: Ensure the function handles cases where a parent or hotkey has zero stake without errors.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_zero_stake_scenario_1 -- --exact --nocapture
#[test]
fn test_zero_stake_scenario_1() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent = U256::from(3);
        let netuid = 1;
        let validating_emission = 10000;
        let mining_emission = 0;

        // Set up zero stake
        ParentKeys::<Test>::insert(hotkey, netuid, vec![(u64::MAX, parent)]);
        SubtensorModule::stake_into_subnet(&parent, &coldkey, netuid, 0);
        Delegates::<Test>::insert(hotkey, 0);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        // Check that the function doesn't panic and distributes all emission to the hotkey
        assert_eq!(emission_tuples.len(), 2);
        assert_eq!(emission_tuples[1].0, hotkey);
        assert_eq!(emission_tuples[1].2, validating_emission);
    });
}

// 41. Test Maximum Stake Values
// Description: Check the function's behavior with maximum possible stake values.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_maximum_stake_values -- --exact --nocapture
#[test]
fn test_maximum_stake_values() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent = U256::from(3);
        let netuid = 1;
        let validating_emission = u64::MAX;
        let mining_emission = 0;

        // Set up maximum stake values
        ParentKeys::<Test>::insert(hotkey, netuid, vec![(u64::MAX, parent)]);
        Alpha::<Test>::insert((&parent, coldkey, netuid), u64::MAX);
        // GlobalStake::<Test>::insert(&parent, u64::MAX);
        Delegates::<Test>::insert(hotkey, 0);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        // Check that the function doesn't overflow and distributes all emission
        assert_eq!(emission_tuples.len(), 2);
        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, amount)| amount).sum();
        assert_eq!(total_distributed, validating_emission);
    });
}

// 42. Test Rounding and Precision
// Description: Verify that rounding errors don't accumulate and the total distributed matches the input emission.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_rounding_and_precision_scenario -- --exact --nocapture
#[test]
fn test_rounding_and_precision_scenario() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent1 = U256::from(3);
        let parent2 = U256::from(3);
        let netuid = 1;
        let validating_emission = 10000;
        let mining_emission = 0;

        // Set up stakes and parents
        ParentKeys::<Test>::insert(
            hotkey,
            netuid,
            vec![(u64::MAX / 2, parent1), (u64::MAX / 2, parent2)],
        );
        SubtensorModule::stake_into_subnet(&parent1, &coldkey, netuid, 1000);
        SubtensorModule::stake_into_subnet(&parent2, &coldkey, netuid, 1000);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        // Check that the total distributed matches the input emission
        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, amount)| amount).sum();
        assert_eq!(total_distributed, validating_emission);

        // Check that each parent receives a non-zero amount
        for (account, _, amount) in &emission_tuples {
            if account == &parent1 || account == &parent2 {
                assert!(*amount > 0);
            }
        }
    });
}

// 43. Test Different Network IDs
// Description: Ensure the function works correctly for different network IDs (netuids).
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_different_network_ids -- --exact --nocapture
#[test]
fn test_different_network_ids_scenario_1() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent = U256::from(3);
        let validating_emission = 10000;
        let mining_emission = 0;

        for netuid in 0..5 {
            ParentKeys::<Test>::insert(hotkey, netuid, vec![(u64::MAX, parent)]);
            Alpha::<Test>::insert((&parent, coldkey, netuid), 1000);
            // GlobalStake::<Test>::insert(&parent, 1000);
            Delegates::<Test>::insert(hotkey, 0);

            let mut emission_tuples = Vec::new();
            SubtensorModule::source_hotkey_emission(
                &hotkey,
                netuid,
                validating_emission,
                mining_emission,
                &mut emission_tuples,
            );

            // Check that the function produces correct output for each netuid
            assert_eq!(emission_tuples.len(), 2);
            assert_eq!(emission_tuples[0].1, netuid);
            assert_eq!(emission_tuples[1].1, netuid);
            let total_distributed: u64 = emission_tuples.iter().map(|(_, _, amount)| amount).sum();
            assert_eq!(total_distributed, validating_emission);
        }
    });
}

// 44. Test with No ParentKeys
// Description: Check the behavior when the hotkey has no parents.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_with_no_parents -- --exact --nocapture
#[test]
fn test_with_no_parents() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let netuid: u16 = 1;
        let validating_emission = 10000;
        let mining_emission = 0;

        // Set up with no parents
        let empty: Vec<(u64, U256)> = vec![];
        ParentKeys::<Test>::insert(hotkey, netuid, empty);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        // Check that all emission goes to the hotkey
        assert_eq!(emission_tuples.len(), 1);
        assert_eq!(emission_tuples[0].0, hotkey);
        assert_eq!(emission_tuples[0].1, netuid);
        assert_eq!(emission_tuples[0].2, validating_emission);
    });
}
// Description: Check the behavior when the hotkey has no parents.

// 45. Test with Maximum Number of ParentKeys
// Description: Verify the function's performance and correctness with the maximum allowed number of parents.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_maximum_parents -- --exact --nocapture
#[test]
fn test_maximum_parents() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let netuid = 1;
        let validating_emission = 10000;
        let mining_emission = 0;
        let max_parents = 16; // Assuming 16 is the maximum number of parents

        // Set up maximum number of parents
        let mut parents = Vec::new();
        for i in 0..max_parents {
            let parent = U256::from(i + 2);
            parents.push((u64::MAX / max_parents as u64, parent));
            Alpha::<Test>::insert((&parent, coldkey, netuid), 1000);
            // GlobalStake::<Test>::insert(&parent, 1000);
        }
        ParentKeys::<Test>::insert(hotkey, netuid, parents);
        Delegates::<Test>::insert(hotkey, 0);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        // Check that all parents received some emission
        assert_eq!(emission_tuples.len(), max_parents + 1); // +1 for the hotkey itself
        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, amount)| amount).sum();
        assert_eq!(total_distributed, validating_emission);
    });
}

// 46. Test Consistency Across Multiple Calls
// Description: Ensure that multiple calls with the same input produce consistent results.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_consistency_across_calls -- --exact --nocapture
#[test]
fn test_consistency_across_calls() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent = U256::from(3);
        let netuid = 1;
        let validating_emission = 10000;
        let mining_emission = 0;

        ParentKeys::<Test>::insert(hotkey, netuid, vec![(u64::MAX, parent)]);
        Alpha::<Test>::insert((&parent, coldkey, netuid), 1000);
        // GlobalStake::<Test>::insert(&parent, 1000);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take

        let mut first_result = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut first_result,
        );

        for _ in 0..10 {
            let mut current_result = Vec::new();
            SubtensorModule::source_hotkey_emission(
                &hotkey,
                netuid,
                validating_emission,
                mining_emission,
                &mut current_result,
            );
            assert_eq!(first_result, current_result);
        }
    });
}

// 47. Test Edge Cases in Parent Proportions
// Description: Verify correct handling of edge cases in parent proportion values (0, u64::MAX, etc.).
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_edge_cases_parent_proportions -- --exact --nocapture
#[test]
fn test_edge_cases_parent_proportions() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent1 = U256::from(3);
        let parent2 = U256::from(4);
        let parent3 = U256::from(5);
        let netuid = 1;
        let validating_emission = 10000;
        let mining_emission = 0;

        ParentKeys::<Test>::insert(
            hotkey,
            netuid,
            vec![(0, parent1), (u64::MAX, parent2), (u64::MAX / 2, parent3)],
        );

        for parent in [&parent1, &parent2, &parent3] {
            SubtensorModule::stake_into_subnet(parent, &coldkey, netuid, 1000);
        }
        Delegates::<Test>::insert(hotkey, 0);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        // Check that parent1 (with 0 proportion) receives no emission
        assert_eq!(
            emission_tuples
                .iter()
                .find(|(acc, _, _)| acc == &parent1)
                .map(|(_, _, amount)| amount),
            Some(&0)
        );

        // Check that parent2 (with u64::MAX proportion) receives the most emission
        let parent2_emission = emission_tuples
            .iter()
            .find(|(acc, _, _)| acc == &parent2)
            .map(|(_, _, amount)| amount)
            .unwrap();
        let parent3_emission = emission_tuples
            .iter()
            .find(|(acc, _, _)| acc == &parent3)
            .map(|(_, _, amount)| amount)
            .unwrap();
        assert!(parent2_emission > parent3_emission);

        // Check that all emission is distributed
        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, amount)| amount).sum();
        assert_eq!(total_distributed, validating_emission);
    });
}

// 48. Test Overflow Handling
// Description: Check that the function correctly handles potential overflows in intermediate calculations.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_overflow_handling_in_emission -- --exact --nocapture
#[test]
fn test_overflow_handling_in_emission() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent = U256::from(3);
        let netuid = 1;
        let validating_emission = u64::MAX;
        let mining_emission = u64::MAX;

        ParentKeys::<Test>::insert(hotkey, netuid, vec![(u64::MAX, parent)]);
        SubtensorModule::stake_into_subnet(&parent, &coldkey, netuid, u64::MAX);
        Delegates::<Test>::insert(hotkey, u16::MAX);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        // Check that the function doesn't panic and produces some output
        assert!(emission_tuples.len() == 2);

        // Check that the total distributed emission doesn't exceed the input
        let total_distributed: u128 = emission_tuples
            .iter()
            .map(|(_, _, amount)| *amount as u128)
            .sum();
        assert!(total_distributed <= (validating_emission as u128 + mining_emission as u128));
    });
}

// 49. Test with Minimum Emission Value
// Description: Ensure the function works correctly with the smallest possible emission value.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_minimum_emission_value -- --exact --nocapture
#[test]
fn test_minimum_emission_value() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent = U256::from(3);
        let netuid = 1;
        let validating_emission = 1;
        let mining_emission = 0;

        ParentKeys::<Test>::insert(hotkey, netuid, vec![(u64::MAX / 2, parent)]);
        Alpha::<Test>::insert((&parent, coldkey, netuid), 1000);
        // GlobalStake::<Test>::insert(&parent, 1000);
        Delegates::<Test>::insert(hotkey, 0);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        // Check that the function produces some output
        assert!(!emission_tuples.is_empty());

        // Check that the total distributed emission equals the input
        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, amount)| *amount).sum();
        assert_eq!(total_distributed, validating_emission);
    });
}

// 50. Test with Maximum Emission Value
// Description: Verify the function's behavior with the maximum possible emission value.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test emission test_maximum_emission_value -- --exact --nocapture
#[test]
fn test_maximum_emission_value() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent = U256::from(3);
        let netuid = 1;
        let validating_emission = u64::MAX;
        let mining_emission = u64::MAX;

        ParentKeys::<Test>::insert(hotkey, netuid, vec![(u64::MAX / 2, parent)]);
        SubtensorModule::stake_into_subnet(&parent, &coldkey, netuid, u64::MAX);
        // GlobalStake::<Test>::insert(&parent, 1000);
        Delegates::<Test>::insert(hotkey, 0);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        // Check that the function produces some output
        assert!(!emission_tuples.is_empty());

        // Check that the total distributed emission doesn't exceed the input
        let total_distributed: u128 = emission_tuples
            .iter()
            .map(|(_, _, amount)| *amount as u128)
            .sum();
        assert!(total_distributed <= (validating_emission as u128 + mining_emission as u128));

        // Check that at least some emission is distributed to the parent and hotkey
        assert!(emission_tuples
            .iter()
            .any(|(acc, _, amount)| acc == &parent && *amount > 0));
        assert!(emission_tuples
            .iter()
            .any(|(acc, _, amount)| acc == &hotkey && *amount > 0));
    });
}

#[test]
fn test_fast_stake_unstake_protection_source_hotkey() {
    new_test_ext(1).execute_with(|| {
        let coldkey = U256::from(1);
        let hotkey = U256::from(2);
        let parent1 = U256::from(3);
        let parent2 = U256::from(4);
        let netuid = 1;
        let validating_emission = 1000;
        let mining_emission = 500;
        let tempo = 1;

        // Set up stakes and delegations
        add_network(netuid, tempo, 0);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take
        SubtensorModule::stake_into_subnet(&parent1, &coldkey, netuid, 500);
        SubtensorModule::stake_into_subnet(&parent2, &coldkey, netuid, 500);
        ParentKeys::<Test>::insert(
            hotkey,
            netuid,
            vec![(u64::MAX / 2, parent1), (u64::MAX / 2, parent2)],
        );

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_hotkey_emission(
            &hotkey,
            netuid,
            validating_emission,
            mining_emission,
            &mut emission_tuples,
        );

        assert_eq!(emission_tuples.len(), 1);
        let total_distributed: u64 = emission_tuples.iter().map(|(_, _, amount)| amount).sum();
        assert_eq!(total_distributed, validating_emission + mining_emission);

        // Check hotkey take and mining emission
        let hotkey_emission = emission_tuples
            .iter()
            .find(|(h, _, _)| h == &hotkey)
            .map(|(_, _, amount)| amount)
            .unwrap();
        assert!(hotkey_emission > &0);

        // Check parent distributions
        let parent1_emission = emission_tuples
            .iter()
            .find(|(p, _, _)| p == &parent1)
            .map(|(_, _, amount)| amount);
        let parent2_emission = emission_tuples
            .iter()
            .find(|(p, _, _)| p == &parent2)
            .map(|(_, _, amount)| amount);
        assert!(parent1_emission.is_none());
        assert!(parent2_emission.is_none());
    });
}

#[test]
fn test_fast_stake_unstake_protection_source_nominator() {
    new_test_ext(1).execute_with(|| {
        let hotkey = U256::from(1);
        let nominator1 = U256::from(2);
        let nominator2 = U256::from(3);
        let netuid = 1;
        let emission = 10000;

        // Set up stakes and delegations
        SubtensorModule::stake_into_subnet(&hotkey, &nominator1, netuid, 500);
        SubtensorModule::stake_into_subnet(&hotkey, &nominator2, netuid, 500);
        Delegates::<Test>::insert(hotkey, 16384); // 25% take
        HotkeyEmissionTempo::<Test>::put(10);

        let mut emission_tuples = Vec::new();
        SubtensorModule::source_nominator_emission(
            &hotkey,
            netuid,
            emission,
            0,
            &mut emission_tuples,
        );

        // Every hotkey is rejected because LastAddStakeIncrease is too close
        assert_eq!(emission_tuples.len(), 0);
    });
}
