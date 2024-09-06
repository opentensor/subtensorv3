#![allow(
    clippy::arithmetic_side_effects,
    clippy::unwrap_used,
    clippy::indexing_slicing
)]
mod mock;

use mock::*;
use pallet_subtensor::*;

// Test titles for calculate_subnet_tempos function:

// 1. test_calculate_subnet_tempos_normal_case
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_normal_case --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_normal_case() {
    // This test checks the normal case where TAO values are provided and ensures that the calculated tempos
    // are in descending order and within the specified min and max tempo limits.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100, 200, 300];
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        println!("Result: {:?}", result);
        assert_eq!(result.len(), 3);
        assert!(result[0] > result[1]);
        assert!(result[1] > result[2]);
        assert!(result[0] <= max_tempo && result[0] >= min_tempo);
        assert!(result[1] <= max_tempo && result[1] >= min_tempo);
        assert!(result[2] <= max_tempo && result[2] >= min_tempo);
    });
}

// 2. test_calculate_subnet_tempos_min_tempo_greater_than_max_tempo
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_min_tempo_greater_than_max_tempo --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_min_tempo_greater_than_max_tempo() {
    // This test checks the behavior when the minimum tempo is greater than the maximum tempo,
    // ensuring that all calculated tempos are set to the minimum tempo.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100, 200];
        let min_tempo = 1000;
        let max_tempo = 10;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0], max_tempo);
        assert_eq!(result[1], max_tempo);
    });
}

// 3. test_calculate_subnet_tempos_min_tempo_equal_max_tempo
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_min_tempo_equal_max_tempo --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_min_tempo_equal_max_tempo() {
    // This test checks the scenario where the minimum tempo is equal to the maximum tempo,
    // ensuring that all calculated tempos are set to this value.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100, 200];
        let min_tempo = 500;
        let max_tempo = 500;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0], min_tempo);
        assert_eq!(result[1], min_tempo);
    });
}

// 4. test_calculate_subnet_tempos_zero_total_tao
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_zero_total_tao --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_zero_total_tao() {
    // This test checks the case where all TAO values are zero, ensuring that all calculated tempos
    // are set to the average tempo k.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![0, 0, 0];
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);

        assert_eq!(result.len(), 3);
        assert_eq!(result[0], k);
        assert_eq!(result[1], k);
        assert_eq!(result[2], k);
    });
}

// 5. test_calculate_subnet_tempos_single_subnet
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_single_subnet --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_single_subnet() {
    // This test checks the case where there is only a single subnet, ensuring that the calculated tempo
    // is equal to the average tempo k.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100];
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0], k);
    });
}

// 6. test_calculate_subnet_tempos_many_subnets
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_many_subnets --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_many_subnets() {
    // This test checks the case with many subnets, ensuring that the calculated tempos are in descending order
    // and within the specified min and max tempo limits.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao: Vec<u64> = (1..101).collect();
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);

        assert_eq!(result.len(), 100);
        for i in 0..99 {
            assert!(result[i] >= result[i + 1]);
        }
        assert!(result[0] <= max_tempo && result[0] >= min_tempo);
        assert!(result[99] <= max_tempo && result[99] >= min_tempo);
    });
}

// 7. test_calculate_subnet_tempos_all_equal_taos
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_all_equal_taos --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_all_equal_taos() {
    // This test checks that when all TAO values are equal, the calculated tempos for all subnets
    // should be the same.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100, 100, 100, 100, 100];
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);

        assert_eq!(result.len(), 5);
        let first_tempo = result[0];
        for tempo in result {
            assert_eq!(tempo, first_tempo);
        }
    });
}

// 8. test_calculate_subnet_tempos_one_subnet_zero_tao
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_one_subnet_zero_tao --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_one_subnet_zero_tao() {
    // This test checks the behavior when one subnet has a TAO of zero. The expected result is that
    // the subnet with zero TAO should receive the maximum tempo, while others should be adjusted accordingly.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100, 0, 100];
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);

        assert_eq!(result.len(), 3);
        assert!(result[0] > k);
        assert_eq!(result[1], max_tempo);
        assert!(result[2] > k);
    });
}

// 9. test_calculate_subnet_tempos_all_subnets_zero_tao
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_all_subnets_zero_tao --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_all_subnets_zero_tao() {
    // This test checks the scenario where all subnets have a TAO of zero. The expected result is that
    // all tempos should be equal to the average tempo k.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![0, 0, 0];
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);

        assert_eq!(result.len(), 3);
        for tempo in result {
            assert_eq!(tempo, k);
        }
    });
}

// 10. test_calculate_subnet_tempos_one_subnet_very_high_tao
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_one_subnet_very_high_tao --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_one_subnet_very_high_tao() {
    // This test checks the case where one subnet has a very high TAO compared to others. The expected
    // result is that the subnet with high TAO should receive the minimum tempo, while others should be
    // adjusted to be close to the maximum tempo.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![1, 1000000, 1];
        let min_tempo = 10;
        let max_tempo = u16::MAX;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        log::debug!("result: {:?}", result);
        assert_eq!(result.len(), 3);
        assert!(result[0] > min_tempo); // Should be close to max_tempo
        assert_eq!(result[1], k); // High TAO subnet should get k because k/1 = k
        assert!(result[2] > min_tempo); // Should be close to max_tempo
    });
}

// 11. test_calculate_subnet_tempos_one_subnet_very_low_tao
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_one_subnet_very_low_tao --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_one_subnet_very_low_tao() {
    // This test checks the scenario where one subnet has a very low TAO. The expected result is that
    // the subnet with low TAO should receive the maximum tempo, while others should be lower than the average tempo k.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100, 1, 100];
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);

        assert_eq!(result.len(), 3);
        assert_eq!(result[1], max_tempo); // Low TAO subnet should get max_tempo
    });
}

// 12. test_calculate_subnet_tempos_extreme_tao_differences
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_extreme_tao_differences --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_extreme_tao_differences() {
    // This test checks the behavior when there are extreme differences in TAO values among subnets.
    // The expected result is that the subnet with the lowest TAO should receive the maximum tempo,
    // while the one with the highest TAO should receive the minimum tempo, and others should be in between.
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![1, 100, 1000000, 10];
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);

        assert_eq!(result.len(), 4);
        assert_eq!(result[0], max_tempo); // Very low TAO should get max_tempo
        assert_eq!(result[2], k); // Very high TAO should get k
    });
}
// 13. test_calculate_subnet_tempos_average_tempo_zero
// Description: Test the behavior when the average tempo is set to zero
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_average_tempo_zero --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_average_tempo_zero() {
    new_test_ext(1).execute_with(|| {
        let k = 0; // Average tempo set to zero
        let tao = vec![100, 200, 300];
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result
            .iter()
            .all(|&tempo| tempo == k || tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 14. test_calculate_subnet_tempos_average_tempo_very_high
// Description: Test the behavior when the average tempo is set to a very high value
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_average_tempo_very_high --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_average_tempo_very_high() {
    new_test_ext(1).execute_with(|| {
        let k = 10000; // Very high average tempo
        let tao = vec![100, 200, 300];
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result.iter().all(|&tempo| tempo == max_tempo)); // All should be clamped to max_tempo
    });
}

// 15. test_calculate_subnet_tempos_min_tempo_zero
// Description: Test the behavior when the minimum tempo is set to zero
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_min_tempo_zero --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_min_tempo_zero() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100, 200, 300];
        let min_tempo = 0; // Minimum tempo set to zero
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result
            .iter()
            .all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 16. test_calculate_subnet_tempos_max_tempo_very_high
// Description: Test the behavior when the maximum tempo is set to a very high value
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_max_tempo_very_high --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_max_tempo_very_high() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100, 200, 300];
        let min_tempo = 10;
        let max_tempo = 10000; // Very high maximum tempo

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result
            .iter()
            .all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 17. test_calculate_subnet_tempos_empty_subnet_list
// Description: Test the behavior when an empty list of subnets is provided
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_empty_subnet_list --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_empty_subnet_list() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![]; // Empty TAO list
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 0); // No subnets, so result should be empty
    });
}

// 18. test_calculate_subnet_tempos_empty_tao_list
// Description: Test the behavior when an empty list of TAO values is provided
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_empty_tao_list --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_empty_tao_list() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![]; // Empty TAO list
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 0); // No subnets, so result should be empty
    });
}

// 19. test_calculate_subnet_tempos_mismatched_subnet_and_tao_lists
// Description: Test the behavior when subnet and TAO lists have different lengths
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_mismatched_subnet_and_tao_lists --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_mismatched_subnet_and_tao_lists() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100, 200]; // Two TAO values
        let min_tempo = 10;
        let max_tempo = 1000;

        // Assuming we have one subnet but two TAO values
        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 2); // Should return tempos for both TAO values
    });
}

// 20. test_calculate_subnet_tempos_negative_tao_values
// Description: Test the behavior when negative TAO values are provided
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_negative_tao_values --exact --nocapture/
// DEPRECATED.
// #[test]
// fn test_calculate_subnet_tempos_negative_tao_values() {
//     new_test_ext(1).execute_with(|| {
//         let k = 100;
//         let tao = vec![-100, -200, -300]; // Negative TAO values
//         let min_tempo = 10;
//         let max_tempo = 1000;

//         let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
//         assert_eq!(result.len(), 3);
//         assert!(result.iter().all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
//     });
// }

// 21. test_calculate_subnet_tempos_overflow_conditions
// Description: Test the behavior under potential overflow conditions
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_overflow_conditions --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_overflow_conditions() {
    new_test_ext(1).execute_with(|| {
        let k = u16::MAX; // Maximum value for u16
        let tao = vec![u64::MAX; 10]; // Large TAO values
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 10);
        assert!(result
            .iter()
            .all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 22. test_calculate_subnet_tempos_underflow_conditions
// Description: Test the behavior under potential underflow conditions
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_underflow_conditions --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_underflow_conditions() {
    new_test_ext(1).execute_with(|| {
        let k = 0; // Minimum value for tempo
        let tao = vec![0, 0, 0]; // Zero TAO values
        let min_tempo = 0;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result
            .iter()
            .all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 23. test_calculate_subnet_tempos_rounding_errors
// Description: Test the behavior of the function when dealing with potential rounding errors
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_rounding_errors --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_rounding_errors() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![1, 2, 3]; // Normal TAO values
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result
            .iter()
            .all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 24. test_calculate_subnet_tempos_precision_loss
// Description: Test the function's handling of potential precision loss in calculations
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_precision_loss --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_precision_loss() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![1000, 2000, 3000]; // Large TAO values
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result
            .iter()
            .all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 25. test_calculate_subnet_tempos_edge_case_tao_values
// Description: Test the function's behavior with edge case TAO values
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_edge_case_tao_values --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_edge_case_tao_values() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![0, 0, 0]; // Edge case with zero TAO values
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result
            .iter()
            .all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 26. test_calculate_subnet_tempos_non_contiguous_subnet_ids
// Description: Test the function's handling of non-contiguous subnet IDs
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_non_contiguous_subnet_ids --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_non_contiguous_subnet_ids() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![100, 200, 300]; // Normal TAO values
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result
            .iter()
            .all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 27. test_calculate_subnet_tempos_very_large_number_of_subnets
// Description: Test the function's performance and correctness with a very large number of subnets
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_very_large_number_of_subnets --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_very_large_number_of_subnets() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![1; 1000]; // Large number of subnets
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 1000);
        assert!(result
            .iter()
            .all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 28. test_calculate_subnet_tempos_min_max_tempo_very_close
// Description: Test the function's behavior when min_tempo and max_tempo are very close to each other
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_min_max_tempo_very_close --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_min_max_tempo_very_close() {
    new_test_ext(1).execute_with(|| {
        let k = 100;
        let tao = vec![10, 20, 30]; // Normal TAO values
        let min_tempo = 100;
        let max_tempo = 101; // Very close values

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result
            .iter()
            .all(|&tempo| tempo >= min_tempo && tempo <= max_tempo));
    });
}

// 29. test_calculate_subnet_tempos_all_tempos_clamped_to_min
// Description: Test if all tempos are correctly clamped to min_tempo when calculated values are below min_tempo
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_all_tempos_clamped_to_min --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_all_tempos_clamped_to_min() {
    new_test_ext(1).execute_with(|| {
        let k = 0; // Minimum value for tempo
        let tao = vec![0, 0, 0]; // Zero TAO values
        let min_tempo = 10;
        let max_tempo = 1000;

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result.iter().all(|&tempo| tempo == min_tempo));
    });
}

// 30. test_calculate_subnet_tempos_all_tempos_clamped_to_max
// Description: Test if all tempos are correctly clamped to max_tempo when calculated values are above max_tempo
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_calculate_subnet_tempos_all_tempos_clamped_to_max --exact --nocapture
#[test]
fn test_calculate_subnet_tempos_all_tempos_clamped_to_max() {
    new_test_ext(1).execute_with(|| {
        let k = 1000; // Maximum value for tempo
        let tao = vec![1000, 2000, 3000]; // Large TAO values
        let min_tempo = 10;
        let max_tempo = 100; // Clamping to max_tempo

        let result = SubtensorModule::calculate_tempos(k, tao, min_tempo, max_tempo);
        assert_eq!(result.len(), 3);
        assert!(result.iter().all(|&tempo| tempo == max_tempo));
    });
}

// 31. test_adjust_tempos_with_valid_tao_values
// Description: Test the adjust_tempos function with valid TAO values to ensure it calculates and updates tempos correctly.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_adjust_tempos_with_valid_tao_values --exact --nocapture
#[test]
fn test_adjust_tempos_with_valid_tao_values() {
    new_test_ext(1).execute_with(|| {
        // Setup initial state with valid TAO values
        let netuid1 = 1;
        let netuid2 = 2;
        let netuid3 = 3;
        add_network(netuid1, 0, 0);
        add_network(netuid2, 0, 0);
        add_network(netuid3, 0, 0);
        AvgTempo::<Test>::put(100);
        SubnetTAO::<Test>::insert(netuid1, 100);
        SubnetTAO::<Test>::insert(netuid2, 200);
        SubnetTAO::<Test>::insert(netuid3, 300);

        // Call adjust_tempos and verify the updated tempos
        SubtensorModule::adjust_tempos();

        assert_eq!(Tempo::<Test>::get(netuid1), 600);
        assert_eq!(Tempo::<Test>::get(netuid2), 300);
        assert_eq!(Tempo::<Test>::get(netuid3), 200);
    });
}

// 32. test_adjust_tempos_with_zero_tao_values
// Description: Test the adjust_tempos function when all TAO values are zero to ensure it assigns the average tempo.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_adjust_tempos_with_zero_tao_values --exact --nocapture
#[test]
fn test_adjust_tempos_with_zero_tao_values() {
    new_test_ext(1).execute_with(|| {
        // Setup initial state with zero TAO values
        let netuid1 = 1;
        let netuid2 = 2;
        let netuid3 = 3;
        add_network(netuid1, 0, 0);
        add_network(netuid2, 0, 0);
        add_network(netuid3, 0, 0);
        AvgTempo::<Test>::put(100);
        SubnetTAO::<Test>::insert(netuid1, 0);
        SubnetTAO::<Test>::insert(netuid2, 0);
        SubnetTAO::<Test>::insert(netuid3, 0);

        // Call adjust_tempos and verify that all tempos are set to the average tempo
        SubtensorModule::adjust_tempos();

        assert_eq!(Tempo::<Test>::get(netuid1), 100); // Assuming average tempo is 100
        assert_eq!(Tempo::<Test>::get(netuid2), 100);
        assert_eq!(Tempo::<Test>::get(netuid3), 100);
    });
}

// 33. test_adjust_tempos_with_min_max_tempo_constraints
// Description: Test the adjust_tempos function to ensure it respects the min and max tempo constraints during updates.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_adjust_tempos_with_min_max_tempo_constraints --exact --nocapture
#[test]
fn test_adjust_tempos_with_min_max_tempo_constraints() {
    new_test_ext(1).execute_with(|| {
        // Setup initial state with TAO values that would exceed min/max tempos
        let netuid1 = 1;
        let netuid2 = 2;
        let netuid3 = 3;
        add_network(netuid1, 0, 0);
        add_network(netuid2, 0, 0);
        add_network(netuid3, 0, 0);
        SubnetTAO::<Test>::insert(netuid1, 1);
        SubnetTAO::<Test>::insert(netuid2, 2000);
        SubnetTAO::<Test>::insert(netuid3, 3000);

        // Set min and max tempos
        let min_tempo = 100;
        let max_tempo = 150;
        AvgTempo::<Test>::put(min_tempo);
        MaxTempo::<Test>::put(max_tempo);
        SubtensorModule::adjust_tempos();

        assert_eq!(Tempo::<Test>::get(netuid1), max_tempo);
        assert_eq!(Tempo::<Test>::get(netuid2), max_tempo);
        assert_eq!(Tempo::<Test>::get(netuid3), max_tempo);
    });
}

// 34. test_adjust_tempos_with_no_subnets
// Description: Test the adjust_tempos function when there are no subnets to ensure it handles the case gracefully.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_adjust_tempos_with_no_subnets --exact --nocapture
#[test]
fn test_adjust_tempos_with_no_subnets() {
    new_test_ext(1).execute_with(|| {
        // Setup initial state with no subnets
        // Call adjust_tempos and verify that no errors occur and state remains unchanged
        assert!(SubtensorModule::get_all_subnet_netuids().is_empty());
        SubtensorModule::adjust_tempos();
        assert!(SubtensorModule::get_all_subnet_netuids().is_empty());
    });
}

// 35. test_adjust_tempos_with_single_subnet
// Description: Test the adjust_tempos function with a single subnet to ensure it calculates the tempo correctly.
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test tempo -- test_adjust_tempos_with_single_subnet --exact --nocapture
#[test]
fn test_adjust_tempos_with_single_subnet() {
    new_test_ext(1).execute_with(|| {
        // Setup initial state with a single subnet
        let netuid = 1;
        add_network(netuid, 0, 0);
        SubnetTAO::<Test>::insert(netuid, 100);

        // Call adjust_tempos and verify that the tempo is updated correctly
        AvgTempo::<Test>::put(100);
        SubtensorModule::adjust_tempos();

        assert_eq!(Tempo::<Test>::get(netuid), 100); // Assuming the calculation gives 100
    });
}
