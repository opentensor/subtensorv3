#![allow(
    clippy::arithmetic_side_effects,
    clippy::unwrap_used,
    clippy::indexing_slicing
)]
mod mock;
use crate::mock::*;
use pallet_subtensor::*;
use sp_core::Get;

// 1. Test Zero Issuance
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_zero_issuance -- --exact --nocapture
#[test]
fn test_zero_issuance() {
    new_test_ext(1).execute_with(|| {
        let result = SubtensorModule::get_block_emission_for_issuance(0);
        assert!(result.is_ok());
        let emission = result.unwrap();
        assert_eq!(emission, DefaultBlockEmission::<Test>::get());
    });
}

// 2. Test Maximum Issuance (Equal to Total Supply)
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_maximum_issuance -- --exact --nocapture
#[test]
fn test_maximum_issuance() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let result = SubtensorModule::get_block_emission_for_issuance(total_supply);
        assert!(result.is_ok());
        let emission = result.unwrap();
        assert_eq!(emission, 0);
    });
}

// 3. Test Issuance Just Below Total Supply
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_issuance_just_below_total_supply -- --exact --nocapture
#[test]
fn test_issuance_just_below_total_supply() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let issuance = total_supply - 1_000_000_000;
        let result = SubtensorModule::get_block_emission_for_issuance(issuance);
        assert!(result.is_ok());
        let emission = result.unwrap();
        assert!(emission > 0);
        assert!(emission < DefaultBlockEmission::<Test>::get());
    });
}
// 4. Test Minimum Non-Zero Issuance
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_minimum_non_zero_issuance -- --exact --nocapture
#[test]
fn test_minimum_non_zero_issuance() {
    new_test_ext(1).execute_with(|| {
        let result = SubtensorModule::get_block_emission_for_issuance(1);
        assert!(result.is_ok());
        let emission = result.unwrap();
        assert!(emission > 0);
        assert!(emission <= DefaultBlockEmission::<Test>::get());
    });
}

// 5. Test Default Block Emission
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_default_block_emission -- --exact --nocapture
#[test]
fn test_default_block_emission() {
    new_test_ext(1).execute_with(|| {
        let result = SubtensorModule::get_block_emission();
        assert!(result.is_ok());
        let emission = result.unwrap();
        assert!(emission > 0);
        assert!(emission <= DefaultBlockEmission::<Test>::get());
    });
}

// 6. Test Logarithm Calculation at Boundary Conditions
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_logarithm_calculation_boundary -- --exact --nocapture
#[test]
fn test_logarithm_calculation_boundary() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let half_supply = total_supply / 2;

        // Test just below half supply
        let result = SubtensorModule::get_block_emission_for_issuance(half_supply - 1);
        assert!(result.is_ok());

        // Test at half supply
        let result = SubtensorModule::get_block_emission_for_issuance(half_supply);
        assert!(result.is_ok());

        // Test just above half supply
        let result = SubtensorModule::get_block_emission_for_issuance(half_supply + 1);
        assert!(result.is_ok());
    });
}

// 7. Test Rounding Behavior
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_rounding_behavior -- --exact --nocapture
#[test]
fn test_rounding_behavior() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let test_issuance = total_supply / 4;

        let result1 = SubtensorModule::get_block_emission_for_issuance(test_issuance);
        let result2 = SubtensorModule::get_block_emission_for_issuance(test_issuance + 1);

        assert!(result1.is_ok() && result2.is_ok());
        let emission1 = result1.unwrap();
        let emission2 = result2.unwrap();

        // Check if rounding is consistent (either always rounding down or always rounding up)
        assert!(emission1 >= emission2);
    });
}
// 8. Test Precision Loss
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_precision_loss -- --exact --nocapture
#[test]
fn test_precision_loss() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let test_issuance = total_supply / 2 - 100_000_000_000; // move away from halving.

        let result1 = SubtensorModule::get_block_emission_for_issuance(test_issuance);
        let result2 = SubtensorModule::get_block_emission_for_issuance(test_issuance + 1);

        assert!(result1.is_ok() && result2.is_ok());
        let emission1 = result1.unwrap();
        let emission2 = result2.unwrap();

        // Check if the difference between emissions is reasonable
        // This assumes that a small change in issuance should not cause a large change in emission
        assert!((emission1 as i64 - emission2 as i64).abs() < 1000);
    });
}

// 9. Test Overflow Handling in Intermediate Calculations
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_overflow_handling -- --exact --nocapture
#[test]
fn test_overflow_handling() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let test_issuance = total_supply - 1;

        let result = SubtensorModule::get_block_emission_for_issuance(test_issuance);
        assert!(result.is_ok());

        // Test with maximum possible issuance
        let result_max = SubtensorModule::get_block_emission_for_issuance(u64::MAX);
        assert!(result_max.is_ok());
        assert_eq!(result_max.unwrap(), 0);
    });
}

// 10. Test Underflow Handling in Intermediate Calculations
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_underflow_handling -- --exact --nocapture
#[test]
fn test_underflow_handling() {
    new_test_ext(1).execute_with(|| {
        // Test with very small issuance values
        let result_small = SubtensorModule::get_block_emission_for_issuance(1);
        assert!(result_small.is_ok());

        let result_zero = SubtensorModule::get_block_emission_for_issuance(0);
        assert!(result_zero.is_ok());

        // The emission for zero issuance should be the maximum (DefaultBlockEmission)
        assert_eq!(result_zero.unwrap(), DefaultBlockEmission::<Test>::get());
    });
}
// 11. Test Division by Zero Prevention
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_division_by_zero_prevention -- --exact --nocapture
#[test]
fn test_division_by_zero_prevention() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();

        // Test with issuance equal to total supply
        let result = SubtensorModule::get_block_emission_for_issuance(total_supply);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 0);

        // Test with issuance greater than total supply
        let result = SubtensorModule::get_block_emission_for_issuance(total_supply + 1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 0);
    });
}

// 12. Test Emission Rate Decrease with Increasing Issuance
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_emission_rate_decrease -- --exact --nocapture
#[test]
fn test_emission_rate_decrease() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let mut last_emission = u64::MAX;

        for i in (0..=10).map(|x| x * (total_supply / 10)) {
            let emission = SubtensorModule::get_block_emission_for_issuance(i).unwrap();
            assert!(
                emission <= last_emission,
                "Emission should decrease or stay the same as issuance increases"
            );
            last_emission = emission;
        }
    });
}

// 14. Test Block Emission Storage Update
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_block_emission_storage_update -- --exact --nocapture
#[test]
fn test_block_emission_storage_update() {
    new_test_ext(1).execute_with(|| {
        let initial_emission = BlockEmission::<Test>::get();
        let new_issuance = TotalSupply::<Test>::get() / 2;

        // Call get_block_emission_for_issuance to trigger an update
        let _ = SubtensorModule::get_block_emission_for_issuance(new_issuance).unwrap();

        let updated_emission = BlockEmission::<Test>::get();
        assert_ne!(
            initial_emission, updated_emission,
            "BlockEmission should be updated"
        );

        // Call again with the same issuance to ensure no unnecessary updates
        let _ = SubtensorModule::get_block_emission_for_issuance(new_issuance).unwrap();
        assert_eq!(
            updated_emission,
            BlockEmission::<Test>::get(),
            "BlockEmission should not change if emission hasn't changed"
        );
    });
}

// 15. Test Consistency Between get_block_emission() and get_block_emission_for_issuance()
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_emission_consistency -- --exact --nocapture
#[test]
fn test_emission_consistency() {
    new_test_ext(1).execute_with(|| {
        let emission_from_get = SubtensorModule::get_block_emission().unwrap();
        let total_issuance = SubtensorModule::get_total_issuance();
        let emission_for_issuance =
            SubtensorModule::get_block_emission_for_issuance(total_issuance).unwrap();

        assert_eq!(
            emission_from_get, emission_for_issuance,
            "Emissions should be consistent between methods"
        );
    });
}

// 16. Test Performance with Large Issuance Values
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_performance_large_issuance -- --exact --nocapture
#[test]
fn test_performance_large_issuance() {
    new_test_ext(1).execute_with(|| {
        let large_issuance = TotalSupply::<Test>::get() - 1_000_000_000;

        let start = std::time::Instant::now();
        let emission = SubtensorModule::get_block_emission_for_issuance(large_issuance).unwrap();
        let duration = start.elapsed();

        println!("Time taken for large issuance calculation: {:?}", duration);
        assert!(
            duration < std::time::Duration::from_millis(10),
            "Calculation took too long"
        );
        assert!(
            emission > 0,
            "Emission should be non-zero for large issuance"
        );
    });
}
// 17. Test Performance with Small Issuance Values
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_performance_small_issuance -- --exact --nocapture
#[test]
fn test_performance_small_issuance() {
    new_test_ext(1).execute_with(|| {
        let small_issuance = 1000; // A small issuance value

        let start = std::time::Instant::now();
        let emission = SubtensorModule::get_block_emission_for_issuance(small_issuance).unwrap();
        let duration = start.elapsed();

        println!("Time taken for small issuance calculation: {:?}", duration);
        assert!(
            duration < std::time::Duration::from_millis(10),
            "Calculation took too long"
        );
        assert!(
            emission > 0,
            "Emission should be non-zero for small issuance"
        );
    });
}

// 18. Test Emission at Key Issuance Milestones (e.g., 25%, 50%, 75% of Total Supply)
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_emission_at_key_milestones -- --exact --nocapture
#[test]
fn test_emission_at_key_milestones() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let milestones = vec![0.25, 0.5, 0.75];
        let mut last_emission = u64::MAX;

        for milestone in milestones {
            let issuance = (total_supply as f64 * milestone) as u64;
            let emission = SubtensorModule::get_block_emission_for_issuance(issuance).unwrap();

            println!(
                "Emission at {}% of total supply: {}",
                milestone * 100.0,
                emission
            );
            assert!(
                emission < last_emission,
                "Emission should decrease as issuance increases"
            );
            last_emission = emission;
        }
    });
}

// 19. Test Behavior Near Total Supply Limit
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_behavior_near_total_supply -- --exact --nocapture
#[test]
fn test_behavior_near_total_supply() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let near_total_supply = total_supply - 1_000_000_000; // Very close to total supply

        let emission_near_limit =
            SubtensorModule::get_block_emission_for_issuance(near_total_supply).unwrap();
        assert!(
            emission_near_limit > 0,
            "Emission should still be positive near total supply"
        );

        let emission_at_limit =
            SubtensorModule::get_block_emission_for_issuance(total_supply).unwrap();
        assert_eq!(
            emission_at_limit, 0,
            "Emission should be zero at total supply"
        );

        let emission_over_limit =
            SubtensorModule::get_block_emission_for_issuance(total_supply + 1).unwrap();
        assert_eq!(
            emission_over_limit, 0,
            "Emission should remain zero above total supply"
        );
    });
}
// 20. Test with Maximum u64 Value as Issuance
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_maximum_u64_issuance -- --exact --nocapture
#[test]
fn test_maximum_u64_issuance() {
    new_test_ext(1).execute_with(|| {
        let max_issuance = u64::MAX;
        let emission = SubtensorModule::get_block_emission_for_issuance(max_issuance).unwrap();
        assert_eq!(
            emission, 0,
            "Emission should be zero for maximum u64 issuance"
        );
    });
}

// 21. Test with Issuance Values That Cause Extreme Residuals
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_extreme_residuals -- --exact --nocapture
#[test]
fn test_extreme_residuals() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let test_cases = vec![
            1,                // Very small issuance
            total_supply / 2, // Half of total supply
            total_supply - 1, // Just below total supply
        ];

        for issuance in test_cases {
            let emission = SubtensorModule::get_block_emission_for_issuance(issuance).unwrap();
            println!("Issuance: {}, Emission: {}", issuance, emission);
            assert!(
                emission <= DefaultBlockEmission::<Test>::get(),
                "Emission should not exceed DefaultBlockEmission"
            );
        }
    });
}

// 22. Test Stability of Output Across Multiple Calls
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_output_stability -- --exact --nocapture
#[test]
fn test_output_stability() {
    new_test_ext(1).execute_with(|| {
        let test_issuance = 1_000_000_000; // 1 billion
        let num_calls = 1000;
        let first_emission =
            SubtensorModule::get_block_emission_for_issuance(test_issuance).unwrap();

        for _ in 0..num_calls {
            let emission = SubtensorModule::get_block_emission_for_issuance(test_issuance).unwrap();
            assert_eq!(
                emission, first_emission,
                "Emission should be stable across multiple calls"
            );
        }
    });
}

// 25. Test with Issuance Values That Produce Very Small Emissions
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_very_small_emissions -- --exact --nocapture
#[test]
fn test_very_small_emissions() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let test_cases = vec![
            total_supply - 1_000_000_000, // Just below total supply
            total_supply - 10_000_000_000,
            total_supply - 100_000_000_000,
            total_supply - 1_000_000_000_000,
        ];

        for issuance in test_cases {
            let emission = SubtensorModule::get_block_emission_for_issuance(issuance).unwrap();
            println!("Issuance: {}, Emission: {}", issuance, emission);
            assert!(emission > 0, "Emission should be positive");
            assert!(emission < 1_000_000_000, "Emission should be very small");
        }
    });
}
// 26. Test Emission Calculation Time Complexity
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_emission_calculation_time_complexity -- --exact --nocapture
#[test]
fn test_emission_calculation_time_complexity() {
    new_test_ext(1).execute_with(|| {
        use std::time::Instant;

        let total_supply = TotalSupply::<Test>::get();
        let test_cases = vec![
            1,
            total_supply / 4,
            total_supply / 2,
            3 * total_supply / 4,
            total_supply - 1,
        ];

        for issuance in test_cases {
            let start = Instant::now();
            let _ = SubtensorModule::get_block_emission_for_issuance(issuance).unwrap();
            let duration = start.elapsed();

            println!("Issuance: {}, Calculation time: {:?}", issuance, duration);
            assert!(
                duration.as_micros() < 1000,
                "Calculation took too long: {:?}",
                duration
            );
        }
    });
}

// 27. Test Emission Values Across Full Range of Possible Issuances
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_emission_values_across_issuance_range -- --exact --nocapture
#[test]
fn test_emission_values_across_issuance_range() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let step = total_supply / 100; // Test 100 points across the range

        let mut last_emission = DefaultBlockEmission::<Test>::get();
        for issuance in (0..total_supply).step_by(step as usize) {
            let emission = SubtensorModule::get_block_emission_for_issuance(issuance).unwrap();
            println!("Issuance: {}, Emission: {}", issuance, emission);

            assert!(
                emission <= last_emission,
                "Emission should decrease or stay the same as issuance increases"
            );
            assert!(
                emission <= DefaultBlockEmission::<Test>::get(),
                "Emission should not exceed DefaultBlockEmission"
            );

            last_emission = emission;
        }
    });
}

// 28. Test Consistency of Emission Decrease Rate
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_consistency_of_emission_decrease_rate -- --exact --nocapture
#[test]
fn test_consistency_of_emission_decrease_rate() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let step = total_supply / 1000; // Test 1000 points across the range

        let mut last_emission = DefaultBlockEmission::<Test>::get();
        let mut last_decrease_rate = 0.0;

        for issuance in (0..total_supply).step_by(step as usize) {
            let emission = SubtensorModule::get_block_emission_for_issuance(issuance).unwrap();

            if last_emission != emission {
                let decrease_rate = (last_emission - emission) as f64 / last_emission as f64;

                if last_decrease_rate != 0.0 {
                    let rate_change = (decrease_rate - last_decrease_rate).abs();
                    println!(
                        "Issuance: {}, Emission: {}, Decrease rate: {}, Rate change: {}",
                        issuance, emission, decrease_rate, rate_change
                    );
                    assert!(
                        rate_change < 0.1,
                        "Emission decrease rate should change smoothly"
                    );
                }

                last_decrease_rate = decrease_rate;
            }

            last_emission = emission;
        }
    });
}

// 30. Test Impact of Floating Point Precision on Final Emission Value
// SKIP_WASM_BUILD=1 RUST_LOG=debug cargo test --test block_emission test_floating_point_precision_impact -- --exact --nocapture
#[test]
fn test_floating_point_precision_impact() {
    new_test_ext(1).execute_with(|| {
        let total_supply = TotalSupply::<Test>::get();
        let default_emission = DefaultBlockEmission::<Test>::get();

        // Test with very small issuance values
        for i in 1..=10 {
            let issuance = i;
            let emission = SubtensorModule::get_block_emission_for_issuance(issuance).unwrap();
            println!("Small Issuance: {}, Emission: {}", issuance, emission);
            assert!(
                emission <= default_emission,
                "Emission should not exceed default emission"
            );
        }

        // Test with issuance values very close to total supply
        for i in 1..=10 {
            let issuance = total_supply.saturating_sub(i * 1_000_000_000);
            let emission = SubtensorModule::get_block_emission_for_issuance(issuance).unwrap();
            println!("Large Issuance: {}, Emission: {}", issuance, emission);
            assert!(
                emission > 0,
                "Emission should be positive when issuance is below total supply"
            );
        }

        // Test consistency of emission values for small changes in issuance
        let base_issuance = total_supply / 2;
        let base_emission =
            SubtensorModule::get_block_emission_for_issuance(base_issuance).unwrap();
        for i in 1..=10 {
            let issuance = base_issuance + i;
            let emission = SubtensorModule::get_block_emission_for_issuance(issuance).unwrap();
            let diff = if base_emission > emission {
                base_emission - emission
            } else {
                0
            };
            println!(
                "Issuance: {}, Emission: {}, Diff from base: {}",
                issuance, emission, diff
            );
            assert!(
                diff <= 1,
                "Emission should not change by more than 1 for small issuance changes"
            );
        }
    });
}
