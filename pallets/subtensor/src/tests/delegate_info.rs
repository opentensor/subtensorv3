use codec::Compact;
use substrate_fixed::types::U64F64;

use super::mock::*;

#[test]
fn test_return_per_1000_tao() {
    let take = // 18% take to the Validator
        Compact::<u16>::from((U64F64::from_num(0.18 * u16::MAX as f64)).saturating_to_num::<u16>());

    // 10_000 TAO total validator stake
    let total_stake = U64F64::from_num(10_000.0 * 1e9);
    // 1000 TAO emissions per day
    let emissions_per_day = U64F64::from_num(1000.0 * 1e9);

    let return_per_1000 =
        SubtensorModule::return_per_1000_tao_test(take, total_stake, emissions_per_day);

    // We expect 82 TAO per day with 10% of total_stake
    let expected_return_per_1000 = U64F64::from_num(82.0);

    let diff_from_expected: f64 = (return_per_1000 / U64F64::from_num(1e9))
        .saturating_sub(expected_return_per_1000)
        .to_num::<f64>();

    let eps: f64 = 0.0005e9; // Precision within 0.0005 TAO
    assert!(
        diff_from_expected.abs() <= eps,
        "Difference from expected: {} is greater than precision: {}",
        diff_from_expected,
        eps
    );
}
