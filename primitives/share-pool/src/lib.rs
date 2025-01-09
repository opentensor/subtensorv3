#![cfg_attr(not(feature = "std"), no_std)]
#![allow(clippy::result_unit_err)]

use sp_std::marker;
use sp_std::ops::Neg;
use substrate_fixed::types::{I64F64, U64F64};

pub trait SharePoolDataOperations<Key> {
    /// Gets shared value
    fn get_shared_value(&self) -> U64F64;
    /// Gets single share for a given key
    fn get_share(&self, key: &Key) -> U64F64;
    // Tries to get a single share for a given key, as a result.
    fn try_get_share(&self, key: &Key) -> Result<U64F64, ()>;
    /// Gets share pool denominator
    fn get_denominator(&self) -> U64F64;
    /// Updates shared value by provided signed value
    fn set_shared_value(&mut self, value: U64F64);
    /// Update single share for a given key by provided signed value
    fn set_share(&mut self, key: &Key, share: U64F64);
    /// Update share pool denominator by provided signed value
    fn set_denominator(&mut self, update: U64F64);
}

/// SharePool struct that depends on the Key type and uses the SharePoolDataOperations
#[derive(Debug)]
pub struct SharePool<K, Ops>
where
    K: Eq,
    Ops: SharePoolDataOperations<K>,
{
    state_ops: Ops,
    phantom_key: marker::PhantomData<K>,
}

impl<K, Ops> SharePool<K, Ops>
where
    K: Eq,
    Ops: SharePoolDataOperations<K>,
{
    pub fn new(ops: Ops) -> Self {
        SharePool {
            state_ops: ops,
            phantom_key: marker::PhantomData,
        }
    }

    pub fn get_value(&self, key: &K) -> u64 {
        let shared_value: U64F64 = self.state_ops.get_shared_value();
        let current_share: U64F64 = self.state_ops.get_share(key);
        let denominator: U64F64 = self.state_ops.get_denominator();

        shared_value
            .checked_div(denominator)
            .unwrap_or(U64F64::from_num(0))
            .saturating_mul(current_share)
            .to_num::<u64>()
    }

    pub fn try_get_value(&self, key: &K) -> Result<u64, ()> {
        match self.state_ops.try_get_share(key) {
            Ok(_) => Ok(self.get_value(key)),
            Err(i) => Err(i),
        }
    }

    /// Update the total shared value.
    /// Every key's associated value effectively updates with this operation
    pub fn update_value_for_all(&mut self, update: i64) {
        let shared_value: U64F64 = self.state_ops.get_shared_value();
        self.state_ops.set_shared_value(if update >= 0 {
            shared_value.saturating_add(U64F64::from_num(update))
        } else {
            shared_value.saturating_sub(U64F64::from_num(update.neg()))
        });
    }

    /// Update the value associated with an item identified by the Key
    pub fn update_value_for_one(&mut self, key: &K, update: i64) {
        let shared_value: U64F64 = self.state_ops.get_shared_value();
        let current_share: U64F64 = self.state_ops.get_share(key);
        let denominator: U64F64 = self.state_ops.get_denominator();

        // First, update shared value
        self.update_value_for_all(update);
        let new_shared_value: U64F64 = self.state_ops.get_shared_value();

        // Then, update this key's share
        if denominator == 0 {
            // Initialize the pool. The first key gets all.
            self.state_ops.set_denominator(new_shared_value);
            self.state_ops.set_share(key, new_shared_value);
        } else {
            // There are already keys in the pool, set or update this key
            let value_per_share: I64F64 = I64F64::from_num(
                shared_value
                    .checked_div(denominator) // denominator is never 0 here
                    .unwrap_or(U64F64::from_num(0)),
            );

            let shares_per_update: I64F64 = I64F64::from_num(update)
                .checked_div(value_per_share)
                .unwrap_or(I64F64::from_num(0));

            if shares_per_update >= 0 {
                self.state_ops.set_denominator(
                    denominator.saturating_add(U64F64::from_num(shares_per_update)),
                );
                self.state_ops.set_share(
                    key,
                    current_share.saturating_add(U64F64::from_num(shares_per_update)),
                );
            } else {
                self.state_ops.set_denominator(
                    denominator.saturating_sub(U64F64::from_num(shares_per_update.neg())),
                );
                self.state_ops.set_share(
                    key,
                    current_share.saturating_sub(U64F64::from_num(shares_per_update.neg())),
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    struct MockSharePoolDataOperations {
        shared_value: U64F64,
        share: BTreeMap<u16, U64F64>,
        denominator: U64F64,
    }

    impl MockSharePoolDataOperations {
        fn new() -> Self {
            MockSharePoolDataOperations {
                shared_value: U64F64::from_num(0),
                share: BTreeMap::new(),
                denominator: U64F64::from_num(0),
            }
        }
    }

    impl SharePoolDataOperations<u16> for MockSharePoolDataOperations {
        fn get_shared_value(&self) -> U64F64 {
            self.shared_value
        }

        fn get_share(&self, key: &u16) -> U64F64 {
            *self.share.get(key).unwrap_or(&U64F64::from_num(0))
        }

        fn try_get_share(&self, key: &u16) -> Result<U64F64, ()> {
            match self.share.get(key) {
                Some(&value) => Ok(value),
                None => Err(()),
            }
        }

        fn get_denominator(&self) -> U64F64 {
            self.denominator
        }

        fn set_shared_value(&mut self, value: U64F64) {
            self.shared_value = value;
        }

        fn set_share(&mut self, key: &u16, share: U64F64) {
            self.share.insert(*key, share);
        }

        fn set_denominator(&mut self, update: U64F64) {
            self.denominator = update;
        }
    }

    #[test]
    fn test_get_value() {
        let mut mock_ops = MockSharePoolDataOperations::new();
        mock_ops.set_denominator(U64F64::from_num(10));
        mock_ops.set_share(&1_u16, U64F64::from_num(3));
        mock_ops.set_share(&2_u16, U64F64::from_num(7));
        mock_ops.set_shared_value(U64F64::from_num(100));
        let share_pool = SharePool::new(mock_ops);
        let result1 = share_pool.get_value(&1);
        let result2 = share_pool.get_value(&2);
        assert_eq!(result1, 30);
        assert_eq!(result2, 70);
    }

    #[test]
    fn test_division_by_zero() {
        let mut mock_ops = MockSharePoolDataOperations::new();
        mock_ops.set_denominator(U64F64::from_num(0)); // Zero denominator
        let pool = SharePool::<u16, MockSharePoolDataOperations>::new(mock_ops);

        let value = pool.get_value(&1);
        assert_eq!(value, 0, "Value should be 0 when denominator is zero");
    }

    #[test]
    fn test_max_shared_value() {
        let mut mock_ops = MockSharePoolDataOperations::new();
        mock_ops.set_shared_value(U64F64::from_num(u64::MAX));
        mock_ops.set_share(&1, U64F64::from_num(3)); // Use a neutral value for share
        mock_ops.set_share(&2, U64F64::from_num(7)); // Use a neutral value for share
        mock_ops.set_denominator(U64F64::from_num(10)); // Neutral value to see max effect
        let pool = SharePool::<u16, MockSharePoolDataOperations>::new(mock_ops);

        let max_value = pool.get_value(&1) + pool.get_value(&2);
        assert!(u64::MAX - max_value <= 5, "Max value should map to u64 MAX");
    }

    #[test]
    fn test_max_share_value() {
        let mut mock_ops = MockSharePoolDataOperations::new();
        mock_ops.set_shared_value(U64F64::from_num(1_000_000_000)); // Use a neutral value for shared value
        mock_ops.set_share(&1, U64F64::from_num(u64::MAX / 2));
        mock_ops.set_share(&2, U64F64::from_num(u64::MAX / 2));
        mock_ops.set_denominator(U64F64::from_num(u64::MAX));
        let pool = SharePool::<u16, MockSharePoolDataOperations>::new(mock_ops);

        let value1 = pool.get_value(&1) as i128;
        let value2 = pool.get_value(&2) as i128;

        assert!((value1 - 500_000_000).abs() <= 1);
        assert!((value2 - 500_000_000).abs() <= 1);
    }

    #[test]
    fn test_denom_precision() {
        let mock_ops = MockSharePoolDataOperations::new();
        let mut pool = SharePool::<u16, MockSharePoolDataOperations>::new(mock_ops);

        pool.update_value_for_one(&1, 1000);

        let value_tmp = pool.get_value(&1) as i128;
        assert_eq!(value_tmp, 1000);

        pool.update_value_for_one(&1, -990);
        pool.update_value_for_one(&2, 1000);
        pool.update_value_for_one(&2, -990);

        let value1 = pool.get_value(&1) as i128;
        let value2 = pool.get_value(&2) as i128;

        assert_eq!(value1, 10);
        assert_eq!(value2, 10);
    }

    #[test]
    fn test_update_value_for_one() {
        let mock_ops = MockSharePoolDataOperations::new();
        let mut pool = SharePool::<u16, MockSharePoolDataOperations>::new(mock_ops);

        pool.update_value_for_one(&1, 1000);

        let value = pool.get_value(&1) as i128;
        assert_eq!(value, 1000);
    }

    #[test]
    fn test_update_value_for_all() {
        let mock_ops = MockSharePoolDataOperations::new();
        let mut pool = SharePool::<u16, MockSharePoolDataOperations>::new(mock_ops);

        pool.update_value_for_all(1000);
        assert_eq!(pool.state_ops.shared_value, U64F64::from_num(1000));
    }
}
