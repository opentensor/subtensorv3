use super::*;
use frame_support::pallet_prelude::{Decode, Encode};
extern crate alloc;
use codec::Compact;

#[freeze_struct("7ba412c8ac3f4677")]
#[derive(Decode, Encode, PartialEq, Eq, Clone, Debug, TypeInfo)]
pub struct StakeInfo<AccountId: TypeInfo + Encode + Decode> {
    hotkey: AccountId,
    coldkey: AccountId,
    stake: Compact<u64>,
}

impl<T: Config> Pallet<T> {
    fn _get_stake_info_for_coldkeys(
        coldkeys: Vec<T::AccountId>,
    ) -> Vec<(T::AccountId, Vec<StakeInfo<T::AccountId>>)> {
        if coldkeys.is_empty() {
            return Vec::new(); // No coldkeys to check
        }

        let mut stake_info: Vec<(T::AccountId, Vec<StakeInfo<T::AccountId>>)> = Vec::new();
        for coldkey_ in coldkeys {
            let mut stake_info_for_coldkey: Vec<StakeInfo<T::AccountId>> = Vec::new();

            for (hotkey, coldkey, stake) in <Stake<T>>::iter() {
                if coldkey == coldkey_ {
                    stake_info_for_coldkey.push(StakeInfo {
                        hotkey,
                        coldkey,
                        stake: stake.into(),
                    });
                }
            }

            stake_info.push((coldkey_, stake_info_for_coldkey));
        }

        stake_info
    }

    pub fn get_stake_info_for_coldkeys(
        coldkeys: Vec<T::AccountId>,
    ) -> Vec<(T::AccountId, Vec<StakeInfo<T::AccountId>>)> {
        if coldkeys.is_empty() {
            return Vec::new(); // Invalid coldkey
        }

        Self::_get_stake_info_for_coldkeys(coldkeys)
    }

    pub fn get_stake_info_for_coldkey(coldkey: T::AccountId) -> Vec<StakeInfo<T::AccountId>> {
        let stake_info = Self::_get_stake_info_for_coldkeys(vec![coldkey]);

        if stake_info.is_empty() {
            Vec::new() // Invalid coldkey
        } else {
            let Some(first) = stake_info.first() else {
                return Vec::new();
            };

            first.1.clone()
        }
    }
}
