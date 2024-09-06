use super::*;
extern crate alloc;
use codec::Compact;
use frame_support::pallet_prelude::{Decode, Encode};

#[derive(Decode, Encode, PartialEq, Eq, Clone, Debug)]
pub struct DynamicInfo<T: Config> {
    owner: T::AccountId,
    netuid: Compact<u16>,
    tempo: Compact<u16>,
    last_step: Compact<u64>,
    blocks_since_last_step: Compact<u64>,
    emission: Compact<u64>,
    alpha_in: Compact<u64>,
    alpha_out: Compact<u64>,
    tao_in: Compact<u64>,
    total_locked: Compact<u64>,
    owner_locked: Compact<u64>,
}

impl<T: Config> Pallet<T> {
    pub fn get_dynamic_info(netuid: u16) -> Option<DynamicInfo<T>> {
        if !Self::if_subnet_exist(netuid) {
            return None;
        }
        let last_step: u64 = LastMechansimStepBlock::<T>::get(netuid);
        let current_block: u64 = Pallet::<T>::get_current_block_as_u64();
        let blocks_since_last_step: u64 = current_block.saturating_sub(last_step);
        Some(DynamicInfo {
            owner: SubnetOwner::<T>::get(netuid),
            netuid: netuid.into(),
            tempo: Tempo::<T>::get(netuid).into(),
            last_step: last_step.into(),
            blocks_since_last_step: blocks_since_last_step.into(),
            emission: EmissionValues::<T>::get(netuid).into(),
            alpha_in: SubnetAlphaIn::<T>::get(netuid).into(),
            alpha_out: SubnetAlphaOut::<T>::get(netuid).into(),
            tao_in: SubnetTAO::<T>::get(netuid).into(),
            total_locked: SubnetLocked::<T>::get(netuid).into(),
            owner_locked: LargestLocked::<T>::get(netuid).into(),
        })
    }
    pub fn get_all_dynamic_info() -> Vec<Option<DynamicInfo<T>>> {
        let netuids: Vec<u16> = Self::get_all_subnet_netuids();
        let mut dynamic_info = Vec::<Option<DynamicInfo<T>>>::new();
        for netuid in netuids.clone().iter() {
            dynamic_info.push(Self::get_dynamic_info(*netuid));
        }
        dynamic_info
    }
}
