use super::*;
extern crate alloc;
use codec::Compact;
use frame_support::pallet_prelude::{Decode, Encode};
use subtensor_macros::freeze_struct;

#[freeze_struct("44fd17b240416875")]
#[derive(Decode, Encode, PartialEq, Eq, Clone, Debug)]
pub struct DynamicInfo<T: Config> {
    netuid: Compact<u16>,
    owner_hotkey: T::AccountId,
    owner_coldkey: T::AccountId,
    subnet_name: Vec<Compact<u8>>,
    token_symbol: Vec<Compact<u8>>,
    tempo: Compact<u16>,
    last_step: Compact<u64>,
    blocks_since_last_step: Compact<u64>,
    emission: Compact<u64>,
    alpha_in: Compact<u64>,
    alpha_out: Compact<u64>,
    tao_in: Compact<u64>,
    alpha_out_emission: Compact<u64>,
    alpha_in_emission: Compact<u64>,
    tao_in_emission: Compact<u64>,
    pending_alpha_emission: Compact<u64>,
    pending_root_emission: Compact<u64>,
    network_registered_at: Compact<u64>,
    subnet_identity: Option<SubnetIdentity>,
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
            netuid: netuid.into(),
            owner_hotkey: SubnetOwnerHotkey::<T>::get(netuid),
            owner_coldkey: SubnetOwner::<T>::get(netuid),
            subnet_name: Self::get_name_for_subnet(netuid)
                .into_iter()
                .map(Compact)
                .collect(),
            token_symbol: Self::get_symbol_for_subnet(netuid)
                .into_iter()
                .map(Compact)
                .collect(),
            tempo: Tempo::<T>::get(netuid).into(),
            last_step: last_step.into(),
            blocks_since_last_step: blocks_since_last_step.into(),
            emission: EmissionValues::<T>::get(netuid).into(),
            alpha_in: SubnetAlphaIn::<T>::get(netuid).into(),
            alpha_out: SubnetAlphaOut::<T>::get(netuid).into(),
            tao_in: SubnetTAO::<T>::get(netuid).into(),
            alpha_out_emission: SubnetAlphaOutEmission::<T>::get(netuid).into(),
            alpha_in_emission: SubnetAlphaInEmission::<T>::get(netuid).into(),
            tao_in_emission: SubnetTaoInEmission::<T>::get(netuid).into(),
            pending_alpha_emission: PendingEmission::<T>::get(netuid).into(),
            pending_root_emission: PendingRootDivs::<T>::get(netuid).into(),
            network_registered_at: NetworkRegisteredAt::<T>::get(netuid).into(),
            subnet_identity: SubnetIdentities::<T>::get(netuid),
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
