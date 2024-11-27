#![cfg_attr(not(feature = "std"), no_std)]
extern crate alloc;
use alloc::vec::Vec;
use codec::Compact;
use frame_support::sp_runtime::AccountId32;
use pallet_subtensor::rpc_info::{
    delegate_info::DelegateInfo,
    neuron_info::{NeuronInfo, NeuronInfoLite},
    stake_info::StakeInfo,
    subnet_info::{SubnetHyperparams, SubnetInfo},
};

// Here we declare the runtime API. It is implemented it the `impl` block in
// src/neuron_info.rs, src/subnet_info.rs, and src/delegate_info.rs
sp_api::decl_runtime_apis! {
    pub trait DelegateInfoRuntimeApi {
        fn get_delegates() -> Vec<DelegateInfo<AccountId32>>;
        fn get_delegate( delegate_account: AccountId32 ) -> Option<DelegateInfo<AccountId32>>;
        fn get_delegated( delegatee_account: AccountId32 ) -> Vec<(DelegateInfo<AccountId32>, Compact<u64>)>;
    }

    pub trait NeuronInfoRuntimeApi {
        fn get_neurons(netuid: u16) -> Vec<NeuronInfo<AccountId32>>;
        fn get_neuron(netuid: u16, uid: u16) -> Option<NeuronInfo<AccountId32>>;
        fn get_neurons_lite(netuid: u16) -> Vec<NeuronInfoLite<AccountId32>>;
        fn get_neuron_lite(netuid: u16, uid: u16) -> Option<NeuronInfoLite<AccountId32>>;
    }

    pub trait SubnetInfoRuntimeApi {
        fn get_subnet_info(netuid: u16) -> Option<SubnetInfo<AccountId32>>;
        fn get_subnets_info() -> Vec<Option<SubnetInfo<AccountId32>>>;
        fn get_subnet_hyperparams(netuid: u16) -> Option<SubnetHyperparams>;
    }

    pub trait StakeInfoRuntimeApi {
        fn get_stake_info_for_coldkey( coldkey_account: AccountId32 ) -> Vec<StakeInfo<AccountId32>>;
        fn get_stake_info_for_coldkeys( coldkey_accounts: Vec<AccountId32> ) -> Vec<(AccountId32, Vec<StakeInfo<AccountId32>>)>;
    }

    pub trait SubnetRegistrationRuntimeApi {
        fn get_network_registration_cost() -> u64;
    }
}
