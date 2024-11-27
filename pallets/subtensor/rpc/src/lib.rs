//! RPC interface for the custom Subtensor rpc methods

use codec::{Decode, Encode};
use jsonrpsee::{
    core::RpcResult,
    proc_macros::rpc,
    types::{error::ErrorObject, ErrorObjectOwned},
};
use sp_blockchain::HeaderBackend;
use sp_core::hexdisplay::AsBytesRef;
use sp_runtime::{traits::Block as BlockT, AccountId32};
use std::sync::Arc;

use sp_api::ProvideRuntimeApi;

pub use subtensor_custom_rpc_runtime_api::{
    DelegateInfoRuntimeApi, NeuronInfoRuntimeApi, SubnetInfoRuntimeApi,
    SubnetRegistrationRuntimeApi,
};

#[rpc(client, server)]
pub trait SubtensorCustomApi<BlockHash> {
    #[method(name = "delegateInfo_getDelegates")]
    fn get_delegates(&self, at: Option<BlockHash>) -> RpcResult<Vec<u8>>;
    #[method(name = "delegateInfo_getDelegate")]
    fn get_delegate(
        &self,
        delegate_account_vec: Vec<u8>,
        at: Option<BlockHash>,
    ) -> RpcResult<Vec<u8>>;
    #[method(name = "delegateInfo_getDelegated")]
    fn get_delegated(
        &self,
        delegatee_account_vec: Vec<u8>,
        at: Option<BlockHash>,
    ) -> RpcResult<Vec<u8>>;

    #[method(name = "neuronInfo_getNeuronsLite")]
    fn get_neurons_lite(&self, netuid: u16, at: Option<BlockHash>) -> RpcResult<Vec<u8>>;
    #[method(name = "neuronInfo_getNeuronLite")]
    fn get_neuron_lite(&self, netuid: u16, uid: u16, at: Option<BlockHash>) -> RpcResult<Vec<u8>>;
    #[method(name = "neuronInfo_getNeurons")]
    fn get_neurons(&self, netuid: u16, at: Option<BlockHash>) -> RpcResult<Vec<u8>>;
    #[method(name = "neuronInfo_getNeuron")]
    fn get_neuron(&self, netuid: u16, uid: u16, at: Option<BlockHash>) -> RpcResult<Vec<u8>>;
    #[method(name = "subnetInfo_getSubnetInfo")]
    fn get_subnet_info(&self, netuid: u16, at: Option<BlockHash>) -> RpcResult<Vec<u8>>;
    #[method(name = "subnetInfo_getSubnetsInfo")]
    fn get_subnets_info(&self, at: Option<BlockHash>) -> RpcResult<Vec<u8>>;
    #[method(name = "subnetInfo_getSubnetHyperparams")]
    fn get_subnet_hyperparams(&self, netuid: u16, at: Option<BlockHash>) -> RpcResult<Vec<u8>>;

    #[method(name = "subnetInfo_getLockCost")]
    fn get_network_lock_cost(&self, at: Option<BlockHash>) -> RpcResult<u64>;
}

pub struct SubtensorCustom<C, P> {
    /// Shared reference to the client.
    client: Arc<C>,
    _marker: std::marker::PhantomData<P>,
}

impl<C, P> SubtensorCustom<C, P> {
    /// Creates a new instance of the TransactionPayment Rpc helper.
    pub fn new(client: Arc<C>) -> Self {
        Self {
            client,
            _marker: Default::default(),
        }
    }
}

/// Error type of this RPC api.
pub enum Error {
    /// The call to runtime failed.
    RuntimeError(String),
}

impl From<Error> for ErrorObjectOwned {
    fn from(e: Error) -> Self {
        match e {
            Error::RuntimeError(e) => ErrorObject::owned(1, e, None::<()>),
        }
    }
}

impl From<Error> for i32 {
    fn from(e: Error) -> i32 {
        match e {
            Error::RuntimeError(_) => 1,
        }
    }
}

impl<C, Block> SubtensorCustomApiServer<<Block as BlockT>::Hash> for SubtensorCustom<C, Block>
where
    Block: BlockT,
    C: ProvideRuntimeApi<Block> + HeaderBackend<Block> + Send + Sync + 'static,
    C::Api: DelegateInfoRuntimeApi<Block>,
    C::Api: NeuronInfoRuntimeApi<Block>,
    C::Api: SubnetInfoRuntimeApi<Block>,
    C::Api: SubnetRegistrationRuntimeApi<Block>,
{
    fn get_delegates(&self, at: Option<<Block as BlockT>::Hash>) -> RpcResult<Vec<u8>> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        match api.get_delegates(at) {
            Err(e) => {
                Err(Error::RuntimeError(format!("Unable to get delegates info: {:?}", e)).into())
            }
            Ok(result) => Ok(result.encode()),
        }
    }

    fn get_delegate(
        &self,
        delegate_account_vec: Vec<u8>,
        at: Option<<Block as BlockT>::Hash>,
    ) -> RpcResult<Vec<u8>> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        let delegate_account = match AccountId32::decode(&mut delegate_account_vec.as_bytes_ref()) {
            Err(e) => {
                return Err(
                    Error::RuntimeError(format!("Unable to get delegates info: {:?}", e)).into(),
                )
            }
            Ok(delegate_account) => delegate_account,
        };
        match api.get_delegate(at, delegate_account) {
            Err(e) => {
                Err(Error::RuntimeError(format!("Unable to get delegates info: {:?}", e)).into())
            }
            Ok(result) => Ok(result.encode()),
        }
    }

    fn get_delegated(
        &self,
        delegatee_account_vec: Vec<u8>,
        at: Option<<Block as BlockT>::Hash>,
    ) -> RpcResult<Vec<u8>> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        let delegatee_account = match AccountId32::decode(&mut delegatee_account_vec.as_bytes_ref())
        {
            Err(e) => {
                return Err(
                    Error::RuntimeError(format!("Unable to get delegates info: {:?}", e)).into(),
                )
            }
            Ok(delegatee_account) => delegatee_account,
        };
        match api.get_delegated(at, delegatee_account) {
            Err(e) => {
                Err(Error::RuntimeError(format!("Unable to get delegates info: {:?}", e)).into())
            }
            Ok(result) => Ok(result.encode()),
        }
    }

    fn get_neurons_lite(
        &self,
        netuid: u16,
        at: Option<<Block as BlockT>::Hash>,
    ) -> RpcResult<Vec<u8>> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        match api.get_neurons_lite(at, netuid) {
            Err(e) => {
                Err(Error::RuntimeError(format!("Unable to get neurons lite info: {:?}", e)).into())
            }
            Ok(neurons) => Ok(neurons.encode()),
        }
    }

    fn get_neuron_lite(
        &self,
        netuid: u16,
        uid: u16,
        at: Option<<Block as BlockT>::Hash>,
    ) -> RpcResult<Vec<u8>> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        match api.get_neuron_lite(at, netuid, uid) {
            Err(e) => {
                Err(Error::RuntimeError(format!("Unable to get neuron lite info: {:?}", e)).into())
            }
            Ok(neuron) => Ok(neuron.encode()),
        }
    }

    fn get_neurons(&self, netuid: u16, at: Option<<Block as BlockT>::Hash>) -> RpcResult<Vec<u8>> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        match api.get_neurons(at, netuid) {
            Err(e) => {
                Err(Error::RuntimeError(format!("Unable to get neurons info: {:?}", e)).into())
            }
            Ok(neurons) => Ok(neurons.encode()),
        }
    }

    fn get_neuron(
        &self,
        netuid: u16,
        uid: u16,
        at: Option<<Block as BlockT>::Hash>,
    ) -> RpcResult<Vec<u8>> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        match api.get_neuron(at, netuid, uid) {
            Err(e) => {
                Err(Error::RuntimeError(format!("Unable to get neuron info: {:?}", e)).into())
            }
            Ok(neuron) => Ok(neuron.encode()),
        }
    }

    fn get_subnet_info(
        &self,
        netuid: u16,
        at: Option<<Block as BlockT>::Hash>,
    ) -> RpcResult<Vec<u8>> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        match api.get_subnet_info(at, netuid) {
            Err(e) => {
                Err(Error::RuntimeError(format!("Unable to get subnet info: {:?}", e)).into())
            }
            Ok(result) => Ok(result.encode()),
        }
    }

    fn get_subnet_hyperparams(
        &self,
        netuid: u16,
        at: Option<<Block as BlockT>::Hash>,
    ) -> RpcResult<Vec<u8>> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        match api.get_subnet_hyperparams(at, netuid) {
            Err(e) => Err(Error::RuntimeError(format!(
                "Unable to get subnet hyperparam info: {:?}",
                e
            ))
            .into()),
            Ok(result) => Ok(result.encode()),
        }
    }

    fn get_subnets_info(&self, at: Option<<Block as BlockT>::Hash>) -> RpcResult<Vec<u8>> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        match api.get_subnets_info(at) {
            Err(e) => {
                Err(Error::RuntimeError(format!("Unable to get subnets info: {:?}", e)).into())
            }
            Ok(result) => Ok(result.encode()),
        }
    }

    fn get_network_lock_cost(&self, at: Option<<Block as BlockT>::Hash>) -> RpcResult<u64> {
        let api = self.client.runtime_api();
        let at = at.unwrap_or_else(|| self.client.info().best_hash);

        api.get_network_registration_cost(at).map_err(|e| {
            Error::RuntimeError(format!("Unable to get subnet lock cost: {:?}", e)).into()
        })
    }
}
