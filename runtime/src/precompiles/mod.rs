use core::marker::PhantomData;
use sp_core::{hashing::keccak_256, H160};
use sp_runtime::AccountId32;

use pallet_evm::{
    ExitError, IsPrecompileResult, Precompile, PrecompileFailure, PrecompileHandle,
    PrecompileResult, PrecompileSet,
};
use pallet_evm_precompile_modexp::Modexp;
use pallet_evm_precompile_sha3fips::Sha3FIPS256;
use pallet_evm_precompile_simple::{ECRecover, ECRecoverPublicKey, Identity, Ripemd160, Sha256};

use crate::Runtime;
use pallet_admin_utils::{PrecompileEnable, PrecompileEnum};

// Include custom precompiles
mod balance_transfer;
mod ed25519;
mod metagraph;
mod staking;

use balance_transfer::*;
use ed25519::*;
use metagraph::*;
use staking::*;

pub struct FrontierPrecompiles<R>(PhantomData<R>);

impl<R> Default for FrontierPrecompiles<R>
where
    R: pallet_evm::Config,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<R> FrontierPrecompiles<R>
where
    R: pallet_evm::Config,
{
    pub fn new() -> Self {
        Self(Default::default())
    }
    pub fn used_addresses() -> [H160; 11] {
        [
            hash(1),
            hash(2),
            hash(3),
            hash(4),
            hash(5),
            hash(1024),
            hash(1025),
            hash(EDVERIFY_PRECOMPILE_INDEX),
            hash(BALANCE_TRANSFER_INDEX),
            hash(STAKING_PRECOMPILE_INDEX),
            hash(METAGRAPH_PRECOMPILE_INDEX),
        ]
    }
}
impl<R> PrecompileSet for FrontierPrecompiles<R>
where
    R: pallet_evm::Config,
{
    fn execute(&self, handle: &mut impl PrecompileHandle) -> Option<PrecompileResult> {
        match handle.code_address() {
            // Ethereum precompiles :
            a if a == hash(1) => Some(ECRecover::execute(handle)),
            a if a == hash(2) => Some(Sha256::execute(handle)),
            a if a == hash(3) => Some(Ripemd160::execute(handle)),
            a if a == hash(4) => Some(Identity::execute(handle)),
            a if a == hash(5) => Some(Modexp::execute(handle)),
            // Non-Frontier specific nor Ethereum precompiles :
            a if a == hash(1024) => Some(Sha3FIPS256::execute(handle)),
            a if a == hash(1025) => Some(ECRecoverPublicKey::execute(handle)),

            a if a == hash(EDVERIFY_PRECOMPILE_INDEX) => {
                if PrecompileEnable::<Runtime>::get(PrecompileEnum::BalanceTransfer) {
                    Some(Ed25519Verify::execute(handle))
                } else {
                    Some(Err(PrecompileFailure::Error {
                        exit_status: ExitError::Other(
                            "Precompile Ed25519Verify is disabled".into(),
                        ),
                    }))
                }
            }
            // Subtensor specific precompiles :
            a if a == hash(BALANCE_TRANSFER_INDEX) => {
                if PrecompileEnable::<Runtime>::get(PrecompileEnum::BalanceTransfer) {
                    Some(BalanceTransferPrecompile::execute(handle))
                } else {
                    Some(Err(PrecompileFailure::Error {
                        exit_status: ExitError::Other(
                            "Precompile Balance Transfer is disabled".into(),
                        ),
                    }))
                }
            }
            a if a == hash(STAKING_PRECOMPILE_INDEX) => {
                if PrecompileEnable::<Runtime>::get(PrecompileEnum::Staking) {
                    Some(StakingPrecompile::execute(handle))
                } else {
                    Some(Err(PrecompileFailure::Error {
                        exit_status: ExitError::Other(
                            "Precompile Balance Transfer is disabled".into(),
                        ),
                    }))
                }
            }

            a if a == hash(METAGRAPH_PRECOMPILE_INDEX) => {
                if PrecompileEnable::<Runtime>::get(PrecompileEnum::Metagraph) {
                    Some(MetagraphPrecompile::execute(handle))
                } else {
                    Some(Err(PrecompileFailure::Error {
                        exit_status: ExitError::Other(
                            "Precompile Balance Transfer is disabled".into(),
                        ),
                    }))
                }
            }

            _ => None,
        }
    }

    fn is_precompile(&self, address: H160, _gas: u64) -> IsPrecompileResult {
        IsPrecompileResult::Answer {
            is_precompile: Self::used_addresses().contains(&address),
            extra_cost: 0,
        }
    }
}

fn hash(a: u64) -> H160 {
    H160::from_low_u64_be(a)
}

/// Returns Ethereum method ID from an str method signature
///
pub fn get_method_id(method_signature: &str) -> [u8; 4] {
    // Calculate the full Keccak-256 hash of the method signature
    let hash = keccak_256(method_signature.as_bytes());

    // Extract the first 4 bytes to get the method ID
    [hash[0], hash[1], hash[2], hash[3]]
}

/// Convert bytes to AccountId32 with PrecompileFailure as Error
/// which consumes all gas
///
pub fn bytes_to_account_id(account_id_bytes: &[u8]) -> Result<AccountId32, PrecompileFailure> {
    AccountId32::try_from(account_id_bytes).map_err(|_| {
        log::info!("Error parsing account id bytes {:?}", account_id_bytes);
        PrecompileFailure::Error {
            exit_status: ExitError::InvalidRange,
        }
    })
}

/// Takes a slice from bytes with PrecompileFailure as Error
///
pub fn get_slice(data: &[u8], from: usize, to: usize) -> Result<&[u8], PrecompileFailure> {
    let maybe_slice = data.get(from..to);
    if let Some(slice) = maybe_slice {
        Ok(slice)
    } else {
        Err(PrecompileFailure::Error {
            exit_status: ExitError::InvalidRange,
        })
    }
}
