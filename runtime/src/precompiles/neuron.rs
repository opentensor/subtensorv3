use pallet_evm::{ExitError, PrecompileFailure, PrecompileHandle, PrecompileResult};
use sp_core::H256;

use crate::precompiles::{dispatch, get_method_id, get_slice};
use sp_std::{vec, vec::Vec};

use crate::{Runtime, RuntimeCall};
pub const NEURON_PRECOMPILE_INDEX: u64 = 2053;

// this is subnets smart contract's(0x0000000000000000000000000000000000000805) sr25519 address
pub const NEURON_CONTRACT_ADDRESS: &str = "5Ha1yegRNUqRYrE9myDohjtkozYniugt7K46AN7ywDSW5MXz";

pub struct NeuronPrecompile;

impl NeuronPrecompile {
    pub fn execute(handle: &mut impl PrecompileHandle) -> PrecompileResult {
        let txdata = handle.input();
        let method_id = get_slice(txdata, 0, 4)?;
        let method_input = txdata
            .get(4..)
            .map_or_else(vec::Vec::new, |slice| slice.to_vec()); // Avoiding borrowing conflicts

        match method_id {
            id if id == get_method_id("setWeights(uint16,uint16[],uint16[],uint64)") => {
                Self::set_weights(handle, &method_input)
            }
            id if id == get_method_id("commitWeights(uint16,uint256)") => {
                Self::commit_weights(handle, &method_input)
            }
            id if id
                == get_method_id("revealWeights(uint16,uint16[],uint16[],uint16[],uint64)") =>
            {
                Self::reveal_weights(handle, &method_input)
            }

            _ => Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            }),
        }
    }

    pub fn set_weights(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, dests, weights, version_key) = Self::parse_netuid_dests_weights(data)?;
        let call = RuntimeCall::SubtensorModule(pallet_subtensor::Call::<Runtime>::set_weights {
            netuid,
            dests,
            weights,
            version_key,
        });

        dispatch(handle, call, NEURON_CONTRACT_ADDRESS)
    }

    pub fn commit_weights(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, commit_hash) = Self::parse_netuid_commit_hash(data)?;

        let call =
            RuntimeCall::SubtensorModule(pallet_subtensor::Call::<Runtime>::commit_weights {
                netuid,
                commit_hash,
            });
        dispatch(handle, call, NEURON_CONTRACT_ADDRESS)
    }

    pub fn reveal_weights(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, uids, values, salt, version_key) =
            Self::parse_netuid_dests_weights_salt(data)?;
        let call =
            RuntimeCall::SubtensorModule(pallet_subtensor::Call::<Runtime>::reveal_weights {
                netuid,
                uids,
                values,
                salt,
                version_key,
            });
        dispatch(handle, call, NEURON_CONTRACT_ADDRESS)
    }

    fn parse_netuid_dests_weights(
        data: &[u8],
    ) -> Result<(u16, Vec<u16>, Vec<u16>, u64), PrecompileFailure> {
        if data.len() < 4 * 32 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }

        let mut netuid_vec = [0u8; 2];
        netuid_vec.copy_from_slice(get_slice(data, 30, 32)?);
        let netuid = u16::from_be_bytes(netuid_vec);

        let mut first_position_vec = [0u8; 2];
        first_position_vec.copy_from_slice(get_slice(data, 62, 64)?);
        let first_position = u16::from_be_bytes(first_position_vec) as usize;

        let mut second_position_vec = [0u8; 2];
        second_position_vec.copy_from_slice(get_slice(data, 94, 96)?);
        let second_position = u16::from_be_bytes(second_position_vec) as usize;

        let mut version_key_vec = [0u8; 8];
        version_key_vec.copy_from_slice(get_slice(data, 120, 128)?);
        let version_key = u64::from_be_bytes(version_key_vec);

        let mut dests = vec![];
        let mut weights = vec![];

        let mut dests_len_vec = [0u8; 2];
        dests_len_vec.copy_from_slice(get_slice(data, first_position + 30, first_position + 32)?);
        let dests_len = u16::from_be_bytes(dests_len_vec) as usize;

        for i in 0..dests_len {
            let mut tmp_vec = [0u8; 2];
            tmp_vec.copy_from_slice(get_slice(
                data,
                first_position + 62 + i * 32,
                first_position + 64 + i * 32,
            )?);
            let dest = u16::from_be_bytes(tmp_vec);
            dests.push(dest);
        }

        let mut weights_len_vec = [0u8; 2];
        weights_len_vec.copy_from_slice(get_slice(
            data,
            second_position + 30,
            second_position + 32,
        )?);
        let weights_len = u16::from_be_bytes(weights_len_vec) as usize;

        for i in 0..weights_len {
            let mut tmp_vec = [0u8; 2];
            tmp_vec.copy_from_slice(get_slice(
                data,
                second_position + 62 + i * 32,
                second_position + 64 + i * 32,
            )?);
            let weight = u16::from_be_bytes(tmp_vec);
            weights.push(weight);
        }

        Ok((netuid, dests, weights, version_key))
    }

    fn parse_netuid_commit_hash(data: &[u8]) -> Result<(u16, H256), PrecompileFailure> {
        if data.len() < 2 * 32 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }

        let mut netuid_vec = [0u8; 2];
        netuid_vec.copy_from_slice(get_slice(data, 30, 32)?);
        let netuid = u16::from_be_bytes(netuid_vec);

        // let mut commit_hash_vec = [0u8; 2];
        // commit_hash_vec.copy_from_slice(get_slice(data, 32, 64)?);
        let commit_hash = H256::from_slice(get_slice(data, 32, 64)?);

        Ok((netuid, commit_hash))
    }

    fn parse_netuid_dests_weights_salt(
        data: &[u8],
    ) -> Result<(u16, Vec<u16>, Vec<u16>, Vec<u16>, u64), PrecompileFailure> {
        if data.len() < 5 * 32 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }

        let mut netuid_vec = [0u8; 2];
        netuid_vec.copy_from_slice(get_slice(data, 30, 32)?);
        let netuid = u16::from_be_bytes(netuid_vec);

        let mut first_position_vec = [0u8; 2];
        first_position_vec.copy_from_slice(get_slice(data, 62, 64)?);
        let first_position = u16::from_be_bytes(first_position_vec) as usize;

        let mut second_position_vec = [0u8; 2];
        second_position_vec.copy_from_slice(get_slice(data, 94, 96)?);
        let second_position = u16::from_be_bytes(second_position_vec) as usize;

        let mut third_position_vec = [0u8; 2];
        third_position_vec.copy_from_slice(get_slice(data, 126, 128)?);
        let third_position = u16::from_be_bytes(third_position_vec) as usize;

        let mut version_key_vec = [0u8; 8];
        version_key_vec.copy_from_slice(get_slice(data, 152, 160)?);
        let version_key = u64::from_be_bytes(version_key_vec);

        let mut uids = vec![];
        let mut values = vec![];
        let mut salt = vec![];

        let mut uids_len_vec = [0u8; 2];
        uids_len_vec.copy_from_slice(get_slice(data, first_position + 30, first_position + 32)?);
        let uids_len = u16::from_be_bytes(uids_len_vec) as usize;

        for i in 0..uids_len {
            let mut tmp_vec = [0u8; 2];
            tmp_vec.copy_from_slice(get_slice(
                data,
                first_position + 62 + i * 32,
                first_position + 64 + i * 32,
            )?);
            let uid = u16::from_be_bytes(tmp_vec);
            uids.push(uid);
        }

        let mut values_len_vec = [0u8; 2];
        values_len_vec.copy_from_slice(get_slice(
            data,
            second_position + 30,
            second_position + 32,
        )?);
        let values_len = u16::from_be_bytes(values_len_vec) as usize;

        for i in 0..values_len {
            let mut tmp_vec = [0u8; 2];
            tmp_vec.copy_from_slice(get_slice(
                data,
                second_position + 62 + i * 32,
                second_position + 64 + i * 32,
            )?);
            let value = u16::from_be_bytes(tmp_vec);
            values.push(value);
        }

        let mut salt_len_vec = [0u8; 2];
        salt_len_vec.copy_from_slice(get_slice(data, third_position + 30, third_position + 32)?);
        let salt_len = u16::from_be_bytes(salt_len_vec) as usize;

        for i in 0..salt_len {
            let mut tmp_vec = [0u8; 2];
            tmp_vec.copy_from_slice(get_slice(
                data,
                third_position + 62 + i * 32,
                third_position + 64 + i * 32,
            )?);
            let value = u16::from_be_bytes(tmp_vec);
            salt.push(value);
        }

        Ok((netuid, uids, values, salt, version_key))
    }
}
