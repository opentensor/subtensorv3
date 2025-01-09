extern crate alloc;
use crate::precompiles::{get_method_id, get_slice};
use crate::Runtime;
use fp_evm::{
    ExitError, ExitSucceed, PrecompileFailure, PrecompileHandle, PrecompileOutput, PrecompileResult,
};
use sp_core::{ByteArray, U256};
use sp_std::vec;
pub const METAGRAPH_PRECOMPILE_INDEX: u64 = 2050;
pub struct MetagraphPrecompile;

const NO_HOTKEY: &str = "no hotkey";

impl MetagraphPrecompile {
    pub fn execute(handle: &mut impl PrecompileHandle) -> PrecompileResult {
        let txdata = handle.input();
        let method_id = get_slice(txdata, 0, 4)?;
        let method_input = txdata
            .get(4..)
            .map_or_else(vec::Vec::new, |slice| slice.to_vec()); // Avoiding borrowing conflicts

        match method_id {
            id if id == get_method_id("getUidCount(uint16)") => Self::get_uid_count(&method_input),
            id if id == get_method_id("getStake(uint16,uint16)") => Self::get_stake(&method_input),
            id if id == get_method_id("getRank(uint16,uint16)") => Self::get_rank(&method_input),
            id if id == get_method_id("getTrust(uint16,uint16)") => Self::get_trust(&method_input),
            id if id == get_method_id("getConsensus(uint16,uint16)") => {
                Self::get_consensus(&method_input)
            }
            id if id == get_method_id("getIncentive(uint16,uint16)") => {
                Self::get_incentive(&method_input)
            }
            id if id == get_method_id("getDividends(uint16,uint16)") => {
                Self::get_dividends(&method_input)
            }
            id if id == get_method_id("getEmission(uint16,uint16)") => {
                Self::get_emission(&method_input)
            }
            id if id == get_method_id("getVtrust(uint16,uint16)") => {
                Self::get_vtrust(&method_input)
            }
            id if id == get_method_id("getValidatorStatus(uint16,uint16)") => {
                Self::get_validator_status(&method_input)
            }
            id if id == get_method_id("getLastUpdate(uint16,uint16)") => {
                Self::get_last_update(&method_input)
            }
            id if id == get_method_id("getIsActive(uint16,uint16)") => {
                Self::get_is_active(&method_input)
            }
            id if id == get_method_id("getAxon(uint16,uint16)") => Self::get_axon(&method_input),
            id if id == get_method_id("getHotkey(uint16,uint16)") => {
                Self::get_hotkey(&method_input)
            }
            id if id == get_method_id("getColdkey(uint16,uint16)") => {
                Self::get_coldkey(&method_input)
            }

            _ => Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            }),
        }
    }

    fn get_uid_count(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid_count = pallet_subtensor::SubnetworkN::<Runtime>::get(netuid);

        let uid_count_u256 = U256::from(uid_count);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&uid_count_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_stake(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;
        let hotkey = pallet_subtensor::Pallet::<Runtime>::get_hotkey_for_net_and_uid(netuid, uid)
            .map_err(|_| PrecompileFailure::Error {
            exit_status: ExitError::InvalidRange,
        })?;

        let stake = pallet_subtensor::Pallet::<Runtime>::get_total_stake_for_hotkey(&hotkey);
        let result_u256 = U256::from(stake);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_rank(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;
        let rank = pallet_subtensor::Pallet::<Runtime>::get_rank_for_uid(netuid, uid);

        let result_u256 = U256::from(rank);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_trust(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let trust = pallet_subtensor::Pallet::<Runtime>::get_trust_for_uid(netuid, uid);

        let result_u256 = U256::from(trust);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_consensus(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let consensus = pallet_subtensor::Pallet::<Runtime>::get_consensus_for_uid(netuid, uid);

        let result_u256 = U256::from(consensus);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_incentive(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let incentive = pallet_subtensor::Pallet::<Runtime>::get_incentive_for_uid(netuid, uid);

        let result_u256 = U256::from(incentive);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_dividends(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let dividends = pallet_subtensor::Pallet::<Runtime>::get_dividends_for_uid(netuid, uid);

        let result_u256 = U256::from(dividends);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_emission(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let emission = pallet_subtensor::Pallet::<Runtime>::get_emission_for_uid(netuid, uid);

        let result_u256 = U256::from(emission);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_vtrust(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let vtrust = pallet_subtensor::Pallet::<Runtime>::get_validator_trust_for_uid(netuid, uid);

        let result_u256 = U256::from(vtrust);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_validator_status(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let validator_permit =
            pallet_subtensor::Pallet::<Runtime>::get_validator_permit_for_uid(netuid, uid);

        let result_u256 = if validator_permit {
            U256::from(1)
        } else {
            U256::from(0)
        };
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_last_update(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let last_update = pallet_subtensor::Pallet::<Runtime>::get_last_update_for_uid(netuid, uid);

        let result_u256 = U256::from(last_update);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_is_active(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let active = pallet_subtensor::Pallet::<Runtime>::get_active_for_uid(netuid, uid);

        let result_u256 = if active { U256::from(1) } else { U256::from(0) };
        let mut result = [0_u8; 32];
        U256::to_big_endian(&result_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_axon(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let hotkey = pallet_subtensor::Pallet::<Runtime>::get_hotkey_for_net_and_uid(netuid, uid)
            .map_err(|_| PrecompileFailure::Error {
            exit_status: ExitError::Other(sp_version::Cow::Borrowed(NO_HOTKEY)),
        })?;

        let axon = pallet_subtensor::Pallet::<Runtime>::get_axon_info(netuid, &hotkey);

        let mut block_result = [0_u8; 32];
        U256::to_big_endian(&U256::from(axon.block), &mut block_result);

        let mut version_result = [0_u8; 32];
        U256::to_big_endian(&U256::from(axon.version), &mut version_result);

        let mut ip_result = [0_u8; 32];
        U256::to_big_endian(&U256::from(axon.ip), &mut ip_result);

        let mut port_result = [0_u8; 32];
        U256::to_big_endian(&U256::from(axon.port), &mut port_result);

        let mut ip_type_result = [0_u8; 32];
        U256::to_big_endian(&U256::from(axon.ip_type), &mut ip_type_result);

        let mut protocol_result = [0_u8; 32];
        U256::to_big_endian(&U256::from(axon.protocol), &mut protocol_result);

        let mut result = [0_u8; 192];
        result[..32].copy_from_slice(&block_result);
        result[32..64].copy_from_slice(&version_result);
        result[64..96].copy_from_slice(&ip_result);
        result[96..128].copy_from_slice(&port_result);
        result[128..160].copy_from_slice(&ip_type_result);
        result[160..].copy_from_slice(&protocol_result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn get_hotkey(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let hotkey = pallet_subtensor::Pallet::<Runtime>::get_hotkey_for_net_and_uid(netuid, uid)
            .map_err(|_| PrecompileFailure::Error {
            exit_status: ExitError::InvalidRange,
        })?;

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: hotkey.as_slice().into(),
        })
    }

    fn get_coldkey(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;
        let uid = Self::parse_uid(get_slice(data, 32, 64)?)?;

        let hotkey = pallet_subtensor::Pallet::<Runtime>::get_hotkey_for_net_and_uid(netuid, uid)
            .map_err(|_| PrecompileFailure::Error {
            exit_status: ExitError::InvalidRange,
        })?;

        let coldkey = pallet_subtensor::Owner::<Runtime>::get(&hotkey);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: coldkey.as_slice().into(),
        })
    }

    fn parse_netuid(data: &[u8]) -> Result<u16, PrecompileFailure> {
        if data.len() < 32 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }
        let mut netuid = [0u8; 2];
        netuid.copy_from_slice(get_slice(data, 30, 32)?);
        let result = u16::from_be_bytes(netuid);
        Ok(result)
    }

    fn parse_uid(data: &[u8]) -> Result<u16, PrecompileFailure> {
        if data.len() < 32 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }
        let mut uid = [0u8; 2];
        uid.copy_from_slice(get_slice(data, 30, 32)?);
        let result = u16::from_be_bytes(uid);
        Ok(result)
    }
}
