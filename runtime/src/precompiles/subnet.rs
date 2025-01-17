use crate::precompiles::{dispatch, get_method_id, get_slice};
use crate::{Runtime, RuntimeCall};
use pallet_evm::{
    ExitError, ExitSucceed, PrecompileFailure, PrecompileHandle, PrecompileOutput, PrecompileResult,
};
use sp_core::U256;
use sp_std::vec;

pub const SUBNET_PRECOMPILE_INDEX: u64 = 2051;
// three bytes with max lenght 1K
pub const MAX_PARAMETER_SIZE: usize = 3 * 1024;

// this is staking smart contract's(0x0000000000000000000000000000000000000803) sr25519 address
pub const SUBNET_CONTRACT_ADDRESS: &str = "5DPSUCb5mZFfizvBDSnRoAqmxV5Bmov2CS3xV773qU6VP1w2";

pub struct SubnetPrecompile;

impl SubnetPrecompile {
    pub fn execute(handle: &mut impl PrecompileHandle) -> PrecompileResult {
        let txdata = handle.input();
        if txdata.len() > MAX_PARAMETER_SIZE {
            log::error!("the length of subnet call is {} ", txdata.len());
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }
        let method_id = get_slice(txdata, 0, 4)?;
        let method_input = txdata
            .get(4..)
            .map_or_else(vec::Vec::new, |slice| slice.to_vec()); // Avoiding borrowing conflicts

        match method_id {
            id if id == get_method_id("registerNetwork(bytes,bytes,bytes)") => {
                Self::register_network(handle, &method_input)
            }
            id if id == get_method_id("registerNetwork()") => {
                Self::register_network(handle, &[0_u8; 0])
            }

            id if id == get_method_id("getServingRateLimit(uint16)") => {
                Self::get_serving_rate_limit(&method_input)
            }
            id if id == get_method_id("setServingRateLimit(uint16,uint64)") => {
                Self::set_serving_rate_limit(handle, &method_input)
            }

            id if id == get_method_id("getMinDifficulty(uint16)") => {
                Self::get_min_difficulty(&method_input)
            }
            id if id == get_method_id("setMinDifficulty(uint16,uint64)") => {
                Self::set_min_difficulty(handle, &method_input)
            }

            id if id == get_method_id("getMaxDifficulty(uint16)") => {
                Self::get_max_difficulty(&method_input)
            }
            id if id == get_method_id("setMaxDifficulty(uint16,uint64)") => {
                Self::set_max_difficulty(handle, &method_input)
            }

            id if id == get_method_id("getWeightsVersionKey(uint16)") => {
                Self::get_weights_version_key(&method_input)
            }
            id if id == get_method_id("setWeightsVersionKey(uint16,uint64)") => {
                Self::set_weights_version_key(handle, &method_input)
            }

            id if id == get_method_id("getWeightsSetRateLimit(uint16)") => {
                Self::get_weights_set_rate_limit(&method_input)
            }
            id if id == get_method_id("setWeightsSetRateLimit(uint16,uint64)") => {
                Self::set_weights_set_rate_limit(handle, &method_input)
            }

            id if id == get_method_id("getAdjustmentAlpha(uint16)") => {
                Self::get_adjustment_alpha(&method_input)
            }
            id if id == get_method_id("setAdjustmentAlpha(uint16,uint64)") => {
                Self::set_adjustment_alpha(handle, &method_input)
            }

            id if id == get_method_id("getMaxWeightLimit(uint16)") => {
                Self::get_max_weight_limit(&method_input)
            }
            id if id == get_method_id("setMaxWeightLimit(uint16,uint64)") => {
                Self::set_max_weight_limit(handle, &method_input)
            }

            id if id == get_method_id("getImmunityPeriod(uint16)") => {
                Self::get_immunity_period(&method_input)
            }
            id if id == get_method_id("setImmunityPeriod(uint16,uint64)") => {
                Self::set_immunity_period(handle, &method_input)
            }

            id if id == get_method_id("getMinAllowedWeights(uint16)") => {
                Self::get_min_allowed_weights(&method_input)
            }
            id if id == get_method_id("setMinAllowedWeights(uint16,uint16)") => {
                Self::set_min_allowed_weights(handle, &method_input)
            }

            id if id == get_method_id("getKappa(uint16)") => Self::get_kappa(&method_input),
            id if id == get_method_id("setKappa(uint16,uint16)") => {
                Self::set_kappa(handle, &method_input)
            }

            id if id == get_method_id("getRho(uint16)") => Self::get_rho(&method_input),
            id if id == get_method_id("setRho(uint16,uint16)") => {
                Self::set_rho(handle, &method_input)
            }

            id if id == get_method_id("getActivityCutoff(uint16)") => {
                Self::get_activity_cutoff(&method_input)
            }
            id if id == get_method_id("setActivityCutoff(uint16,uint16)") => {
                Self::set_activity_cutoff(handle, &method_input)
            }

            id if id == get_method_id("getNetworkRegistrationAllowed(uint16)") => {
                Self::get_network_registration_allowed(&method_input)
            }
            id if id == get_method_id("setNetworkRegistrationAllowed(uint16,bool)") => {
                Self::set_network_registration_allowed(handle, &method_input)
            }

            id if id == get_method_id("getNetworkPowRegistrationAllowed(uint16)") => {
                Self::get_network_pow_registration_allowed(&method_input)
            }
            id if id == get_method_id("setNetworkPowRegistrationAllowed(uint16,bool)") => {
                Self::set_network_pow_registration_allowed(handle, &method_input)
            }

            id if id == get_method_id("getMinBurn(uint16)") => Self::get_min_burn(&method_input),
            id if id == get_method_id("setMinBurn(uint16,uint64)") => {
                Self::set_min_burn(handle, &method_input)
            }

            id if id == get_method_id("getMaxBurn(uint16)") => Self::get_max_burn(&method_input),
            id if id == get_method_id("setMaxBurn(uint16,uint64)") => {
                Self::set_max_burn(handle, &method_input)
            }

            id if id == get_method_id("getDifficulty(uint16)") => {
                Self::get_difficulty(&method_input)
            }
            id if id == get_method_id("setDifficulty(uint16,uint64)") => {
                Self::set_difficulty(handle, &method_input)
            }

            id if id == get_method_id("getBondsMovingAverage(uint16)") => {
                Self::get_bonds_moving_average(&method_input)
            }
            id if id == get_method_id("setBondsMovingAverage(uint16,uint64)") => {
                Self::set_bonds_moving_average(handle, &method_input)
            }

            id if id == get_method_id("getCommitRevealWeightsEnabled(uint16)") => {
                Self::get_commit_reveal_weights_enabled(&method_input)
            }
            id if id == get_method_id("setCommitRevealWeightsEnabled(uint16,bool)") => {
                Self::set_commit_reveal_weights_enabled(handle, &method_input)
            }

            id if id == get_method_id("getLiquidAlphaEnabled(uint16)") => {
                Self::get_liquid_alpha_enabled(&method_input)
            }
            id if id == get_method_id("setLiquidAlphaEnabled(uint16,bool)") => {
                Self::set_liquid_alpha_enabled(handle, &method_input)
            }

            id if id == get_method_id("getAlphaValues(uint16)") => {
                Self::get_alpha_values(&method_input)
            }
            id if id == get_method_id("setAlphaValues(uint16,uint16,uint16)") => {
                Self::set_alpha_values(handle, &method_input)
            }

            id if id == get_method_id("getCommitRevealWeightsInterval(uint16)") => {
                Self::get_commit_reveal_weights_interval(&method_input)
            }
            id if id == get_method_id("setCommitRevealWeightsInterval(uint16,uint64)") => {
                Self::set_commit_reveal_weights_interval(handle, &method_input)
            }
            _ => Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            }),
        }
    }

    fn register_network(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let call = if data.is_empty() {
            RuntimeCall::SubtensorModule(
                pallet_subtensor::Call::<Runtime>::register_network_with_identity {
                    identity: None,
                },
            )
        } else {
            let (subnet_name, github_repo, subnet_contact) =
                Self::parse_register_network_parameters(data)?;

            let identity: pallet_subtensor::SubnetIdentityOf = pallet_subtensor::SubnetIdentityOf {
                subnet_name,
                github_repo,
                subnet_contact,
            };

            // Create the register_network callcle
            RuntimeCall::SubtensorModule(
                pallet_subtensor::Call::<Runtime>::register_network_with_identity {
                    identity: Some(identity),
                },
            )
        };

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_serving_rate_limit(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::ServingRateLimit::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_serving_rate_limit(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, serving_rate_limit) = Self::parse_netuid_u64_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_serving_rate_limit {
                netuid,
                serving_rate_limit,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_min_difficulty(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::MinDifficulty::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_min_difficulty(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, min_difficulty) = Self::parse_netuid_u64_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_min_difficulty {
                netuid,
                min_difficulty,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_max_difficulty(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::MaxDifficulty::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_max_difficulty(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, max_difficulty) = Self::parse_netuid_u64_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_max_difficulty {
                netuid,
                max_difficulty,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_weights_version_key(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::WeightsVersionKey::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_weights_version_key(
        handle: &mut impl PrecompileHandle,
        data: &[u8],
    ) -> PrecompileResult {
        let (netuid, weights_version_key) = Self::parse_netuid_u64_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_weights_version_key {
                netuid,
                weights_version_key,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_weights_set_rate_limit(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::WeightsSetRateLimit::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_weights_set_rate_limit(
        handle: &mut impl PrecompileHandle,
        data: &[u8],
    ) -> PrecompileResult {
        let (netuid, weights_set_rate_limit) = Self::parse_netuid_u64_parameter(data)?;

        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_weights_set_rate_limit {
                netuid,
                weights_set_rate_limit,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_adjustment_alpha(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::AdjustmentAlpha::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_adjustment_alpha(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, adjustment_alpha) = Self::parse_netuid_u64_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_adjustment_alpha {
                netuid,
                adjustment_alpha,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_max_weight_limit(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::MaxWeightsLimit::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_max_weight_limit(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, max_weight_limit) = Self::parse_netuid_u16_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_max_weight_limit {
                netuid,
                max_weight_limit,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_immunity_period(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::ImmunityPeriod::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_immunity_period(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, immunity_period) = Self::parse_netuid_u16_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_immunity_period {
                netuid,
                immunity_period,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_min_allowed_weights(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::MinAllowedWeights::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_min_allowed_weights(
        handle: &mut impl PrecompileHandle,
        data: &[u8],
    ) -> PrecompileResult {
        let (netuid, min_allowed_weights) = Self::parse_netuid_u16_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_min_allowed_weights {
                netuid,
                min_allowed_weights,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_kappa(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::Kappa::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_kappa(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, kappa) = Self::parse_netuid_u16_parameter(data)?;
        let call = RuntimeCall::AdminUtils(pallet_admin_utils::Call::<Runtime>::sudo_set_kappa {
            netuid,
            kappa,
        });

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_rho(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::Rho::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_rho(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, rho) = Self::parse_netuid_u16_parameter(data)?;
        let call = RuntimeCall::AdminUtils(pallet_admin_utils::Call::<Runtime>::sudo_set_rho {
            netuid,
            rho,
        });

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_activity_cutoff(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::ActivityCutoff::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_activity_cutoff(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, activity_cutoff) = Self::parse_netuid_u16_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_activity_cutoff {
                netuid,
                activity_cutoff,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_network_registration_allowed(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::NetworkRegistrationAllowed::<Runtime>::get(netuid);

        let value_u256 = if value { U256::from(1) } else { U256::from(0) };
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_network_registration_allowed(
        handle: &mut impl PrecompileHandle,
        data: &[u8],
    ) -> PrecompileResult {
        let (netuid, registration_allowed) = Self::parse_netuid_bool_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_network_registration_allowed {
                netuid,
                registration_allowed,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_network_pow_registration_allowed(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::NetworkPowRegistrationAllowed::<Runtime>::get(netuid);

        let value_u256 = if value { U256::from(1) } else { U256::from(0) };
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_network_pow_registration_allowed(
        handle: &mut impl PrecompileHandle,
        data: &[u8],
    ) -> PrecompileResult {
        let (netuid, registration_allowed) = Self::parse_netuid_bool_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_network_pow_registration_allowed {
                netuid,
                registration_allowed,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_min_burn(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::MinBurn::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_min_burn(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, min_burn) = Self::parse_netuid_u64_parameter(data)?;
        let call =
            RuntimeCall::AdminUtils(pallet_admin_utils::Call::<Runtime>::sudo_set_min_burn {
                netuid,
                min_burn,
            });

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_max_burn(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::MaxBurn::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_max_burn(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, max_burn) = Self::parse_netuid_u64_parameter(data)?;
        let call =
            RuntimeCall::AdminUtils(pallet_admin_utils::Call::<Runtime>::sudo_set_max_burn {
                netuid,
                max_burn,
            });

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_difficulty(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::Difficulty::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_difficulty(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, difficulty) = Self::parse_netuid_u64_parameter(data)?;
        let call =
            RuntimeCall::AdminUtils(pallet_admin_utils::Call::<Runtime>::sudo_set_difficulty {
                netuid,
                difficulty,
            });

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_bonds_moving_average(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::BondsMovingAverage::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_bonds_moving_average(
        handle: &mut impl PrecompileHandle,
        data: &[u8],
    ) -> PrecompileResult {
        let (netuid, bonds_moving_average) = Self::parse_netuid_u64_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_bonds_moving_average {
                netuid,
                bonds_moving_average,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_commit_reveal_weights_enabled(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::CommitRevealWeightsEnabled::<Runtime>::get(netuid);

        let value_u256 = if value { U256::from(1) } else { U256::from(0) };
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_commit_reveal_weights_enabled(
        handle: &mut impl PrecompileHandle,
        data: &[u8],
    ) -> PrecompileResult {
        let (netuid, enabled) = Self::parse_netuid_bool_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_commit_reveal_weights_enabled {
                netuid,
                enabled,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_liquid_alpha_enabled(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::LiquidAlphaOn::<Runtime>::get(netuid);

        let value_u256 = if value { U256::from(1) } else { U256::from(0) };
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_liquid_alpha_enabled(
        handle: &mut impl PrecompileHandle,
        data: &[u8],
    ) -> PrecompileResult {
        let (netuid, enabled) = Self::parse_netuid_bool_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_liquid_alpha_enabled { netuid, enabled },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_alpha_values(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let (alpha_low, alpha_high) = pallet_subtensor::AlphaValues::<Runtime>::get(netuid);

        let mut value_u256 = U256::from(alpha_low);
        let mut result = [0_u8; 64];
        U256::to_big_endian(&value_u256, &mut result[0..]);

        value_u256 = U256::from(alpha_high);
        U256::to_big_endian(&value_u256, &mut result[32..]);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_alpha_values(handle: &mut impl PrecompileHandle, data: &[u8]) -> PrecompileResult {
        let (netuid, alpha_low, alpha_high) = Self::parse_netuid_u16_u16_parameter(data)?;
        let call =
            RuntimeCall::AdminUtils(pallet_admin_utils::Call::<Runtime>::sudo_set_alpha_values {
                netuid,
                alpha_low,
                alpha_high,
            });

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn get_commit_reveal_weights_interval(data: &[u8]) -> PrecompileResult {
        let netuid = Self::parse_netuid(data)?;

        let value = pallet_subtensor::RevealPeriodEpochs::<Runtime>::get(netuid);

        let value_u256 = U256::from(value);
        let mut result = [0_u8; 32];
        U256::to_big_endian(&value_u256, &mut result);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: result.into(),
        })
    }

    fn set_commit_reveal_weights_interval(
        handle: &mut impl PrecompileHandle,
        data: &[u8],
    ) -> PrecompileResult {
        let (netuid, interval) = Self::parse_netuid_u64_parameter(data)?;
        let call = RuntimeCall::AdminUtils(
            pallet_admin_utils::Call::<Runtime>::sudo_set_commit_reveal_weights_interval {
                netuid,
                interval,
            },
        );

        dispatch(handle, call, SUBNET_CONTRACT_ADDRESS)
    }

    fn parse_register_network_parameters(
        data: &[u8],
    ) -> Result<(vec::Vec<u8>, vec::Vec<u8>, vec::Vec<u8>), PrecompileFailure> {
        let mut buf = [0_u8; 4];

        // get all start point for three data items: name, repo and contact
        buf.copy_from_slice(get_slice(data, 28, 32)?);
        let subnet_name_start: usize = u32::from_be_bytes(buf) as usize;

        buf.copy_from_slice(get_slice(data, 60, 64)?);
        let github_repo_start: usize = u32::from_be_bytes(buf) as usize;

        buf.copy_from_slice(get_slice(data, 92, 96)?);
        let subnet_contact_start: usize = u32::from_be_bytes(buf) as usize;

        // get name
        buf.copy_from_slice(get_slice(
            data,
            subnet_name_start + 28,
            subnet_name_start + 32,
        )?);
        let subnet_name_len: usize = u32::from_be_bytes(buf) as usize;

        let mut name_vec = vec![0; subnet_name_len];
        name_vec.copy_from_slice(get_slice(
            data,
            subnet_name_start + 32,
            subnet_name_start + subnet_name_len + 32,
        )?);

        // get repo data
        buf.copy_from_slice(get_slice(
            data,
            github_repo_start + 28,
            github_repo_start + 32,
        )?);
        let github_repo_len: usize = u32::from_be_bytes(buf) as usize;

        let mut repo_vec = vec![0; github_repo_len];
        repo_vec.copy_from_slice(get_slice(
            data,
            github_repo_start + 32,
            github_repo_start + github_repo_len + 32,
        )?);

        // get contact data
        buf.copy_from_slice(get_slice(
            data,
            subnet_contact_start + 28,
            subnet_contact_start + 32,
        )?);
        let subnet_contact_len: usize = u32::from_be_bytes(buf) as usize;

        let mut contact_vec = vec![0; subnet_contact_len];
        contact_vec.copy_from_slice(get_slice(
            data,
            subnet_contact_start + 32,
            subnet_contact_start + subnet_contact_len + 32,
        )?);

        Ok((name_vec, repo_vec, contact_vec))
    }

    fn parse_netuid(data: &[u8]) -> Result<u16, PrecompileFailure> {
        if data.len() < 32 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }
        let mut netuid_vec = [0u8; 2];
        netuid_vec.copy_from_slice(get_slice(data, 30, 32)?);
        let netuid = u16::from_be_bytes(netuid_vec);

        Ok(netuid)
    }

    fn parse_netuid_u64_parameter(data: &[u8]) -> Result<(u16, u64), PrecompileFailure> {
        if data.len() < 64 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }
        let mut netuid_vec = [0u8; 2];
        netuid_vec.copy_from_slice(get_slice(data, 30, 32)?);
        let netuid = u16::from_be_bytes(netuid_vec);

        let mut parameter_vec = [0u8; 8];
        parameter_vec.copy_from_slice(get_slice(data, 56, 64)?);
        let parameter = u64::from_be_bytes(parameter_vec);

        Ok((netuid, parameter))
    }

    fn parse_netuid_u16_parameter(data: &[u8]) -> Result<(u16, u16), PrecompileFailure> {
        if data.len() < 64 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }
        let mut netuid_vec = [0u8; 2];
        netuid_vec.copy_from_slice(get_slice(data, 30, 32)?);
        let netuid = u16::from_be_bytes(netuid_vec);

        let mut parameter_vec = [0u8; 2];
        parameter_vec.copy_from_slice(get_slice(data, 62, 64)?);
        let parameter = u16::from_be_bytes(parameter_vec);

        Ok((netuid, parameter))
    }

    fn parse_netuid_u16_u16_parameter(data: &[u8]) -> Result<(u16, u16, u16), PrecompileFailure> {
        if data.len() < 64 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }
        let mut netuid_vec = [0u8; 2];
        netuid_vec.copy_from_slice(get_slice(data, 30, 32)?);
        let netuid = u16::from_be_bytes(netuid_vec);

        let mut parameter_1_vec = [0u8; 2];
        parameter_1_vec.copy_from_slice(get_slice(data, 62, 64)?);
        let parameter_1 = u16::from_be_bytes(parameter_1_vec);

        let mut parameter_2_vec = [0u8; 2];
        parameter_2_vec.copy_from_slice(get_slice(data, 94, 96)?);
        let parameter_2 = u16::from_be_bytes(parameter_2_vec);

        Ok((netuid, parameter_1, parameter_2))
    }

    fn parse_netuid_bool_parameter(data: &[u8]) -> Result<(u16, bool), PrecompileFailure> {
        if data.len() < 64 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::InvalidRange,
            });
        }
        let mut netuid_vec = [0u8; 2];
        netuid_vec.copy_from_slice(get_slice(data, 30, 32)?);
        let netuid = u16::from_be_bytes(netuid_vec);

        let mut parameter_vec = [0_u8];
        parameter_vec.copy_from_slice(get_slice(data, 63, 64)?);

        let parameter = parameter_vec[0] != 0;

        Ok((netuid, parameter))
    }
}
