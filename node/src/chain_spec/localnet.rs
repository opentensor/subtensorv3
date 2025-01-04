// Allowed since it's actually better to panic during chain setup when there is an error
#![allow(clippy::unwrap_used)]

use babe_primitives::AuthorityId as BabeId;
use node_subtensor_runtime::{opaque::SessionKeys, BABE_GENESIS_EPOCH_CONFIG, UNITS};
use pallet_staking::Forcing;
use sp_authority_discovery::AuthorityId as AuthorityDiscoveryId;
use sp_consensus_grandpa::AuthorityId as GrandpaId;
use sp_runtime::Perbill;
use sp_staking::StakerStatus;

use super::*;

pub fn localnet_config(single_authority: bool) -> Result<ChainSpec, String> {
    let wasm_binary = WASM_BINARY.ok_or_else(|| "Development wasm not available".to_string())?;

    // Give front-ends necessary data to present to users
    let mut properties = sc_service::Properties::new();
    properties.insert("tokenSymbol".into(), "TAO".into());
    properties.insert("tokenDecimals".into(), 9.into());
    properties.insert("ss58Format".into(), 42.into());

    Ok(ChainSpec::builder(
        wasm_binary,
        Extensions {
            bad_blocks: Some(HashSet::from_iter(vec![
                // Example bad block
                H256::from_str(
                    "0xc174d485de4bc3813ac249fe078af605c74ff91d07b0a396cf75fa04f81fa312",
                )
                .unwrap(),
            ])),
            ..Default::default()
        },
    )
    .with_name("Bittensor")
    .with_protocol_id("bittensor")
    .with_id("bittensor")
    .with_chain_type(ChainType::Development)
    .with_genesis_config_patch(localnet_genesis(
        // Initial PoA authorities (Validators)
        // aura | grandpa
        if single_authority {
            // single authority allows you to run the network using a single node
            vec![authority_keys_from_seed("Alice")]
        } else {
            vec![
                authority_keys_from_seed("Alice"),
                authority_keys_from_seed("Bob"),
            ]
        },
        // Pre-funded accounts
        true,
    ))
    .with_properties(properties)
    .build())
}

fn localnet_genesis(
    initial_authorities: Vec<(AccountId, GrandpaId, BabeId, AuthorityDiscoveryId)>,
    _enable_println: bool,
) -> serde_json::Value {
    let mut balances = vec![
        (
            get_account_id_from_seed::<sr25519::Public>("Alice"),
            1000000000000000u128,
        ),
        (
            get_account_id_from_seed::<sr25519::Public>("Bob"),
            1000000000000000u128,
        ),
        (
            get_account_id_from_seed::<sr25519::Public>("Charlie"),
            1000000000000000u128,
        ),
        (
            get_account_id_from_seed::<sr25519::Public>("Dave"),
            2000000000000u128,
        ),
        (
            get_account_id_from_seed::<sr25519::Public>("Eve"),
            2000000000000u128,
        ),
        (
            get_account_id_from_seed::<sr25519::Public>("Ferdie"),
            2000000000000u128,
        ),
    ];

    for a in initial_authorities.iter() {
        balances.push((a.0.clone(), 2000000000000u128));
    }

    // Check if the environment variable is set
    if let Ok(bt_wallet) = env::var("BT_DEFAULT_TOKEN_WALLET") {
        if let Ok(decoded_wallet) = Ss58Codec::from_ss58check(&bt_wallet) {
            balances.push((decoded_wallet, 1_000_000_000_000_000u128));
        } else {
            eprintln!("Invalid format for BT_DEFAULT_TOKEN_WALLET.");
        }
    }

    let trimvirate_members: Vec<AccountId> = bounded_vec![
        get_account_id_from_seed::<sr25519::Public>("Alice"),
        get_account_id_from_seed::<sr25519::Public>("Bob"),
        get_account_id_from_seed::<sr25519::Public>("Charlie"),
    ];

    let senate_members: Vec<AccountId> = bounded_vec![
        get_account_id_from_seed::<sr25519::Public>("Dave"),
        get_account_id_from_seed::<sr25519::Public>("Eve"),
        get_account_id_from_seed::<sr25519::Public>("Ferdie"),
    ];

    const STAKE: u64 = 1000 * UNITS;
    serde_json::json!({
        "balances": { "balances": balances },
        "session": {
            "keys": initial_authorities
                .iter()
                .map(|x| {
                    (
                        x.0.clone(),
                        x.0.clone(),
                        SessionKeys {
                            grandpa: x.1.clone(),
                            babe: x.2.clone(),
                            authority_discovery: x.3.clone(),
                        },
                    )
                })
                .collect::<Vec<_>>(),
        },
        "staking": {
            "minimumValidatorCount": 1,
            "validatorCount": initial_authorities.len() as u32,
            "stakers": initial_authorities
                .iter()
                .map(|x| (x.0.clone(), x.0.clone(), STAKE, StakerStatus::<AccountId>::Validator))
                .collect::<Vec<_>>(),
            "invulnerables": initial_authorities.iter().map(|x| x.0.clone()).collect::<Vec<_>>(),
            "forceEra": Forcing::NotForcing,
            "slashRewardFraction": Perbill::from_percent(10),
        },
        "babe": { "epochConfig": BABE_GENESIS_EPOCH_CONFIG },
        "sudo": {
            "key": Some(get_account_id_from_seed::<sr25519::Public>("Alice"))
        },
        "triumvirateMembers": {
            "members": trimvirate_members
        },
        "senateMembers": {
            "members": senate_members,
        },
    })
}
