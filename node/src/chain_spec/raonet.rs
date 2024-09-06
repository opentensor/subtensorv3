// Allowed since it's actually better to panic during chain setup when there is an error
#![allow(clippy::unwrap_used)]

use super::*;

pub fn raonet_config() -> Result<ChainSpec, String> {
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
    .with_genesis_config_patch(raonet_genesis(
        // Initial PoA authorities (Validators)
        // aura | grandpa
        vec![
            // Keys for debug
            authority_keys_from_seed("Alice"),
            authority_keys_from_seed("Bob"),
        ],
        // Pre-funded accounts
        true,
    ))
    .with_properties(properties)
    .build())
}

fn raonet_genesis(
    initial_authorities: Vec<(AuraId, GrandpaId)>,
    _enable_println: bool,
) -> serde_json::Value {
    let mut balances = vec![
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HjCxT2TjvXK99dUVZibtyE2tBkkTXBXazaHX1LiXHKWddtD",
            )
            .unwrap(), // .gagichce | SN2 | [τ, β]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GUBDTciKQ3tYgiLG6EWPh8gnhvD6ERMvroULdLcisDbh896",
            )
            .unwrap(), // Ch3RNØbØG | η ז
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CwKmx4NzAzpqJYpZdNAWZhi4bqQ7PcRe6JVNma76GSVZcoo",
            )
            .unwrap(), // Xrunner(τ, τ)
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CoSC9nRDT4CbnEGLCHcC8PxgpJsWpSdF3RLnTx2aBvX3qPu",
            )
            .unwrap(), // Faybian
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5H6anQpbof6JvBnJNCfq4zNHrfKgpH5JTUWys5cveyb9rdPp",
            )
            .unwrap(), // .HudsonGraeme [τ, β]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Dq2o8s1wjV9NyyU24jxtTLhQCPqnhQcNqciv8nG1YfnqeX8",
            )
            .unwrap(), // atel[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HYziU8CefF3qT7PbMQUJi3XFFnpn3F91EmRjkwF5uAqFc9N",
            )
            .unwrap(), // A.Choji [τ, ?]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DXpDJgY2ep9eY4ZSbtPCSoeNgDif5BkfpYNYYReDM2V7ueN",
            )
            .unwrap(), // 1e-4 [τ, µ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CiUxGA5dTh1cPdgH67Kt62x4w5aKubnmWZMYSrZzoB4hpQi",
            )
            .unwrap(), // Jaszczomp [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EABa2cSSgsSajF43uhrsxB3nDnGt5cz7aqGD7bRzBBnDRN2",
            )
            .unwrap(), // Rapido [τ, ⛏]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GLKkGbFacjQHkMLrAtTGEp4HAwaz3k4GwxGqj8zkmpeY9nk",
            )
            .unwrap(), // sena[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FHCbUkGX8EmoFtsQkoDqdPgzCUTMZTCrVkHnAp52wYQtVBz",
            )
            .unwrap(), // !   Fish [τ τ]
            50_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EPYa3Krjukbft2woziQ72dU8G9mbf4q7Kq378GFxXaiKDKq",
            )
            .unwrap(), // ! carro [τ, ∆] - manifold labs
            50_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Hj53nAGMgtpLHCT7x7VrHDvD9oWpggDyp5rCWJEFE27aHXd",
            )
            .unwrap(), // 0xUnicorn [ τ, τ ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DfeQ6vbsdA7na4oPQRFJjZXFCyh328cWBHDbpXL4X3tbhW3",
            )
            .unwrap(), // dox[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Gdau2r445N3bpBJcncsGLwYfZch9A5tUZ7BYi1iJ245oFrp",
            )
            .unwrap(), // biττenleo (SocialTensor) [τ, ψ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CRp59FW5YBLRCXBzsFmBbEn9VmrZroRqw5aHnwUMMmB6oqN",
            )
            .unwrap(), // 𝕯𝖎𝖒𝖊𝖓𝖘𝖎𝖔𝖓𝖆𝖑[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5F9epb8cjCrbQUShmjS4J11fZedW3KXWjtQ2Aw8oRVY15EUF",
            )
            .unwrap(), // Cipher [τ , ⛏]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FAFURRJ6BHSoAHZHPVRbwv6yU3firjoq6jYSV4itNdEJrAk",
            )
            .unwrap(), // strongarnold [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HgQA9WkXr3Hey1GS23BCxoWUpzia8NsKx18fmo4tDYEGFYp",
            )
            .unwrap(), // Lucian [τ, נ] SN41
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5F4iYayfdJ5aiNewwRWsmpDkd4nUjye1PmjpUUgRHt8T2Hgp",
            )
            .unwrap(), // Ellesmier [τ, α] | Macrocosmos
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GBW59apgzU66Kj8BgpiHV6qX5NckgYUyP9wKUCSrwBJfop7",
            )
            .unwrap(), // EclipseVortex [ז, η]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5F7PwP2ysRFzhBoSV3qrKwvJYJm8V8dCvXJUAAePJjX1GUjy",
            )
            .unwrap(), // CryptoMinedMind | [η, ז]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CoibftDozmPsLptsuANZywkkNBGBX6YY1ebyq9GrCsBB2Jt",
            )
            .unwrap(), // junius
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GpVT5jHZ2TaRdeQFv3PqX5JaHLrmBYtdtspUHcXKVkcPmsQ",
            )
            .unwrap(), // ryan4ai [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CLaMq98jgFstBPUgs3Q24tsfVAnoVngnopRD4g3vbXRgEEx",
            )
            .unwrap(), // shiny  [τ , 🧠]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GxfvDVtJ3aLf39CZDzR7USMAH4Y6RgGz6LKb3r68wHwc8cz",
            )
            .unwrap(), // richie[τ, ⛏]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5D2DvfDXiqAacdeKLNcGS7n8PYyYemr328FbNi4e5qT6QGaC",
            )
            .unwrap(), // wejh
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GgafAFzo3qWtbsokjHB2Z6G4TBGnR1Bae3nyssUBanyfGqA",
            )
            .unwrap(), // ..Nesτa Onur.. (τ, ⚒)
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GWYVx2HB3Z5Y1bbvAnsbQH3Wm324LHwTrhjSuL24fQarcVP",
            )
            .unwrap(), // MrNiche [τ, ψ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5G91gohpiVSfdzaX1172Np7BiMvFvZTgMhXq5fkZ1WS9v2cr",
            )
            .unwrap(), // BτGuy | τ, ψ/ך
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HEUjwdVCDMhHsVXNrYh2ojQzGBdKVSohQdXhZWQaJFSwqqz",
            )
            .unwrap(), // Kat
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FQwzwuZWJbJkAkERVSEtH8cUcPbMog795EiRccMnupkDUfo",
            )
            .unwrap(), // Thomas | Taoshi [τ, θ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GWLycZqbb1L66wQ22ft1hEnfsAvubu47panBLGeJLaigsaq",
            )
            .unwrap(), // mx [τ, ρ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CQHgsPSzpiT1fVzkBLAo2FNxsBqFWhKCx5jbFcy6KLh5Vc3",
            )
            .unwrap(), // alex-drocks [τ, ⛏]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CkV7PPFWh8EihTK5uLm7VNF4C9hiKJ9UeJJwQuByn3bx82L",
            )
            .unwrap(), // Carroτ [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HYrTL47sVFgwtXWA1uFUHgKzmWa66iYCdXVbVHteKXkDpFZ",
            )
            .unwrap(), // Frank : BitAudit[t, t]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DD7xG5TJ48W7j7DUbHG5whrAkwUrbPe1NahiFbKqzytfhVt",
            )
            .unwrap(), // Spiigot [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FNHpx53rqBfaqHuBhGEZGcNz23EKiVAQnGRzZ4XmMoNToVx",
            )
            .unwrap(), // 0x_Terry[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FkLT4UZmXn3mE3rpLyBjz94tkT6KFwME3L11S12Pdkmera1",
            )
            .unwrap(), // Loayei  [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FKpvBG7FZA2QntxEbgYgVTNfZJuhA7vqv2R9dMSq7qvCoYL",
            )
            .unwrap(), // !ACI
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5G4FseBtaQd8sqeC98ZEL7xgtF2GSdueMXwUs8vsBENs4Ysn",
            )
            .unwrap(), // Sai [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HjFjnE4icJvcCKAhpdVmqbfpdREPucbZhDj6NpUyMdYtL9m",
            )
            .unwrap(), // truelce
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5D2a5qugTKWU5e4jWNxZw345TMp73xshEvR6YpGAtVFQaxV6",
            )
            .unwrap(), // lukia
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CqyyBm6Rp7QMDZQyV2hx27tzJfvmV6RkKcJCYNf41zBDb76",
            )
            .unwrap(), // TS-Gunner [t, t]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5F2Hyz5EtZM1hGFqAxQNnb6T7T6NeVn9BFjNd8yW78Zg4PNa",
            )
            .unwrap(), // Boom
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DcsPiqn4Y3YY4AwrPvgtoqm1mznYvxmJPKQfL2tA25CZSvj",
            )
            .unwrap(), // Von [τ τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DsnfJs84niPnWuuXxwqGdb6smcx67fjR9aK3HnSxUUprHgV",
            )
            .unwrap(), // SuNiX [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DkZkDCzdSxFL1qfdhv7oE15Z7cyQCVuUPCLtt4bB4Shq4dy",
            )
            .unwrap(), // moxi
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FNuyRXqdv1uqrmtPQwAPYxmhrGkK1J3TGQkTeJEEzJcZKaP",
            )
            .unwrap(), // Spaceτime [τ, ∞]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FcHCRFYMbHTWsHyMFWm3Q5opKCaCH92sMMTj47o76X3WLwW",
            )
            .unwrap(), // Paradigma [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CXgmrp6Ts5igz9uxSdQQy9ERUVaJFtswzaSBUXhb3Ci7drK",
            )
            .unwrap(), // specialK [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GHs2a3GoqLvk4wZTzE5sEL5Aa1vQUaViB99AYUsJJWf6sds",
            )
            .unwrap(), // toilaluan [τ, ψ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Ef3XfVyz4mz8ESrMs6ZNZe5iWhdiK4Y4cZUZSGmP9ywh4kW",
            )
            .unwrap(), // namoray [τ, τ)
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FbiUU9cUUCZ653LhKT6r7fhyv2y6zMeyN78P2LwwmjbTX3y",
            )
            .unwrap(), // mogmachine ::  [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EssvMunCjyXhYFW8CzWKTdP4vjARAGsqU6wExSBfwJQLisy",
            )
            .unwrap(), // opendansor[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FC3QnWXoDnY2n1oT228vrYyde1LUhbY9C4nwsLFggFZwGgQ",
            )
            .unwrap(), // whisperwind [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CWxB1KSob742nXX2p9SydFPgVwCmZeW5ZHcqUJRnoCWgnLC",
            )
            .unwrap(), // Gus[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CvaAT12X8nJBczNDm6QoqFgDrsWhtzkr4F5wdnD8yYz326C",
            )
            .unwrap(), // Abe [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DLXQLrVZVRiBWCMduwhnFkEYLeoq7HDeR2Q4iLXQWmJYcj7",
            )
            .unwrap(), // Watchmaker [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Hn7ui6XRD29thkzc1SW25miRg3KM57WZ3fuCjUbBocj6RbG",
            )
            .unwrap(), // Sam  | Taoshi [τ, θ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GppQD1aV13DdsafHyi4KQVzhx9Zeeek1ukz2WvBfMTiTWH3",
            )
            .unwrap(), // Mars [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CFYcspxYVpkJuVKq4U9niSzk5kGKUfCgehGsoep51LYo6su",
            )
            .unwrap(), // τuτa⏐Mentat Minds - [ 𝞃 , 𝞃 ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DFQHMScdqve2zpeJcFCXc7XM7HNL3UWgC33k2KJBrak4xKK",
            )
            .unwrap(), // τaomind [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Des8SvYMitS1x9wp37Rdhqyv6nbk8iwfT7uXijCBv3E4M14",
            )
            .unwrap(), // hodler0 [τ , τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5ECP53D9KTArYC3qJoURv86tT3d8G6JRUf5SHu1rejdD43uR",
            )
            .unwrap(), // dagness [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GzrpTnXm39f1Gd3xtsDHzAtK6JG5z2HauRS7vnDuyCY4Rtg",
            )
            .unwrap(), // Cradow :: LLM-Defender [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CXK9amwMeiYLUE7X3FGmyXoprTBt3UJb6wePXiTH3kT9Bb4",
            )
            .unwrap(), // Arrash | Taoshi [τ, θ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GWoqXesG9JuQDSLjCmkNxgnwVP11gezBxA17W8ZVPKkmNNS",
            )
            .unwrap(), // superhappychris [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5C51vbY3SvPPiLrxTviJ1kZBmjjMz5VzR2anbiHFtb2JuqQC",
            )
            .unwrap(), // [ τ ] passion [ τ ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5G1iCdwUBjnXxGfJYzho1dToWTCkYyBF6Vq5sAJP7ftHKE1b",
            )
            .unwrap(), // Jordan | Taoshi [τ, θ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Gc2ZeqZtfYU3cfiLo9sxFvFBCgZXSgXKwV7qg8EcxV2EY2M",
            )
            .unwrap(), // Galina
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FQm9kDYyG5UcpMqNS7oeYqGtc59WQMWgdU1dpwQBZoAZ7xE",
            )
            .unwrap(), // jack_frost
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Gn7h42vbRYHts3ijZSaA4WnHChN5mn3aqJsQQgcrjBfwaSY",
            )
            .unwrap(), // SandGod |Menτaτ Minds [ 𝞃, 𝞃 ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FTjgzjxd9GDqzptYSy1ERME25vrmhHkngL8vxXnDoPfCuPk",
            )
            .unwrap(), // watermelon[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FjYhLtAzbfMqKbXp8ek2NKQKF9U5qfoDhjUQhjt4S7bhARH",
            )
            .unwrap(), // RonX [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EsmkLf4VnpgNM31syMjAWsUrQdW2Yu5xzWbv6oDQydP9vVx",
            )
            .unwrap(), // τθnγ [τ, ך] - SN35
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FjzEVaWYd67NcbRmePRQL94c496MaVPxdPHRWgYzoRFtKuY",
            )
            .unwrap(), // prav [τ,Δ]  - manifold.inc
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FNqfWE5DvzHiUZqyep3Bt4M9c5W3TTwu1PwDdbehYn17AwZ",
            )
            .unwrap(), // dougsillars taostats [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5H6FxT6K6bynitUcU7Q5wh8KJmBPTkhaXe5AKsgsPk69X4jJ",
            )
            .unwrap(), // cisterciansis [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EZDJVUPYFffn8EFpo5JTLBoD6win4WEKqy1891YTop8V6Ye",
            )
            .unwrap(), // Nick [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Cif4VnNQyTz6QzgzJrYEZY77BEPE3L6uhbLPgbV3L5LSxey",
            )
            .unwrap(), // rubin [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EZkWyz71eW8Ep7pfzaMk9PaT5Rx2CtBgYtUR9tWHFfdamKy",
            )
            .unwrap(), // Tiger [τ, ן] SN40
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5F1PWV2U3WMpEvLQDt6Lmnv5AmWqYWNmbYgW3Yj2pSF3G8Ya",
            )
            .unwrap(), // afterpartyjohn | ReadyAI [τ,ט]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CSwvff2fGySoxYztCZCdMEUPEmu6vZ8HbTnhweMbeUmh6mM",
            )
            .unwrap(), // 0xWash  [ 𝜏, 𝜏] (Eltopee)
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HW8c7RfHXZHPVK8AwfaQkvuGScWdwo4c4cUxT1fRHzyECsu",
            )
            .unwrap(), // on13chain [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DS3HKzvCeqeoHfStTGS7PnHVoCAACsiuisx5mfvQjswSZcH",
            )
            .unwrap(), // Rovertter
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EZ1PtU4S89DqLeGXtcxj1EL4jSQoUeFstmMMi4Pa3YaSkgG",
            )
            .unwrap(), // Capτain [τ, ?]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Fc3VkJg13YQt7B1PUCp4MeuLSd1R4NSPnDUQcbJg7JQE2gU",
            )
            .unwrap(), // cornbread998 | SN15 [τ, ο]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CiFWGUsbMGEXY4TeUB5NGmsKqxrTrBc1TGsjaABhMn71Yxi",
            )
            .unwrap(), // Sherwin | Graphiτe | SN43 [τ, ע]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CtTNTV9Pk7Uy8jgESwQAPUCqVMDi1kSNBEFUHsDXbEQStnq",
            )
            .unwrap(), // Sangar[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CJ2ZtY8c5UfSTTDkJMDa3HU7TvxWxHUjtWyThfvj18uFckV",
            )
            .unwrap(), // Cow Boy [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DFVYVLFZyYFtGF94vyn67a5QvhKfcDmSiTs8A2ECxJWJ6UP",
            )
            .unwrap(), // indianaJ [τ, ?]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Constt5Qpa9rDzpy15vuXGymG2KpzRkE4jKJEFHXkmWgnxL",
            )
            .unwrap(), // μ | coldint | SN29 [τ, ה]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DiKdaWbhZieXLvYv72RYLUVT88ZG197iiYeDKS7Ss72k259",
            )
            .unwrap(), // Emily
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HMxSn8wAAy2a9zRF9tMeDadqCH9GjVLEAu14kvs7HeRtoE4",
            )
            .unwrap(), // davfields | ReadyAI [τ,ט]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FCBeJik1NyPU3Ybx7UfKioELN5Jx6JaVnYNN1ufwNza1sCc",
            )
            .unwrap(), // borgg [τ, ג]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5H4D6hMo1ozTPJ6HLdLv591sThbY37FNuBas8mRBsGWyyVDC",
            )
            .unwrap(), // Thanos | Datura [τ, τ)
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Ea1bNLgXLaRnSVyNVjsiq8wfzqUpSRdeY7CN68fLhE5RQaK",
            )
            .unwrap(), // Aireze [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DZLHDRnRKJi936Vvr6rAPStjw9PHkTnrt8Sb8mpFHqczm9A",
            )
            .unwrap(), // taoalchemy [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Ca9fz3DnwhRB47zCCuXcCgDdGnNdkCtPBo471pZWp3geMiT",
            )
            .unwrap(), // Just [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5E9YYVNu8aJBZxww6rWRL64LkUEySZNJtkzjqAADjkDniHUv",
            )
            .unwrap(), // cristopia [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EvdfSLqPfovo2fDtijwNHL5DAKy9wZWiW9smqaAtKYZhp39",
            )
            .unwrap(), // sol [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5ECLx92iZ4TyD7NqgSWopadVeZFi1zr38kMCERAwKtnrpeAF",
            )
            .unwrap(), // captain_america [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5F1k451t2EQyQxBVpSxokmPeULWcHKunx8idgNP8vpguBKHK",
            )
            .unwrap(), // ame_eshwar [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EZ4xxbedw8SKqN3oLjZKv61KWehAVL26buJP93K6i1sd66N",
            )
            .unwrap(), // X-blazer
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GTZtWVdLT8CQB2nidJszn6THFMDcnNf9udta4mrNJnDpkuX",
            )
            .unwrap(), // Roman | Corτex [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CqhhYNESoHDNpRH6rvPdr5NqMZy87n8ptkYZZAH73XDEpyv",
            )
            .unwrap(), // dan (τ,τ)
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EgvTaftth7S7Gz9UpnLm2AbCdS9wcw8HeZfVrxNrLippUfC",
            )
            .unwrap(), // Rahul | Foundry [τ, ד]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5ESAMx17PWX2t6roLVc1H9xLmMgLk9VCbG2J4a7VWzacuhpD",
            )
            .unwrap(), // sam0x17 [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Ebcyb3gfWZ2JMkBMjZuDNr1MaeoW5hJVH92M9dwn4HVBN6t",
            )
            .unwrap(), // ulτrashiny [τ, ⛏]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Hg72rESMS4NVFuwRKm9p9JCCQKpcd2V43yeWKjkxut3AgBb",
            )
            .unwrap(), // Gabriel_YAY [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HeGiZUnAdykNzVcTa8EDx37XqUytSpEKXQwbQJKSPh3rJQ1",
            )
            .unwrap(), // τensorbud [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DUDLpJ3G6fzhQTwNQXciNypy2QDpAUrqaETymjqxLhVdNB3",
            )
            .unwrap(), // HippoHoppy1337 [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GC1vJSs1VjhGjtLCQsFxoQn3Gp5mBYqD37dhrooC4iqT6WQ",
            )
            .unwrap(), // shr1ftyy [τ, τ] | Sturdy
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CCwi5Qas4uW4BZ52K3mhpYEh23UMeBTAduTZuNgJVoqTMVq",
            )
            .unwrap(), // Abyssu [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EAArrR9SJnBn93wCnvYZDWwpJWT2cc8rn8MbU79ctWyqPNj",
            )
            .unwrap(), // Nam [τ, ג]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5He3NLYn5iUwNSCfDtc43MoeydZ4VPfV5iGp14wxcRMmpYkd",
            )
            .unwrap(), // Pardus [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HKbrZcWVc4kSaUAP4KA2Sn5DGYs56Hh32nAf9zFwmU5ifU4",
            )
            .unwrap(), // xam2210 [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Cm3btu89SvBHEE3kXt3nuhWz7YBd6oU6dRYsBzDscPDgg3R",
            )
            .unwrap(), // Tullio [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DchRGKk7wD75vEZKZSn7mBXc3h9vLGNAYJdYjKiuhur1Sis",
            )
            .unwrap(), // axilo [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GbwSp7m4AF1hYpoXtkwaveDjuNHifHJbktfW2UkQfVVenLg",
            )
            .unwrap(), // AM[t,villageidiot]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Ev5heU9heLZYVtvNqiGrKz2wzgaSAXpYbHq6cryXvQD3BsS",
            )
            .unwrap(), // isebarn | Datura [τ, τ)
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FZo562PuvFVBihqU9XWaXxtmWbfY21tBYa1goTtNS1vq4bd",
            )
            .unwrap(), // SandGod |Menτaτ Minds [ 𝞃, 𝞃 ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FW6djdCybRYo8XFT5sTQowgq74ovxseJsmcDGFu6zg12Erv",
            )
            .unwrap(), // demon [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EHVUNEqz1js5LdnW56hFpqKAV2pEGa7GCA2z6r7GVdLyTZE",
            )
            .unwrap(), // vune [τ, τ)
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FQnXs1GzD4sdNhKvsmd1kNRQohAJNwK32nrGeK3M5dKz4D2",
            )
            .unwrap(), // kubernauτis [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HdmLXWm9eQrrDTMu71bdUDe1NQzfL1VZyuaL9S16DHzkJ1h",
            )
            .unwrap(), // C[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FuvWGyFou3dvWx7psuqnTYRGHmrB7qNmK4TowzNjH1NBqqE",
            )
            .unwrap(), // Ivan [τ, β]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GQXcG4wTtCiNrWSnxMyM475pXCgPTprsxw4yULQ65aPF48d",
            )
            .unwrap(), // Mikel [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DJargsqTiyrUue1rYuVLDcZ2DLzEBv6r6WKDF9hidyVTo5u",
            )
            .unwrap(), // Anton.noderunner [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EUpxpNaMxorYdS6NUGwVVCtXZA3dGUjWZv8dUN9PrecKDqy",
            )
            .unwrap(), // crux [τ, ...] | Macrocosmos
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CoxgQtQ7gjbDNecnTiqu7R2XVGF5AP2pkKKPajhbRZ13rAr",
            )
            .unwrap(), // CrypτicMax [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5D4jSKrve48ANdeJJrtrGSXbZ6LizzP2PTKKaDsiRt3zYEnp",
            )
            .unwrap(), // Raoτens☯r [ττ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5D7G5Td6KEaWCnj1oUipe5cgXd8rkkNupmaNfFiRj2GdWXVN",
            )
            .unwrap(), // Canti[τ, υ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Fehi6mrk3Um8v3yuRVEd5TissXypGhanQVXLN5QXCi1cpeX",
            )
            .unwrap(), // Tegridy
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FbrREXh2YYMmkZR7Ho7Qc9LShf6v6toTWBz3uxPNN5tuRkn",
            )
            .unwrap(), // Rok [τ, ?]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DPKKVbMyjHPtB3jKne89JqKa12k5UTHPH73ugSiusMobQsT",
            )
            .unwrap(), // cosmic [ τ, ο ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GVsK7YYyKZHUKMhZVa8f5XNhitcTJq2RiVhX37C2ceyf5tT",
            )
            .unwrap(), // ligature[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FPMxTEgipM45mdvXuCoba3EeKtBy7ic91bdp9xJxthxJe2z",
            )
            .unwrap(), // Internal[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5G3sVeEiMoegwurqrsVuLXo1JDwem18x4knbdFC8gmDF96Wq",
            )
            .unwrap(), // zk [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EsoW9GiNLdWw55XE3TpXx1zxE8w9r5BpA5f1tjYfDYFbbum",
            )
            .unwrap(), // Lord Nadejde.eth [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EbszTEcgBHJbaZQcr68HDWyV6Bi1s2ucjH9kCyRx6Pj7Anm",
            )
            .unwrap(), // Terps [down, bad]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Dt4pnfzBpUbERtuHAoQt4Lz4taBSRdUEo1L8nm36FqLvBBT",
            )
            .unwrap(), // Gon - manifold labs [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Chis615kESNvQryodkHFPnERP4mhdx7Q13GEoFnZ8M4fGHZ",
            )
            .unwrap(), // Sarokhan - manifold labs [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CigqSgq7epMyKA8U9YydhHAFWHwTqVpscGpr57HB28LNbLE",
            )
            .unwrap(), // James - Manifold [τ,τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GisA6XvD4QgXmkCdkmG15nVMoWstgGWfRCopvaheqDsk8LH",
            )
            .unwrap(), // snowman [τ,τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CkkadxUj3aW86y598j1PcZFjWzSUiRNSZLpZSHRdV8yGJTA",
            )
            .unwrap(), // tensorguru [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Fc8ggzwmUMVBWgPjAArTHy6JZS6BJ5q5m7BtqVCSwReDiMQ",
            )
            .unwrap(), // τaocorn [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GRFWWdieftgVvcYSFgYe3nVmFvnGRdheSGUMXKAs5BffzBr",
            )
            .unwrap(), // xponentcrisis [τ, τ] Tensorplex
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5ES2Ua8d4o5NChiFiqDrPJbaisvHnwWQbnmnrZKcEZcwcFCw",
            )
            .unwrap(), // Dubs [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FNT6J8qbDUEYP8jmyzVxxRM1d2kRs8xFHKLAv2Maz9sy9hv",
            )
            .unwrap(), // cisterciansis [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5GRbLgWotN4pCp3ZvsAWvYyYbZqvG4zp5zZJeKHH4VfiQdbc",
            )
            .unwrap(), // Daryxx [τ τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FPJsVFKTYCoywFKGPtA1EaGPAnHm9WnwLzQH6yRVe8wMvTe",
            )
            .unwrap(), // opsx [τ, ζ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5Eeqea7DucVuocCymHYGuTF88dRHaboSiVxA6G6rp1WejZMh",
            )
            .unwrap(), // Kei [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FNuMAaCwm14ZWriHME46LgoP3iaa2dM4uND7kKsF4FjQhDL",
            )
            .unwrap(), // John[τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5EeBuJRFUMS3CgisL1FT2w4AdqSQVGWRGNsTdR5YrFd189PT",
            )
            .unwrap(), // GregZaitsev [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5DUFdkP4rJrkXq9pfrWMHQS8zgiwXBZRgw2MMEAnBot59Taz",
            )
            .unwrap(), // Bob
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5FRo4vab84LM3aiK4DijnVawGDKagLGLzfn95j9tjDaHja8Z",
            )
            .unwrap(), // Const [τ, τ]
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5HjEUemUaXSkxPcxGYiLykHmi5VfXBh5NCeNXYMbj9akYHbn",
            )
            .unwrap(), // Jip.
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5CwAuY3BekpE6wpopgQjDhpB2PutsuynLjsaSH8wvHKrsq9P",
            )
            .unwrap(), // Algod.
            100_000_000_000u128,
        ),
        (
            <AccountId32 as Ss58Codec>::from_ss58check(
                "5H5wqFQp2Kq6C9mJJpymRmeywxdYXp5hfWTtPM4NKhFG77jr",
            )
            .unwrap(), // Samuel.
            100_000_000_000u128,
        ),
    ];

    // Check if the environment variable is set
    if let Ok(bt_wallet) = env::var("BT_DEFAULT_TOKEN_WALLET") {
        if let Ok(decoded_wallet) = Ss58Codec::from_ss58check(&bt_wallet) {
            balances.push((decoded_wallet, 1_000_000_000_000_000u128));
        } else {
            eprintln!("Invalid format for BT_DEFAULT_TOKEN_WALLET.");
        }
    }

    let trimvirate_members: Vec<AccountId> = bounded_vec![
        <AccountId32 as Ss58Codec>::from_ss58check(
            "5FRo4vab84LM3aiK4DijnVawGDKagLGLzfn95j9tjDaHja8Z"
        )
        .unwrap(),
        <AccountId32 as Ss58Codec>::from_ss58check(
            "5HjEUemUaXSkxPcxGYiLykHmi5VfXBh5NCeNXYMbj9akYHbn"
        )
        .unwrap(),
        <AccountId32 as Ss58Codec>::from_ss58check(
            "5DUFdkP4rJrkXq9pfrWMHQS8zgiwXBZRgw2MMEAnBot59Taz"
        )
        .unwrap(),
    ];

    let senate_members: Vec<AccountId> = bounded_vec![
        get_account_id_from_seed::<sr25519::Public>("Dave"),
        get_account_id_from_seed::<sr25519::Public>("Eve"),
        get_account_id_from_seed::<sr25519::Public>("Ferdie"),
    ];

    serde_json::json!({
        "balances": { "balances": balances },
        "aura": {
            "authorities": initial_authorities.iter().map(|x| (x.0.clone())).collect::<Vec<_>>()
        },
        "grandpa": {
            "authorities": initial_authorities
                .iter()
                .map(|x| (x.1.clone(), 1))
                .collect::<Vec<_>>()
        },
        "sudo": {
            "key": <AccountId32 as Ss58Codec>::from_ss58check("5FRo4vab84LM3aiK4DijnVawGDKagLGLzfn95j9tjDaHja8Z").unwrap()
        },
        "triumvirateMembers": {
            "members": trimvirate_members
        },
        "senateMembers": {
            "members": senate_members,
        },
    })
}
