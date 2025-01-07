{
	setSudo(address): {
		_genesis+: {
			sudo+: {
				key: address,
			},
		}
	},
	resetBalances: {
		_genesis+: {
			balances+: {
				balances: [],
			},
		},
	},
	giveBalance(address, amount): {
		_genesis+: {
			balances+: {
				balances+: [
					[address, amount],
				],
			},
		},
	},
	resetSessionKeys: {
		_genesis+: {
			session+: {
				keys: [],
			},
		}
	},
	addSessionKey(key): {
		_genesis+: {
			session+: {
				keys+: [key],
			},
		},
	},
	resetAuraKeys: {
		_genesis+: {
			aura+: {
				authorities: [],
			},
		},
	},
	addAuraKey(key): {
		_genesis+: {
			aura+: {
				authorities+: [key],
			},
		},
	},
	resetCollatorSelectionInvulnerables: {
		_genesis+: {
			collatorSelection+: {
				invulnerables: [],
			},
		}
	},
	addCollatorSelectionInvulnerable(key): {
		_genesis+: {
			collatorSelection+: {
				invulnerables+: [key],
			},
		},
	},
	resetParachainStakingCandidates: {
		_genesis+: {
			parachainStaking+: {
				candidates: [],
			},
		},
	},
	addParachainStakingCandidate(key): {
		_genesis+: {
			parachainStaking+: {
				candidates+: [key],
			},
		},
	},
	resetStakingInvulnerables: {
		_genesis+: {
			staking+: {
				invulnerables: [],
			},
		},
	},
	addStakingInvulnerable(key): {
		_genesis+: {
			staking+: {
				invulnerables+: [key],
			},
		},
	},
	resetStakingStakers: {
		_genesis+: {
			staking+: {
				stakers: [],
			},
		},
	},
	addStakingStaker(key): {
		_genesis+: {
			staking+: {
				stakers+: [key],
			},
		},
	},
	setStakingValidatorCount(count): {
		_genesis+: {
			staking+: {
				validatorCount: count,
			},
		},
	},
	resetAuthorMappingMappings: {
		_genesis+: {
			authorMapping+: {
				mappings: [],
			},
		},
	},
	addAuthorMappingMapping(key): {
		_genesis+: {
			authorMapping+: {
				mappings+: [key],
			},
		},
	},
	resetParas: {
		_genesis+: {
			paras+: {
				paras: [],
			},
		},
	},
	addPara(para_id, head, wasm, parachain = true): {
		_genesis+: {
			paras+: {
				paras+: [[
					para_id,
					{
						genesis_head: head,
						validation_code: wasm,
						parachain: parachain,
					},
				]],
			},
		},
	},

	resetHrmps: {
		_genesis+: {
			hrmp+: {
				preopenHrmpChannels: [],
			},
		},
	},
	openHrmp(sender, receiver, maxCapacity, maxMessageSize): {
		_genesis+: {
			hrmp+: {
				preopenHrmpChannels+: [
					[sender, receiver, maxCapacity, maxMessageSize],
				],
			},
		},
	},

	resetNetworking(root): {
		assert !(super?._networkingWasReset ?? false): 'network should not be reset twice',

		bootNodes: [
			'/dns/%s/tcp/30333/p2p/%s' % [node.hostname, node.nodeIdentity],
			for [?, node] in root.nodes
		],
		chainType: 'Development',
		telemetryEndpoints: [],
		codeSubstitutes: {},
		properties: {
			ss58Format: 42,
			tokenDecimals: 9,
			tokenSymbol: "TAO"
		},

		// COMPAT: cumulus template
		// In baedeker, relay chain config is passed explicitly, rendering this argument to not being used
		[if 'relay_chain' in root then 'relay_chain']: 'not_used',
		// COMPAT: some chains use camelCase here
		[if 'relayChain' in root then 'relayChain']: 'not_used',

		_networkingWasReset:: true,
	},

	simplifyGenesisName(): function(prev)
	local genesisKind = if 'runtimeGenesis' in prev.genesis then 'sane-1.5-runtimeGenesis' else if 'runtimeAndCode' in prev.genesis then 'deprecated-runtimeAndCode' else if 'runtime_genesis_config' in prev.genesis.runtime then 'rococo' else 'sane';
	prev {
		_genesisKind: genesisKind,
	} +
	if genesisKind == 'rococo' then {
		_genesis::: prev.genesis.runtime.runtime_genesis_config + {system+: {code: '0x42424242'}},
		_code::: prev.genesis.runtime.runtime_genesis_config.system.code,
		genesis+: {
			runtime+: {
				runtime_genesis_config:: error 'unsimplify genesis name first',
			},
		},
	} else if genesisKind == 'sane' then {
		_genesis::: prev.genesis.runtime + {system+: {code: '0x42424242'}},
		_code::: prev.genesis.runtime.system.code,
		genesis+: {
			runtime:: error 'unsimplify genesis name first',
		},
	} else if genesisKind == 'deprecated-runtimeAndCode' then {
		_genesis::: prev.genesis.runtimeAndCode.runtime + {system+: {code: '0x42424242'}},
		_code::: prev.genesis.runtimeAndCode.code,
		genesis+: {
			runtimeAndCode::: error 'unsimplify genesis name first',
		},
	} else if genesisKind == 'sane-1.5-runtimeGenesis' then {
		_runtimeGenesisKind::: if 'config' in prev.genesis.runtimeGenesis then 'config' else 'patch',
		_genesis::: prev.genesis.runtimeGenesis[self._runtimeGenesisKind] + {system+: {code: '0x42424242'}},
		_code::: prev.genesis.runtimeGenesis?.code,
		genesis+: {
			runtimeGenesis:: error 'unsimplify genesis name first',
		},
	},

	unsimplifyGenesisName(): function(prev)
	prev {
		_runtimeGenesisKind:: error 'simplify genesis name first',
		_genesis:: error 'simplify genesis name first',
		_code:: error 'simplify genesis name first',
		_genesisKind:: error 'genesis was resimplified',
	} +
	if prev?._genesisKind == 'rococo' then assert prev._genesis.system.code == '0x42424242' : 'use _code for overriding code!'; {
		genesis+: {
			runtime+: {
				runtime_genesis_config::: prev._genesis + {
					system+: {
						code: prev._code,
					},
				},
			},
		},
	} else if prev?._genesisKind == 'sane' then assert prev._genesis.system.code == '0x42424242' : 'use _code for overriding code!'; {
		genesis+: {
			runtime::: prev._genesis + {
				system+: {
					code: prev._code,
				},
			},
		},
	} else if prev?._genesisKind == 'deprecated-runtimeAndCode' then assert prev._genesis.system.code == '0x42424242' : 'use _code for overriding code!'; {
		genesis+: {
			runtimeAndCode::: {
				code: prev._code,
				runtime: prev._genesis + {
					system+: {
						code:: error 'use _code for overriding code!',
					},
				},
			},
		},
	} else if prev?._genesisKind == 'sane-1.5-runtimeGenesis' then assert prev._genesis.system.code == '0x42424242' : 'use _code for overriding code!'; {
		genesis+: {
			runtimeGenesis::: {
				code: prev._code,
				[prev._runtimeGenesisKind]: prev._genesis + {
					system+: {
						code:: error 'use _code for overriding code!',
					},
				},
			},
		},
	} else error 'unknown genesis kind: %s' % [prev._genesis],

	// FIXME: Merge polkaLaunchRelay and polkaLaunchPara?
	// Due to refactoring, pararelays are somewhat supported.

	polkaLaunchShared(root): local
		isEth = root.signatureSchema == 'Ethereum',
		// FIXME: support soft derivations in ecdsaSeed, then unhardcode private keys here.
		// Soft derivations here are
		//  Alith:     m/44'/60'/0'/0/0
		//  Baltathar: m/44'/60'/0'/0/1
		// Root mnemonic for both is standard substrate "bottom drive obey lake curtain smoke basket hold race lonely fit walk", which is implied by *Seed functions

		// Alice/Alith
		accountA = if !isEth then root.addressSeed('//Alice') else '0xf24FF3a9CF04c71Dbc94D0b566f7A27B94566cac',
		// Bob/Baltathar
		accountB = if !isEth then root.addressSeed('//Bob') else '0x3Cd0A705a2DC65e5b1E1205896BaA2be8A07c6e0',
		// Charlie/Charleth
		accountC = if !isEth then root.addressSeed('//Charlie') else '0x798d4Ba9baf0064Ec19eB4F0a1a45785ae9D6DFc',
		// Dave/Dorothy
		accountD = if !isEth then root.addressSeed('//Dave') else '0x773539d4Ac0e786233D90A233654ccEE26a613D9',
		// Eve/Ethan
		accountE = if !isEth then root.addressSeed('//Eve') else '0xFf64d3F6efE2317EE2807d223a0Bdc4c0c49dfDB', 
	; [
		function(prev) if 'sudo' in prev._genesis then bdk.mixer([
			$.setSudo(accountA),
		])(prev) else prev,
		$.resetBalances,
		$.giveBalance(accountA, 2000000000000000000000000000000),
		$.giveBalance(accountB, 2000000000000000000000000000000),
		$.giveBalance(accountC, 2000000000000000000000000000000),
		$.giveBalance(accountD, 2000000000000000000000000000000),
		$.giveBalance(accountE, 2000000000000000000000000000000),
		// Regardless of validator id assignment, every method (staking/collator-selection/etc) wants stash to have some
		// money.
		[
			$.giveBalance(node.wallets.stash, 2000000000000000000000000000000),
			for [?, node] in root.nodes
		],
		// pallet-session manages pallet-aura/pallet-grandpa, if there is no pallet-session: authority should be set directly for aura.
		// pallet-aura also should not have keys, if there keys are specified using pallet-aura.
		function(prev) bdk.mixer([
			if 'session' in prev._genesis then $.resetSessionKeys,
			if 'aura' in prev._genesis then $.resetAuraKeys,
		])(prev),
		function(prev) bdk.mixer(if 'session' in prev._genesis then [
			$.addSessionKey([
				// Account id
				if root.validatorIdAssignment == 'staking' then node.wallets.controller
				else node.wallets.stash,
				// Validator id
				node.wallets.stash,
				local k = node.keys; {
					[name]: k[key]
					for [name, key] in node.wantedKeys.sessionKeys
				},
			])
			for [?, node] in root.nodes
		] else if 'aura' in prev._genesis then [
			$.addAuraKey(node.keys.aura)
			for [?, node] in root.nodes
		] else [])(prev),
	],

	// Alter spec in the same way as polkadot-launch does this, in most cases this should
	// be everything needed to start working node
	polkaLaunchRelay(root, hrmp = []): $.polkaLaunchShared(root) + [
		function(prev) if 'staking' in prev._genesis then bdk.mixer([
			$.resetStakingInvulnerables,
			$.resetStakingStakers,
			[
				[
					$.addStakingInvulnerable(node.wallets.stash),
					$.addStakingStaker([
						node.wallets.stash,
						node.wallets.controller,
						100000000000000,
						'Validator',
					]),
				],
				for [?, node] in root.nodes
			],
			$.setStakingValidatorCount(std.length(root.nodes)),
		])(prev) else prev,
		function(prev) bdk.mixer([
			[
				$.resetParas,
			],
			[
				// FIXME: Also bump parachainRegistrar last id if para_id >= 2000?
				$.addPara(para.paraId, para.genesisHead, para.genesisWasm),
				for [paraname, para] in root.parachains
			],
		])(prev),
		function(prev) bdk.mixer([
			[
				$.resetHrmps,
			],
			[
				$.openHrmp(ch[0], ch[1], ch[2], ch[3]),
				for ch in hrmp
			],
		])(prev),
		function(prev) if 'configuration' in prev._genesis then local
			prevConfig = prev?._genesis.configuration?.config ?? {},
			ifExists(f, o) = if f in o then f;
		prev {
			_genesis+: {
				configuration+: {
					config+: {
						hrmp_max_parachain_outbound_channels: 20,
						[ifExists('hrmp_max_parathread_outbound_channels', prevConfig)]: 20,
						hrmp_max_parachain_inbound_channels: 20,
						[ifExists('hrmp_max_parathread_inbound_channels', prevConfig)]: 20,
						[ifExists('pvf_checking_enabled', prevConfig)]: true,
						max_validators: 300,
						[ifExists('max_validators_per_core', prevConfig)]: 20,
						[ifExists('scheduling_lookahead', prevConfig)]: 1
					},
				},
			},
		} else prev,
		// function(prev) std.trace(prev),
	],
	polkaLaunchPara(root): $.polkaLaunchShared(root) + [
		function(prev) if 'collatorSelection' in prev._genesis then bdk.mixer([
			$.resetCollatorSelectionInvulnerables,
			[
				$.addCollatorSelectionInvulnerable(node.wallets.stash),
				for [?, node] in root.nodes
			],
		])(prev) else prev,

		// COMPAT: moonbeam
		function(prev) if 'parachainStaking' in prev._genesis then bdk.mixer([
			$.resetParachainStakingCandidates,
			[
				$.addParachainStakingCandidate([node.wallets.stash, 10000000000000000000000000]),
				for [?, node] in root.nodes
			],
		])(prev) else prev,
		// COMPAT: moonbeam
		function(prev) if 'authorMapping' in prev._genesis then bdk.mixer([
			$.resetAuthorMappingMappings,
			[
				$.addAuthorMappingMapping([node.keys?.aura ?? node.keys.nmbs, node.wallets.stash]),
				for [?, node] in root.nodes
			],
		])(prev) else prev,
	],

	genericRelay(root, hrmp = []): bdk.mixer([
		$.resetNetworking(root),
		$.simplifyGenesisName(),
		$.polkaLaunchRelay(root, hrmp),
		$.unsimplifyGenesisName(),
	]),
	genericPara(root): bdk.mixer([
		$.resetNetworking(root),
		$.simplifyGenesisName(),
		$.polkaLaunchPara(root),
		$.unsimplifyGenesisName(),
	]),
}
