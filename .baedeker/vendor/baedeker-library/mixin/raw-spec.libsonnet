local m = import './spec.libsonnet';
local {encodeGrandpaKeys} = import '../util/grandpaKeys.libsonnet';
local strToHex(str) = cql.toHex(std.encodeUTF8(str));
local
	account(name) = cql.sr25519Seed(name),
	unwrapNewtype(struct) = local names = std.objectFields(struct);
		if std.length(names) == 1 then struct[names[0]]
		else struct,
	WELLKNOWN_CODE = '0x3a636f6465',
;

{
	resetSystem: function(prev) prev {
		_storage+: {
			System+: {
				// Magic value mandated by spec, nice
				// [69, 69, 69, ..., 69, 69, 69]
				local hash69 = '0x' + '45' * 32,
				BlockHash: {
					"0": hash69,
				},
				EventCount: 0,
				EventTopics: {},
				Events: [],
				ExtrinsicCount: 0,
				ExtrinsicData: {},

				// Block header
				ParentHash: hash69,
				Number: 0,
				Digest: [],
			},
			Aura+: {
				CurrentSlot: std.bigint('0'),
			},
			[if 'CollatorSelection' in prev._storage then 'CollatorSelection']+: {
				LastAuthoredBlock: {},
			},
			[if 'Session' in prev._storage then 'Session']+: {
				CurrentIndex: 0,
			},
			// Full reset
			[if 'Authorship' in prev._storage then 'Authorship']: {},
		},
	},

	setSudo(key): {
		_storage+: {
			Sudo+: {
				Key: key,
			},
		},
	},
	giveBalance(address): {
		_storage+: {
			// Not updating total issuance: no big difference.
			System+: {
				Account+: std.trace('Altering account %s: %s' % [address, super.Account?.[address] ?? '<new>'], {
					[address]+: {
						nonce: super?.nonce ?? 0,
						// Leaks
						consumers: super?.consumers ?? 1,
						providers: super?.providers ?? 1,
						sufficients: super?.sufficients ?? 0,
						data+: {
							free: std.bigint(1000000000),
							reserved: super?.reserved ?? std.bigint(0),
							frozen: super?.reserved ?? std.bigint(0),
							flags: super?.flags ?? std.bigint(0),
						},
					},
				}),
			},
		},
	},
	resetAuraAuthorities: function(prev) prev {
		_storage+: {
			Aura+: {
				Authorities: [],
			},
			[if 'AuraExt' in prev._storage then 'AuraExt']+: {
				Authorities: [],
			},
		},
	},
	addAuraAuthority(key): function(prev) prev {
		_storage+: {
			Aura+: {
				Authorities+: [cql.ss58(key)],
			},
			[if 'AuraExt' in prev._storage then 'AuraExt']+: {
				Authorities+: [cql.ss58(key)],
			},
		},
	},
	resetGrandpaAuthorities: function(prev) prev {
		_storage+: {
			Grandpa+: {
				Authorities: [],
			},
		},
	},
	addGrandpaAuthority(key): function(prev) prev {
		_storage+: {
			Grandpa+: {
				Authorities+: [
					[
						cql.ss58(key),
						std.bigint(1),
					]
				],
			},
		},
	},
	resetSessionKeys: {
		_storage+: {
			Session+: {
				DisabledValidators: [],
				KeyOwner: {},
				NextKeys: {},
				QueuedKeys: [],
				Validators: [],
			},
		},
	},
	addSessionKey([_accountId, _validatorId, _keys]): {
		local accountId = cql.ss58(_accountId),
		local validatorId = cql.ss58(_validatorId),
		local keys = {
			[cql.toHex(std.encodeUTF8(key))]: cql.ss58(data),
			for [key, data] in _keys
		},
		_storage+: {
			// FIXME: Should increase consumers/providers for account
			Session+: {
				KeyOwner+: {
					[std.toString([key, data])]: validatorId,
					for [key, data] in keys
				},
				NextKeys+: {
					[validatorId]: unwrapNewtype(keys),
				},
				QueuedKeys+: [
					[
						validatorId,
						unwrapNewtype(keys),
					],
				],
				Validators+: [validatorId],
			},
		},
	},
	resetInvulnerables: function(prev) prev {
		_storage+: {
			[if 'CollatorSelection' in prev._storage then 'CollatorSelection']+: {
				Invulnerables: [],
			},
		}
	},
	addInvulnerable(key): function(prev) prev {
		_storage+: {
			[if 'CollatorSelection' in prev._storage then 'CollatorSelection']+: {
				Invulnerables+: [cql.ss58(key)],
			},
		}
	},

	setCodeRaw(code): function(prev) prev {
		genesis+: {
			raw+: {
				top+: {
					[WELLKNOWN_CODE]: code,
				},
			},
		},
	},

	// Compatible, as storage remains the same
	resetNetworking: m.resetNetworking,

	decodeSpec(): function(prev) local dump = cql.fullDump(prev.genesis.raw.top); prev {
		_originalDump:: dump,
		_storage::: dump,
		genesis+: {
			raw+: {
				top:: error "reencode storage first"
			},
		},
	},
	reencodeSpec(): function(prev) prev {
		_originalDump:: error "decode storage first",
		_storage:: error "decode storage first",
		genesis+: {
			raw+: {
				top::: prev._originalDump._rebuild(prev._storage),
			},
		},
	},

	polkaLaunchPara(root): [
		$.resetSystem,
		$.setSudo(account('//Alice')),
		$.giveBalance(account('//Alice')),
		$.resetAuraAuthorities,
		[
			$.addAuraAuthority(node.keys.aura),
			for [?, node] in root.nodes
		],
		$.resetGrandpaAuthorities,
		[
			$.addGrandpaAuthority(node.keys.gran),
			for [?, node] in root.nodes
		],
		function(prev) bdk.mixer(if 'Session' in prev._storage then [
			$.resetSessionKeys,
			[
				$.addSessionKey([
					node.keys.aura,
					node.keys.aura,
					{
						aura: node.keys.aura,
					},
				]),
				for [?, node] in root.nodes
			],
		] else [])(prev),
		$.resetInvulnerables,
		[
			$.addInvulnerable(node.keys.aura),
			for [?, node] in root.nodes
		],
	],
}
