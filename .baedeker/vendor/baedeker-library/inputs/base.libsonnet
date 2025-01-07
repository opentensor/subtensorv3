local
	genesisState = import '../util/genesisState.libsonnet',
	{mixinAllChains, ...} = import '../util/mixin.libsonnet',
	k = import '../mixin/keys.libsonnet',
;

function(prev, final)

// :code
local WELLKNOWN_CODE = '0x3a636f6465';

local genesisMixin = {
	// TODO: Process from wasm once native runtime free world lands.
	specJson: cql.description('<build spec for %s>' % self.path, bdk.processSpec(self.bin, self.spec)),
	genesisWasm: self.specJson.genesis.raw.top[WELLKNOWN_CODE],
	genesisWasmData: cql.runtimeWasm(self.genesisWasm),
	genesisStateVersion: self.genesisWasmData.version.state_version,
	genesisHead: genesisState(self.specJson, self.genesisStateVersion),

	ss58Format: super?.ss58Format ?? 42,
	signatureSchema: super?.signatureSchema ?? 'Sr25519',
	// FIXME: Try to guess from runtime metadata.
	// If null - try to guess the schema.
	// I.e use StashOf of pallet_staking, if staking presents in schema, and so on.
	validatorIdAssignment: super?.validatorIdAssignment ?? 'none',

	addressSeed(seed):: cql.addressSeed(self.signatureSchema, seed, self.ss58Format),
};

local mergedChains = (prev + mixinAllChains(prev, function(chain, path) genesisMixin + {
	path: path,
	nodes+: {
		[nodename]+: local hostname = '%s-node-%s' % [path, nodename]; {
			hostname: hostname,
			wantedKeys:
				if node?.wantedKeys == 'para' then k.paraWantedKeys($)
				else if node?.wantedKeys == 'para-ed' then k.paraWantedKeys($, ed = true)
				else if node?.wantedKeys == 'para-nimbus' then k.paraWantedKeys($, nimbus = true)
				else if node?.wantedKeys == 'relay' then k.relayWantedKeys($)
				else if node?.wantedKeys == 'standalone' then k.standaloneWantedKeys($)
				else if std.isObject(node?.wantedKeys) then node?.wantedKeys
				else if !('wantedKeys' in node) then {}
				else error 'Unknown wantedKeys: %s' % node?.wantedKeys,
		},
		for [nodename, node] in (chain?.nodes ?? {})
	},
}));

mergedChains + mixinAllChains(mergedChains, function(chain, path) {
	nodes+: {
		[nodename]+: bdk.ensureKeys(node.hostname, node.wantedKeys, chain.ss58Format),
		for [nodename, node] in (chain?.nodes ?? {})
	},
})
