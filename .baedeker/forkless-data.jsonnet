local
m = import 'baedeker-library/mixin/spec.libsonnet',
rm = import 'baedeker-library/mixin/raw-spec.libsonnet',
;

function(relay_spec, forked_spec, fork_source)

local relay = {
	name: 'subtensor',
	bin: 'bin/subtensor',
	spec: {Raw:{
		local modifyRaw = bdk.mixer([
			rm.resetNetworking($),
			rm.decodeSpec(),
			rm.polkaLaunchPara($),
			rm.reencodeSpec(),
		]),
		raw_spec: modifyRaw({
			name: "Bittensor",
			id: "%s_local" % forked_spec,
			chainType: "Development",
			codeSubstitutes: {},
			properties: {
				ss58Format: 42,
				tokenDecimals: 9,
				tokenSymbol: "TAO"
			},
			genesis: {
				raw: {
					top: cql.chain(fork_source).latest._preloadKeys._raw,
					childrenDefault: {},
				},
			},
		}),
	}},
	nodes: {
		[name]: {
			bin: $.bin,
			wantedKeys: 'standalone',
		},
		for name in ['alice', 'bob', 'charlie']
	},
};

relay + {
}
