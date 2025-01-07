local {flattenChains, flattenNodes, ...} = import '../util/mixin.libsonnet';

function(prev, final)

local v = {
	bind(source, target, read_only = true): {
		type: 'bind',
		source: source,
		target: target,
		read_only: read_only,
	},
	volume(name, target, nocopy = true): {
		type: 'volume',
		source: name,
		target: target,
		volume: {
			nocopy: nocopy,
		},
	},
	tmpfs(target): {
		type: 'tmpfs',
		target: target,
	},
};

local
hostMounts = bdk.dockerMounts(),
hostVolumes = [
	v.tmpfs('/tmp'),
] + [
	v.bind('/%s' % path, '/%s' % path),
	for path in hostMounts
];

local binToObj(bin, config) =
if std.isString(bin) then {
	image: config.emptyImage,
	entrypoint: bin,
	dockerBased:: false,
	volumes: hostVolumes,
} else if 'dockerImage' in bin then {
	image: bin.dockerImage,
	[if 'docker' in bin then 'entrypoint']: bin.docker,
	dockerBased:: true,
} else {
	image: config.emptyImage,
	entrypoint: bin['local'],
	dockerBased:: false,
	volumes: hostVolumes,
};

local WELLKNOWN_CODE = '0x3a636f6465';
local metadataFromKeys(keys) = cql.runtimeWasm(keys[WELLKNOWN_CODE]).metadata;

// TODO: Show diff
local diffRaw(old, new) = local
oldKeys = std.objectFields(old), newKeys = std.objectFields(new),
oldMetadata = metadataFromKeys(old), newMetadata = metadataFromKeys(new),
fancyDump(meta, data) = std.manifestJson(cql.dump(meta, data, {omit_empty: true, include_defaults: false})),
;
'removed data:\n' +
fancyDump(oldMetadata, {
	[key]: old[key],
	for key in std.setDiff(oldKeys, newKeys)
}) +
'\n\nadded data:\n' +
fancyDump(newMetadata, {
	[key]: new[key],
	for key in std.setDiff(newKeys, oldKeys)
}) +
'\n\nupdated, old:\n' +
fancyDump(oldMetadata, {
	[key]: old[key],
	for key in std.setInter(oldKeys, newKeys)
	if old[key] != new[key]
}) +
'\n\nupdated, new:\n' +
fancyDump(newMetadata, {
	[key]: new[key],
	for key in std.setInter(oldKeys, newKeys)
	if old[key] != new[key]
});

local assertEqualSpecsReconciler(_old, _new) = local old = std.parseJson(_old), new = std.parseJson(_new);
if old.genesis.raw.top != new.genesis.raw.top then error 'reconcilation disabled, and genesis is not equal:\n' + diffRaw(old.genesis.raw.top, new.genesis.raw.top) else _new;

{
	_output:: {
		dockerCompose+: {
			_config+:: {
				emptyImage: error 'missing empty image',
				outputRoot: error 'missing output root',
			},
		},
	},
} + prev + {
	_output+: {
		dockerCompose+: {
			['specs/%s.json' % chain.path]: std.manifestJsonEx(chain.specJson, '   ', preserve_order = true) + '\n',
			for chain in flattenChains(final)
		} + {
			['reconcile_specs/%s.json' % chain.path]:: assertEqualSpecsReconciler,
			for chain in flattenChains(final)
		} + {
			local config = self._config,
			_composeConfig+:: {
				version: '3.4',
				services+: {
					[node.hostname]: binToObj(node.bin, config) + {
						command: [
							'--name=%s' % node.hostname,
							'--validator',
							'--base-path=%s' % node?.expectedDataPath ?? '/chaindata',
							'--chain=/chain-spec.json',
							'--keystore-path=/keystore',
							'--node-key-file=/node-key',
							'--no-mdns',
							// Removed in new versions of substrate, will not escape docker host network anyways
							// '--no-private-ipv4',
							'--detailed-log-output',
							'--execution=wasm',
							'--unsafe-rpc-external',
							'--rpc-cors=all',
						] + (if node?.legacyRpc ?? false then [
							'--rpc-port=9933',
							'--ws-port=9944',
							'--unsafe-ws-external',
						] else [
							'--rpc-port=9944',
						]) + (node?.extraArgs ?? []) + (if node._parentChain != null /*&& node.parentConnection == "internal"*/ then ([
							'--',
							'--base-path=/chaindata-parent',
							'--chain=/chain-spec-parent.json',
							'--execution=wasm',
						] + (if node?.legacyRpc ?? false then [
							'--rpc-port=9833',
							'--ws-port=9844',
						] else [
							'--rpc-port=9844'
						]) + (node?.extraArgsInternalParent ?? [])) else []),
						[if 'rpcPort' in node || 'extraPorts' in node then 'ports']: (if 'rpcPort' in node then [
							'%s:9944' % node.rpcPort,
						] else []) + (node?.extraPorts ?? []),
						// TODO: nocopy may cause problems if this directory is already used in container,
						// but it is also helps with containers, which are run by unprivileged account.
						// Should there be init container, which issues correct chown+chmod?
						volumes+: [
							v.bind(bdk.toRelative(config.outputRoot, node.localKeystoreDir), '/keystore'),
							v.bind(bdk.toRelative(config.outputRoot, node.localNodeFile), '/node-key'),
							v.bind('specs/%s.json' % node._chain.path, '/chain-spec.json'),
							v.volume('chaindata-%s' % node.hostname, node?.expectedDataPath ?? '/chaindata', nocopy = false),
						] + (if node._parentChain != null /*&& node.parentConnection == "internal"*/ then [
							v.bind('specs/%s.json' % node._parentChain.path, '/chain-spec-parent.json'),
							v.volume('chaindata-%s-parent' % node.hostname, '/chaindata-parent', nocopy = false),
						] else []),
					} + (node?.extraCompose ?? {}),
					for node in flattenNodes(final)
				},
				networks: {
					chainnet: {
						driver: 'bridge',
					},
				},
				volumes: {
					['chaindata-%s' % node.hostname]: null,
					for node in flattenNodes(final)
				} + {
					['chaindata-%s-parent' % node.hostname]: null,
					for node in flattenNodes(final)
					if node._parentChain != null
					// if node.parentConnection == "internal"
				},
			},
			'docker-compose.yml': std.manifestYamlDoc(self._composeConfig, quote_keys = false, preserve_order = true) + '\n',
		},
	},
}
