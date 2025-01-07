// Implementation of export-genesis-state in jsonnet, exports genesis head in format suitable for polkadot.
local t = import './meta.libsonnet';

// Basic header definition, only things required for genesis state building are included.
local types = t.metadata({
	// Although hash/block number is generic, all substrate chains use blake2_256 for hash, and u32 for number.
	// Currently, there is no way to query such metadata from the chain, and using other types are not feasible,
	// as u32 block number is enough for 136 years of block production, assuming 1 block per second.
	header: t.s({
		parent_hash: $.hash,
		number: $.number,
		state_root: $.hash,
		extrinsic_root: $.hash,
		digest: $.digest,
	}),

	digest: t.s({
		logs: $.vecstub,
	}),
	vecu8: t.v($.u8),
	hash: t.a($.u8, 32),
	number: t.c($.u32),

	u8: t.p('u8'),
	u32: t.p('u32'),

	// It is impossible to initialize stub type, as it is recursive with no way to stop recursion.
	vecstub: t.v($.stub),
	stub: t.s({
		__doNotTryToInitialize__: $.stub,
		// chainql automatically unwraps newtype structs, this field will make stub struct not newtype.
		_: $.stub,
	}),
});

local storageRoot(storage, stateVersion) =
	cql.blake2_256Root(storage.top + {
		[key]: cql.blake2_256Root(tree, stateVersion),
		for [key, tree] in storage.childrenDefault
	}, stateVersion);

function(spec, stateVersion)
assert spec.genesis.raw != {}: 'not a raw spec!';

types._encode(0, {
	parent_hash: '0x' + '00' * 32,
	number: 0,
	state_root: storageRoot(spec.genesis.raw, stateVersion),
	extrinsic_root: cql.blake2_256Root({}, stateVersion),
	digest: [],
})
