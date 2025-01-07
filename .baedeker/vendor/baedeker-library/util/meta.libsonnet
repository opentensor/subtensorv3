// json-encoded (chainql-flavored) runtime metadata builder

local def(t, v) = {
	type: {
		def: {
			[t]: v,
		},
	},
};

{
	types(o): std.objectValues(o + {
		[name]+: {id: id},
		for [id, name] in std.mapWithIndex(function(i, v) [i, v], std.objectFieldsEx(o, false, preserve_order = true))
	}),
	metadata(o): cql.dump({
		types: {
			types: $.types(o),
		},
		pallets: [],
		// Required, but shouldn't be used by callers
		extrinsic: {ty: 0, version: 0, signed_extensions: []},
		ty: 0,
	}, {}),

	// Primitive type
	p(n): def('primitive', n),
	// Vec<t>
	v(t): def('sequence', {
		type: t.id,
	}),
	// struct, with value types specified in f
	s(f): def('composite', {
		fields: [
			{
				name: key,
				type: value.id,
			},
			for {key, value} in std.objectKeysValues(f, preserve_order = true)
		],
	}),
	// [t; s]
	a(t, s): def('array', {
		len: s,
		type: t.id,
	}),
	// Compact<t>
	c(t): def('compact', {
		type: t.id,
	}),
}
