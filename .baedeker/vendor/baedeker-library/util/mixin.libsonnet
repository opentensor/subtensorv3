{
	mixinAllChains(chain, mixin, path = chain?.name ?? 'relay'): mixin(chain, path = path) + {
		parachains+: {
			[paraname]+: $.mixinAllChains(para, mixin, path = "%s-%s" % [path, paraname])
			for [paraname, para] in (chain?.parachains ?? {})
		},
	},
	mixinAllNodes(chain, mixin, mixinChain = function(v) {}): $.mixinAllChains(chain, function(chain, path) {
		nodes+: {
			[nodename]+: mixin(node),
			for [nodename, node] in chain?.nodes
		},
	} + mixinChain(chain)),
	mixinRolloutNodes(chain, mixin, mixinChain = function(v) {}, percent = 1, leave = null): $.mixinAllChains(chain, function(chain, path) {
		nodes+: local length = std.length(chain?.nodes ?? {}); {
			[nodename]+: if ((i + 1) / length <= percent) && (leave == null || i < length - leave) then mixin(node)
			else {}
			for [i, {key: nodename, value: node}] in std.mapWithIndex(function(i, v) [i, v], std.objectKeysValues(chain?.nodes))
		},
	} + mixinChain(chain)),
	flattenNodes(chain, parent = null): std.join([], [
		[
			node + {
				_chain:: chain,
				_parentChain:: parent,
			},
			for [?, node] in (chain?.nodes ?? {})
		],
	] + [
		$.flattenNodes(para, chain),
		for [?, para] in (chain?.parachains ?? {})
	]),
	flattenChains(chain): std.join([], [
		[
			chain,
		],
	] + [
		$.flattenChains(para),
		for [?, para] in (chain?.parachains ?? {})
	]),
}
