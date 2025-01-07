local {mixinRolloutNodes, ...} = import '../util/mixin.libsonnet';

{
	rewriteNodePaths(paths, for_nodes = true, for_chain = true, percent = 1, leave = null, extra_node_mixin = {}, extra_chain_mixin = {}):
	local mkBin(obj, node) = if 'bin' in obj && std.isString(obj.bin) && obj.bin in paths then ({
		bin: paths[obj.bin],
	} + if node then extra_node_mixin else extra_chain_mixin) else {};
	function(prev) prev + mixinRolloutNodes(prev,
		function(node) if for_nodes then mkBin(node, true) else {},
		function(chain) if for_chain then mkBin(chain, false) else {},
		percent = percent, leave = leave
	)
}
