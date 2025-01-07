local {flattenNodes, flattenChains, ...} = import '../util/mixin.libsonnet';

function(prev, final)
prev + {
	_output+:: {
		dockerCompose+: {
			_wellKnownBalancerUrl:: super?._wellKnownBalancerUrl ?? 'BALANCER_URL',
		},
		dockerComposeDiscover+: local 
			balancerUrl = final._output.dockerCompose._wellKnownBalancerUrl,
		; std.join('\n', [
			'BDK_BALANCER=http://%s/' % balancerUrl,
		] + [
			'%s_ID=%i' % [std.strReplace(std.asciiUpper(chain.path), '-', '_'), chain.paraId]
			for chain in flattenChains(prev)
			if 'paraId' in chain
		] + [
			'%s_HTTP_URL=http://%s/%s/' % [std.strReplace(std.asciiUpper(chain.path), '-', '_'), balancerUrl, chain.path]
			for chain in flattenChains(prev)
		] + [
			'%s_URL=ws://%s/%s/' % [std.strReplace(std.asciiUpper(chain.path), '-', '_'), balancerUrl, chain.path]
			for chain in flattenChains(prev)
		] + [
			'%s_STASH=%s' % [std.strReplace(std.asciiUpper(node.hostname), '-', '_'), node.wallets.stash]
			for chain in flattenChains(prev)
			if 'paraId' in chain
			for node in flattenNodes(chain)
		] + ['']),
	},
}
