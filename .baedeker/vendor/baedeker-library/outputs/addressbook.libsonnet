local {flattenNodes, ...} = import '../util/mixin.libsonnet';

function(prev) {
	_output+: {
		addressbook: 'Copy the following snippet to browser console on polkadot apps:\n' + std.join('\n', [
			'',
			'// Optional: do not execute if you have something important saved in polkadot apps!',
			'// localStorage.clear();'
		] + [
			'localStorage["address:%s"] = JSON.stringify(%s);' % [cql.ss58(wallet), {
				address: wallet,
				meta: {
					name: "%s (%s)" % [node.hostname, walletname]
				},
			}],
			for node in flattenNodes(prev)
			for [walletname, wallet] in ([
				[walletname, wallet]
				for [walletname, wallet] in node.wallets
				if walletname == 'stash'
			] + [
				[walletname, wallet]
				for [walletname, wallet] in node.keys
				if walletname == 'aura' || walletname == 'babe'
			])
		]),
	},
}
