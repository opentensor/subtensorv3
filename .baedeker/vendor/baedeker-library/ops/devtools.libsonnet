local {flattenChains, flattenNodes, ...} = import '../util/mixin.libsonnet';

function(prev) prev {
	_output+: {
		dockerCompose+: {
			_nginxLocations+:: [
				'location /apps/ { proxy_pass http://polkadot-apps/; }',
			],
			_nginxDependencies+:: ['polkadot-apps'],
			_composeConfig+:: {
				services+: {
					'polkadot-apps': {
						// TODO: We can provide custom endpoint list to this container using ENV. But changes to this file are needed.
						// https://github.com/polkadot-js/apps/blob/0366991f685a80147f46eb69a23285acb15bc6b7/packages/apps-config/src/endpoints/development.ts#L19
						image: 'jacogr/polkadot-js-apps:latest@sha256:b052771165a82833f68b569a74a198b09d8e1d0cce097e804cf60bc06a4faf7b',
					},
				},
			},
			// Yep, sorry for this
			'ops/index.html': std.strReplace(importstr './debug.ejs', 'DATA_JSON', std.manifestJson({
				chains: [
					{
						path: chain.path,
					},
					for chain in flattenChains(prev)
				],
			})),
		},
	},
}
