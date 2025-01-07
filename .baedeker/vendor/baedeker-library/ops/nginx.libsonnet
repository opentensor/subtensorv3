local {flattenChains, flattenNodes, ...} = import '../util/mixin.libsonnet';

function(prev)

prev {
	_output+: {
		dockerCompose+: {
			local locations = self._nginxLocations,
			local dependencies = self._nginxDependencies,
			local composeFiles = self,
			_nginxDependencies+:: [
				node.hostname
				for node in flattenNodes(prev)
			],
			_nginxLocations+:: [
				local shared = {
					name: chain.path,
				};
				std.join('\n', [
					'location /%(name)s/ { try_files /nonexistent @%(name)s-$http_upgrade; }' % shared,
					'location @%(name)s-websocket {' % shared,
					'\tproxy_pass http://%(name)s-websocket;' % shared,
					'\tproxy_http_version 1.1;',
					'\tproxy_set_header Upgrade "websocket";',
					'\tproxy_set_header Connection "upgrade";',
					'}',
					'location @%(name)s- {' % shared,
					'\tproxy_pass http://%(name)s-http;' % shared,
					'}',
				]),
				for chain in flattenChains(prev)
			],
			local configStr = std.join('\n\n', [
				local shared = {
					name: chain.path,
				};
				std.join('\n', [
					'upstream %(name)s-websocket {' % shared,
					'\tip_hash;',
					std.join('\n', [
						'\tserver %s:9944;' % node.hostname
						for [?, node] in (chain?.nodes ?? {})
					]),
					'}',
					'upstream %(name)s-http {' % shared,
					'\tip_hash;',
					std.join('\n', [
						'\tserver %s:9944;' % node.hostname
						for [?, node] in (chain?.nodes ?? {})
						if !(node?.legacyRpc ?? false)
					] + [
						'\tserver %s:9933;' % node.hostname
						for [?, node] in (chain?.nodes ?? {})
						if (node?.legacyRpc ?? false)
					]),
					'}',
				]),
				for chain in flattenChains(prev)
			] + ['server {', 'listen 80;', 'add_header Access-Control-Allow-Origin *;'] + [
				std.join('\n', locations),
			] + ['}']),
			'ops/nginx.conf': configStr,
			_composeConfig+:: {
				services+: {
					nginx: {
						image: 'nginx:latest@sha256:48a84a0728cab8ac558f48796f901f6d31d287101bc8b317683678125e0d2d35',
						volumes+: [
							{
								type: 'bind',
								source: 'ops/nginx.conf',
								target: '/etc/nginx/conf.d/default.conf',
								read_only: true,
							},
							// Introduce arbitrary dependency on config hash to force container restart when it changes
							{
								type: 'bind',
								source: 'ops/nginx.conf',
								target: '/config/%s%s' % [
									std.md5(configStr),
									std.md5(composeFiles?.['ops/index.html'] ?? ''),
								],
								read_only: true,
							},
						] + (if 'ops/index.html' in composeFiles then [
							{
								type: 'bind',
								source: 'ops/index.html',
								target: '/etc/nginx/html/index.html',
								read_only: true,
							},
						] else []),
						depends_on: dependencies,
					},
				},
			},
		},
	},
}
