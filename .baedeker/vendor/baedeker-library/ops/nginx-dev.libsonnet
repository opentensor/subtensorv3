
local nginx = import './nginx.libsonnet';

function(prev, nginxExposePort = 9699, nginxExposeHost = '127.0.0.1') nginx(prev) {
	_output+: {
		dockerCompose+: {
			_wellKnownBalancerUrl:: '%s:%d' % [nginxExposeHost, nginxExposePort],
			_composeConfig+: {
				services+: {
					nginx+: {
						ports+: [
							'%s:%d:80' % [nginxExposeHost, nginxExposePort]
						],
					},
				},
			},
		},
	},
}
