local dotenv = {
    [std.splitLimit(line, "=", 2)[0]]: std.splitLimit(line, "=", 2)[1]
    for line in std.split(importstr "../.env", "\n")
    if line != ""
    if std.member(line, "=")
};

function(prev, repoDir)
(import 'baedeker-library/ops/rewrites.libsonnet').rewriteNodePaths({
	'bin/subtensor':'%s/target/release/subtensor' % repoDir,
})(prev)
