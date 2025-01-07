#!/bin/bash

: "${BUILD_BINARY:=1}"
# : "${FEATURES:=pow-faucet}"

FULL_PATH=".baedeker/.bdk-env/specs/subtensor.json"

if [[ $BUILD_BINARY == "1" ]]; then
	echo "*** Building substrate binary..."
	# cargo build --release --features "$FEATURES"
	cargo build --release
	echo "*** Binary compiled"
fi

echo "*** Purging previous state..."
./target/release/node-subtensor purge-chain -y --base-path /tmp/charlie --chain="$FULL_PATH" >/dev/null 2>&1
./target/release/node-subtensor purge-chain -y --base-path /tmp/bob --chain="$FULL_PATH" >/dev/null 2>&1
./target/release/node-subtensor purge-chain -y --base-path /tmp/alice --chain="$FULL_PATH" >/dev/null 2>&1
echo "*** Previous chainstate purged"

echo "*** Starting localnet nodes..."
alice_start=(
    ./target/release/node-subtensor
    --base-path /tmp/alice
    --chain="$FULL_PATH"
    --keystore-path=./.baedeker/.bdk-env/secret/keystore-subtensor-node-alice
    --node-key-file=./.baedeker/.bdk-env/secret/node/subtensor-node-alice
    --port 30334
    --rpc-port 9946
    --validator
    --rpc-cors=all
    --rpc-external
    --unsafe-rpc-external
    --rpc-methods=unsafe
    --allow-private-ipv4
    --discover-local
)

bob_start=(
    ./target/release/node-subtensor
    --base-path /tmp/bob
    --chain="$FULL_PATH"
    --keystore-path=./.baedeker/.bdk-env/secret/keystore-subtensor-node-bob
    --node-key-file=./.baedeker/.bdk-env/secret/node/subtensor-node-bob
    --port 30335
    --rpc-port 9935
    --validator
    --allow-private-ipv4
    --discover-local
)

charlie_start=(
    ./target/release/node-subtensor
    --base-path /tmp/charlie
    --chain="$FULL_PATH"
    --keystore-path=./.baedeker/.bdk-env/secret/keystore-subtensor-node-charlie
    --node-key-file=./.baedeker/.bdk-env/secret/node/subtensor-node-charlie
    --port 30336
    --rpc-port 9936
    --validator
    --allow-private-ipv4
    --discover-local
)

(trap 'kill 0' SIGINT; ("${alice_start[@]}" 2>&1) & ("${bob_start[@]}" 2>&1) & ("${charlie_start[@]}" 2>&1))
