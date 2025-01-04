#!/bin/bash

# Check if `--no-purge` passed as a parameter
NO_PURGE=0
for arg in "$@"; do
  if [ "$arg" = "--no-purge" ]; then
    NO_PURGE=1
    break
  fi
done

# Determine the directory this script resides in. This allows invoking it from any location.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"

# The base directory of the subtensor project
BASE_DIR="$SCRIPT_DIR/.."

# get parameters
# Get the value of fast_runtime from the first argument
fast_runtime=${1:-"True"}

# Check the value of fast_runtime
if [ "$fast_runtime" == "False" ]; then
  # Block of code to execute if fast_runtime is False
  echo "fast_runtime is Off"
  : "${CHAIN:=local}"
  : "${BUILD_BINARY:=1}"
  : "${FEATURES:="pow-faucet"}"
else
  # Block of code to execute if fast_runtime is not False
  echo "fast_runtime is On"
  : "${CHAIN:=local}"
  : "${BUILD_BINARY:=1}"
  : "${FEATURES:="pow-faucet fast-runtime"}"
fi

SPEC_PATH="${SCRIPT_DIR}/specs/"
FULL_PATH="$SPEC_PATH$CHAIN.json"

# Kill any existing nodes which may have not exited correctly after a previous
# run.
pkill -9 'node-subtensor'

if [ ! -d "$SPEC_PATH" ]; then
  echo "*** Creating directory ${SPEC_PATH}..."
  mkdir $SPEC_PATH
fi

if [[ $BUILD_BINARY == "1" ]]; then
  echo "*** Building substrate binary..."
  cargo build --workspace --profile=release --features "$FEATURES" --manifest-path "$BASE_DIR/Cargo.toml"
  echo "*** Binary compiled"
fi

echo "*** Building chainspec..."
"$BASE_DIR/target/release/node-subtensor" build-spec --disable-default-bootnode --raw --chain $CHAIN >$FULL_PATH
echo "*** Chainspec built and output to $FULL_PATH"

# generate node keys
$BASE_DIR/target/release/node-subtensor key generate-node-key --chain="$FULL_PATH" --base-path /tmp/alice
$BASE_DIR/target/release/node-subtensor key generate-node-key --chain="$FULL_PATH" --base-path /tmp/bob

# generate node keys
$BASE_DIR/target/release/node-subtensor key generate-node-key --chain="$FULL_PATH" --base-path /tmp/alice
$BASE_DIR/target/release/node-subtensor key generate-node-key --chain="$FULL_PATH" --base-path /tmp/bob

if [ $NO_PURGE -eq 1 ]; then
  echo "*** Purging previous state skipped..."
else
  echo "*** Purging previous state..."
  "$BASE_DIR/target/release/node-subtensor" purge-chain -y --base-path /tmp/bob --chain="$FULL_PATH" >/dev/null 2>&1
  "$BASE_DIR/target/release/node-subtensor" purge-chain -y --base-path /tmp/alice --chain="$FULL_PATH" >/dev/null 2>&1
  "$BASE_DIR/target/release/node-subtensor" purge-chain -y --base-path /tmp/charlie --chain="$FULL_PATH" >/dev/null 2>&1
  "$BASE_DIR/target/release/node-subtensor" purge-chain -y --base-path /tmp/dave --chain="$FULL_PATH" >/dev/null 2>&1
  echo "*** Previous chainstate purged"
fi

echo "*** Starting localnet nodes..."
alice_start=(
  "$BASE_DIR/target/release/node-subtensor"
  --base-path /tmp/alice
  --chain="$FULL_PATH"
  --alice
  --port 30334
  --rpc-port 9944
  --validator
  --rpc-cors=all
  --allow-private-ipv4
  --discover-local
  --unsafe-force-node-key-generation
)

bob_start=(
  "$BASE_DIR"/target/release/node-subtensor
  --base-path /tmp/bob
  --chain="$FULL_PATH"
  --bob
  --port 30335
  --rpc-port 9945
  --validator
  --rpc-cors=all
  --allow-private-ipv4
  --discover-local
  --unsafe-force-node-key-generation
#  --offchain-worker=Never
)

charlie_start=(
  "$BASE_DIR"/target/release/node-subtensor
  --base-path /tmp/charlie
  --chain="$FULL_PATH"
  --charlie
  --port 30335
  --rpc-port 9944
  --validator
  --rpc-cors=all
  --allow-private-ipv4
  --discover-local
  --unsafe-force-node-key-generation
)

dave_start=(
  "$BASE_DIR"/target/release/node-subtensor
  --base-path /tmp/dave
  --chain="$FULL_PATH"
  --dave
  --port 30335
  --rpc-port 9943
  --validator
  --rpc-cors=all
  --allow-private-ipv4
  --discover-local
  --unsafe-force-node-key-generation
)

trap 'pkill -P $$' EXIT SIGINT SIGTERM

(
  # ("${alice_start[@]}" 2>&1) &
  ("${bob_start[@]}" 2>&1) &
  ("${charlie_start[@]}" 2>&1) &
  ("${dave_start[@]}" 2>&1)
  wait
)
