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
# Get the value of fast_blocks from the first argument
raonet=${1:-"True"}

# Check the value of fast_blocks
if [ "$raonet" == "False" ]; then
  # Block of code to execute if fast_blocks is False
  echo "raonet is Off"
  : "${CHAIN:=raonet}"
  : "${BUILD_BINARY:=1}"
  : "${FEATURES:="raonet"}"
else
  # Block of code to execute if fast_blocks is not False
  echo "raonet is On"
  : "${CHAIN:=raonet}"
  : "${BUILD_BINARY:=1}"
  : "${FEATURES:="raonet"}"
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
  cargo build --workspace --profile production --features "$FEATURES" --manifest-path "$BASE_DIR/Cargo.toml"
  echo "*** Binary compiled"
fi

echo "*** Building chainspec..."
"$BASE_DIR/target/production/node-subtensor" build-spec --disable-default-bootnode --raw --chain $CHAIN >$FULL_PATH
echo "*** Chainspec built and output to file"

if [ $NO_PURGE -eq 1 ]; then
  echo "*** Purging previous state skipped..."
else
  echo "*** Purging previous state..."
  "$BASE_DIR/target/production/node-subtensor" purge-chain -y --base-path /tmp/bob --chain="$FULL_PATH" >/dev/null 2>&1
  "$BASE_DIR/target/production/node-subtensor" purge-chain -y --base-path /tmp/alice --chain="$FULL_PATH" >/dev/null 2>&1
  echo "*** Previous chainstate purged"
fi

echo "*** Starting raonet nodes..."
alice_start=(
  "$BASE_DIR/target/production/node-subtensor"
  --base-path /tmp/alice
  --chain="$FULL_PATH"
  --alice
  --port 30334
  --rpc-port 9944
  --validator
  --rpc-cors=all
  --allow-private-ipv4
  --discover-local
  --unsafe-rpc-external
  --rpc-methods=unsafe
  --unsafe-force-node-key-generation
)

bob_start=(
  "$BASE_DIR"/target/production/node-subtensor
  --base-path /tmp/bob
  --chain="$FULL_PATH"
  --bob
  --port 30335
  --rpc-port 9945
  --validator
  --allow-private-ipv4
  --discover-local
  --unsafe-force-node-key-generation
)

trap 'pkill -P $$' EXIT SIGINT SIGTERM

(
  ("${alice_start[@]}" 2>&1) &
  ("${bob_start[@]}" 2>&1)
  wait
)
