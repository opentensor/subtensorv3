#!/usr/bin/env bash

# Tries runtime upgrade (via try-runtime).
#
# Usage:
#   try-runtime-upgrade.sh [-p <runtime-path>] [-u <live-chain-url>]
#
# Dependencies:
#   - rust toolchain
#   - try-runtime-cli

set -eou pipefail

runtime_wasm_path="./target/release/wbuild/node-subtensor-runtime/node_subtensor_runtime.compact.wasm"
live_chain_url="wss://dev.chain.opentensor.ai:443"

parse_args() {
  while getopts "p:u:" opt; do
    case "${opt}" in
    p) runtime_wasm_path="${OPTARG}" ;;
    u) live_chain_url="${OPTARG}" ;;
    *) echo "Usage: $(basename "$0") [-p <runtime-path>] [-u <live-chain-url>]" && exit 1 ;;
    esac
  done
}

build_runtime() {
  cargo build -p node-subtensor-runtime --release --features "metadata-hash,try-runtime"
}

do_try_runtime() {
  try-runtime --runtime "$runtime_wasm_path" on-runtime-upgrade live --uri "$live_chain_url"
}

parse_args "$@"
build_runtime
do_try_runtime
