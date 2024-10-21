#!/bin/sh
set -ex

# Get the list of pallet directories
pallet_dirs=("subtensor" "commitments" "registry" "admin-utils")

echo "detected pallets: $pallet_dirs"

# Chain spec and output directory
chain_spec="finney"

for pallet_dir in $pallet_dirs
do
  # Use the directory name with underscores (replace hyphens with underscores) for the pallet argument
  pallet_name="pallet_$(echo "$pallet_dir" | sed 's/-/_/g')"

  # Use the original directory name for the output path
  output_pallet="$pallet_dir"

  echo "Benchmarking $pallet_name..."
  cargo run --profile=production --features=runtime-benchmarks,try-runtime,skip-broken-benchmarks \
    -p node-subtensor -- benchmark pallet \
    --chain $chain_spec \
    --wasm-execution=compiled \
    --pallet "$pallet_name" \
    --extrinsic '*' \
    --steps 1 \
    --repeat 1 \
    --output "pallets/$output_pallet/src/weights.rs" \
    --template ./.maintain/frame-weight-template.hbs
done

echo "All pallets have been benchmarked and weights updated."
