# Running local network with full production state

## Baedeker tool overview 

The Baedeker tool helps to pull the full state from a Substrate network, modify it as needed, and output it as a raw spec.

It also creates node secrets (aura/grandpa keys) and writes them into the modified raw spec's aura and grandpa state.

## Running locally

### 1. Install baedeker from source

```shell
git clone https://github.com/UniqueNetwork/baedeker.git && cd baedeker
cargo build --release
sudo cp ./target/release/baedeker /usr/local/bin
sudo chmod +x /usr/local/bin/baedeker
```

### 2. Execute baedeker to pull the state from Finney

Note 1: The .bdk-env/specs folder should be empty, the chain spec will not be overwritten.
Note 2: Endpoint may be replaced to pull the state from another chain, e.g. from testnet

```shell
.baedeker/up.sh .baedeker/forkless-data.jsonnet --tla-str=forked_spec=subtensor --tla-str=fork_source=wss://entrypoint-finney.opentensor.ai
```

### 3. What to expect

#### Expected output example

Note: The process of pulling Finney state from a public archive node may take 10-20 minutes.

```
 INFO baedeker: evaluating config
 INFO baedeker: evaluating input config
 WARN baedeker::library: resulting spec will not work on the remote machine, impure bdk.dockerMounts() was used!
loading metadata
preloading all keys
loading keys by prefix []
loaded 1000 keys
loaded 1000 keys
...
loaded 737 keys
loaded keys, last chunk was 737
preloading 30000 keys
...
preloading 13734 keys
loading keys by prefix []
rebuilding pallet Aura
rebuilding pallet Balances
rebuilding storage Account
...
rebuilding storage Voting
rebuilding pallet TriumvirateMembers
```

#### Baedeker will provide the following output to `.bdk-env` (added to .gitignore)

  - The raw spec to `.bdk-env/specs/` folder
  - Node secrets to `.bdk-env/secret/` folder

### 4. Run the local network

```shell
./scripts/localnet-baedeker.sh
```

