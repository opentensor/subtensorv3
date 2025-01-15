pragma solidity ^0.8.0;

address constant ISTAKING_ADDRESS = 0x0000000000000000000000000000000000000801;

interface IStaking {
  /**
   * @dev Adds a subtensor stake corresponding to the value sent with the transaction, associated
   * with the `hotkey`.
   *
   * This function allows external accounts and contracts to stake TAO into the subtensor pallet,
   * which effectively calls `add_stake` on the subtensor pallet with specified hotkey as a parameter
   * and coldkey being the hashed address mapping of H160 sender address to Substrate ss58 address as
   * implemented in Frontier HashedAddressMapping:
   * https://github.com/polkadot-evm/frontier/blob/2e219e17a526125da003e64ef22ec037917083fa/frame/evm/src/lib.rs#L739
   *
   * @param hotkey The hotkey public key (32 bytes).
   * @param netuid The subnet to stake to (uint256).
   *
   * Requirements:
   * - `hotkey` must be a valid hotkey registered on the network, ensuring that the stake is
   *   correctly attributed.
   */
  function addStake(bytes32 hotkey, uint256 netuid) external payable;

  /**
   * @dev Removes a subtensor stake `amount` from the specified `hotkey`.
   *
   * This function allows external accounts and contracts to unstake TAO from the subtensor pallet,
   * which effectively calls `remove_stake` on the subtensor pallet with specified hotkey as a parameter
   * and coldkey being the hashed address mapping of H160 sender address to Substrate ss58 address as
   * implemented in Frontier HashedAddressMapping:
   * https://github.com/polkadot-evm/frontier/blob/2e219e17a526125da003e64ef22ec037917083fa/frame/evm/src/lib.rs#L739
   *
   * @param hotkey The hotkey public key (32 bytes).
   * @param amount The amount to unstake in rao.
   * @param netuid The subnet to stake to (uint256).
   *
   * Requirements:
   * - `hotkey` must be a valid hotkey registered on the network, ensuring that the stake is
   *   correctly attributed.
   * - The existing stake amount must be not lower than specified amount
   */
  function removeStake(bytes32 hotkey, uint256 amount, uint16 netuid) external;

  /**
   * @dev Returns the amount of RAO staked by the coldkey.
   *
   * This function allows external accounts and contracts to query the amount of RAO staked by the coldkey
   * which effectively calls `get_coldkey_stake` on the subtensor pallet with
   * specified coldkey as a parameter.
   *
   * @param coldkey The coldkey public key (32 bytes).
   * @return The amount of RAO staked by the coldkey.
   */
  function getStakeColdkey(bytes32 coldkey) external view returns (uint256);

  /**
  * @dev Returns the stake amount associated with the specified `hotkey` and `coldkey`.
  *
  * This function retrieves the current stake amount linked to a specific hotkey and coldkey pair.
  * It is a view function, meaning it does not modify the state of the contract and is free to call.
  *
  * @param hotkey The hotkey public key (32 bytes).
  * @param coldkey The coldkey public key (32 bytes).
  * @param netuid The subnet the stake is on (uint256).
  * @return The current stake amount in uint256 format.
  */
  function getStake(bytes32 hotkey, bytes32 coldkey, uint256 netuid) external view returns (uint256);
}
