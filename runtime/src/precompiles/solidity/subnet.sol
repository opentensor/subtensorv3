pragma solidity ^0.8.0;

address constant ISTAKING_ADDRESS = 0x0000000000000000000000000000000000000803;

interface ISubnet {
  /// Registers a new network without specifying details.
  function registerNetwork() external payable;
  /// Registers a new network with specified subnet name, GitHub repository, and contact information.
  function registerNetwork(bytes memory subnetName, bytes memory githubRepo, bytes memory subnetContact) external payable;
}