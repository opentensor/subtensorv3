pragma solidity ^0.8.0;

address constant ISTAKING_ADDRESS = 0x0000000000000000000000000000000000000805;

interface INeuron {
    function setWeights(
        uint16 netuid,
        uint16[] memory dests,
        uint16[] memory weights,
        uint64 versionKey
    ) external payable;

    function commitWeights(uint16 netuid, uint256 commitHash) external payable;

    function revealWeights(
        uint16 netuid,
        uint16[] memory uids,
        uint16[] memory values,
        uint16[] memory salt,
        uint64 versionKey
    ) external payable;
}
