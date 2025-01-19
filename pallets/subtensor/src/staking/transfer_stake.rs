use super::*;

impl<T: Config> Pallet<T> {
    /// Transfers stake from one coldkey to another.
    ///
    /// # Arguments
    /// * `origin` - The origin of the transaction, which must be signed by the `origin_hotkey`.
    /// * `hotkey` - The account ID of the hotkey from which the stake is being moved.
    /// * `destination_coldkey` - The account ID of the coldkey to which the stake is being moved.
    /// * `netuid` - The network ID of the subnet.
    /// * `alpha_amount` - The amount of alpha to move.
    ///
    /// # Returns
    /// * `DispatchResult` - Indicates the success or failure of the operation.
    ///
    /// # Errors
    /// This function will return an error if:
    /// * The origin is not signed by the `origin_coldkey`.
    /// * The subnet does not exist.
    /// * The `hotkey` does not exist.
    ///
    /// # Events
    /// Emits a `StakeTransferred` event upon successful completion of the stake movement.
    pub fn do_transfer_stake(
        origin: T::RuntimeOrigin,
        hotkey: T::AccountId,
        destination_coldkey: T::AccountId,
        netuid: u16,
        alpha_amount: u64,
    ) -> dispatch::DispatchResult {
        // --- 1. Check that the origin is signed by the origin_hotkey.
        let coldkey = ensure_signed(origin)?;

        // --- 2. Check that the subnet exists.
        ensure!(Self::if_subnet_exist(netuid), Error::<T>::SubnetNotExists);

        // --- 3. Check that the hotkey exists.
        ensure!(
            Self::hotkey_account_exists(&hotkey),
            Error::<T>::HotKeyAccountNotExists
        );

        // --- 4. Get the current alpha stake for the origin hotkey-coldkey pair in the origin subnet
        let origin_alpha =
            Self::get_stake_for_hotkey_and_coldkey_on_subnet(&hotkey, &coldkey, netuid);
        ensure!(
            alpha_amount <= origin_alpha,
            Error::<T>::NotEnoughStakeToWithdraw
        );

        let origin_tao: u64 = Self::swap_alpha_for_tao(netuid, alpha_amount);

        // --- 5. Unstake the amount of alpha from the origin coldkey
        Self::decrease_stake_for_hotkey_and_coldkey_on_subnet(
            &hotkey.clone(),
            &coldkey.clone(),
            netuid,
            alpha_amount,
        );

        // --- 6. Stake the amount of alpha for the destination coldkey
        Self::increase_stake_for_hotkey_and_coldkey_on_subnet(
            &hotkey.clone(),
            &destination_coldkey.clone(),
            netuid,
            alpha_amount,
        );

        // --- 7. Log the event.
        log::info!(
            "StakeTransferred( coldkey:{:?}, origin_hotkey:{:?}, origin_netuid:{:?}, destination_hotkey:{:?}, destination_netuid:{:?} )",
            coldkey.clone(),
            destination_coldkey.clone(),
            hotkey,
            netuid.clone(),
            origin_tao
        );
        Self::deposit_event(Event::StakeTransferred(
            coldkey,
            destination_coldkey,
            hotkey,
            netuid,
            origin_tao,
        ));

        // -- 8. Ok and return.
        Ok(())
    }
}
