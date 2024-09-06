use super::*;
// use substrate_fixed::types::I96F32;

impl<T: Config> Pallet<T> {
    /// Moves stake from one hotkey to another across subnets.
    ///
    /// # Arguments
    /// * `origin` - The origin of the transaction, which must be signed by the `origin_hotkey`.
    /// * `origin_hotkey` - The account ID of the hotkey from which the stake is being moved.
    /// * `destination_hotkey` - The account ID of the hotkey to which the stake is being moved.
    /// * `origin_netuid` - The network ID of the origin subnet.
    /// * `destination_netuid` - The network ID of the destination subnet.
    ///
    /// # Returns
    /// * `DispatchResult` - Indicates the success or failure of the operation.
    ///
    /// # Errors
    /// This function will return an error if:
    /// * The origin is not signed by the `origin_hotkey`.
    /// * Either the origin or destination subnet does not exist.
    /// * The `origin_hotkey` or `destination_hotkey` does not exist.
    /// * There are locked funds that cannot be moved across subnets.
    ///
    /// # Events
    /// Emits a `StakeMoved` event upon successful completion of the stake movement.
    pub fn do_move_stake(
        origin: T::RuntimeOrigin,
        origin_hotkey: T::AccountId,
        destination_hotkey: T::AccountId,
        origin_netuid: u16,
        destination_netuid: u16,
        amount_moved: Option<u64>,
    ) -> dispatch::DispatchResult {
        // --- 1. Check that the origin is signed by the origin_hotkey.
        let coldkey = ensure_signed(origin)?;

        // --- 2. Check that the subnet exists.
        ensure!(
            Self::if_subnet_exist(origin_netuid),
            Error::<T>::SubnetNotExists
        );
        ensure!(
            Self::if_subnet_exist(destination_netuid),
            Error::<T>::SubnetNotExists
        );

        // --- 3. Check that the origin_hotkey exists.
        ensure!(
            Self::hotkey_account_exists(&origin_hotkey),
            Error::<T>::HotKeyAccountNotExists
        );

        // --- 4. Check that the destination_hotkey exists.
        ensure!(
            Self::hotkey_account_exists(&destination_hotkey),
            Error::<T>::HotKeyAccountNotExists
        );

        // -- 5. Ensure we are not moving locked funds across subnets.
        if origin_netuid != destination_netuid {
            // You cannot move locked funds across subnets.
            ensure!(
                !Locks::<T>::contains_key((origin_netuid, origin_hotkey.clone(), coldkey.clone())),
                Error::<T>::NotEnoughStakeToWithdraw
            );
        }

        // --- 6. Get the current alpha stake for the origin hotkey-coldkey pair in the origin subnet
        // or use amount_moved
        let origin_alpha = Alpha::<T>::get((origin_hotkey.clone(), coldkey.clone(), origin_netuid));

        let move_alpha = match amount_moved {
            Some(amount) if amount <= origin_alpha => amount,
            _ => origin_alpha,
        };

        ensure!(move_alpha > 0, Error::<T>::MoveAmountCanNotBeZero);

        // --- 7. Unstake the full amount of alpha from the origin subnet, converting it to TAO
        let origin_tao = Self::unstake_from_subnet(
            &origin_hotkey.clone(),
            &coldkey.clone(),
            origin_netuid,
            move_alpha,
        );

        // --- 8. Stake the resulting TAO into the destination subnet for the destination hotkey
        Self::stake_into_subnet(
            &destination_hotkey.clone(),
            &coldkey.clone(),
            destination_netuid,
            origin_tao,
        );

        // --- 9. Swap the locks.
        if Locks::<T>::contains_key((origin_netuid, origin_hotkey.clone(), coldkey.clone())) {
            let lock_data =
                Locks::<T>::take((origin_netuid, origin_hotkey.clone(), coldkey.clone()));
            Locks::<T>::insert(
                (
                    destination_netuid,
                    destination_hotkey.clone(),
                    coldkey.clone(),
                ),
                lock_data,
            );
        }

        // --- 10. Log the event.
        log::info!(
            "StakeMoved( coldkey:{:?}, origin_hotkey:{:?}, origin_netuid:{:?}, destination_hotkey:{:?}, destination_netuid:{:?} )",
            coldkey.clone(),
            origin_hotkey.clone(),
            origin_netuid,
            destination_hotkey.clone(),
            destination_netuid
        );
        Self::deposit_event(Event::StakeMoved(
            coldkey,
            origin_hotkey,
            origin_netuid,
            destination_hotkey,
            destination_netuid,
        ));

        // -- 11. Ok and return.
        Ok(())
    }
}
