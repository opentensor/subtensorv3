use super::*;
use frame_support::traits::Get;
use substrate_fixed::{transcendental::log2, types::I96F32};

impl<T: Config> Pallet<T> {
    /// Calculates the dynamic TAO emission for a given subnet.
    ///
    /// This function determines the three terms tao_in, alpha_in, alpha_out
    /// which are consecutively, 1) the amount of tao injected into the pool
    /// 2) the amount of alpha injected into the pool, and 3) the amount of alpha
    /// left to be distributed towards miners/validators/owners per block.
    ///
    /// # Arguments
    /// * `netuid` - The unique identifier of the subnet.
    /// * `tao_emission` - The amount of tao to distribute for this subnet.
    /// * `alpha_block_emission` - The maximum alpha emission allowed for the block.
    ///
    /// # Returns
    /// * `(u64, u64, u64)` - A tuple containing:
    ///   - `tao_in_emission`: The adjusted TAO emission always lower or equal to tao_emission
    ///   - `alpha_in_emission`: The adjusted alpha emission amount to be added into the pool.
    ///   - `alpha_out_emission`: The remaining alpha emission after adjustments to be distributed to miners/validators.
    ///
    /// The algorithm ensures that the pool injection of tao_in_emission, alpha_in_emission does not effect the pool price
    /// It also ensures that the total amount of alpha_in_emission + alpha_out_emission sum to 2 * alpha_block_emission
    /// It also ensures that 1 < alpha_out_emission < 2 * alpha_block_emission and 0 < alpha_in_emission < alpha_block_emission.
    pub fn get_dynamic_tao_emission(
        netuid: u16,
        tao_emission: u64,
        alpha_block_emission: u64,
    ) -> (u64, u64, u64) {
        // Init terms.
        let mut tao_in_emission: I96F32 = I96F32::from_num(tao_emission);
        let float_alpha_block_emission: I96F32 = I96F32::from_num(alpha_block_emission);

        // Get alpha price for subnet.
        let alpha_price: I96F32 = Self::get_alpha_price(netuid);
        log::debug!("{:?} - alpha_price: {:?}", netuid, alpha_price);

        // Get initial alpha_in
        let mut alpha_in_emission: I96F32 = I96F32::from_num(tao_emission)
            .checked_div(alpha_price)
            .unwrap_or(float_alpha_block_emission);

        // Check if we are emitting too much alpha_in
        if alpha_in_emission >= float_alpha_block_emission {
            log::debug!(
                "{:?} - alpha_in_emission: {:?} > alpha_block_emission: {:?}",
                netuid,
                alpha_in_emission,
                float_alpha_block_emission
            );

            // Scale down tao_in
            tao_in_emission = alpha_price.saturating_mul(float_alpha_block_emission);

            // Set to max alpha_block_emission
            alpha_in_emission = float_alpha_block_emission;
        }

        // Avoid rounding errors.
        if tao_in_emission < I96F32::from_num(1) || alpha_in_emission < I96F32::from_num(1) {
            alpha_in_emission = I96F32::from_num(0);
            tao_in_emission = I96F32::from_num(0);
        }

        // Set Alpha in emission.
        let alpha_out_emission = I96F32::from_num(2)
            .saturating_mul(float_alpha_block_emission)
            .saturating_sub(alpha_in_emission);

        // Log results.
        log::debug!("{:?} - tao_in_emission: {:?}", netuid, tao_in_emission);
        log::debug!("{:?} - alpha_in_emission: {:?}", netuid, alpha_in_emission);
        log::debug!(
            "{:?} - alpha_out_emission: {:?}",
            netuid,
            alpha_out_emission
        );

        // Return result.
        (
            tao_in_emission.to_num::<u64>(),
            alpha_in_emission.to_num::<u64>(),
            alpha_out_emission.to_num::<u64>(),
        )
    }

    /// Calculates the block emission based on the total issuance.
    ///
    /// This function computes the block emission by applying a logarithmic function
    /// to the total issuance of the network. The formula used takes into account
    /// the current total issuance and adjusts the emission rate accordingly to ensure
    /// a smooth issuance curve. The emission rate decreases as the total issuance increases,
    /// following a logarithmic decay.
    ///
    /// # Returns
    /// * 'Result<u64, &'static str>': The calculated block emission rate or error.
    ///
    pub fn get_block_emission() -> Result<u64, &'static str> {
        // Convert the total issuance to a fixed-point number for calculation.
        Self::get_block_emission_for_issuance(Self::get_total_issuance())
    }

    /// Returns the block emission for an issuance value.
    pub fn get_block_emission_for_issuance(issuance: u64) -> Result<u64, &'static str> {
        // Convert issuance to a float for calculations below.
        let total_issuance: I96F32 = I96F32::from_num(issuance);
        // Check to prevent division by zero when the total supply is reached
        // and creating an issuance greater than the total supply.
        if total_issuance >= I96F32::from_num(TotalSupply::<T>::get()) {
            return Ok(0);
        }
        // Calculate the logarithmic residual of the issuance against half the total supply.
        let residual: I96F32 = log2(
            I96F32::from_num(1.0)
                .checked_div(
                    I96F32::from_num(1.0)
                        .checked_sub(
                            total_issuance
                                .checked_div(
                                    I96F32::from_num(2.0)
                                        .saturating_mul(I96F32::from_num(10_500_000_000_000_000.0)),
                                )
                                .ok_or("Logarithm calculation failed")?,
                        )
                        .ok_or("Logarithm calculation failed")?,
                )
                .ok_or("Logarithm calculation failed")?,
        )
        .map_err(|_| "Logarithm calculation failed")?;
        // Floor the residual to smooth out the emission rate.
        let floored_residual: I96F32 = residual.floor();
        // Calculate the final emission rate using the floored residual.
        // Convert floored_residual to an integer
        let floored_residual_int: u64 = floored_residual.to_num::<u64>();
        // Multiply 2.0 by itself floored_residual times to calculate the power of 2.
        let mut multiplier: I96F32 = I96F32::from_num(1.0);
        for _ in 0..floored_residual_int {
            multiplier = multiplier.saturating_mul(I96F32::from_num(2.0));
        }
        let block_emission_percentage: I96F32 = I96F32::from_num(1.0).saturating_div(multiplier);
        // Calculate the actual emission based on the emission rate
        let block_emission: I96F32 = block_emission_percentage
            .saturating_mul(I96F32::from_num(DefaultBlockEmission::<T>::get()));
        // Convert to u64
        let block_emission_u64: u64 = block_emission.to_num::<u64>();
        if BlockEmission::<T>::get() != block_emission_u64 {
            BlockEmission::<T>::put(block_emission_u64);
        }
        Ok(block_emission_u64)
    }
}
