use super::*;
use substrate_fixed::types::I96F32;

impl<T: Config> Pallet<T> {
    /// Adjusts the tempos for all subnets based on their Token-Adjusted Ownership (TAO) values.
    ///
    /// This function performs the following steps:
    /// 1. Retrieves all subnet network IDs.
    /// 2. Collects the TAO values for each subnet (excluding subnet 0).
    /// 3. Calculates new tempos for each subnet using `calculate_subnet_tempos`.
    /// 4. Updates the tempo for each subnet in storage.
    ///
    /// The tempo calculation uses predefined values for average, minimum, and maximum tempos.
    /// These values ensure that the tempos stay within a reasonable range while being proportional
    /// to each subnet's TAO.
    pub fn adjust_tempos() {
        // Retrieve all subnet network IDs
        let subnets = Self::get_all_subnet_netuids()
            .into_iter()
            .filter(|&netuid| netuid != 0)
            .collect::<Vec<u16>>();

        // Collect TAO values for each subnet, excluding subnet 0
        // TAO (Token-Adjusted Ownership) represents the stake-weighted ownership of each subnet
        let subnet_tao: Vec<u64> = subnets
            .iter()
            .map(|&netuid| SubnetTAO::<T>::get(netuid))
            .collect();

        // Define tempo parameters
        let k = AvgTempo::<T>::get(); // Base tempo value
        let min_tempo = AvgTempo::<T>::get(); // Minimum allowed tempo
        let max_tempo = MaxTempo::<T>::get(); // Maximum allowed tempo

        // Calculate new tempos for each subnet based on their TAO
        let subnet_tempos = Self::calculate_tempos(k, subnet_tao, min_tempo, max_tempo);

        // Update the tempo for each subnet in storage
        subnets
            .iter()
            .zip(subnet_tempos.iter())
            .for_each(|(&netuid, &tempo)| {
                Tempo::<T>::insert(netuid, tempo);
            });

        // TODO: Consider adding logging or events to track tempo adjustments
    }

    /// Calculates the tempos for each subnet based on their TAO (Token-Adjusted Ownership) values.
    ///
    /// This function distributes the total tempo across subnets proportionally to their TAO.
    /// The calculation ensures that subnets with higher TAO receive a larger share of the total tempo.
    /// If the total subnet TAO is zero, it returns the average tempo for each subnet.
    ///
    /// # Arguments
    /// * `k` - The average tempo per subnet, used to calculate the total tempo.
    /// * `tao` - A vector of TAO values for each subnet.
    /// * `min_tempo` - The minimum allowed tempo value.
    /// * `max_tempo` - The maximum allowed tempo value.
    ///
    /// # Returns
    /// A `Vec<u16>` where each element is the calculated tempo for the corresponding subnet.
    ///
    /// # Notes
    /// - If `min_tempo` is greater than or equal to `max_tempo`, all subnets will be assigned the `min_tempo`.
    /// - The calculated tempo for each subnet is clamped between `min_tempo` and `max_tempo`.
    /// - If the total TAO across all subnets is zero, each subnet is assigned `k` (clamped between `min_tempo` and `max_tempo`).
    pub fn calculate_tempos(k: u16, tao: Vec<u64>, min_tempo: u16, max_tempo: u16) -> Vec<u16> {
        // Check for inconsistent min and max
        log::debug!(
            "Checking if min_tempo >= max_tempo: min_tempo = {}, max_tempo = {}",
            min_tempo,
            max_tempo
        );
        if min_tempo >= max_tempo {
            log::debug!(
                "min_tempo >= max_tempo, returning vec of max_tempo with length {}",
                tao.len()
            );
            return vec![max_tempo; tao.len()];
        }

        // Sum the total amount of TAO using saturating math and convert to float, ensuring no overflow occurs.
        let total_tao: I96F32 = I96F32::from_num(
            tao.iter()
                .copied()
                .fold(0u64, |acc, x| acc.saturating_add(x)),
        );
        log::debug!("Total TAO: {}", total_tao);

        // Check for zero.
        log::debug!("Checking if total_tao == 0");
        if total_tao == I96F32::from_num(0) {
            // If total TAO is zero, return average tempo for each subnet
            log::debug!("Total TAO is zero, returning average tempo for each subnet");
            let average_tempo = I96F32::from_num(k)
                .clamp(I96F32::from_num(min_tempo), I96F32::from_num(max_tempo))
                .to_num::<u16>();
            log::debug!("Average tempo: {}", average_tempo);
            return vec![average_tempo; tao.len()];
        }

        // Get the normalized tao values.
        log::debug!("Calculating normalized TAO values");
        let norm_tao: Vec<I96F32> = tao
            .iter()
            .map(|&x| {
                let normalized = I96F32::from_num(x).saturating_div(total_tao);
                log::debug!("Normalized TAO: {}", normalized);
                normalized
            })
            .collect();

        // Calculate the tempos using the harmonic mean relation
        log::debug!("Calculating tempos using harmonic mean relation");
        let tempos: Vec<I96F32> = norm_tao
            .into_iter()
            .map(|value| {
                if value > 0 {
                    let tempo = (I96F32::from_num(k).saturating_div(value))
                        .clamp(I96F32::from_num(min_tempo), I96F32::from_num(max_tempo));
                    log::debug!("Calculated tempo: {}", tempo);
                    tempo
                } else {
                    log::debug!("Value <= 0, using max_tempo: {}", max_tempo);
                    I96F32::from_num(max_tempo)
                }
            })
            .collect();

        // Convert the tempos to integers.
        log::debug!("Converting tempos to integers");
        let result = tempos
            .into_iter()
            .map(|tempo| {
                let rounded_tempo = tempo.round().to_num::<u16>();
                log::debug!("Rounded tempo: {}", rounded_tempo);
                rounded_tempo
            })
            .collect();
        log::debug!("Final tempos: {:?}", result);
        result
    }
}
