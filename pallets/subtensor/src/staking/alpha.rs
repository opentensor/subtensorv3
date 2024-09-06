use super::*;
use crate::epoch::math::*;
use frame_support::IterableStorageDoubleMap;
use substrate_fixed::types::{I32F32, I64F64, I96F32};

impl<T: Config> Pallet<T> {
    /// Retrieves the global global weight as a normalized value between 0 and 1.
    ///
    /// This function performs the following steps:
    /// 1. Fetches the global weight from storage using the GlobalWeight storage item.
    /// 2. Converts the retrieved u64 value to a fixed-point number (I96F32).
    /// 3. Normalizes the weight by dividing it by the maximum possible u64 value.
    /// 4. Returns the normalized weight as an I96F32 fixed-point number.
    ///
    /// The normalization ensures that the returned value is always between 0 and 1,
    /// regardless of the actual stored weight value.
    ///
    /// # Returns
    /// * `I96F32` - The normalized global global weight as a fixed-point number between 0 and 1.
    ///
    /// # Note
    /// This function uses saturating division to prevent potential overflow errors.
    pub fn get_global_weight() -> I96F32 {
        // Step 1: Fetch the global weight from storage
        let stored_weight = GlobalWeight::<T>::get();

        // Step 2: Convert the u64 weight to I96F32
        let weight_fixed = I96F32::from_num(stored_weight);

        // Step 3: Normalize the weight by dividing by u64::MAX
        // This ensures the result is always between 0 and 1
        weight_fixed.saturating_div(I96F32::from_num(u64::MAX))
    }

    /// Sets the global global weight in storage.
    ///
    /// This function performs the following steps:
    /// 1. Takes the provided weight value as a u64.
    /// 2. Updates the GlobalWeight storage item with the new value.
    ///
    /// # Arguments
    /// * `weight` - The new global weight value to be set, as a u64.
    ///
    /// # Effects
    /// This function modifies the following storage item:
    /// - `GlobalWeight`: Updates it with the new weight value.
    ///
    /// # Note
    /// The weight is stored as a raw u64 value. To get the normalized weight between 0 and 1,
    /// use the `get_global_weight()` function.
    pub fn set_global_weight(weight: u64) {
        // Update the GlobalWeight storage with the new weight value
        GlobalWeight::<T>::put(weight);
    }

    /// Calculates the weighted combination of alpha and global tao for hotkeys on a subnet.
    ///
    /// This function performs the following steps:
    /// 1. Retrieves the subnet size (number of neurons).
    /// 2. Fetches all hotkeys (neuron keys) on the specified subnet.
    /// 3. Calculates the alpha stake vector:
    ///    a. Initializes a vector with zeros.
    ///    b. For each hotkey, retrieves its inherited stake and stores it in the vector.
    ///    c. Normalizes the alpha stake vector.
    /// 4. Calculates the global tao stake vector:
    ///    a. Initializes a vector with zeros.
    ///    b. For each hotkey, retrieves its global stake and stores it in the vector.
    ///    c. Normalizes the global tao stake vector.
    /// 5. Combines alpha and global tao stakes:
    ///    a. Retrieves the global global weight.
    ///    b. For each neuron, calculates a weighted average of its alpha and global tao stakes.
    ///    c. Normalizes the combined stake vector.
    /// 6. Converts the combined stake vector from 64-bit to 32-bit fixed-point representation.
    /// 7. Returns the final stake weights for each neuron on the subnet.
    ///
    /// # Arguments
    /// * `netuid` - Network unique identifier specifying the subnet context for stake weight calculation.
    ///
    /// # Returns
    /// * `(Vec<I32F32>, Vec<u64>, Vec<u64>)` - A tuple containing:
    ///   - A vector of stake weights for each hotkey (neuron) on the specified subnet, represented as 32-bit fixed-point numbers.
    ///   - A vector of raw alpha stakes for each hotkey (neuron) on the specified subnet.
    ///   - A vector of raw global tao stakes for each hotkey (neuron) on the specified subnet.
    pub fn get_stake_weights_for_network(netuid: u16) -> (Vec<I32F32>, Vec<u64>, Vec<u64>) {
        // Step 1: Get the subnet size (number of neurons).
        let n: u16 = Self::get_subnetwork_n(netuid);

        // Step 2: Retrieve all hotkeys (neuron keys) on this subnet.
        let hotkeys: Vec<(u16, T::AccountId)> =
            <Keys<T> as IterableStorageDoubleMap<u16, u16, T::AccountId>>::iter_prefix(netuid)
                .collect();

        // Step 3: Calculate the alpha stake vector.
        // Initialize a vector to store alpha stakes for each neuron.
        let mut alpha_stake: Vec<I64F64> = vec![I64F64::from_num(0.0); n as usize];
        let mut raw_alpha_stake: Vec<u64> = vec![0; n as usize];
        for (uid_i, hotkey) in &hotkeys {
            let alpha: u64 = Self::get_inherited_alpha_for_hotkey_on_subnet(hotkey, netuid);

            if let Some(stake) = alpha_stake.get_mut(*uid_i as usize) {
                *stake = I64F64::from_num(alpha);
            } else {
                log::error!("UID {} is out of bounds for alpha_stake", uid_i);
            }

            if let Some(stake) = raw_alpha_stake.get_mut(*uid_i as usize) {
                *stake = alpha;
            } else {
                log::error!("UID {} is out of bounds for raw_alpha_stake", uid_i);
            }
        }
        // Normalize the alpha stake vector.
        inplace_normalize_64(&mut alpha_stake);

        // Step 4: Calculate the global tao stake vector.
        // Initialize a vector to store global tao stakes for each neuron.
        let mut global_tao_stake: Vec<I64F64> = vec![I64F64::from_num(0.0); n as usize];
        let mut raw_global_tao_stake: Vec<u64> = vec![0; n as usize];
        for (uid_i, hotkey) in &hotkeys {
            let global: u64 = Self::get_inherited_global_for_hotkey_on_subnet(hotkey, netuid);
            // global_tao_stake[*uid_i as usize] = I64F64::from_num(global);

            if let Some(stake) = global_tao_stake.get_mut(*uid_i as usize) {
                *stake = I64F64::from_num(global);
            } else {
                // Handle the case where uid_i is out of bounds
                log::error!(
                    "UID {} is out of bounds for global_tao_stake, size of subnetworks as {}",
                    uid_i,
                    n
                );
            }

            // raw_global_tao_stake[*uid_i as usize] = global;

            if let Some(stake) = raw_global_tao_stake.get_mut(*uid_i as usize) {
                *stake = global;
            } else {
                // Handle the case where uid_i is out of bounds
                log::error!(
                    "UID {} is out of bounds for raw_global_tao_stake, size of subnetworks as {}",
                    uid_i,
                    n
                );
            }
        }
        // Normalize the global tao stake vector.
        inplace_normalize_64(&mut global_tao_stake);

        // Step 5: Combine alpha and global tao stakes.
        // Retrieve the global global weight.
        let global_weight: I64F64 = I64F64::from_num(Self::get_global_weight());
        // Calculate the weighted average of alpha and global tao stakes for each neuron.
        let mut stake_weights: Vec<I64F64> = alpha_stake
            .iter()
            .zip(global_tao_stake.iter())
            .map(|(alpha, global)| {
                // Weighted average: (1 - global_weight) * alpha + global_weight * global
                (I64F64::from_num(1.0).saturating_sub(global_weight))
                    .saturating_mul(*alpha)
                    .saturating_add(global_weight.saturating_mul(*global))
            })
            .collect();
        // Normalize the combined stake weights.
        inplace_normalize_64(&mut stake_weights); // no need to normalize

        // Step 6: Convert the combined stake values from 64-bit to 32-bit fixed-point representation.
        let stake_weights_32 = vec_fixed64_to_fixed32(stake_weights);

        (stake_weights_32, raw_alpha_stake, raw_global_tao_stake)
    }

    /// Calculates the total global stake held by a hotkey on a subnet, considering child/parent relationships.
    ///
    /// This function performs the following steps:
    /// 1. Retrieves the initial global global stake for the hotkey.
    /// 2. Retrieves the list of children and parents for the hotkey on the subnet.
    /// 3. Calculates the global stake allocated to children:
    ///    a. For each child, computes the proportion of stake to be allocated.
    ///    b. Accumulates the total stake allocated to all children.
    /// 4. Calculates the global stake received from parents:
    ///    a. For each parent, retrieves the parent's global stake.
    ///    b. Computes the proportion of the parent's stake to be inherited.
    ///    c. Accumulates the total stake inherited from all parents.
    /// 5. Computes the final global stake by adjusting the initial stake:
    ///    a. Subtracts the stake allocated to children.
    ///    b. Adds the stake inherited from parents.
    /// 6. Returns the final global stake value.
    ///
    /// # Arguments
    /// * `hotkey` - AccountId of the hotkey whose total global stake is to be calculated.
    /// * `netuid` - Network unique identifier specifying the subnet context.
    ///
    /// # Returns
    /// * `u64` - The total global stake for the hotkey on the subnet after considering the stakes
    ///           allocated to children and inherited from parents.
    ///
    /// # Note
    /// This function uses saturating arithmetic to prevent overflows.
    pub fn get_inherited_global_for_hotkey_on_subnet(hotkey: &T::AccountId, netuid: u16) -> u64 {
        // Step 1: Retrieve the initial global global stake for the hotkey.
        // This represents the hotkey's stake across all subnets.
        let initial_global_tao: I96F32 = I96F32::from_num(Self::get_global_for_hotkey(hotkey));

        // Initialize variables to track stake allocated to children and inherited from parents.
        let mut global_tao_to_children: I96F32 = I96F32::from_num(0);
        let mut global_tao_from_parents: I96F32 = I96F32::from_num(0);

        // Step 2: Retrieve the lists of parents and children for the hotkey on the subnet.
        let parents: Vec<(u64, T::AccountId)> = Self::get_parents(hotkey, netuid);
        let children: Vec<(u64, T::AccountId)> = Self::get_children(hotkey, netuid);

        // Step 3: Calculate the total global stake allocated to children.
        for (proportion, _) in children {
            // Convert the proportion to a normalized value between 0 and 1.
            let normalized_proportion: I96F32 =
                I96F32::from_num(proportion).saturating_div(I96F32::from_num(u64::MAX));

            // Calculate the amount of stake to be allocated to this child.
            let global_tao_proportion_to_child: I96F32 =
                I96F32::from_num(initial_global_tao).saturating_mul(normalized_proportion);

            // Accumulate the total stake allocated to children.
            global_tao_to_children =
                global_tao_to_children.saturating_add(global_tao_proportion_to_child);
        }

        // Step 4: Calculate the total global stake received from parents.
        for (proportion, parent) in parents {
            // Retrieve the parent's global stake.
            let parent_global_tao: I96F32 = I96F32::from_num(Self::get_global_for_hotkey(&parent));

            // Convert the proportion to a normalized value between 0 and 1.
            let normalized_proportion: I96F32 =
                I96F32::from_num(proportion).saturating_div(I96F32::from_num(u64::MAX));

            // Calculate the amount of stake inherited from this parent.
            let global_tao_proportion_from_parent: I96F32 =
                I96F32::from_num(parent_global_tao).saturating_mul(normalized_proportion);

            // Accumulate the total stake inherited from parents.
            global_tao_from_parents =
                global_tao_from_parents.saturating_add(global_tao_proportion_from_parent);
        }

        // Step 5: Compute the final global stake.
        // Subtract the stake allocated to children and add the stake inherited from parents.
        let finalized_global: I96F32 = initial_global_tao
            .saturating_sub(global_tao_to_children)
            .saturating_add(global_tao_from_parents);

        // Step 6: Return the final global stake value.
        finalized_global.to_num::<u64>()
    }

    /// Calculates the total inherited stake (alpha) held by a hotkey on a network, considering child/parent relationships.
    ///
    /// This function performs the following steps:
    /// 1. Retrieves the initial alpha (stake) for the hotkey on the specified subnet.
    /// 2. Retrieves the list of children and parents for the hotkey on the subnet.
    /// 3. Calculates the alpha allocated to children:
    ///    a. For each child, computes the proportion of alpha to be allocated.
    ///    b. Accumulates the total alpha allocated to all children.
    /// 4. Calculates the alpha received from parents:
    ///    a. For each parent, retrieves the parent's stake on the subnet.
    ///    b. Computes the proportion of the parent's stake to be inherited.
    ///    c. Accumulates the total alpha inherited from all parents.
    /// 5. Computes the final inherited alpha by adjusting the initial alpha:
    ///    a. Subtracts the alpha allocated to children.
    ///    b. Adds the alpha inherited from parents.
    /// 6. Returns the final inherited alpha value.
    ///
    /// # Arguments
    /// * `hotkey` - AccountId of the hotkey whose total inherited stake is to be calculated.
    /// * `netuid` - Network unique identifier specifying the subnet context.
    ///
    /// # Returns
    /// * `u64` - The total inherited alpha for the hotkey on the subnet after considering the stakes
    ///           allocated to children and inherited from parents.
    ///
    /// # Note
    /// This function uses saturating arithmetic to prevent overflows.
    pub fn get_inherited_alpha_for_hotkey_on_subnet(hotkey: &T::AccountId, netuid: u16) -> u64 {
        // Step 1: Retrieve the initial total stake (alpha) for the hotkey on the specified subnet.
        let initial_alpha: I96F32 =
            I96F32::from_num(Self::get_stake_for_hotkey_on_subnet(hotkey, netuid));

        // Initialize variables to track alpha allocated to children and inherited from parents.
        let mut alpha_to_children: I96F32 = I96F32::from_num(0);
        let mut alpha_from_parents: I96F32 = I96F32::from_num(0);

        // Step 2: Retrieve the lists of parents and children for the hotkey on the subnet.
        let parents: Vec<(u64, T::AccountId)> = Self::get_parents(hotkey, netuid);
        let children: Vec<(u64, T::AccountId)> = Self::get_children(hotkey, netuid);

        // Step 3: Calculate the total alpha allocated to children.
        for (proportion, _) in children {
            // Convert the proportion to a normalized value between 0 and 1.
            let normalized_proportion: I96F32 =
                I96F32::from_num(proportion).saturating_div(I96F32::from_num(u64::MAX));

            // Calculate the amount of alpha to be allocated to this child.
            let alpha_proportion_to_child: I96F32 =
                I96F32::from_num(initial_alpha).saturating_mul(normalized_proportion);

            // Add this child's allocation to the total alpha allocated to children.
            alpha_to_children = alpha_to_children.saturating_add(alpha_proportion_to_child);
        }

        // Step 4: Calculate the total alpha inherited from parents.
        for (proportion, parent) in parents {
            // Retrieve the parent's total stake on this subnet.
            let parent_alpha: I96F32 =
                I96F32::from_num(Self::get_stake_for_hotkey_on_subnet(&parent, netuid));

            // Convert the proportion to a normalized value between 0 and 1.
            let normalized_proportion: I96F32 =
                I96F32::from_num(proportion).saturating_div(I96F32::from_num(u64::MAX));

            // Calculate the amount of alpha to be inherited from this parent.
            let alpha_proportion_from_parent: I96F32 =
                I96F32::from_num(parent_alpha).saturating_mul(normalized_proportion);

            // Add this parent's contribution to the total alpha inherited from parents.
            alpha_from_parents = alpha_from_parents.saturating_add(alpha_proportion_from_parent);
        }

        // Step 5: Calculate the final inherited alpha for the hotkey.
        let finalized_alpha: I96F32 = initial_alpha
            .saturating_sub(alpha_to_children) // Subtract alpha allocated to children
            .saturating_add(alpha_from_parents); // Add alpha inherited from parents

        // Step 6: Return the final inherited alpha value.
        finalized_alpha.to_num::<u64>()
    }

    /// Retrieves the global value (TAO equivalent) for a given hotkey and coldkey pair across all subnets.
    ///
    /// This function performs the following steps:
    /// 1. Initializes a variable to accumulate the total TAO equivalent.
    /// 2. Iterates over all subnet network IDs (netuids).
    /// 3. For each subnet:
    ///    a. Calculates the global value for the hotkey and coldkey pair on that subnet.
    ///    b. Adds this value to the total TAO equivalent.
    /// 4. Converts the accumulated fixed-point total to a u64 value.
    /// 5. Returns the final total TAO equivalent.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey (neuron).
    /// * `coldkey` - The account ID of the coldkey (owner).
    ///
    /// # Returns
    /// * `u64` - The total global value (TAO equivalent) for the hotkey and coldkey pair across all subnets.
    pub fn get_global_for_hotkey_and_coldkey(hotkey: &T::AccountId, coldkey: &T::AccountId) -> u64 {
        // Initialize the total TAO equivalent to zero using fixed-point arithmetic for precision
        let mut total_tao_equivalent: I96F32 = I96F32::from_num(0);

        // Iterate over all subnet network IDs (netuids)
        for netuid in Self::get_all_subnet_netuids() {
            // Calculate the global value for the hotkey and coldkey pair on this subnet
            let subnet_global =
                Self::get_global_for_hotkey_and_coldey_on_subnet(hotkey, coldkey, netuid);

            // Add the subnet's global value to the total, using saturating addition to prevent overflow
            total_tao_equivalent =
                total_tao_equivalent.saturating_add(I96F32::from_num(subnet_global));
        }

        // Convert the total TAO equivalent from fixed-point to u64 and return
        total_tao_equivalent.to_num::<u64>()
    }

    /// Retrieves the global value (TAO equivalent) for a given hotkey across all subnets.
    ///
    /// This function performs the following steps:
    /// 1. Initializes a variable to accumulate the total TAO equivalent.
    /// 2. Iterates over all subnet network IDs (netuids).
    /// 3. For each subnet:
    ///    a. Calculates the global value for the hotkey on that subnet.
    ///    b. Adds this value to the total TAO equivalent.
    /// 4. Converts the accumulated fixed-point total to a u64 value.
    /// 5. Returns the final total TAO equivalent.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey (neuron).
    ///
    /// # Returns
    /// * `u64` - The total global value (TAO equivalent) for the hotkey across all subnets.
    pub fn get_global_for_hotkey(hotkey: &T::AccountId) -> u64 {
        // Initialize the total TAO equivalent to zero using fixed-point arithmetic for precision
        let mut total_tao_equivalent: I96F32 = I96F32::from_num(0);

        // Iterate over all subnet network IDs (netuids)
        for netuid in Self::get_all_subnet_netuids() {
            // Calculate the global value for the hotkey on this subnet
            let subnet_global = Self::get_global_for_hotkey_on_subnet(hotkey, netuid);

            // Add the subnet's global value to the total, using saturating addition to prevent overflow
            total_tao_equivalent =
                total_tao_equivalent.saturating_add(I96F32::from_num(subnet_global));
        }

        // Convert the total TAO equivalent from fixed-point to u64 and return
        total_tao_equivalent.to_num::<u64>()
    }

    /// Retrieves the global value (TAO equivalent) for a given hotkey on a specific subnet.
    ///
    /// This function performs the following steps:
    /// 1. Retrieves the total stake (alpha) for the hotkey on the specified subnet.
    /// 2. Converts the alpha value to its TAO equivalent using the subnet's current state.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey (neuron).
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `u64` - The global value (TAO equivalent) for the hotkey on the specified subnet.
    ///
    /// # Note
    /// This function considers the total stake of the hotkey across all coldkeys on the subnet.
    pub fn get_global_for_hotkey_on_subnet(hotkey: &T::AccountId, netuid: u16) -> u64 {
        // Step 1: Retrieve the total stake (alpha) for the hotkey on this subnet.
        // This includes stakes from all coldkeys associated with this hotkey.
        let alpha: u64 = Self::get_stake_for_hotkey_on_subnet(hotkey, netuid);

        // Step 2: Convert the alpha value to its TAO equivalent.
        // This conversion takes into account the current state of the subnet,
        // including the total outstanding alpha and total TAO in the subnet.
        Self::alpha_to_global(alpha, netuid)
    }

    /// Retrieves the global value (TAO equivalent) for a specific hotkey and coldkey pair on a subnet.
    ///
    /// This function performs the following steps:
    /// 1. Retrieves the stake (alpha) for the specific hotkey and coldkey pair on the subnet.
    /// 2. Converts the alpha value to its TAO equivalent using the subnet's current state.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey (neuron).
    /// * `coldkey` - The account ID of the coldkey (owner).
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `u64` - The global value (TAO equivalent) for the hotkey and coldkey pair on the specified subnet.
    ///
    /// # Note
    /// This function considers only the stake associated with the specific hotkey-coldkey pair,
    /// not the total stake of the hotkey across all coldkeys.
    pub fn get_global_for_hotkey_and_coldey_on_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
    ) -> u64 {
        // Step 1: Retrieve the stake (alpha) for the specific hotkey-coldkey pair on this subnet.
        // This value represents the stake associated only with this particular combination.
        let alpha: u64 = Alpha::<T>::get((hotkey, coldkey, netuid));

        // Step 2: Convert the alpha value to its TAO equivalent.
        // This conversion takes into account the current state of the subnet,
        // including the total outstanding alpha and total TAO in the subnet.
        Self::alpha_to_global(alpha, netuid)
    }

    /// Converts an alpha value to its global TAO equivalent on a specific subnet.
    ///
    /// This function performs the following steps:
    /// 1. Retrieves the total outstanding alpha for the subnet.
    /// 2. Retrieves the total TAO for the subnet.
    /// 3. Calculates the proportion of the given alpha to the total outstanding alpha.
    /// 4. Multiplies this proportion by the total subnet TAO to get the TAO equivalent.
    /// 5. Converts the result to a u64 value and returns it.
    ///
    /// # Arguments
    /// * `alpha` - The alpha value to be converted.
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `u64` - The TAO equivalent of the given alpha value on the specified subnet.
    ///
    /// # Note
    /// This function uses fixed-point arithmetic (I96F32) for precise calculations.
    /// If the division by alpha_outstanding results in an error (e.g., division by zero),
    /// the function returns 0 as a fallback.
    pub fn alpha_to_global(alpha: u64, netuid: u16) -> u64 {
        // Step 1: Retrieve the total outstanding alpha for the subnet.
        // This represents the sum of all alpha values in the subnet.
        let alpha_outstanding: I96F32 = I96F32::from_num(SubnetAlphaOut::<T>::get(netuid));

        // Step 2: Retrieve the total TAO for the subnet.
        // This represents the total stake in TAO units for the subnet.
        let subnet_tao: I96F32 = I96F32::from_num(SubnetTAO::<T>::get(netuid));

        // Step 3 & 4: Calculate the TAO equivalent for the given alpha value.
        // This is done by:
        // a) Converting the input alpha to I96F32 for precise calculation
        // b) Dividing it by the total outstanding alpha to get the proportion
        // c) Multiplying this proportion by the total subnet TAO
        let tao_equivalent: u64 = (I96F32::from_num(alpha)
            .checked_div(alpha_outstanding)
            .unwrap_or(I96F32::from_num(0.0)) // If division fails, use 0 as fallback
            .saturating_mul(subnet_tao))
        .to_num::<u64>(); // Step 5: Convert the result back to u64

        // Return the calculated TAO equivalent
        tao_equivalent
    }

    /// Retrieves the total stake (alpha) for a given hotkey on a specific subnet.
    ///
    /// This function performs the following step:
    /// 1. Retrieves and returns the total alpha value associated with the hotkey on the specified subnet.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey.
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `u64` - The total alpha value for the hotkey on the specified subnet.
    ///
    /// # Note
    /// This function returns the cumulative stake across all coldkeys associated with this hotkey on the subnet.
    pub fn get_stake_for_hotkey_on_subnet(hotkey: &T::AccountId, netuid: u16) -> u64 {
        // Retrieve and return the total alpha this hotkey owns on this subnet.
        // This value represents the sum of stakes from all coldkeys associated with this hotkey.
        TotalHotkeyAlpha::<T>::get(hotkey, netuid)
    }

    /// Retrieves the total stake (alpha) for a given coldkey on a specific subnet.
    ///
    /// This function performs the following step:
    /// 1. Retrieves and returns the total alpha value associated with the coldkey on the specified subnet.
    ///
    /// # Arguments
    /// * `coldkey` - The account ID of the coldkey.
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `u64` - The total stake (alpha) for the coldkey on the specified subnet.
    ///
    /// # Note
    /// This function returns the cumulative stake across all hotkeys associated with this coldkey on the subnet.
    pub fn get_stake_for_coldkey_on_subnet(coldkey: &T::AccountId, netuid: u16) -> u64 {
        // Retrieve and return the total alpha this coldkey owns on this subnet.
        // This value represents the sum of stakes across all hotkeys associated with this coldkey.
        TotalColdkeyAlpha::<T>::get(coldkey, netuid)
    }

    /// Checks if a specific hotkey-coldkey pair has enough stake on a subnet to fulfill a given decrement.
    ///
    /// This function performs the following steps:
    /// 1. Retrieves the current stake for the hotkey-coldkey pair on the specified subnet.
    /// 2. Compares this stake with the requested decrement amount.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey.
    /// * `coldkey` - The account ID of the coldkey.
    /// * `netuid` - The unique identifier of the subnet.
    /// * `decrement` - The amount of stake to be potentially decremented.
    ///
    /// # Returns
    /// * `bool` - True if the account has enough stake to fulfill the decrement, false otherwise.
    ///
    /// # Note
    /// This function only checks the stake for the specific hotkey-coldkey pair, not the total stake of the hotkey or coldkey individually.
    pub fn has_enough_stake_on_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
        decrement: u64,
    ) -> bool {
        // Retrieve the current stake for this hotkey-coldkey pair on the subnet
        let current_stake =
            Self::get_stake_for_hotkey_and_coldkey_on_subnet(hotkey, coldkey, netuid);

        // Compare the current stake with the requested decrement
        // Return true if the current stake is greater than or equal to the decrement
        current_stake >= decrement
    }

    /// Retrieves the alpha (stake) value for a given hotkey and coldkey pair on a specific subnet.
    ///
    /// This function performs the following steps:
    /// 1. Takes the hotkey, coldkey, and subnet ID as input parameters.
    /// 2. Accesses the Alpha storage map to retrieve the stake value.
    /// 3. Returns the retrieved stake value as a u64.
    ///
    /// # Arguments
    /// * `hotkey` - The account ID of the hotkey (neuron).
    /// * `coldkey` - The account ID of the coldkey (owner).
    /// * `netuid` - The unique identifier of the subnet.
    ///
    /// # Returns
    /// * `u64` - The alpha (stake) value for the specified hotkey-coldkey pair on the given subnet.
    ///
    /// # Note
    /// This function retrieves the stake specific to the hotkey-coldkey pair, not the total stake of the hotkey or coldkey individually.
    pub fn get_stake_for_hotkey_and_coldkey_on_subnet(
        hotkey: &T::AccountId,
        coldkey: &T::AccountId,
        netuid: u16,
    ) -> u64 {
        // Step 1: Access the Alpha storage map
        // The Alpha map stores stake values for each (hotkey, coldkey, netuid) combination

        // Step 2: Retrieve the stake value using the provided parameters
        // If no stake exists for this combination, the default value of 0 will be returned
        Alpha::<T>::get((hotkey, coldkey, netuid))
    }
}
