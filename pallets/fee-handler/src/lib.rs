//! # Fee Handler Pallet
//!
//! This pallet implements custom handling of fees through implementing 
//! pallet-transaction-payment `Config::OnChargeTransaction`.

#![cfg_attr(not(feature = "std"), no_std)]

use codec::{Decode, Encode};
use scale_info::TypeInfo;

use frame_support::{
	dispatch::{
		DispatchInfo, DispatchResult, GetDispatchInfo, PostDispatchInfo,
	},
	traits::{EstimateCallFee, IsSubType},
};
use sp_runtime::{
	traits::{
		DispatchInfoOf, Dispatchable, PostDispatchInfoOf,
		SignedExtension, Zero,
	},
	transaction_validity::{
		TransactionPriority, TransactionValidity, TransactionValidityError, ValidTransaction,
	},
};

use pallet_transaction_payment::{
	Config as TransactionPaymentConfig, OnChargeTransaction, Pallet as TransactionPaymentPallet,
	ChargeTransactionPayment as TPChargeTransactionPayment
};
use pallet_subtensor::Call as PalletSubtensorCall;

type BalanceOf<T> = <<T as TransactionPaymentConfig>::OnChargeTransaction as OnChargeTransaction<T>>::Balance;

#[frame_support::pallet]
pub mod pallet {
	use frame_support::pallet_prelude::*;
	use frame_system::pallet_prelude::*;

	use super::*;

	#[pallet::pallet]
	pub struct Pallet<T>(_);

	#[pallet::config(with_default)]
	pub trait Config: frame_system::Config {
	}

	#[pallet::hooks]
	impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {
	}
}

#[derive(Encode, Decode, Clone, Eq, PartialEq, TypeInfo)]
#[scale_info(skip_type_params(T))]
pub struct ChargeTransactionPayment<T: TransactionPaymentConfig>(#[codec(compact)] BalanceOf<T>);

impl<T: TransactionPaymentConfig + pallet_subtensor::Config> ChargeTransactionPayment<T>
where
	<T as frame_system::Config>::RuntimeCall: Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo>,
	BalanceOf<T>: Send + Sync,
{
	/// utility constructor. Used only in client/factory code.
	pub fn from(fee: BalanceOf<T>) -> Self {
		Self(fee)
	}

	/// Returns the tip as being chosen by the transaction sender.
	pub fn tip(&self) -> BalanceOf<T> {
		self.0
	}

	fn withdraw_fee(
		&self,
		who: &T::AccountId,
		call: &<T as frame_system::Config>::RuntimeCall,
		info: &DispatchInfoOf<<T as frame_system::Config>::RuntimeCall>,
		len: usize,
	) -> Result<
		(
			BalanceOf<T>,
			<<T as TransactionPaymentConfig>::OnChargeTransaction as OnChargeTransaction<T>>::LiquidityInfo,
		),
		TransactionValidityError,
	> 
	where <T as frame_system::Config>::RuntimeCall: IsSubType<pallet_subtensor::Call<T>>
	{
		let tip = self.0;
		let fee = TransactionPaymentPallet::<T>::compute_fee(len as u32, info, tip);

		match IsSubType::<PalletSubtensorCall<T>>::is_sub_type(call) {
			// add_stake and remove_stake send their fees to SubnetTAO 
			Some(PalletSubtensorCall::add_stake { hotkey: _, netuid: _, amount_staked: _ }) => {
				<<T as TransactionPaymentConfig>::OnChargeTransaction as OnChargeTransaction<T>>::withdraw_fee(
					who, call, info, fee, tip,
				)
				.map(|i| (fee, i))
			},
			// All other extrinsics pay fees normally
			_ => {
				<<T as TransactionPaymentConfig>::OnChargeTransaction as OnChargeTransaction<T>>::withdraw_fee(
					who, call, info, fee, tip,
				)
				.map(|i| (fee, i))
			}
		}
	}

	/// Get an appropriate priority for a transaction with the given `DispatchInfo`
	pub fn get_priority(
		info: &DispatchInfoOf<<T as frame_system::Config>::RuntimeCall>,
		len: usize,
		tip: BalanceOf<T>,
		final_fee: BalanceOf<T>,
	) -> TransactionPriority {
		TPChargeTransactionPayment::<T>::get_priority(info, len, tip, final_fee)
	}
}

impl<T: TransactionPaymentConfig> core::fmt::Debug for ChargeTransactionPayment<T> {
	#[cfg(feature = "std")]
	fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
		write!(f, "ChargeTransactionPayment<{:?}>", self.0)
	}
	#[cfg(not(feature = "std"))]
	fn fmt(&self, _: &mut core::fmt::Formatter) -> core::fmt::Result {
		Ok(())
	}
}

impl<T: TransactionPaymentConfig + pallet_subtensor::Config> SignedExtension for ChargeTransactionPayment<T>
where
	BalanceOf<T>: Send + Sync + From<u64>,
	<T as frame_system::Config>::RuntimeCall: Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo> + IsSubType<pallet_subtensor::Call<T>>,
{
	const IDENTIFIER: &'static str = "ChargeTransactionPayment";
	type AccountId = T::AccountId;
	type Call = <T as frame_system::Config>::RuntimeCall;
	type AdditionalSigned = ();
	type Pre = (
		// tip
		BalanceOf<T>,
		// who paid the fee - this is an option to allow for a Default impl.
		Self::AccountId,
		// imbalance resulting from withdrawing the fee
		<<T as TransactionPaymentConfig>::OnChargeTransaction as OnChargeTransaction<T>>::LiquidityInfo,
	);
	fn additional_signed(&self) -> core::result::Result<(), TransactionValidityError> {
		Ok(())
	}

	fn validate(
		&self,
		who: &Self::AccountId,
		call: &Self::Call,
		info: &DispatchInfoOf<Self::Call>,
		len: usize,
	) -> TransactionValidity {
		let (final_fee, _) = self.withdraw_fee(who, call, info, len)?;
		let tip = self.0;
		Ok(ValidTransaction {
			priority: Self::get_priority(info, len, tip, final_fee),
			..Default::default()
		})
	}

	fn pre_dispatch(
		self,
		who: &Self::AccountId,
		call: &Self::Call,
		info: &DispatchInfoOf<Self::Call>,
		len: usize,
	) -> Result<Self::Pre, TransactionValidityError> {
		let (_fee, imbalance) = self.withdraw_fee(who, call, info, len)?;
		Ok((self.0, who.clone(), imbalance))
	}

	fn post_dispatch(
		maybe_pre: Option<Self::Pre>,
		info: &DispatchInfoOf<Self::Call>,
		post_info: &PostDispatchInfoOf<Self::Call>,
		len: usize,
		result: &DispatchResult,
	) -> Result<(), TransactionValidityError> {
		TPChargeTransactionPayment::<T>::post_dispatch(maybe_pre, info, post_info, len, result)
	}
}

impl<T: TransactionPaymentConfig, AnyCall: GetDispatchInfo + Encode> EstimateCallFee<AnyCall, BalanceOf<T>>
	for crate::pallet::Pallet<T>
where
	T::RuntimeCall: Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo>,
{
	fn estimate_call_fee(call: &AnyCall, post_info: PostDispatchInfo) -> BalanceOf<T> {
		let len = call.encoded_size() as u32;
		let info = call.get_dispatch_info();
		TransactionPaymentPallet::<T>::compute_actual_fee(len, &info, &post_info, Zero::zero())
	}
}
