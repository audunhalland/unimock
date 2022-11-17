use crate::{as_owned::AsOwned, value_chain::ValueChain};
use std::borrow::Borrow;

/// Trait that describes how an output value is temporarily stored by Unimock.
pub trait Output {
    /// The type of the output temporarily stored inside Unimock.
    type Type: 'static;
}

/// Trait that describes the output signature of a mocked function.
pub trait OutputSig<'u, O: Output> {
    /// The type of the output compatible with the function signature.
    type Sig;

    #[doc(hidden)]
    fn from_output(value: O::Type, value_chain: &'u ValueChain) -> Self::Sig;

    #[doc(hidden)]
    fn try_borrow_output(value: &'u O::Type) -> Result<Self::Sig, SignatureError>;
}

#[doc(hidden)]
pub enum SignatureError {
    NotOwned,
    NotBorrowed,
}

/// Trait for constructing an output by borrowing from another value.
pub trait FromBorrow<T: ?Sized>: Output {
    /// Construct an output by using some value that can be borrowed from to produce the output.
    fn from_borrow(
        value: impl std::borrow::Borrow<T> + Send + Sync + 'static,
    ) -> <Self as Output>::Type;
}

#[doc(hidden)]
pub struct Owned<T>(std::marker::PhantomData<T>);

impl<T: 'static> Output for Owned<T> {
    type Type = T;
}

impl<'u, T: 'static> OutputSig<'u, Self> for Owned<T> {
    type Sig = T;

    fn from_output(value: <Self as Output>::Type, _: &'u ValueChain) -> Self::Sig {
        value
    }

    fn try_borrow_output(_: &'u <Self as Output>::Type) -> Result<Self::Sig, SignatureError> {
        Err(SignatureError::NotOwned)
    }
}

/// This type describes a function output that is a reference borrowed from `Self`.
pub struct Borrowed<T: ?Sized + 'static>(std::marker::PhantomData<T>);

impl<T: ?Sized + 'static> Output for Borrowed<T> {
    type Type = Box<dyn Borrow<T> + Send + Sync>;
}

impl<T: ?Sized + 'static> FromBorrow<T> for Borrowed<T> {
    fn from_borrow(
        value: impl std::borrow::Borrow<T> + Send + Sync + 'static,
    ) -> <Self as Output>::Type {
        Box::new(value)
    }
}

impl<'u, T: ?Sized + 'static> OutputSig<'u, Self> for Borrowed<T> {
    type Sig = &'u T;

    fn from_output(value: <Borrowed<T> as Output>::Type, value_chain: &'u ValueChain) -> Self::Sig {
        let value_ref = value_chain.push(value);

        value_ref.as_ref().borrow()
    }

    fn try_borrow_output(
        value: &'u <Borrowed<T> as Output>::Type,
    ) -> Result<Self::Sig, SignatureError> {
        Ok(value.as_ref().borrow())
    }
}

#[doc(hidden)]
pub struct StaticRef<T: ?Sized>(std::marker::PhantomData<T>);

impl<T: ?Sized + 'static> Output for StaticRef<T> {
    type Type = &'static T;
}

impl<'u, T: ?Sized + 'static> OutputSig<'u, Self> for StaticRef<T> {
    type Sig = &'static T;

    fn from_output(value: <Self as Output>::Type, _: &ValueChain) -> Self::Sig {
        value
    }

    fn try_borrow_output(value: &'u <Self as Output>::Type) -> Result<Self::Sig, SignatureError> {
        Ok(*value)
    }
}

#[doc(hidden)]
// This type describes a function output that is a mix of owned and borrowed data.
//
// The typical example is `Option<&T>`.
pub struct Mixed<T>(std::marker::PhantomData<T>);

impl<T: AsOwned<'static>> Output for Mixed<T> {
    type Type = <T as AsOwned<'static>>::Owned;
}

impl<'u, T, O> OutputSig<'u, O> for Mixed<T>
where
    O: Output,
    <O as Output>::Type: Send + Sync,
    T: AsOwned<'u, Owned = O::Type>,
{
    type Sig = T;

    fn from_output(value: <O as Output>::Type, value_chain: &'u ValueChain) -> Self::Sig {
        let value_ref = value_chain.push(value);
        <T as AsOwned>::reborrow(value_ref)
    }

    fn try_borrow_output(value: &'u <O as Output>::Type) -> Result<Self::Sig, SignatureError> {
        Ok(<T as AsOwned>::reborrow(value))
    }
}
