use crate::possess::Possess;
use std::borrow::Borrow;

/// Trait that describes how an output value is temporarily stored by Unimock.
pub trait Output {
    /// The type of the output temporarily stored inside Unimock.
    type Type: 'static;
}

pub enum IntoSigError {
    NotOwned,
    NotBorrowed,
}

/// Trait that describes the output signature of a mocked function.
pub trait OutputSig<'u, 'i, O: Output> {
    /// The type of the output compatible with the function signature.
    type Sig;

    fn try_into_sig(value: O::Type) -> Result<Self::Sig, IntoSigError>;

    fn try_borrow_sig(value: &'u O::Type) -> Result<Self::Sig, IntoSigError>;
}

pub trait BorrowOutput: Output
where
    <Self as Output>::Type: Send + Sync,
{
}
pub trait IntoBorrowOutputType<T: ?Sized>: Output {
    fn into_borrow_output_type(
        value: impl std::borrow::Borrow<T> + Send + Sync + 'static,
    ) -> <Self as Output>::Type;
}
pub trait StaticRefOutput: Output {}

// Owned

pub struct Owned<T>(std::marker::PhantomData<T>);

impl<T: 'static> Output for Owned<T> {
    type Type = T;
}

impl<'u, 'i, T: 'static> OutputSig<'u, 'i, Self> for Owned<T> {
    type Sig = T;

    fn try_into_sig(value: <Self as Output>::Type) -> Result<Self::Sig, IntoSigError> {
        Ok(value)
    }

    fn try_borrow_sig(_: &'u <Self as Output>::Type) -> Result<Self::Sig, IntoSigError> {
        Err(IntoSigError::NotOwned)
    }
}

/// This type describes a function output that is a reference borrowed from `Self`.
pub struct Borrowed<T: ?Sized + 'static>(std::marker::PhantomData<T>);

impl<T: ?Sized + 'static> Output for Borrowed<T> {
    type Type = Box<dyn Borrow<T> + Send + Sync>;
}

impl<T: ?Sized + 'static> BorrowOutput for Borrowed<T> {}

impl<T: ?Sized + 'static> IntoBorrowOutputType<T> for Borrowed<T> {
    fn into_borrow_output_type(
        value: impl std::borrow::Borrow<T> + Send + Sync + 'static,
    ) -> <Self as Output>::Type {
        Box::new(value)
    }
}

impl<'u, 'i, T: ?Sized + 'static> OutputSig<'u, 'i, Borrowed<T>> for Borrowed<T> {
    type Sig = &'u T;

    fn try_into_sig(_: <Borrowed<T> as Output>::Type) -> Result<Self::Sig, IntoSigError> {
        Err(IntoSigError::NotBorrowed)
    }

    fn try_borrow_sig(value: &'u <Borrowed<T> as Output>::Type) -> Result<Self::Sig, IntoSigError> {
        Ok(value.as_ref().borrow())
    }
}

/// This type describes a function output that is a static reference.
pub struct StaticRef<T: ?Sized>(std::marker::PhantomData<T>);

impl<T: ?Sized + 'static> Output for StaticRef<T> {
    type Type = &'static T;
}

impl<'u, 'i, T: ?Sized + 'static> OutputSig<'u, 'i, Self> for StaticRef<T> {
    type Sig = &'static T;

    fn try_into_sig(value: <Self as Output>::Type) -> Result<Self::Sig, IntoSigError> {
        Ok(value)
    }

    fn try_borrow_sig(value: &'u <Self as Output>::Type) -> Result<Self::Sig, IntoSigError> {
        Ok(*value)
    }
}

/// This type describes a function output that is a mix of owned and borrowed data.
///
/// The typical example is `Option<&T>`.
pub struct Mixed<T>(std::marker::PhantomData<T>);

impl<T: Possess<'static>> Output for Mixed<T> {
    type Type = <T as Possess<'static>>::Possessed;
}

impl<'u, 'i, T, O> OutputSig<'u, 'i, O> for Mixed<T>
where
    O: Output,
    T: Possess<'u, Possessed = O::Type>,
{
    type Sig = T;

    fn try_into_sig(_: <O as Output>::Type) -> Result<Self::Sig, IntoSigError> {
        Err(IntoSigError::NotBorrowed)
    }

    fn try_borrow_sig(value: &'u <O as Output>::Type) -> Result<Self::Sig, IntoSigError> {
        Ok(<T as Possess>::reborrow(value))
    }
}
