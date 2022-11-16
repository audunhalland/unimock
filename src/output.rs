use crate::possess::Possess;
use std::borrow::Borrow;

pub trait Output {
    /// Version of output without lifetimes
    type Type: 'static;
}

pub trait OutputSig<'u, 'i, O: Output> {
    type Sig;

    fn project(value: O::Type) -> Option<Self::Sig> {
        None
    }

    fn project_ref(value: &'u O::Type) -> Option<Self::Sig> {
        None
    }
}

pub trait OwnedOutput: Output {}
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
}

impl<T: 'static> OwnedOutput for Owned<T> {}

// Borrowed

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

pub struct BorrowSelf<'u, T: ?Sized + 'static>(std::marker::PhantomData<&'u T>);

impl<'u, 'i, T: ?Sized + 'static> OutputSig<'u, 'i, Borrowed<T>> for BorrowSelf<'u, T> {
    type Sig = &'u T;
}

pub struct BorrowInputs<'i, T: ?Sized + 'static>(std::marker::PhantomData<&'i T>);

impl<'u, 'i, T: ?Sized + 'static> OutputSig<'u, 'i, Borrowed<T>> for BorrowInputs<'i, T> {
    type Sig = &'i T;
}

// Static

pub struct StaticRef<T: ?Sized>(std::marker::PhantomData<T>);

impl<T: ?Sized + 'static> Output for StaticRef<T> {
    type Type = &'static T;
}

impl<T: ?Sized + 'static> OwnedOutput for StaticRef<T> {}

impl<'u, 'i, T: ?Sized + 'static> OutputSig<'u, 'i, Self> for StaticRef<T> {
    type Sig = &'static T;
}

// Complex

pub struct Complex<T>(std::marker::PhantomData<T>);
pub struct ComplexSig<T>(std::marker::PhantomData<T>);

impl<T: Possess<'static>> Output for Complex<T> {
    type Type = <T as Possess<'static>>::Possessed;
}

impl<T: Possess<'static>> OwnedOutput for Complex<T> {}

impl<'u, 'i, T, O> OutputSig<'u, 'i, O> for ComplexSig<T>
where
    O: Output,
    T: Possess<'u, Possessed = O::Type>,
{
    type Sig = T;

    fn project_ref(value: &'u <O as Output>::Type) -> Option<Self::Sig> {
        Some(<T as Possess>::reborrow(value))
    }
}
