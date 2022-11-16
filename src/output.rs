use crate::possess::Possess;
use std::borrow::Borrow;

pub trait Output {
    /// Version of output without lifetimes
    type Type: 'static;
}

pub trait OutputSig<'u, O: Output> {
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

impl<'u, T: 'static> OutputSig<'u, Self> for Owned<T> {
    type Sig = T;
}

impl<T: 'static> OwnedOutput for Owned<T> {}

// Ref

pub struct Ref<T: ?Sized + 'static>(std::marker::PhantomData<T>);

impl<T: ?Sized + 'static> Output for Ref<T> {
    type Type = Box<dyn Borrow<T> + Send + Sync>;
}

impl<T: ?Sized + 'static> BorrowOutput for Ref<T> {}

impl<T: ?Sized + 'static> IntoBorrowOutputType<T> for Ref<T> {
    fn into_borrow_output_type(
        value: impl std::borrow::Borrow<T> + Send + Sync + 'static,
    ) -> <Self as Output>::Type {
        Box::new(value)
    }
}

pub struct RefSig<'u, T: ?Sized + 'static>(std::marker::PhantomData<&'u T>);

impl<'u, T: ?Sized + 'static> OutputSig<'u, Ref<T>> for RefSig<'u, T> {
    type Sig = &'u T;
}

// Static

pub struct StaticRef<T: ?Sized>(std::marker::PhantomData<T>);

impl<T: ?Sized + 'static> Output for StaticRef<T> {
    type Type = &'static T;
}

impl<T: ?Sized + 'static> OwnedOutput for StaticRef<T> {}

impl<'u, T: ?Sized + 'static> OutputSig<'u, Self> for StaticRef<T> {
    type Sig = &'static T;
}

// Complex

pub struct Complex<T>(std::marker::PhantomData<T>);
pub struct ComplexSig<T>(std::marker::PhantomData<T>);

impl<T: Possess<'static>> Output for Complex<T> {
    type Type = <T as Possess<'static>>::Possessed;
}

impl<T: Possess<'static>> OwnedOutput for Complex<T> {}

impl<'a, T, O> OutputSig<'a, O> for ComplexSig<T>
where
    O: Output,
    T: Possess<'a, Possessed = O::Type>,
{
    type Sig = T;

    fn project_ref(value: &'a <O as Output>::Type) -> Option<Self::Sig> {
        Some(<T as Possess>::reborrow(value))
    }
}
