use crate::possess::Possess;
use std::borrow::Borrow;

pub trait Output<'u> {
    type Type;
}

pub trait StoreOutput<'u>: Output<'u> {
    type Stored: Send + Sync;

    fn reborrow_complex(stored: &'u Self::Stored) -> Option<Self::Type> {
        None
    }
}

pub trait OwnedOutput<'u>: StoreOutput<'u> {}
pub trait RefOutput<'u>: StoreOutput<'u> {}
pub trait StaticRefOutput: StoreOutput<'static> {}
pub trait ComplexOutput<'u>: StoreOutput<'u> {}

pub struct Owned<T: ?Sized>(std::marker::PhantomData<T>);

impl<'u, T> Output<'u> for Owned<T> {
    type Type = T;
}

impl<'u, T: Send + Sync> StoreOutput<'u> for Owned<T> {
    type Stored = T;
}

impl<'u, T: Send + Sync> OwnedOutput<'u> for Owned<T> {}

pub struct Ref<'a, T: ?Sized>(std::marker::PhantomData<&'a T>);

impl<'u, T: ?Sized> Output<'u> for Ref<'u, T> {
    type Type = &'u T;
}

impl<'u, T: ?Sized> StoreOutput<'u> for Ref<'u, T> {
    type Stored = Box<dyn Borrow<T> + Send + Sync>;
}

impl<'u, T: ?Sized> RefOutput<'u> for Ref<'u, T> {}

pub struct StaticRef<T: ?Sized>(std::marker::PhantomData<T>);

impl<'u, T: ?Sized + 'static> Output<'u> for StaticRef<T> {
    type Type = &'static T;
}

impl<T: ?Sized + 'static + Send + Sync> StoreOutput<'static> for StaticRef<T> {
    type Stored = &'static T;
}

impl<T: ?Sized + 'static + Send + Sync> StaticRefOutput for StaticRef<T> {}

pub struct Complex<'u, T: ?Sized + Possess<'u>>(std::marker::PhantomData<&'u T>);

impl<'u, T: Possess<'u>> Output<'u> for Complex<'u, T> {
    type Type = T;
}

impl<'u, T: Possess<'u>> StoreOutput<'u> for Complex<'u, T>
where
    <T as Possess<'u>>::Possessed: Send + Sync,
{
    type Stored = <T as Possess<'u>>::Possessed;

    fn reborrow_complex(stored: &'u Self::Stored) -> Option<Self::Type> {
        Some(<T as Possess>::reborrow(stored))
    }
}

impl<'u, T: Possess<'u>> ComplexOutput<'u> for Complex<'u, T> where
    <T as Possess<'u>>::Possessed: Send + Sync
{
}
