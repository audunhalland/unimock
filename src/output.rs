use crate::possess::Possess;
use std::borrow::Borrow;

pub trait Output {
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

pub trait OutputOld<'u> {
    type Type;
}

pub trait StoreOutputOld<'u>: OutputOld<'u> {
    type Stored: Send + Sync + 'static;

    fn load(stored: Self::Stored) -> Option<Self::Type>;

    fn load_ref(stored: &'u Self::Stored) -> Option<Self::Type>;
}

pub trait OwnedOutput: Output {}

pub trait OwnedOutputOld<'u>: StoreOutputOld<'u> {}
pub trait RefOutputOld<'u>: StoreOutputOld<'u> {}
pub trait StaticRefOutputOld: StoreOutputOld<'static> {}
pub trait ComplexOutputOld<'u>: StoreOutputOld<'u> {}

// Owned

pub struct Owned<T>(std::marker::PhantomData<T>);

impl<T: 'static> Output for Owned<T> {
    type Type = T;
}

impl<'u, T: 'static> OutputSig<'u, Self> for Owned<T> {
    type Sig = T;
}

impl<T: 'static> OwnedOutput for Owned<T> {}

impl<'u, T: 'static> OutputOld<'u> for Owned<T> {
    type Type = T;
}

impl<'u, T: Send + Sync + 'static> StoreOutputOld<'u> for Owned<T> {
    type Stored = T;

    fn load(stored: Self::Stored) -> Option<Self::Type> {
        Some(stored)
    }

    fn load_ref(_: &'u Self::Stored) -> Option<Self::Type> {
        None
    }
}

impl<'u, T: Send + Sync + 'static> OwnedOutputOld<'u> for Owned<T> {}

// Ref

pub struct Ref<T: ?Sized + 'static>(std::marker::PhantomData<T>);

impl<T: ?Sized + 'static> Output for Ref<T> {
    type Type = Box<dyn Borrow<T> + Send + Sync>;
}

pub struct RefSig<'u, T: ?Sized + 'static>(std::marker::PhantomData<&'u T>);

impl<'u, T: ?Sized + 'static> OutputSig<'u, Ref<T>> for RefSig<'u, T> {
    type Sig = &'u T;
}

pub struct RefOld<'a, T: ?Sized>(std::marker::PhantomData<&'a T>);

impl<'u, T: ?Sized> OutputOld<'u> for RefOld<'u, T> {
    type Type = &'u T;
}

impl<'u, T: ?Sized + 'static> StoreOutputOld<'u> for RefOld<'u, T> {
    type Stored = Box<dyn Borrow<T> + Send + Sync>;

    fn load(_: Self::Stored) -> Option<Self::Type> {
        None
    }

    fn load_ref(stored: &'u Self::Stored) -> Option<Self::Type> {
        Some(stored.as_ref().borrow())
    }
}

impl<'u, T: ?Sized + 'static> RefOutputOld<'u> for RefOld<'u, T> {}

// Static

pub struct StaticRef<T: ?Sized>(std::marker::PhantomData<T>);

impl<T: ?Sized + 'static> Output for StaticRef<T> {
    type Type = &'static T;
}

impl<'u, T: ?Sized + 'static> OutputSig<'u, Self> for StaticRef<T> {
    type Sig = &'static T;
}

impl<'u, T: ?Sized + 'static> OutputOld<'u> for StaticRef<T> {
    type Type = &'static T;
}

impl<T: ?Sized + 'static + Send + Sync> StoreOutputOld<'static> for StaticRef<T> {
    type Stored = &'static T;

    fn load(stored: Self::Stored) -> Option<Self::Type> {
        Some(stored)
    }

    fn load_ref(stored: &'static Self::Stored) -> Option<Self::Type> {
        Some(*stored)
    }
}

impl<T: ?Sized + 'static + Send + Sync> StaticRefOutputOld for StaticRef<T> {}

// Complex

pub struct Complex<T>(std::marker::PhantomData<T>);
pub struct ComplexSig<T>(std::marker::PhantomData<T>);

impl<T: Possess<'static>> Output for Complex<T> {
    type Type = <T as Possess<'static>>::Possessed;
}

impl<'a, T, O> OutputSig<'a, O> for ComplexSig<T>
where
    O: Output,
    T: Possess<'a, Possessed = O::Type>,
{
    type Sig = T;

    /*
    fn project_ref(value: &'a <O as Output>::Type) -> Option<Self::Sig> {
        Some(<S as Possess>::reborrow(value))
    }
    */
}

pub struct ComplexOld<'u, T: ?Sized + Possess<'u>>(std::marker::PhantomData<&'u T>);

impl<'u, T: Possess<'u>> OutputOld<'u> for ComplexOld<'u, T> {
    type Type = T;
}

impl<'u, T: Possess<'u>> StoreOutputOld<'u> for ComplexOld<'u, T>
where
    <T as Possess<'u>>::Possessed: Send + Sync,
{
    type Stored = <T as Possess<'u>>::Possessed;

    fn load(_: Self::Stored) -> Option<Self::Type> {
        None
    }

    fn load_ref(stored: &'u Self::Stored) -> Option<Self::Type> {
        Some(<T as Possess>::reborrow(stored))
    }
}

impl<'u, T: Possess<'u>> ComplexOutputOld<'u> for ComplexOld<'u, T> where
    <T as Possess<'u>>::Possessed: Send + Sync
{
}
