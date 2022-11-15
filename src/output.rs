use crate::unborrow::Unborrow;
use std::borrow::Borrow;

pub trait Output {
    type Type;
}

pub trait StoreOutput: Output {
    type Stored: Send + Sync;
}

pub trait OwnedOutput: StoreOutput {}
pub trait RefOutput: StoreOutput {}
pub trait StaticRefOutput: StoreOutput {}
pub trait ComplexOutput: StoreOutput {}

pub struct Owned<T: ?Sized>(std::marker::PhantomData<T>);

impl<T> Output for Owned<T> {
    type Type = T;
}

impl<T: Send + Sync> StoreOutput for Owned<T> {
    type Stored = T;
}

impl<T: Send + Sync> OwnedOutput for Owned<T> {}

pub struct Ref<'a, T: ?Sized>(std::marker::PhantomData<&'a T>);

impl<'u, T: ?Sized> Output for Ref<'u, T> {
    type Type = &'u T;
}

impl<'u, T: ?Sized> StoreOutput for Ref<'u, T> {
    type Stored = Box<dyn Borrow<T> + Send + Sync>;
}

impl<'a, T: ?Sized> RefOutput for Ref<'a, T> {}

pub struct StaticRef<T: ?Sized>(std::marker::PhantomData<T>);

impl<T: ?Sized + 'static> Output for StaticRef<T> {
    type Type = &'static T;
}

impl<T: ?Sized + 'static + Send + Sync> StoreOutput for StaticRef<T> {
    type Stored = &'static T;
}

impl<T: ?Sized + 'static + Send + Sync> StaticRefOutput for StaticRef<T> {}

pub struct Complex<T: ?Sized + Unborrow>(std::marker::PhantomData<T>);

impl<T: Unborrow> Output for Complex<T> {
    type Type = T;
}

impl<T: Unborrow> StoreOutput for Complex<T>
where
    <T as Unborrow>::Unborrowed: Send + Sync,
{
    type Stored = <T as Unborrow>::Unborrowed;
}

impl<T: Unborrow> ComplexOutput for Complex<T> where <T as Unborrow>::Unborrowed: Send + Sync {}

mod test {
    use super::*;

    trait MockFn3 {
        type Output<'s>: Output
        where
            Self: 's;

        fn store_value(&mut self, value: <Self::Output<'_> as StoreOutput>::Stored)
        where
            for<'s> Self::Output<'s>: StoreOutput,
        {
        }

        fn get_value(&self) -> <Self::Output<'_> as Output>::Type {
            panic!()
        }
    }

    trait Mockable {
        fn owned(&self) -> String;
        fn borrow(&self) -> &str;
        fn complex(&self) -> Option<&str>;
    }

    struct MockOwned;
    struct MockBorrowed;
    struct MockComplex;

    impl MockFn3 for MockOwned {
        type Output<'s> = Owned<String>;
    }

    impl MockFn3 for MockBorrowed {
        type Output<'s> = Ref<'s, str>;
    }

    impl MockFn3 for MockComplex {
        type Output<'s> = Complex<Option<&'s str>>;
    }

    fn test_it() {
        let owned: String = MockOwned.get_value();
        let borrowed: &str = MockBorrowed.get_value();
        let complex: Option<&str> = MockComplex.get_value();

        MockBorrowed.store_value(Box::new("".to_string()));
    }
}
