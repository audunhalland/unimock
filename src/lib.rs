//!
//! `unimock` is a library that makes it easy to create mock objects that implement _multiple traits_ at the same time.
//!
//! unimock exports a single type, [Unimock], that will implement all your annotated traits:
//!
//! ```rust
//! use unimock::*;
//! #[unimock]
//! trait Foo {
//!     fn foo(&self) -> i32;
//! }
//!
//! #[unimock]
//! trait Bar {
//!     fn bar(&self) -> i32;
//! }
//!
//! fn sum(foobar: impl Foo + Bar) -> i32 {
//!     foobar.foo() + foobar.bar()
//! }
//!
//! fn test() {
//!     let unimock = Unimock::new()
//!         .mock(|foo: &mut MockFoo| {
//!             foo.expect_foo().return_const(40);
//!         })
//!         .mock(|bar: &mut MockBar| {
//!             bar.expect_bar().return_const(2);
//!         });
//!
//!     let answer = sum(unimock);
//!     assert_eq!(42, answer);
//! }
//! ```
//!
//! `unimock` uses [`mockall`] to mock single traits, which is where the `MockFoo` and `MockBar` types above come from.
//!
//! [`mockall`]: https://docs.rs/mockall/latest/mockall/
//!
//! `unimock` also works with `async_trait`:
//!
//! ```rust
//! use unimock::*;
//! use async_trait::*;
//! #[unimock]
//! #[async_trait]
//! trait Baz {
//!     async fn baz(&self, arg: String) -> i32;
//! }
//! ```

#![forbid(unsafe_code)]
// For the mock-fn feature:
#![feature(generic_associated_types)]

mod builders;

pub use builders::*;

use std::any::{Any, TypeId};
use std::collections::HashMap;

///
/// Autogenerate a mock implementation of a trait.
///
/// This macro does two things:
/// 1. Autogenerate a `mockall` implementation by invoking [`mockall::automock`].
/// 2. Autogenerate an implementation for [Unimock].
///
/// [`mockall::automock`]: https://docs.rs/mockall/latest/mockall/attr.automock.html
///
/// Example
/// ```rust
/// use unimock::*;
///
/// #[unimock]
/// trait MyTrait {
///     fn foo(&self);
/// }
///
/// fn do_something(my_trait: impl MyTrait) {
///     my_trait.foo();
/// }
///
/// fn using_mockall() {
///     // Since `do_something` is only bounded by one trait, we can use the mockall type directly:
///     do_something(MockMyTrait::new()); // note: this will panic!
/// }
///
/// fn using_unimock() {
///     // If `do_something` had multiple trait bounds, we would have two choices:
///     // 1. implement a specialized mock type using `mockall::mock`
///     // 2. Just use `Unimock`:
///     do_something(Unimock::new());  // note: this will panic!
/// }
///
/// # fn main() {
/// # let _ = std::panic::catch_unwind(|| using_mockall());
/// # let _ = std::panic::catch_unwind(|| using_unimock());
/// # }
/// ```
pub use unimock_macros::unimock;

pub use unimock_macros::unimock_next;

enum FallbackMode {
    ReturnDefault,
    CallOriginal,
    Panic,
}

/// Unimock stores a collection of mock objects, with the end goal of implementing
/// all the mocked traits. The trait implementaion is achieved through using the [unimock] macro.
pub struct Unimock {
    fallback_mode: FallbackMode,
    mocks: HashMap<TypeId, Box<dyn Any + Send + Sync>>,
    impls: HashMap<TypeId, ImplStorage>,
}

impl Unimock {
    /// Create a new, empty Unimock. Attempting to call implemented traits on an empty instance will panic at runtime.
    pub fn new() -> Self {
        Self {
            fallback_mode: FallbackMode::Panic,
            mocks: HashMap::new(),
            impls: HashMap::new(),
        }
    }

    pub fn mock_with<const N: usize>(_: [DynSignatureImpl; N]) -> Self {
        Self {
            fallback_mode: FallbackMode::Panic,
            mocks: HashMap::new(),
            impls: HashMap::new(),
        }
    }

    /// Configure a specific mock. The type must implement [Default]. Each stored mock is keyed by its [TypeId],
    /// so repeatedly calling this method with the same receiving type will use the same instance.
    ///
    /// When a trait is mocked using [unimock], its _mocked implementation_ must be used in this function.
    ///
    /// # Example
    /// ```rust
    /// use unimock::*;
    ///
    /// #[unimock]
    /// trait MyTrait {}
    ///
    /// # fn main() {
    /// let unimock = Unimock::new().mock(|my_trait: &mut MockMyTrait| {
    ///     /* ... */
    /// });
    /// # }
    /// ```
    pub fn mock<M, F>(mut self, f: F) -> Self
    where
        M: Default + Send + Sync + 'static,
        F: FnOnce(&mut M),
    {
        f(self
            .mocks
            .entry(TypeId::of::<M>())
            .or_insert_with(|| Box::new(M::default()))
            .downcast_mut()
            .unwrap());

        self
    }

    pub fn mock_fn<S, F>(mut self, _: S, f: F) -> Self
    where
        S: Signature + 'static,
        F: (for<'s> Fn(<S as Signature>::Args<'s>) -> <S as Signature>::Output)
            + Send
            + Sync
            + 'static,
    {
        let boxed_mock = BoxedMockFn(Box::new(f));

        self.impls
            .insert(TypeId::of::<S>(), ImplStorage::MockFn(Box::new(boxed_mock)));

        self
    }

    pub fn call_original<S>(mut self, _: S) -> Self
    where
        S: Signature + 'static,
    {
        self.impls
            .insert(TypeId::of::<S>(), ImplStorage::CallOriginal);
        self
    }

    /// Get a specific mock created with `mock`. Panics at runtime if the type is not registered.
    pub fn get<T: std::any::Any>(&self, trait_name: &'static str) -> &T {
        self.mocks
            .get(&TypeId::of::<T>())
            .and_then(|any| any.downcast_ref())
            .unwrap_or_else(|| panic!("{}", self.missing_trait_error(trait_name)))
    }

    pub fn get_impl<'s, S: Signature + 'static>(&'s self, api_name: &'static str) -> Impl<'s, S> {
        self.impls
            .get(&TypeId::of::<S>())
            .map(Impl::from_storage)
            .unwrap_or_else(|| Impl::from_fallback(&self.fallback_mode, api_name))
    }

    fn missing_trait_error(&self, trait_name: &'static str) -> String {
        format!("Missing mock for trait {trait_name}")
    }
}

pub trait IntoUnimock {
    fn unimock(self) -> Unimock;
}

impl<const N: usize> IntoUnimock for [DynSignatureImpl; N] {
    fn unimock(self) -> Unimock {
        Unimock::new()
    }
}

///
/// The `Sig` trait describes some function signature that may be mocked by unimock.
/// The types that implement this trait act as stand-ins to represent some mockable API.
///
pub trait Signature: Sized {
    /// The arguments to the mock function
    type Args<'i>;
    /// The output of the mock function
    type Output;

    fn mock<F>(self, f: F) -> DynSignatureImpl
    where
        Self: 'static,
        F: FnOnce(&mut MockBuilder<Self>),
    {
        let mut builder = MockBuilder::<Self>::new();
        f(&mut builder);

        DynSignatureImpl {
            signature: TypeId::of::<Self>(),
            storage: ImplStorage::ReturnDefault,
        }
    }
}

pub struct DynSignatureImpl {
    signature: TypeId,
    storage: ImplStorage,
}

pub enum Impl<'s, S: Signature + 'static> {
    ReturnDefault,
    CallOriginal,
    MockFn(&'s dyn for<'i> Fn(S::Args<'i>) -> S::Output),
}

impl<'s, S: Signature + 'static> Impl<'s, S> {
    fn from_storage(storage: &'s ImplStorage) -> Self {
        match storage {
            ImplStorage::ReturnDefault => Self::ReturnDefault,
            ImplStorage::CallOriginal => Self::CallOriginal,
            ImplStorage::MockFn(any) => {
                let boxed_mock = any.downcast_ref::<BoxedMockFn<S>>().unwrap();
                Self::MockFn(&boxed_mock.0)
            }
        }
    }

    fn from_fallback(fallback_mode: &FallbackMode, api_name: &'static str) -> Self {
        match fallback_mode {
            FallbackMode::ReturnDefault => Self::ReturnDefault,
            FallbackMode::CallOriginal => Self::CallOriginal,
            FallbackMode::Panic => panic!("No mock implementation found for {api_name}"),
        }
    }
}

enum ImplStorage {
    ReturnDefault,
    CallOriginal,
    MockFn(Box<dyn std::any::Any + Send + Sync + 'static>),
}

struct BoxedMockFn<S: Signature>(Box<dyn (for<'s> Fn(S::Args<'s>) -> S::Output) + Send + Sync>);
