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

    pub fn mock_with<const N: usize>(dyn_impls: [DynMockImpl; N]) -> Self {
        let mut impls = HashMap::new();

        for dyn_impl in dyn_impls.into_iter() {
            impls.insert(dyn_impl.id, dyn_impl.storage);
        }

        Self {
            fallback_mode: FallbackMode::Panic,
            mocks: HashMap::new(),
            impls,
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

    pub fn call_original<M>(mut self, _: M) -> Self
    where
        M: Mock + 'static,
    {
        self.impls
            .insert(TypeId::of::<M>(), ImplStorage::CallOriginal);
        self
    }

    /// Get a specific mock created with `mock`. Panics at runtime if the type is not registered.
    pub fn get<T: std::any::Any>(&self, trait_name: &'static str) -> &T {
        self.mocks
            .get(&TypeId::of::<T>())
            .and_then(|any| any.downcast_ref())
            .unwrap_or_else(|| panic!("{}", self.missing_trait_error(trait_name)))
    }

    pub fn get_impl<'s, M: Mock + 'static>(&'s self) -> Impl<'s, M> {
        self.impls
            .get(&TypeId::of::<M>())
            .map(Impl::from_storage)
            .unwrap_or_else(|| Impl::from_fallback(&self.fallback_mode))
    }

    fn missing_trait_error(&self, trait_name: &'static str) -> String {
        format!("Missing mock for trait {trait_name}")
    }
}

pub trait IntoUnimock {
    fn unimock(self) -> Unimock;
}

impl<const N: usize> IntoUnimock for [DynMockImpl; N] {
    fn unimock(self) -> Unimock {
        Unimock::mock_with(self)
    }
}

///
/// Trait describing a single mockable item.
/// The trait needs to be implemented by some type. That type will be defined
/// by the macro, and will act as the entrypoint for configuring the mock.
///
pub trait Mock: Sized {
    /// The arguments to the mock function
    type Args<'i>;
    /// The output of the mock function
    type Output;

    const NAME: &'static str;

    fn mock<F>(self, f: F) -> DynMockImpl
    where
        Self: 'static,
        F: FnOnce(&mut MockBuilder<Self>),
    {
        let mut builder = MockBuilder::<Self>::new();
        f(&mut builder);

        DynMockImpl {
            id: TypeId::of::<Self>(),
            storage: ImplStorage::Mock(Box::new(builder.to_mock_impl())),
        }
    }
}

///
/// A single dynamic mock implementation with all generics erased.
///
pub struct DynMockImpl {
    id: TypeId,
    storage: ImplStorage,
}

pub enum Impl<'s, M: Mock + 'static> {
    ReturnDefault,
    CallOriginal,
    //MockFn(&'s dyn for<'i> Fn(M::Args<'i>) -> M::Output),
    Mock(&'s MockImpl<M>),
}

impl<'s, M: Mock + 'static> Impl<'s, M> {
    fn from_storage(storage: &'s ImplStorage) -> Self {
        match storage {
            ImplStorage::ReturnDefault => Self::ReturnDefault,
            ImplStorage::CallOriginal => Self::CallOriginal,
            ImplStorage::Mock(any) => {
                let mock_impl = any.downcast_ref::<MockImpl<M>>().unwrap();
                Self::Mock(&mock_impl)
            }
        }
    }

    fn from_fallback(fallback_mode: &FallbackMode) -> Self {
        match fallback_mode {
            FallbackMode::ReturnDefault => Self::ReturnDefault,
            FallbackMode::CallOriginal => Self::CallOriginal,
            FallbackMode::Panic => panic!("No mock implementation found for {}", M::NAME),
        }
    }
}

enum ImplStorage {
    ReturnDefault,
    CallOriginal,
    Mock(Box<dyn std::any::Any + Send + Sync + 'static>),
}

pub struct MockImpl<M: Mock> {
    candidates: Vec<MockCandidate<M>>,
}

impl<M: Mock> MockImpl<M> {
    pub fn invoke<'i>(&self, args: M::Args<'i>) -> M::Output {
        if self.candidates.is_empty() {
            panic!("No registered mock implementation for {}", M::NAME);
        }

        for candidate in self.candidates.iter() {
            if let Some(arg_matcher) = candidate.arg_matcher.as_ref() {
                if !arg_matcher(&args) {
                    continue;
                }
            }

            if let Some(answer_factory) = candidate.answer_factory.as_ref() {
                return answer_factory(args);
            } else {
                panic!("No answer for call to {}(..)", M::NAME);
            }
        }

        panic!("No matching mocks for call to {}(..)", M::NAME);
    }
}

pub(crate) struct MockCandidate<M: Mock> {
    pub(crate) arg_matcher: Option<Box<dyn (for<'i> Fn(&M::Args<'i>) -> bool) + Send + Sync>>,
    pub(crate) answer_factory:
        Option<Box<dyn (for<'i> Fn(M::Args<'i>) -> M::Output) + Send + Sync>>,
}
