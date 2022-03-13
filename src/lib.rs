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

/// Unimock stores a collection of mock objects, with the end goal of implementing
/// all the mocked traits. The trait implementaion is achieved through using the [unimock] macro.
pub struct Unimock {
    mocks: HashMap<TypeId, Box<dyn Any + Send + Sync>>,
}

impl Unimock {
    /// Create a new, empty Unimock. Attempting to call implemented traits on an empty instance will panic at runtime.
    pub fn new() -> Self {
        Self {
            mocks: HashMap::new(),
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

    /// Get a specific mock created with `mock`. Panics at runtime if the type is not registered.
    pub fn get<T: std::any::Any>(&self, trait_name: &'static str) -> &T {
        self.mocks
            .get(&TypeId::of::<T>())
            .and_then(|any| any.downcast_ref())
            .unwrap_or_else(|| panic!("{}", self.missing_trait_error(trait_name)))
    }

    fn missing_trait_error(&self, trait_name: &'static str) -> String {
        format!("Missing mock for trait {trait_name}")
    }
}
