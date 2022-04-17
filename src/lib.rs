//!
//! `unimock` is a library that makes it easy to create mock objects that implement _multiple traits_ at the same time.
//!
//! unimock exports a single type, [Unimock], that will implement all your annotated traits:
//!
//! ```rust
//! #![feature(generic_associated_types)]
//! use unimock::*;
//! #[unimock_next]
//! trait Foo {
//!     fn foo(&self) -> i32;
//! }
//!
//! #[unimock_next]
//! trait Bar {
//!     fn bar(&self) -> i32;
//! }
//!
//! fn sum(foobar: impl Foo + Bar) -> i32 {
//!     foobar.foo() + foobar.bar()
//! }
//!
//! /*
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
//! */
//!
//! fn test_next() {
//!     assert_eq!(
//!         42,
//!         sum(
//!             mock(Foo__foo, |each| {
//!                 each.call(matching!()).returns(40);
//!             })
//!             .also(Bar__bar, |each| {
//!                 each.call(matching!()).returns(2);
//!             })
//!         )
//!     );
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
//! #![feature(generic_associated_types)]
//! use unimock::*;
//! use async_trait::*;
//! #[unimock_next]
//! #[async_trait]
//! trait Baz {
//!     async fn baz(&self, arg: String) -> i32;
//! }
//! ```

#![forbid(unsafe_code)]
// For the mock-fn feature:
#![feature(generic_associated_types)]

pub mod builders;

#[doc(hidden)]
pub mod mock;

mod counter;

use std::any::TypeId;
use std::collections::HashMap;
use std::sync::atomic::AtomicUsize;

///
/// Autogenerate mocks for all methods in the annotated traits, and `impl` it for [Unimock].
///
/// Mock generation happens by declaring a new [Mock]-implementing struct for each method.
///
/// Example
/// ```rust
/// #![feature(generic_associated_types)]
/// use unimock::*;
///
/// #[unimock_next]
/// trait Trait1 {
///     fn a(&self) -> i32;
///     fn b(&self) -> i32;
/// }
///
/// #[unimock_next]
/// trait Trait2 {
///     fn c(&self) -> i32;
/// }
///
/// fn sum(obj: impl Trait1 + Trait2) -> i32 {
///     obj.a() + obj.b() + obj.c()
/// }
///
/// fn test() {
///     let single_mock: Unimock = mock(Trait1__a, |_| {});
///     sum(single_mock); // note: panics at runtime!
/// }
/// ```
pub use unimock_macros::unimock_next;

///
/// Macro to ease argument pattern matching.
/// This macro produces a closure expression suitable for passing to [builders::Each::call].
///
/// Takes inspiration from [std::matches] and works similarly, except that the value to match
/// can be removed as a macro argument, since it is instead received as the closure argument.
///
/// Unimock uses tuples to represent multiple arguments. A single argument is not a tuple.
/// To avoid the extra set of parentheses for simple multi-argument matchers, there is
/// a special syntax that accepts several top-level patterns:
/// `matching!("a", "b")` will expand to `matching!(("a", "b"))`.
///
/// # Example
///
/// ```rust
/// # use unimock::*;
///
/// fn one_str() {
///     fn args(_: impl Fn(&(&str)) -> bool) {}
///     args(matching!("a"));
/// }
///
/// fn three_strs() {
///     fn args(_: impl Fn(&(&str, &str, &str)) -> bool) {}
///     args(matching!("a", _, "c"));
///     args(matching!(("a", "b", "c") | ("d", "e", "f")));
///     args(matching!(("a", b, "c") if b.contains("foo")));
/// }
/// ```
///
/// # Auto-"coercions"
///
/// Since the input expression being matched is generated by the macro, you would
/// normally suffer from the following problem when matching some non-`&str` function input:
///
/// ```compile_fail
/// # fn test() -> bool {
/// let string = String::new();
/// match &string {
///     "foo" => true, // expected struct `String`, found `str`
///     _ => false,
/// # }
/// }
/// ```
///
/// To help ergonomics, the `matching` macro recognizes certain literals used in the
/// patterns, and performs appropriate type conversion at the correct places:
///
/// ```rust
/// # use unimock::*;
/// struct Newtype(String);
///
/// fn exotic_strings() {
///     fn args(_: impl Fn(&(String, std::borrow::Cow<'static, str>, Newtype, i32)) -> bool) {}
///     args(matching!(("a", _, "c", _) | (_, "b", _, 42)));
/// }
///
/// // Newtype works by implementing the following:
/// impl std::convert::AsRef<str> for Newtype {
///     fn as_ref(&self) -> &str {
///         self.0.as_str()
///     }
/// }
/// ```
///
/// Internally it works by calling [as_str_ref] on inputs matched by a string literal.
pub use unimock_macros::matching;

enum FallbackMode {
    Panic,
    CallOriginal,
}

/// Unimock's purpose is to be an implementor of downstream traits via mock objects.
/// A single mock object provides the implementation of a single trait method.
///
/// The interaction with these mock objects always happen via the Unimock facade and
/// the traits that it implements.
pub struct Unimock {
    fallback_mode: FallbackMode,
    impls: HashMap<TypeId, mock::DynImpl>,
}

impl Unimock {
    /// Create a new, empty Unimock. Attempting to call implemented traits on an empty instance will panic at runtime.
    pub fn empty() -> Self {
        Self {
            fallback_mode: FallbackMode::Panic,
            impls: HashMap::new(),
        }
    }

    /// Extend this unimock with another API mock.
    pub fn also<M, F>(mut self, _: M, f: F) -> Self
    where
        M: Mock + 'static,
        F: FnOnce(&mut builders::Each<M>),
    {
        let mut each = builders::Each::new();
        f(&mut each);

        self.impls.insert(
            TypeId::of::<M>(),
            mock::DynImpl::Mock(Box::new(mock::MockImpl::from_each(each))),
        );
        self
    }

    /// Create a unimock that instead of trying to mock, tries to call some original implementation of any API.
    ///
    /// What is considered an original implementation, is not something that unimock concerns itself with,
    /// the behaviour is completely customized for each trait and it is also opt-in.
    ///
    /// If some implementation has no way to call an "original implementation", it should panic.
    ///
    /// A call-original unimock may be `union`ed together with normal mocks, effectively
    /// creating a mix of real and mocked APIs, allowing deeper tests than mock-only mode can provide.
    pub fn call_original() -> Self {
        Self {
            fallback_mode: FallbackMode::CallOriginal,
            impls: HashMap::new(),
        }
    }

    pub(crate) fn with_single_mock(type_id: TypeId, dyn_impl: mock::DynImpl) -> Self {
        Self {
            fallback_mode: FallbackMode::Panic,
            impls: [(type_id, dyn_impl)].into(),
        }
    }

    /// Look up a stored mock object and expose it as a dynamic implementation
    /// of a function. The implementation can be one of two types: A mock and an
    /// instruction to call a real implementation.
    pub fn get_impl<'s, M: Mock + 'static>(&'s self) -> mock::Impl<'s, M> {
        self.impls
            .get(&TypeId::of::<M>())
            .map(mock::Impl::from_storage)
            .unwrap_or_else(|| mock::Impl::from_fallback(&self.fallback_mode))
    }
}

///
/// Trait describing a single mockable function interface.
///
/// To be useful, traits need to be implemented by types. But things we want to
/// mock are _functions_, not types. Unimock works by defining an empty struct
/// that _represents_ some trait method:
///
/// ```rust
/// trait Mockable {
///     fn method(&self);
/// }
///
/// // The method can be referred to via the following empty struct:
/// struct Mockable_method;
///
/// /* impl Mock for Mockable_method ... */
/// ```
///
pub trait Mock: Sized {
    /// The direct inputs to the mock function.
    type Inputs<'i>;

    /// The output of the mock function.
    type Output;

    /// The number of inputs.
    const N_INPUTS: u8;

    /// The name to use for runtime errors.
    const NAME: &'static str;
}

/// Mock some mockable API.
#[inline]
pub fn mock<M, F>(_: M, f: F) -> Unimock
where
    M: Mock + 'static,
    F: FnOnce(&mut builders::Each<M>),
{
    let mut each = builders::Each::new();
    f(&mut each);
    Unimock::with_single_mock(
        TypeId::of::<M>(),
        mock::DynImpl::Mock(Box::new(mock::MockImpl::from_each(each))),
    )
}

pub trait CallOriginal {
    // Call the original implementation of this API
    fn call_original() -> Unimock
    where
        Self: 'static,
    {
        Unimock {
            fallback_mode: FallbackMode::Panic,
            impls: [(TypeId::of::<Self>(), mock::DynImpl::CallOriginal)].into(),
        }
    }
}

///
/// Internal trait implemented by references that allows transforming from `&T` to `&'static T`
/// by leaking memory.
///
/// The trait is implemented for all `&T`. It allows functions to refer to the non-referenced owned value `T`,
/// and leak that.
///
pub trait LeakOutput {
    type Owned: 'static;

    fn leak(value: Self::Owned) -> Self;
}

impl<T: 'static> LeakOutput for &T {
    type Owned = T;

    fn leak(value: Self::Owned) -> Self {
        Box::leak(Box::new(value))
    }
}

/// Convert any type implementing `AsRef<str>` to a `&str`.
/// Used by [matching].
pub fn as_str_ref<T>(input: &T) -> &str
where
    T: AsRef<str>,
{
    input.as_ref()
}
