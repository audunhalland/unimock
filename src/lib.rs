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
mod counter;

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
    Panic,
    ReturnDefault,
    CallOriginal,
}

impl FallbackMode {
    fn union(self, other: FallbackMode) -> Self {
        match (self, other) {
            (Self::Panic, other) => other,
            (other, Self::Panic) => other,
            (_, other) => other,
        }
    }
}

/// Unimock stores a collection of mock objects, with the end goal of implementing
/// all the mocked traits. The trait implementaion is achieved through using the [unimock] macro.
pub struct Unimock {
    fallback_mode: FallbackMode,
    mocks: HashMap<TypeId, Box<dyn Any + Send + Sync>>,
    impls: HashMap<TypeId, DynImpl>,
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

    ///
    /// Compose a new unimock by consuming an array of simpler unimocks by a union operation.
    /// The passed unimocks
    ///
    pub fn union<const N: usize>(unimocks: [Unimock; N]) -> Self {
        let mut impls = HashMap::new();
        let mut fallback_mode = FallbackMode::Panic;

        for unimock in unimocks.into_iter() {
            fallback_mode = fallback_mode.union(unimock.fallback_mode);
            impls.extend(unimock.impls);
        }

        Self {
            fallback_mode,
            mocks: HashMap::new(),
            impls,
        }
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

impl Default for Unimock {
    fn default() -> Self {
        Unimock {
            fallback_mode: FallbackMode::ReturnDefault,
            mocks: HashMap::new(),
            impls: HashMap::new(),
        }
    }
}

///
/// Trait describing a single mockable item.
/// The trait needs to be implemented by some type. That type will be defined
/// by the macro, and will act as the entrypoint for configuring the mock.
///
pub trait Mock: Sized {
    /// The direct inputs to the mock function
    type Inputs<'i>;
    /// The inputs as references for matching
    type InputRefs<'i>;
    /// The output of the mock function
    type Output;

    const NAME: &'static str;

    fn input_refs<'i, 'o>(inputs: &'o Self::Inputs<'i>) -> Self::InputRefs<'o>;

    fn mock<F>(self, f: F) -> Unimock
    where
        Self: 'static,
        F: FnOnce(&mut Each<Self>),
    {
        let mut each = Each::<Self>::new();
        f(&mut each);

        Unimock {
            fallback_mode: FallbackMode::Panic,
            mocks: HashMap::new(),
            impls: [(
                TypeId::of::<Self>(),
                DynImpl::Mock(Box::new(each.to_mock_impl())),
            )]
            .into(),
        }
    }
}

pub trait ReturnDefault {
    fn return_default() -> Unimock
    where
        Self: 'static,
    {
        Unimock {
            fallback_mode: FallbackMode::Panic,
            mocks: HashMap::new(),
            impls: [(TypeId::of::<Self>(), DynImpl::ReturnDefault)].into(),
        }
    }
}

pub trait CallOriginal {
    // Call the original implementation of this API
    fn call_original() -> Unimock
    where
        Self: 'static,
    {
        Unimock {
            fallback_mode: FallbackMode::Panic,
            mocks: HashMap::new(),
            impls: [(TypeId::of::<Self>(), DynImpl::CallOriginal)].into(),
        }
    }
}

pub enum Impl<'s, M: Mock + 'static> {
    Mock(&'s MockImpl<M>),
    ReturnDefault,
    CallOriginal,
}

impl<'s, M: Mock + 'static> Impl<'s, M> {
    fn from_storage(storage: &'s DynImpl) -> Self {
        match storage {
            DynImpl::ReturnDefault => Self::ReturnDefault,
            DynImpl::CallOriginal => Self::CallOriginal,
            DynImpl::Mock(any) => {
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

enum DynImpl {
    Mock(Box<dyn std::any::Any + Send + Sync + 'static>),
    ReturnDefault,
    CallOriginal,
}

pub struct MockImpl<M: Mock> {
    patterns: Vec<CallPattern<M>>,
}

impl<M: Mock> MockImpl<M> {
    pub fn invoke<'i>(&'i self, inputs: M::Inputs<'i>) -> M::Output {
        if self.patterns.is_empty() {
            panic!("No registered call patterns for {}", M::NAME);
        }

        {
            let input_refs = M::input_refs(&inputs);
        }

        for pattern in self.patterns.iter() {
            if let Some(arg_matcher) = pattern.arg_matcher.as_ref() {
                if !arg_matcher(&inputs) {
                    continue;
                }
            }

            pattern.call_counter.tick();

            if let Some(output_factory) = pattern.output_factory.as_ref() {
                return output_factory(inputs);
            } else {
                panic!(
                    "No output available for matching call to {}[#{}]",
                    M::NAME,
                    pattern.pat_index
                );
            }
        }

        panic!("No matching mocks for call to {}(..)", M::NAME);
    }
}

pub(crate) struct CallPattern<M: Mock> {
    pat_index: usize,
    pub arg_matcher: Option<Box<dyn (for<'i> Fn(&M::Inputs<'i>) -> bool) + Send + Sync>>,
    pub call_counter: counter::CallCounter,
    pub output_factory: Option<Box<dyn (for<'i> Fn(M::Inputs<'i>) -> M::Output) + Send + Sync>>,
}

impl<M: Mock> Drop for CallPattern<M> {
    fn drop(&mut self) {
        self.call_counter.verify(M::NAME, self.pat_index);
    }
}

///
/// Macro to ease argument matching.
/// This macro will produce a closure expression to use for mock matching.
///
#[macro_export]
macro_rules! matching {
    // Special syntax for matching several arguments without requiring tuple syntax:
    // `matching!("a", "b")` becomes `matching!(("a", "b"))`:
    ($arg0:pat_param, $( $argn:pat_param ),*) => {
        matching!(($arg0, $( $argn ),*))
    };
    ($(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        |args| match *args {
            $( $pattern )|+ $( if $guard )? => true,
            _ => false
        }
    };
}
