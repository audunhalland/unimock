//!
//! `unimock` is a library that makes it easy to create mock objects that implement _multiple traits_ at the same time.
//!
//! unimock exports a single type, [Unimock], that will implement all your annotated traits:
//!
//! ```rust
//! #![feature(generic_associated_types)]
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
//! `unimock` also works with `async_trait`:
//!
//! ```rust
//! #![feature(generic_associated_types)]
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

/// Various builder-like types for composing mock behaviour on functions.
pub mod builders;

#[doc(hidden)]
pub mod mock;

mod counter;

use std::any::TypeId;
use std::collections::HashMap;
use std::sync::atomic::AtomicBool;

///
/// Autogenerate mocks for all methods in the annotated traits, and `impl` it for [Unimock].
///
/// Mock generation happens by declaring a new [Api]-implementing struct for each method.
///
/// Example
/// ```rust
/// #![feature(generic_associated_types)]
/// use unimock::*;
///
/// #[unimock]
/// trait Trait1 {
///     fn a(&self) -> i32;
///     fn b(&self) -> i32;
/// }
///
/// #[unimock]
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
pub use unimock_macros::unimock;

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

#[derive(Clone, Copy)]
enum FallbackMode {
    Error,
    Fallthrough,
}

/// Unimock's purpose is to be an implementor of downstream traits via mock objects.
/// A single mock object provides the implementation of a single trait method.
///
/// The interaction with these mock objects always happen via the Unimock facade and
/// the traits that it implements.
pub struct Unimock {
    fallback_mode: FallbackMode,
    impls: HashMap<TypeId, mock::DynImpl>,
    has_panicked: AtomicBool,
}

impl Unimock {
    /// Create a new, empty Unimock. Attempting to call implemented traits on an empty instance will panic at runtime.
    pub fn empty() -> Self {
        Self {
            fallback_mode: FallbackMode::Error,
            impls: HashMap::new(),
            has_panicked: AtomicBool::new(false),
        }
    }

    /// Extend this instance with a mock of another [Api].
    pub fn also<A: Api + 'static>(
        mut self,
        _: A,
        setup: impl FnOnce(&mut builders::Each<A>),
    ) -> Self {
        let mut each = builders::Each::new();
        setup(&mut each);

        self.impls.insert(
            TypeId::of::<A>(),
            mock::DynImpl(Box::new(mock::MockImpl::from_each(each))),
        );
        self
    }

    /// Change unregistered [Api] fallback behaviour from explicitly panicking to trying to call archetypes.
    pub fn otherwise_invoke_archetypes(mut self) -> Self {
        self.fallback_mode = FallbackMode::Fallthrough;
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
            fallback_mode: FallbackMode::Fallthrough,
            impls: HashMap::new(),
            has_panicked: AtomicBool::new(false),
        }
    }

    pub(crate) fn with_single_mock(type_id: TypeId, dyn_impl: mock::DynImpl) -> Self {
        Self {
            fallback_mode: FallbackMode::Error,
            impls: [(type_id, dyn_impl)].into(),
            has_panicked: AtomicBool::new(false),
        }
    }

    /// Perform function-application against some [Api].
    pub fn apply<'i, A: Api + 'static>(&'i self, inputs: A::Inputs<'i>) -> mock::Outcome<'i, A> {
        match mock::apply(
            self.impls.get(&TypeId::of::<A>()),
            inputs,
            self.fallback_mode,
        ) {
            Ok(outcome) => outcome,
            Err(message) => {
                self.has_panicked
                    .store(true, std::sync::atomic::Ordering::Relaxed);
                panic!("{}", message);
            }
        }
    }
}

impl Drop for Unimock {
    fn drop(&mut self) {
        // skip verification if panic already occured.
        if self.has_panicked.load(std::sync::atomic::Ordering::Relaxed) || std::thread::panicking()
        {
            return;
        }

        let mut errors = Vec::new();
        for (_, dyn_impl) in self.impls.iter() {
            dyn_impl.0.verify(&mut errors);
        }

        if !errors.is_empty() {
            let error_string = errors.join("\n");
            panic!("{}", error_string);
        }
    }
}

///
/// Trait describing some functional API for which unimock may provide implementation.
/// _Inversion of Control_ in Rust is achieved through method dispatch, so the items described
/// will usually be _methods_ in _traits_.
///
/// As `Api` is a trait itself, it needs to be implemented to be useful. Because trait methods
/// are not types, a _surrogate_ need to be introduced:
///
/// ```rust
/// trait Mockable {
///     fn method(&self);
/// }
///
/// // The method can be referred to via the following empty surrogate struct:
/// struct Mockable__method;
///
/// /* impl Api for Mockable_method ... */
/// ```
///
pub trait Api: Sized {
    /// The inputs to the function.
    type Inputs<'i>;

    /// The output of the function.
    type Output;

    /// The number of inputs.
    const N_INPUTS: u8;

    /// The name to use for runtime errors.
    const NAME: &'static str;
}

/// [Api] that has an _archetypical implementation_.
/// TODO: Find better name
///
/// An archetypal implementation is a free-standing function, not part of a trait,
/// where the first parameter is generic (`self`-replacement), and the rest of the parameters are
/// identical to [Api::Inputs]:
///
/// ```rust
/// # #![feature(generic_associated_types)]
/// # use unimock::*;
/// #[unimock(archetypes=[my_archetype])]
/// trait DoubleNumber {
///     fn double_number(&self, a: i32) -> i32;
/// }
///
/// // A _real function_ which performs number doubling!
/// fn my_archetype<T>(_: T, a: i32) -> i32 {
///     a * 2
/// }
/// ```
///
/// Archetypal functions make sense when the reason to define a mockable trait
/// is _solely_ for the purpose of inversion-of-control at test-time: Release code
/// need only one way to double a number.
///
/// Free-standing archetypal functions enables arbitrarily deep integration testing
/// in unimock-based application architectures. When unimock calls an archetypal,
/// it inserts itself as the generic first parameter. When this parameter is
/// bounded by traits, the archetype `fn` is given capabilities to call other APIs,
/// though only indirectly. Each method invocation happening during a test will invisibly pass
/// through unimock, resulting in a great level of control. Consider:
///
/// ```rust
/// # #![feature(generic_associated_types)]
/// # use unimock::*;
/// #[unimock(archetypes=[my_factorial])]
/// trait Factorial {
///     fn factorial(&self, input: u32) -> u32;
/// }
///
/// // will it eventually panic?
/// fn my_factorial(f: &impl Factorial, input: u32) -> u32 {
///     f.factorial(input - 1) * input
/// }
///
/// assert_eq!(
///     120,
///     // well, not in the test, at least!
///     mock(Factorial__factorial, |each| {
///         each.call(matching!((input) if *input <= 1)).returns(1u32); // unimock controls the API call
///         each.call(matching!(_)).invokes_archetype();
///     })
///     .factorial(5)
/// );
/// ```
///
pub trait Archetypal: Api {}

/// Mock a single [Api].
pub fn mock<A: Api + 'static>(_: A, setup: impl FnOnce(&mut builders::Each<A>)) -> Unimock {
    let mut each = builders::Each::new();
    setup(&mut each);
    Unimock::with_single_mock(
        TypeId::of::<A>(),
        mock::DynImpl(Box::new(mock::MockImpl::from_each(each))),
    )
}

/// Conveniently leak some value to produce a static reference.
pub trait Leak {
    fn leak(self) -> &'static Self;
}

impl<T: 'static> Leak for T {
    fn leak(self) -> &'static Self {
        Box::leak(Box::new(self))
    }
}

///
/// Internal trait implemented by references that allows transforming from `&T` to `&'static T`
/// by leaking memory.
///
/// The trait is implemented for all `&T`. It allows functions to refer to the non-referenced owned value `T`,
/// and leak that.
///
pub trait LeakInto {
    type Owned: 'static;

    fn leak_into(value: Self::Owned) -> Self;
}

impl<T: 'static> LeakInto for &T {
    type Owned = T;

    fn leak_into(value: Self::Owned) -> Self {
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
