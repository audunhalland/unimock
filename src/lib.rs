//!
//! `unimock` is a library for defining _mock implementations_ of traits.
//!
//! Mocking, in a broad sense, is a way to control API behaviour during test execution.
//!
//! The _uni_ in unimock indicates one-ness: All mockable traits are implemented by a single type, [Unimock].
//! This design allows for a great flexibility in coding style, as will be demonstrated further down.
//!
//! The first code example is the smallest possible use of unimock:
//!
//! ```rust
//! # #![feature(generic_associated_types)]
//! use unimock::*;
//!
//! #[unimock]
//! trait Foo {}
//!
//! fn takes_foo(foo: impl Foo) {}
//!
//! takes_foo(mock(None));
//! ```
//!
//! 1. `trait Foo` is declared with a [`#[unimock]`](unimock) annotation which makes its behaviour mockable.
//! 2. `fn takes_foo` accepts some type that implements the trait. This function adheres to zero-cost _Inversion of Control/Dependency Inversion_.
//! 3. A mock instantiation by calling [`mock(None)`](mock()), which returns a [Unimock] value which is passed into `takes_foo`.
//!
//! The [mock()] function takes an argument, in this case the value `None`. The argument is _what behaviour are we mocking_, in this case [None] at all!
//! `Foo` contains no methods, so there is no behaviour to mock.
//!
//! # Methods and behaviour mocking
//!
//! In order to be somewhat useful, the traits we abstract over should contain some methods. In a unit test for some function, we'd like
//! to mock the behaviour of that function's dependencies (expressed as trait bounds).
//!
//! [mock()] accepts a collection of [Clause]s. Clauses carry the full recipe on how Unimock will behave once instantiated.
//!
//! Given some trait,
//!
//! ```rust
//! # #![feature(generic_associated_types)]
//! # use unimock::*;
//! #[unimock]
//! trait Foo {
//!     fn foo(&self) -> i32;
//! }
//! ```
//!
//! we would like to tell unimock what `Foo::foo`'s behaviour will be, i.e. what it will return. In order to do that, we first need to refer to the method.
//! In Rust, trait methods aren't reified entities, they are not types nor values, so they cannot be referred to in code.
//! Therefore, the unimock macro creates a surrogate type to represent it. By default, this type will be called
//!
//! `Foo__foo`.
//!
//! This type will implement [MockFn], which is the entrypoint for creating clauses:
//!
//! ```rust
//! # #![feature(generic_associated_types)]
//! # use unimock::*;
//! #[unimock]
//! trait Foo {
//!     fn foo(&self) -> i32;
//! }
//!
//! fn test_me(foo: impl Foo) -> i32 {
//!     foo.foo()
//! }
//!
//! let clause = Foo__foo::each_call(matching!()).returns(1337).in_any_order();
//!
//! assert_eq!(1337, test_me(mock(Some(clause))));
//! ```
//!
//! [Clause] construction is a type-state machine that in this example goes through 3 steps:
//!
//! 1. `Foo__foo::each_call(matching!())`: Define a _call pattern_. Each call to `Foo::foo` that matches the empty argument list (i.e. always matching, since the method is paremeter-less)
//! 2. `.returs(1337)`: Each matching call will return the value `1337`
//! 3. `.in_any_order()`: this directive describes how the resulting Clause behaves in relation to other clauses in the behaviour description, and returns it. In this example there is only one clause.
//!
//! ### Call patterns (matching inputs)
//!
//! It is common to want to control how a function will respond in relation to what input is given to it!
//! Inputs are matched by a function that receives the inputs as a tuple, and returns whether it matched as a [bool].
//! A specific [MockFn] together with an input matcher is referred to as a _call pattern_ from now on.
//!
//! The [matching] macro provides syntax sugar for argument matching. It has a syntax inspired by the [std::matches] macro.
//!
//! Inputs being matched is a condition that needs to be fulfilled in order for the rest of the call pattern to be evaluated.
//!
//! ### Specifying outputs
//! Specifying outputs can be done in several ways. The simplest one is `returns(something)`. Different ways of specifying outputs are
//! found in [build::Match].
//!
//! # Combining clauses
//! [mock()] accepts as argument anything that can be converted to a clause iterator, so that you can specify more that one kind of behaviour!
//! An iterator has a specific order of items, and sometimes the order of clauses matters too. It will depend on the type of clause.
//!
//! Other mocking libraries often have distinctions between several kinds of "test doubles". Terminology varies. Unimock uses this terminology:
//!
//! * _Mock_: A test double where every valid interaction must be declared up front.
//! * _Spy_: A test double which behaves as release code, unless behaviour is overridden.
//! * _Stub_: Defined behaviour for a single function, where the order of calls does not matter.
//!
//! Now that terminology is in place for unimock, let's look at various ways to combine clauses.
//!
//! ```rust
//! # #![feature(generic_associated_types)]
//! # use unimock::*;
//! #[unimock]
//! trait Foo {
//!     fn foo(&self, arg: i32) -> i32;
//! }
//!
//! #[unimock]
//! trait Bar {
//!     fn bar(&self, arg: i32) -> i32;
//! }
//!
//! fn test_me(deps: &(impl Foo + Bar), arg: i32) -> i32 {
//!     deps.bar(deps.foo(arg))
//! }
//!
//! assert_eq!(
//!     42,
//!     test_me(
//!         &mock([
//!             Foo__foo::each_call(matching!(_))
//!                 .answers(|arg| arg * 3)
//!                 .in_any_order(),
//!             Bar__bar::each_call(matching!((arg) if *arg > 20))
//!                 .answers(|arg| arg * 2)
//!                 .in_any_order(),
//!         ]),
//!         7
//!     )
//! );
//!
//! // alternatively, define _stubs_ for each method.
//! // This is a nice way to group methods by introducing a closure scope:
//! assert_eq!(
//!     42,
//!     test_me(
//!         &mock([
//!             Foo__foo::stub(|each| {
//!                 each.call(matching!(1337)).returns(1024);
//!                 each.call(matching!(_)).answers(|arg| arg * 3);
//!             }),
//!             Bar__bar::stub(|each| {
//!                 each.call(matching!((arg) if *arg > 20)).answers(|arg| arg * 2);
//!             }),
//!         ]),
//!         7
//!     )
//! );
//! ```
//!
//! In both these examples, the order in which the clauses are specified do not matter, _except for input matching_.
//! In order for unimock to find the correct response, call patterns will be matched in the sequence they were defined.
//!
//! # Interaction verifications
//!
//! Unimock has one built-in verification that is always enabled:
//!
//! _Every [MockFn] that is introduced in some clause, *must* be called at least once._
//!
//! If this requirement is not met, Unimock will panic inside its Drop implementation. The reason
//! is to help avoiding "bit rot" accumulating over time inside test code. When refactoring release
//! code, tests should always follow along and not be overly generic.
//!
//! Every unimock verification happens automatically in [Unimock::drop].
//!
//! ### Optional call count expectations in call patterns
//! To make a call count expectation for a specific call pattern, look at [build::QuantifyResponse], which
//! has methods like `once()`, `n_times(n)` and `at_least_times(n)`.
//!
//! With exact quantification in place, we can produce output sequences by chaining output definitions:
//!
//! ```no_compile
//! each.call(matching!(_)).returns(1).n_times(2).then().returns(2);
//! ```
//!
//! The output sequence will be `[1, 1, 2, 2, 2, ..]`. A call pattern like this is _expected_ to be called at least 3 times.
//! 2 times because of the first exact output sequence, then at least one time because of the `.then()` combinator.
//!
//! ### Verifying exact sequence of calls
//! Exact call sequences may be expressed using _strictly ordered clauses_. Use [MockFn::next_call] to define a call pattern, and
//! [build::QuantifiedResponse::in_order] to make it into a clause.
//!
//! ```no_compile
//! mock([
//!     Foo_foo::next_call(matching!(3)).returns(5).once().in_order(),
//!     Bar_bar::next_call(matching!(8)).returns(7).n_times(2).in_order(),
//! ]);
//! ```
//!
//! Order-sensitive clauses and order-insensitive clauses (like `::stub`) do not interfere with each other.
//! However, these kinds of clauses cannot be combined _for the same MockFn_ in a single Unimock value.
//!
//! # Application architecture
//!
//! Writing larger, testable applications with unimock requires some degree of architectural discipline. We already know
//! how to specify dependencies using trait bounds. But would this scale in practice when several layers are involved?
//! One of the main features of unimock is that all traits are implemented by [Unimock]. This means that trait bounds
//! can be composed, and we can use _one value_ that implements all our dependencies:
//!
//! ```rust
//! # trait A {}
//! # trait B {}
//! # trait C {}
//! fn some_function(deps: &(impl A + B + C), arg: i32) {
//!     // ..
//! }
//! ```
//!
//! In a way, this function resembles a `self`-receiving function. The `deps` argument is how the function
//! abstracts over its dependencies. Let's keep this call convention and let it scale a bit by introducing two layers:
//!
//! ```rust
//! use std::any::Any;
//!
//! trait A {
//!     fn a(&self, arg: i32) -> i32;
//! }
//!
//! trait B {
//!     fn b(&self, arg: i32) -> i32;
//! }
//!
//! fn a(deps: &impl B, arg: i32) -> i32 {
//!     deps.b(arg) + 1
//! }
//!
//! fn b(deps: &impl Any, arg: i32) -> i32 {
//!     arg + 1
//! }
//! ```
//!
//! The dependency from `fn a` to `fn b` is completely abstracted away, and in test mode
//! the `deps: &impl X` gets substituted with `deps: &Unimock`. Unimock is only concerned with the
//! testing side of the picture. To wire all of this together to a full-fledged runtime solution
//! without too much boilerplate, reach for the _[entrait pattern](https://docs.rs/entrait)_.
//!
//! ### Combining release code and mocks: Spying
//! Unimock can be used to create arbitrarily deep integration tests, mocking away layers only indirectly used.
//! For that to work, unimock needs to know how to call the "real" implementation of traits.
//!
//! See the documentation of [Unmock] and [spy] to see how this works.
//!
//! Although this can be implemented with unimock directly, it works best with a higher-level
//! macro like [entrait](https://docs.rs/entrait).
//!
//! # Misc
//! Unimock works best with high-level abstractions over function calls. It does not work that well
//! with generic traits or traits with associated types.

#![forbid(unsafe_code)]
#![warn(missing_docs)]
// For the mock-fn feature:
#![feature(generic_associated_types)]

/// Types used for building and defining mock behaviour.
pub mod build;
/// APIs used by macros, not intended to be used directly.
pub mod macro_api;
/// Traits and types used for describing the properties of various mock types.
pub mod property;

mod counter;
mod error;
mod mock;

use std::any::TypeId;
use std::collections::HashMap;
use std::panic::RefUnwindSafe;
use std::sync::atomic::{AtomicBool, AtomicUsize};
use std::sync::{Arc, Mutex};

///
/// Autogenerate mocks for all methods in the annotated traits, and `impl` it for [Unimock].
///
/// Mock generation happens by declaring a new [MockFn]-implementing struct for each method.
///
/// # Example
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
///     // Unimock now implements both traits:
///     sum(mock(None)); // note: panics at runtime!
///
///     // Mock a single method (still panics, because all 3 must be mocked:):
///     sum(mock(Some(Trait1__a::next_call(|_| true).returns(0).once().in_order())));
/// }
/// ```
///
/// # Arguments
/// The unimock macro accepts a number of comma-separated key-value configuration parameters:
///
/// * `#[unimock(mod=ident)]`: Puts the [MockFn] types in a new module named `ident`.
/// * `#[unimock(as=[a, b, c])]`: Given there are e.g. 3 methods in the annotated trait, assigns the names `a`, `b` and `c` for the [MockFn] types respectively, in the same order as the trait methods.
/// * `#[unimock(unmock=[a, b, _])`: Given there are e.g. 3 methods in the annotated trait, uses the given paths as unmock implementations. The functions are assigned to the methods
///   in the same order as the methods are listed in the trait. A value of `_` means _no unmock support_ for that method. See [Unmock](crate::Unmock) for more information.
pub use unimock_macros::unimock;

///
/// Macro to ease argument pattern matching.
/// This macro produces a closure expression suitable for passing to [build::Each::call].
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
/// Internally it works by calling [macro_api::as_str_ref] on inputs matched by a string literal.
pub use unimock_macros::matching;

#[derive(Clone, Copy)]
enum FallbackMode {
    Error,
    Unmock,
}

/// Unimock's purpose is to be an implementor of downstream traits via mock objects.
/// A single mock object provides the implementation of a single trait method.
///
/// The interaction with these mock objects always happen via the Unimock facade and
/// the traits that it implements.
pub struct Unimock {
    fallback_mode: FallbackMode,
    original_instance: bool,
    state: Arc<SharedState>,
}

struct SharedState {
    impls: HashMap<TypeId, mock::DynMockImpl>,
    next_call_index: AtomicUsize,
    panic_reasons: Mutex<Vec<error::MockError>>,
}

impl Unimock {
    /// Evaluate a [MockFn] given some inputs, to produce its output.
    pub fn eval<'i, 's: 'i, F: MockFn + 'static>(
        &'s self,
        inputs: F::Inputs<'i>,
        inputs_debug: String,
    ) -> macro_api::Evaluation<'i, F::Output, F>
    where
        F::Output: Sized,
    {
        match mock::eval_sized::<F>(
            self.state.impls.get(&TypeId::of::<F>()),
            inputs,
            inputs_debug,
            &self.state.next_call_index,
            self.fallback_mode,
        ) {
            Ok(eval) => eval,
            Err(mock_error) => panic!("{}", self.prepare_panic(mock_error)),
        }
    }

    /// Evaluate a [MockFn] given some inputs, to produce its output, where output is borrowed from `self`.
    pub fn eval_borrowed<'i, 's: 'i, F: MockFn + 'static>(
        &'s self,
        inputs: F::Inputs<'i>,
        inputs_debug: String,
    ) -> macro_api::Evaluation<'i, &'s F::Output, F> {
        match mock::eval_unsized_self_borrowed::<F>(
            self.state.impls.get(&TypeId::of::<F>()),
            inputs,
            inputs_debug,
            &self.state.next_call_index,
            self.fallback_mode,
        ) {
            Ok(eval) => eval,
            Err(mock_error) => panic!("{}", self.prepare_panic(mock_error)),
        }
    }

    /// Evaluate a [MockFn] given some inputs, to produce its output, where output is a static reference to `F::Output`.
    pub fn eval_static_ref<'i, 's: 'i, F: MockFn + 'static>(
        &'s self,
        inputs: F::Inputs<'i>,
        inputs_debug: String,
    ) -> macro_api::Evaluation<'i, &'static F::Output, F> {
        match mock::eval_unsized_static_ref::<F>(
            self.state.impls.get(&TypeId::of::<F>()),
            inputs,
            inputs_debug,
            &self.state.next_call_index,
            self.fallback_mode,
        ) {
            Ok(eval) => eval,
            Err(mock_error) => panic!("{}", self.prepare_panic(mock_error)),
        }
    }

    fn prepare_panic(&self, error: error::MockError) -> String {
        let msg = error.to_string();

        let mut panic_reasons = self.state.panic_reasons.lock().unwrap();
        panic_reasons.push(error.clone());

        msg
    }
}

impl Clone for Unimock {
    fn clone(&self) -> Unimock {
        Unimock {
            fallback_mode: self.fallback_mode,
            original_instance: false,
            state: self.state.clone(),
        }
    }
}

impl Drop for Unimock {
    fn drop(&mut self) {
        // skip verification if not the original instance.
        if !self.original_instance {
            return;
        }

        // skip verification if already panicking in the original thread.
        if std::thread::panicking() {
            return;
        }

        let strong_count = Arc::strong_count(&self.state);

        if strong_count > 1 {
            panic!("Unimock cannot verify calls, because the original instance got dropped while there are clones still alive.");
        }

        fn panic_if_nonempty(errors: &[error::MockError]) {
            if errors.is_empty() {
                return;
            }

            let error_strings = errors
                .into_iter()
                .map(|err| err.to_string())
                .collect::<Vec<_>>();
            panic!("{}", error_strings.join("/n"));
        }

        {
            // if already panicked, it must be in another thread. Forward that panic to the original thread.
            // (if original is even still in the original thread.. But panic as close to the test "root" as possible)
            let panic_reasons = self.state.panic_reasons.lock().unwrap();
            panic_if_nonempty(&panic_reasons);
        }

        let mut mock_errors = Vec::new();
        for (_, dyn_impl) in self.state.impls.iter() {
            dyn_impl.verify(&mut mock_errors);
        }
        panic_if_nonempty(&mock_errors);
    }
}

///
/// The main trait used for unimock configuration.
///
/// `MockFn` describes functional APIs that may be called via dispatch, a.k.a. _Inversion of Control_.
/// Virtuality should be regarded as as test-time virtuality: A virtual function is either the real deal (see [Unmock]) OR it is mocked.
///
/// In Rust, the most convenient way to perform a virtualized/dispatched function call is to
/// call a trait method.
///
/// `MockFn` only provides metadata about an API, it is not directly callable.
///
/// As this is a trait itself, it needs to be implemented to be useful. Methods are not types,
/// so we cannot implement `MockFn` for those. But a surrogate type can be introduced:
///
/// ```rust
/// trait ILoveToMock {
///     fn method(&self);
/// }
///
/// // The method can be referred to via the following empty surrogate struct:
/// struct ILoveToMock__method;
///
/// /* impl MockFn for Mockable_method ... */
/// ```
///
pub trait MockFn: Sized + 'static {
    /// The inputs to the function.
    type Inputs<'i>;

    /// The output of the function.
    type Output: ?Sized;

    /// The number of inputs.
    const N_INPUTS: u8;

    /// The name to use for runtime errors.
    const NAME: &'static str;

    /// Create a stubbing clause by grouping calls.
    ///
    /// A stub sets up call patterns on a single function, that can be matched in any order.
    ///
    /// For exact order verification, reach for [Self::next_call] instead.
    fn stub<F>(each_fn: F) -> Clause
    where
        F: FnOnce(&mut build::Each<Self>),
    {
        let mut each = build::Each::new();
        each_fn(&mut each);
        each.to_clause()
    }

    /// Define a stub-like call pattern directly on the [MockFn].
    ///
    /// This is a shorthand to avoid calling [MockFn::stub] if there is only one call pattern
    /// that needs to be specified on this MockFn.
    fn each_call<M>(matching: M) -> build::Match<'static, Self, property::InAnyOrder>
    where
        M: (for<'i> Fn(&Self::Inputs<'i>) -> bool) + Send + Sync + RefUnwindSafe + 'static,
    {
        build::new_standalone_match(
            mock::TypedMockImpl::new_standalone(Box::new(matching)),
            property::InAnyOrder,
        )
    }

    /// Initiate a call pattern builder intended to be used as a [Clause]
    /// with exact order verification. The build sequence should end with [build::QuantifiedResponse::in_order].
    ///
    /// This differens from [Self::stub], in that that a stub defines all call patterns without any
    /// specific required call order. This function takes only single input matcher, that MUST be
    /// matched in the order specified, relative to other next calls.
    fn next_call<M>(matching: M) -> build::Match<'static, Self, property::InOrder>
    where
        M: (for<'i> Fn(&Self::Inputs<'i>) -> bool) + Send + Sync + RefUnwindSafe + 'static,
    {
        build::new_standalone_match(
            mock::TypedMockImpl::new_standalone(Box::new(matching)),
            property::InOrder,
        )
    }
}

/// [MockFn] with the ability to unmock into a unique true implementation.
///
/// A true implementation must be a standalone function, not part of a trait,
/// where the first parameter is generic (a `self`-replacement), and the rest of the parameters are
/// identical to [MockFn::Inputs]:
///
/// ```rust
/// # #![feature(generic_associated_types)]
/// # use unimock::*;
/// #[unimock(unmocked=[my_original])]
/// trait DoubleNumber {
///     fn double_number(&self, a: i32) -> i32;
/// }
///
/// // The true implementation is a regular, generic function which performs number doubling!
/// fn my_original<T>(_: T, a: i32) -> i32 {
///     a * 2
/// }
/// ```
///
/// The unmock feature makes sense when the reason to define a mockable trait
/// is _solely_ for the purpose of inversion-of-control at test-time: Release code
/// need only one way to double a number.
///
/// Standalone functions enables arbitrarily deep integration testing
/// in unimock-based application architectures. When unimock calls the true implementation,
/// it inserts itself as the generic first parameter. When this parameter is
/// bounded by traits, the original `fn` is given capabilities to call other APIs,
/// though only indirectly. Each method invocation happening during a test will invisibly pass
/// through unimock, resulting in a great level of control. Consider:
///
/// ```rust
/// # #![feature(generic_associated_types)]
/// # use unimock::*;
/// #[unimock(unmocked=[my_factorial])]
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
///     mock([
///         Factorial__factorial::stub(|each| {
///             each.call(matching!((input) if *input <= 1)).returns(1_u32); // unimock controls the API call
///             each.call(matching!(_)).unmocked();
///         })
///     ])
///     .factorial(5)
/// );
/// ```
///
pub trait Unmock: MockFn {}

/// Construct a unimock instance that works like a mock or a stub, from a set of [Clause]es.
///
/// Every call hitting the instance must be declared in advance as an input clause,
/// or else panic will ensue.
#[inline]
pub fn mock<I>(clauses: I) -> Unimock
where
    I: IntoIterator<Item = Clause>,
{
    mock_from_iterator(&mut clauses.into_iter(), FallbackMode::Error)
}

/// Construct a unimock instance that works like a spy, where every clause
/// acts as an override over the default behaviour, which is to hit
/// "real world" code using the [Unmock] feature.
///
/// # Example
/// ```rust
/// # #![feature(generic_associated_types)]
/// # use unimock::*;
///
/// #[unimock(unmocked=[real_foo])]
/// trait Trait {
///     fn foo(&self);
/// }
///
/// fn real_foo<T: std::any::Any>(_: &T) {
///     println!("real thing");
/// }
///
/// // Spy objects that spies on nothing:
/// spy(None).foo();
/// spy([]).foo();
/// // prints "real thing" x 2
///
/// spy(Some(Trait__foo::next_call(matching!()).returns(()).once().in_order())).foo();
/// // does not print
///
/// // spy object that prevents the real
///
/// ```
#[inline]
pub fn spy<I>(clauses: I) -> Unimock
where
    I: IntoIterator<Item = Clause>,
{
    mock_from_iterator(&mut clauses.into_iter(), FallbackMode::Unmock)
}

fn mock_from_iterator(
    clause_iterator: &mut dyn Iterator<Item = Clause>,
    fallback_mode: FallbackMode,
) -> Unimock {
    let mut assembler = mock::MockAssembler::new();

    fn assemble(
        clause: Clause,
        assembler: &mut mock::MockAssembler,
    ) -> Result<(), mock::AssembleError> {
        match clause.0 {
            build::ClausePrivate::Single(dyn_impl) => {
                dyn_impl.assemble_into(assembler)?;
            }
            build::ClausePrivate::Multiple(vec) => {
                for clause in vec.into_iter() {
                    assemble(clause, assembler)?;
                }
            }
        }
        Ok(())
    }

    for clause in clause_iterator {
        match assemble(clause, &mut assembler) {
            Ok(_) => {}
            Err(error) => panic!("{}", error.to_string()),
        }
    }

    Unimock {
        fallback_mode,
        original_instance: true,
        state: Arc::new(SharedState {
            impls: assembler.impls,
            next_call_index: AtomicUsize::new(0),
            panic_reasons: Mutex::new(vec![]),
        }),
    }
}

/// A clause from which unimock instances are created.
///
/// There can be more than one clause for each [MockFn] instance,
/// these will be combined together at construction time.
///
/// Clause is non-generic and uses dynamic dispatch internally.
/// It also implements `From<I> where I: IntoIterator<Item = Clause>`,
/// so one clause can contain several other clauses in a hierarchical manner.
/// This means that clauses can be returned from helper functions
/// and reused several times:
///
/// ```rust
/// #![feature(generic_associated_types)]
/// use unimock::*;
/// #[unimock]
/// trait Foo {
///     fn foo(&self, i: i32) -> i32;
/// }
///
/// #[unimock]
/// trait Bar {
///     fn bar(&self, i: i32) -> i32;
/// }
///
/// #[unimock]
/// trait Baz {
///     fn baz(&self, i: i32) -> i32;
/// }
///
/// // reusable function
/// fn foo_bar_setup_clause() -> Clause {
///     [
///         Foo__foo::each_call(matching!(_)).returns(1).in_any_order(),
///         Bar__bar::each_call(matching!(_)).returns(2).in_any_order(),
///     ]
///     .into()
/// }
///
/// let unimock = mock([
///     foo_bar_setup_clause(),
///     Baz__baz::each_call(matching!(_)).returns(3).in_any_order()
/// ]);
/// assert_eq!(6, unimock.foo(0) + unimock.bar(0) + unimock.baz(0));
/// ```
#[must_use]
pub struct Clause(pub(crate) build::ClausePrivate);
