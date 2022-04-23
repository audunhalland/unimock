use std::panic;

use crate::counter::*;
use crate::util::*;
use crate::*;

/// A clause from which unimock instances are created.
///
/// One describes behaviour for a single [MockFn].
///
/// There can be more than one clause for each [MockFn] instance,
/// these will be combined together at construction time.
///
/// Clauses are type-erased and uses dynamic dispatch internally.
/// It also implements `From<I> where I: IntoIterator<Item = Clause>`,
/// so one clause can contain several other clauses.
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
/// fn foo_bar_setup_clause() -> unimock::build::Clause {
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
pub struct Clause(pub(crate) ClausePrivate);

impl<I: IntoIterator<Item = Clause>> From<I> for Clause {
    fn from(clauses: I) -> Self {
        Clause(ClausePrivate::Multiple(clauses.into_iter().collect()))
    }
}

pub(crate) enum ClausePrivate {
    Single(mock::DynImpl),
    Multiple(Vec<Clause>),
}

// Different kinds of builders,
pub mod kind {
    pub trait StubKind {}
    pub trait MockKind {}

    pub struct Mock;
    pub struct Stub;

    impl MockKind for Mock {}
    impl StubKind for Stub {}
}

/// Builder for defining a series of cascading call patterns
/// on a specific [MockFn].
pub struct Each<F: MockFn> {
    mock_impl: mock::TypedMockImpl<F>,
}

impl<F> Each<F>
where
    F: MockFn + 'static,
{
    /// Define the next call pattern, given some input matcher.
    ///
    /// The new call pattern will be matched after any previously defined call patterns on the same [Each] instance.
    ///
    /// The method returns a [DefineOutput], which is used to define how unimock responds to the matched call.
    pub fn call<'c, M>(&'c mut self, matching: M) -> DefineOutput<'c, F, kind::Stub>
    where
        M: (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync + RefUnwindSafe + 'static,
    {
        self.mock_impl.patterns.push(mock::CallPattern {
            input_matcher: Box::new(matching),
            call_index_range: Default::default(),
            call_counter: counter::CallCounter::new(counter::CountExpectation::AtLeast(0)),
            responder: mock::Responder::Error,
        });

        DefineOutput {
            pattern: PatternWrapper::Grouped(self.mock_impl.patterns.last_mut().unwrap()),
            kind: kind::Stub,
        }
    }

    pub(crate) fn new_stub(input_debugger: mock::InputDebugger<F>) -> Self {
        Self {
            mock_impl: mock::TypedMockImpl::with_input_debugger(
                input_debugger,
                mock::MockImplKind::Stub,
            ),
        }
    }

    pub(crate) fn to_clause(self) -> Clause {
        Clause(ClausePrivate::Single(mock::DynImpl(Box::new(
            self.mock_impl,
        ))))
    }
}

pub(crate) enum PatternWrapper<'p, F: MockFn> {
    Grouped(&'p mut mock::CallPattern<F>),
    Standalone(mock::TypedMockImpl<F>),
}

impl<'p, F: MockFn> PatternWrapper<'p, F> {
    fn get_mut(&mut self) -> &mut mock::CallPattern<F> {
        match self {
            PatternWrapper::Grouped(p) => *p,
            PatternWrapper::Standalone(mock_impl) => mock_impl.patterns.last_mut().unwrap(),
        }
    }
}

/// Create a new standalone call pattern.
///
/// A standalone call pattern is the only call pattern in a mock impl,
/// in addition it owns its own mock impl.
pub(crate) fn new_standalone_define_output<'p, F: MockFn + 'static, K>(
    mock_impl: mock::TypedMockImpl<F>,
    kind: K,
) -> DefineOutput<'p, F, K> {
    DefineOutput {
        pattern: PatternWrapper::Standalone(mock_impl),
        kind,
    }
}

/// A builder for setting up the response for a matched call pattern.
pub struct DefineOutput<'p, F: MockFn, K> {
    pattern: PatternWrapper<'p, F>,
    kind: K,
}

impl<'p, F, K> DefineOutput<'p, F, K>
where
    F: MockFn + 'static,
{
    /// Specify the output of the call pattern by providing a value.
    /// The output type must implement [Clone] and cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it.
    pub fn returns(mut self, value: impl Into<F::Output>) -> QuantifyResponse<'p, F, K>
    where
        F::Output: Send + Sync + Clone + RefUnwindSafe + 'static,
    {
        let value = value.into();
        self.pattern.get_mut().responder =
            mock::Responder::Closure(Box::new(move |_| value.clone()));
        self.next_state()
    }

    /// Specify the output of the call pattern by calling `Default::default()`.
    pub fn returns_default(mut self) -> QuantifyResponse<'p, F, K>
    where
        F::Output: Default,
    {
        self.pattern.get_mut().responder =
            mock::Responder::Closure(Box::new(|_| Default::default()));
        self.next_state()
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters.
    pub fn answers<A, R>(mut self, func: A) -> QuantifyResponse<'p, F, K>
    where
        A: (for<'i> Fn(F::Inputs<'i>) -> R) + Send + Sync + RefUnwindSafe + 'static,
        R: Into<F::Output>,
    {
        self.pattern.get_mut().responder =
            mock::Responder::Closure(Box::new(move |inputs| func(inputs).into()));
        self.next_state()
    }

    /// Specify the output of the call pattern to be a static reference to the
    /// passed owned value, by leaking its memory. This version leaks the value once.
    pub fn returns_leak<T>(mut self, value: impl Into<T>) -> QuantifyResponse<'p, F, K>
    where
        F::Output: Send + Sync + RefUnwindSafe + Copy + LeakInto<Owned = T> + 'static,
    {
        let leaked = <F::Output as LeakInto>::leak_into(value.into());
        self.pattern.get_mut().responder = mock::Responder::Closure(Box::new(move |_| leaked));
        self.next_state()
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters, then create a static reference
    /// to it by leaking. Note that this version will produce a new memory leak for
    /// _every invocation_ of the answer function.
    pub fn answers_leak<A, R, O>(mut self, func: A) -> QuantifyResponse<'p, F, K>
    where
        A: (for<'i> Fn(F::Inputs<'i>) -> R) + Send + Sync + RefUnwindSafe + 'static,
        R: Into<O>,
        F::Output: LeakInto<Owned = O>,
    {
        self.pattern.get_mut().responder = mock::Responder::Closure(Box::new(move |inputs| {
            let owned_output = func(inputs).into();
            <F::Output as LeakInto>::leak_into(owned_output)
        }));
        self.next_state()
    }

    /// Prevent this call pattern from succeeding by explicitly panicking with a custom message.
    pub fn panics(mut self, message: impl Into<String>) -> QuantifyResponse<'p, F, K> {
        let message = message.into();
        self.pattern.get_mut().responder =
            mock::Responder::Closure(Box::new(move |_| panic!("{}", message)));
        self.next_state()
    }

    /// Instruct this call pattern to invoke the [Unmock]ed function.
    pub fn unmocked(mut self) -> QuantifyResponse<'p, F, K>
    where
        F: Unmock,
    {
        self.pattern.get_mut().responder = mock::Responder::Unmock;
        self.next_state()
    }

    fn next_state(self) -> QuantifyResponse<'p, F, K> {
        QuantifyResponse {
            pattern: self.pattern,
            kind: self.kind,
        }
    }
}

/// Builder for defining how a call pattern gets verified.
pub struct QuantifyResponse<'p, F: MockFn, K> {
    pattern: PatternWrapper<'p, F>,
    kind: K,
}

impl<'p, F, K> QuantifyResponse<'p, F, K>
where
    F: MockFn + 'static,
{
    /// Expect this call pattern to be called exactly once.
    pub fn once(mut self) -> ExactlyQuantifiedResponse<'p, F, K> {
        self.pattern
            .get_mut()
            .call_counter
            .set_expectation(CountExpectation::Exactly(1));
        self.into_exact()
    }

    /// Expect this call pattern to be called exactly the specified number of times.
    pub fn exactly(mut self, times: usize) -> ExactlyQuantifiedResponse<'p, F, K> {
        self.pattern
            .get_mut()
            .call_counter
            .set_expectation(CountExpectation::Exactly(times));
        self.into_exact()
    }

    /// Expect this call pattern to be called at least the specified number of times.
    pub fn at_least(mut self, times: usize) {
        self.pattern
            .get_mut()
            .call_counter
            .set_expectation(CountExpectation::AtLeast(times));
    }

    /// Turn the call pattern into a stubbing clause, without any overall call order verification.
    pub fn in_any_order(self) -> Clause
    where
        K: kind::StubKind,
    {
        match self.pattern {
            PatternWrapper::Standalone(mock_impl) => {
                Clause(ClausePrivate::Single(mock::DynImpl(Box::new(mock_impl))))
            }
            _ => panic!("Cannot expect a next call among group of call patterns"),
        }
    }

    fn into_exact(self) -> ExactlyQuantifiedResponse<'p, F, K> {
        ExactlyQuantifiedResponse {
            pattern: self.pattern,
            kind: self.kind,
        }
    }
}

/// End of response definition
pub struct ExactlyQuantifiedResponse<'p, F: MockFn, K> {
    pattern: PatternWrapper<'p, F>,
    kind: K,
}

impl<'p, F, K> ExactlyQuantifiedResponse<'p, F, K>
where
    F: MockFn + 'static,
{
    /// After a response has been exactly quantified, prepare to define a new output
    /// that will be returned after the quantification has been met.
    pub fn then(self) -> DefineOutput<'p, F, K> {
        DefineOutput {
            pattern: self.pattern,
            kind: self.kind,
        }
    }

    /// Turn this _exactly quantified_ definition into a [Clause] expectation, that can be included
    /// in a sequence of ordered clauses that specify calls to different functions
    /// that must be called in the exact order specified.
    ///
    /// # Example
    /// ```rust
    /// # #![feature(generic_associated_types)]
    /// use unimock::*;
    ///
    /// #[unimock]
    /// trait Trait {
    ///     fn method(&self, arg: i32) -> &'static str;
    /// }
    ///
    /// let m = mock([
    ///     // the first call MUST be method(1) and it will return "a"
    ///     Trait__method::next(matching!(1)).returns("a").once().in_order(),
    ///     // the second call MUST be method(1) and it will return "b"
    ///     Trait__method::next(matching!(2)).returns("b").once().in_order(),
    ///     // there may be no more calls to this mock, as it has no stubs in it
    /// ]);
    ///
    /// assert_eq!("a", m.method(1));
    /// assert_eq!("b", m.method(2));
    /// ```
    pub fn in_order(self) -> Clause
    where
        K: kind::MockKind,
    {
        match self.pattern {
            PatternWrapper::Standalone(mock_impl) => {
                Clause(ClausePrivate::Single(mock::DynImpl(Box::new(mock_impl))))
            }
            _ => panic!("Cannot expect a next call among group of call patterns"),
        }
    }

    pub fn in_any_order(self) -> Clause
    where
        K: kind::StubKind,
    {
        match self.pattern {
            PatternWrapper::Standalone(mock_impl) => {
                Clause(ClausePrivate::Single(mock::DynImpl(Box::new(mock_impl))))
            }
            _ => panic!("Cannot expect a next call among group of call patterns"),
        }
    }
}
