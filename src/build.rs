use std::panic;

use crate::util::*;
use crate::*;

impl<I: IntoIterator<Item = Clause>> From<I> for Clause {
    fn from(clauses: I) -> Self {
        Clause(ClausePrivate::Multiple(clauses.into_iter().collect()))
    }
}

pub(crate) enum ClausePrivate {
    Single(mock::DynImpl),
    Multiple(Vec<Clause>),
}

pub mod call_order {
    pub trait LenientKind {}

    pub trait StrictKind {}

    pub struct Lenient;

    pub struct Strict;

    impl StrictKind for Strict {}
    impl LenientKind for Lenient {}
}

pub mod quantification {
    pub trait ExactKind {}

    pub struct Exact;
    pub struct AtLeast;

    impl ExactKind for Exact {}
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
    pub fn call<'c, M>(&'c mut self, matching: M) -> DefineOutput<'c, F, call_order::Lenient>
    where
        M: (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync + RefUnwindSafe + 'static,
    {
        self.mock_impl.patterns.push(mock::CallPattern {
            input_matcher: Box::new(matching),
            call_index_range: Default::default(),
            call_counter: counter::CallCounter::new(0, counter::Exactness::AtLeast),
            responders: vec![],
        });

        DefineOutput {
            pattern: PatternWrapper::Grouped(self.mock_impl.patterns.last_mut().unwrap()),
            response_index: 0,
            call_order: call_order::Lenient,
        }
    }

    pub(crate) fn new_stub(input_debugger: mock::InputDebugger<F>) -> Self {
        Self {
            mock_impl: mock::TypedMockImpl::with_input_debugger(
                input_debugger,
                mock::PatternMatchMode::FullCascadeForEveryCall,
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
pub(crate) fn new_standalone_define_output<'p, F: MockFn + 'static, C>(
    mock_impl: mock::TypedMockImpl<F>,
    call_order: C,
) -> DefineOutput<'p, F, C> {
    DefineOutput {
        pattern: PatternWrapper::Standalone(mock_impl),
        response_index: 0,
        call_order,
    }
}

/// A builder for setting up the response for a matched call pattern.
pub struct DefineOutput<'p, F: MockFn, C> {
    pattern: PatternWrapper<'p, F>,
    response_index: usize,
    call_order: C,
}

impl<'p, F, C> DefineOutput<'p, F, C>
where
    F: MockFn + 'static,
{
    /// Specify the output of the call pattern by providing a value.
    /// The output type must implement [Clone] and cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it.
    pub fn returns(self, value: impl Into<F::Output>) -> QuantifyResponse<'p, F, C>
    where
        F::Output: Send + Sync + Clone + RefUnwindSafe + 'static,
    {
        let value = value.into();
        self.responder(mock::Responder::Closure(Box::new(move |_| value.clone())))
    }

    /// Specify the output of the call pattern by calling `Default::default()`.
    pub fn returns_default(self) -> QuantifyResponse<'p, F, C>
    where
        F::Output: Default,
    {
        self.responder(mock::Responder::Closure(Box::new(|_| Default::default())))
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters.
    pub fn answers<A, R>(self, func: A) -> QuantifyResponse<'p, F, C>
    where
        A: (for<'i> Fn(F::Inputs<'i>) -> R) + Send + Sync + RefUnwindSafe + 'static,
        R: Into<F::Output>,
    {
        self.responder(mock::Responder::Closure(Box::new(move |inputs| {
            func(inputs).into()
        })))
    }

    /// Specify the output of the call pattern to be a static reference to the
    /// passed owned value, by leaking its memory. This version leaks the value once.
    pub fn returns_leak<T>(self, value: impl Into<T>) -> QuantifyResponse<'p, F, C>
    where
        F::Output: Send + Sync + RefUnwindSafe + Copy + LeakInto<Owned = T> + 'static,
    {
        let leaked = <F::Output as LeakInto>::leak_into(value.into());
        self.responder(mock::Responder::Closure(Box::new(move |_| leaked)))
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters, then create a static reference
    /// to it by leaking. Note that this version will produce a new memory leak for
    /// _every invocation_ of the answer function.
    pub fn answers_leak<A, R, O>(self, func: A) -> QuantifyResponse<'p, F, C>
    where
        A: (for<'i> Fn(F::Inputs<'i>) -> R) + Send + Sync + RefUnwindSafe + 'static,
        R: Into<O>,
        F::Output: LeakInto<Owned = O>,
    {
        self.responder(mock::Responder::Closure(Box::new(move |inputs| {
            let owned_output = func(inputs).into();
            <F::Output as LeakInto>::leak_into(owned_output)
        })))
    }

    /// Prevent this call pattern from succeeding by explicitly panicking with a custom message.
    pub fn panics(self, message: impl Into<String>) -> QuantifyResponse<'p, F, C> {
        let message = message.into();

        self.responder(mock::Responder::Closure(Box::new(move |_| {
            panic!("{}", message)
        })))
    }

    /// Instruct this call pattern to invoke the [Unmock]ed function.
    pub fn unmocked(self) -> QuantifyResponse<'p, F, C>
    where
        F: Unmock,
    {
        self.responder(mock::Responder::Unmock)
    }

    fn responder(mut self, responder: mock::Responder<F>) -> QuantifyResponse<'p, F, C> {
        self.pattern
            .get_mut()
            .responders
            .push(mock::CallOrderResponder {
                response_index: self.response_index,
                responder,
            });
        QuantifyResponse {
            pattern: self.pattern,
            response_index: self.response_index,
            call_order: self.call_order,
        }
    }
}

/// Builder for defining how a call pattern gets verified.
pub struct QuantifyResponse<'p, F: MockFn, C> {
    pattern: PatternWrapper<'p, F>,
    response_index: usize,
    call_order: C,
}

impl<'p, F, C> QuantifyResponse<'p, F, C>
where
    F: MockFn + 'static,
{
    /// Expect this call pattern to be called exactly once.
    pub fn once(mut self) -> QuantifiedResponse<'p, F, C, quantification::Exact> {
        self.pattern
            .get_mut()
            .call_counter
            .add_to_minimum(1, counter::Exactness::Exact);
        self.into_exact(1)
    }

    /// Expect this call pattern to be called exactly the specified number of times.
    pub fn n_times(mut self, times: usize) -> QuantifiedResponse<'p, F, C, quantification::Exact> {
        self.pattern
            .get_mut()
            .call_counter
            .add_to_minimum(times, counter::Exactness::Exact);
        self.into_exact(times)
    }

    /// Expect this call pattern to be called at least the specified number of times.
    pub fn at_least_times(
        mut self,
        times: usize,
    ) -> QuantifiedResponse<'p, F, C, quantification::AtLeast> {
        self.pattern
            .get_mut()
            .call_counter
            .add_to_minimum(times, counter::Exactness::AtLeast);
        QuantifiedResponse {
            pattern: self.pattern,
            response_index: self.response_index + times,
            call_order: self.call_order,
            _quantification: quantification::AtLeast,
        }
    }

    /// Turn the call pattern into a stubbing clause, without any overall call order verification.
    pub fn in_any_order(self) -> Clause
    where
        C: call_order::LenientKind,
    {
        match self.pattern {
            PatternWrapper::Standalone(mock_impl) => {
                Clause(ClausePrivate::Single(mock::DynImpl(Box::new(mock_impl))))
            }
            _ => panic!("Cannot expect a next call among group of call patterns"),
        }
    }

    fn into_exact(self, times: usize) -> QuantifiedResponse<'p, F, C, quantification::Exact> {
        QuantifiedResponse {
            pattern: self.pattern,
            response_index: self.response_index + times,
            call_order: self.call_order,
            _quantification: quantification::Exact,
        }
    }
}

/// An exactly quantified response, i.e. the number of times it
/// is expected to respond is an exact number.
pub struct QuantifiedResponse<'p, F: MockFn, C, Q> {
    pattern: PatternWrapper<'p, F>,
    response_index: usize,
    call_order: C,
    _quantification: Q,
}

impl<'p, F, C, Q> QuantifiedResponse<'p, F, C, Q>
where
    F: MockFn + 'static,
{
    /// Prepare to set up a new response, which will take effect after the current
    /// response has been yielded.
    /// In order to make an output sequence, the preceding output must be exactly quantified.
    pub fn then(mut self) -> DefineOutput<'p, F, C>
    where
        Q: quantification::ExactKind,
    {
        // Opening for a new response, which will be non-exactly quantified unless otherwise specified,
        // set the exactness to AtLeastPlusOne now.
        // The reason it is AtLeastPlusOne is the additive nature.
        // We do not want to add anything to the number now, because it could be added to
        // later in QuantifyResponse. We just want to express that when using `then`, it
        // should be called at least one time, if not `then` would be unnecessary.
        self.pattern
            .get_mut()
            .call_counter
            .add_to_minimum(0, counter::Exactness::AtLeastPlusOne);

        DefineOutput {
            pattern: self.pattern,
            response_index: self.response_index,
            call_order: self.call_order,
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
    ///     Trait__method::next_call(matching!(1)).returns("a").once().in_order(),
    ///     // the second call MUST be method(1) and it will return "b"
    ///     Trait__method::next_call(matching!(2)).returns("b").once().in_order(),
    ///     // there may be no more calls to this mock, as it has no stubs in it
    /// ]);
    ///
    /// assert_eq!("a", m.method(1));
    /// assert_eq!("b", m.method(2));
    /// ```
    pub fn in_order(self) -> Clause
    where
        C: call_order::StrictKind,
        Q: quantification::ExactKind,
    {
        match self.pattern {
            PatternWrapper::Standalone(mock_impl) => {
                Clause(ClausePrivate::Single(mock::DynImpl(Box::new(mock_impl))))
            }
            _ => panic!(),
        }
    }

    pub fn in_any_order(self) -> Clause
    where
        C: call_order::LenientKind,
    {
        match self.pattern {
            PatternWrapper::Standalone(mock_impl) => {
                Clause(ClausePrivate::Single(mock::DynImpl(Box::new(mock_impl))))
            }
            _ => panic!(),
        }
    }
}
