use crate::call_pattern;
use crate::property::*;
use crate::DynMockFn;
use crate::*;

use std::panic;

impl<I: IntoIterator<Item = Clause>> From<I> for Clause {
    fn from(clauses: I) -> Self {
        Clause(ClausePrivate::Tree(clauses.into_iter().collect()))
    }
}

pub(crate) enum ClausePrivate {
    Leaf(ClauseLeaf),
    Tree(Vec<Clause>),
}

pub(crate) struct ClauseLeaf {
    pub dyn_mock_fn: DynMockFn,
    pub kind: ClauseLeafKind,
}

pub(crate) enum ClauseLeafKind {
    Stub(Vec<call_pattern::DynCallPatternBuilder>),
    InAnyOrder(call_pattern::DynCallPatternBuilder),
    InOrder(call_pattern::DynCallPatternBuilder),
}

impl ClauseLeafKind {
    pub fn pattern_match_mode(&self) -> mock_impl::PatternMatchMode {
        match self {
            Self::Stub(_) | Self::InAnyOrder(_) => mock_impl::PatternMatchMode::InAnyOrder,
            Self::InOrder(_) => mock_impl::PatternMatchMode::InOrder,
        }
    }

    pub fn into_pattern_builders(self) -> Vec<call_pattern::DynCallPatternBuilder> {
        match self {
            Self::Stub(builders) => builders,
            Self::InAnyOrder(builder) => vec![builder],
            Self::InOrder(builder) => vec![builder],
        }
    }
}

pub(crate) struct CallPatternBuilder<F: MockFn> {
    input_matcher: Box<dyn (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync>,
    call_counter: counter::CallCounter,
    responders: Vec<call_pattern::CallOrderResponder<F>>,
}

impl<F: MockFn> CallPatternBuilder<F> {
    pub fn from_input_matcher(
        matcher: Box<dyn (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync>,
    ) -> Self {
        Self {
            input_matcher: matcher,
            call_counter: Default::default(),
            responders: vec![],
        }
    }

    pub fn into_dyn(self) -> call_pattern::DynCallPatternBuilder {
        call_pattern::DynCallPatternBuilder {
            call_counter: self.call_counter,
            match_and_respond: Box::new(call_pattern::MatchAndRespond {
                input_matcher: self.input_matcher,
                responders: self.responders,
            }),
        }
    }
}

pub(crate) enum BuilderWrapper<'p, F: MockFn> {
    Borrowed(&'p mut CallPatternBuilder<F>),
    Owned(CallPatternBuilder<F>),
}

impl<'p, F: MockFn> BuilderWrapper<'p, F> {
    fn get_mut(&mut self) -> &mut CallPatternBuilder<F> {
        match self {
            Self::Borrowed(builder) => *builder,
            Self::Owned(builder) => builder,
        }
    }
}

/// Builder for defining a series of cascading call patterns on a specific [MockFn].
pub struct Each<F: MockFn> {
    patterns: Vec<CallPatternBuilder<F>>,
}

impl<F> Each<F>
where
    F: MockFn + 'static,
{
    /// Define the next call pattern, given some input matcher.
    ///
    /// The new call pattern will be matched after any previously defined call patterns on the same [Each] instance.
    ///
    /// The method returns a [Match], which is used to define how unimock responds to the matched call.
    pub fn call<'e, M>(&'e mut self, matching: M) -> Match<'e, F, InAnyOrder>
    where
        M: (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync + 'static,
    {
        self.patterns.push(CallPatternBuilder {
            input_matcher: Box::new(matching),
            call_counter: counter::CallCounter::new(0, counter::Exactness::AtLeast),
            responders: vec![],
        });

        Match {
            builder: BuilderWrapper::Borrowed(self.patterns.last_mut().unwrap()),
            response_index: 0,
            ordering: InAnyOrder,
        }
    }

    pub(crate) fn new() -> Self {
        Self { patterns: vec![] }
    }

    pub(crate) fn to_clause(self) -> Clause {
        if self.patterns.is_empty() {
            panic!("Stub contained no call patterns");
        }

        Clause(ClausePrivate::Leaf(ClauseLeaf {
            dyn_mock_fn: DynMockFn::new::<F>(),
            kind: ClauseLeafKind::Stub(
                self.patterns
                    .into_iter()
                    .map(|builder| builder.into_dyn())
                    .collect(),
            ),
        }))
    }
}

/// A matched call pattern, ready for setting up a response.
pub struct Match<'p, F: MockFn, O: Ordering> {
    builder: BuilderWrapper<'p, F>,
    response_index: usize,
    ordering: O,
}

impl<'p, F, O> Match<'p, F, O>
where
    F: MockFn + 'static,
    O: Ordering,
{
    /// Create a new owned call pattern match.
    pub(crate) fn with_owned_builder(pattern_builder: CallPatternBuilder<F>, ordering: O) -> Self {
        Match {
            builder: BuilderWrapper::Owned(pattern_builder),
            response_index: 0,
            ordering,
        }
    }

    /// Specify the output of the call pattern by providing a value.
    /// The output type must implement [Clone] and cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it.
    pub fn returns(self, value: impl Into<F::Output>) -> QuantifyResponse<'p, F, O>
    where
        F::Output: Send + Sync + Clone + 'static,
    {
        let value = value.into();
        self.responder(call_pattern::Responder::Value(Box::new(
            call_pattern::StoredValueSlot(value),
        )))
    }

    /// Specify the output of the call pattern by calling `Default::default()`.
    pub fn returns_default(self) -> QuantifyResponse<'p, F, O>
    where
        F::Output: Default,
    {
        self.responder(call_pattern::Responder::Closure(Box::new(|_| {
            Default::default()
        })))
    }

    /// Specify the output of the call to be a borrow of the provided value.
    /// This works well when the lifetime of the returned reference is the same as `self`.
    /// Using this for `'static` references will produce a runtime error. For static references, use [Match::returns_static].
    pub fn returns_ref<T>(self, value: T) -> QuantifyResponse<'p, F, O>
    where
        T: std::borrow::Borrow<F::Output> + Sized + Send + Sync + 'static,
    {
        self.responder(call_pattern::Responder::Borrowable(Box::new(value)))
    }

    /// Specify the output of the call to be a reference to static value.
    /// This must be used when the returned reference in the mocked trait is `'static`.
    pub fn returns_static(self, value: &'static F::Output) -> QuantifyResponse<'p, F, O>
    where
        F::Output: Send + Sync + 'static,
    {
        self.responder(call_pattern::Responder::StaticRefClosure(Box::new(
            move |_| value,
        )))
    }

    /// Specify the output of the call pattern by invoking the given closure that can then compute it based on input parameters.
    pub fn answers<A, R>(self, func: A) -> QuantifyResponse<'p, F, O>
    where
        A: (for<'i> Fn(<F as MockInputs<'i>>::Inputs) -> R) + Send + Sync + 'static,
        R: Into<F::Output>,
        F::Output: Sized,
    {
        self.responder(call_pattern::Responder::Closure(Box::new(move |inputs| {
            func(inputs).into()
        })))
    }

    /// Specify the output of the call pattern to be a static reference to leaked memory.
    ///
    /// The value may be based on the value of input parameters.
    ///
    /// This version will produce a new memory leak for _every invocation_ of the answer function.
    ///
    /// This method should only be used when computing a reference based
    /// on input parameters is necessary, which should not be a common use case.
    pub fn answers_leaked_ref<A, R>(self, func: A) -> QuantifyResponse<'p, F, O>
    where
        A: (for<'i> Fn(<F as MockInputs<'i>>::Inputs) -> R) + Send + Sync + 'static,
        R: std::borrow::Borrow<F::Output> + 'static,
        F::Output: Sized,
    {
        self.responder(call_pattern::Responder::StaticRefClosure(Box::new(
            move |inputs| {
                let value = func(inputs);
                let leaked_ref = Box::leak(Box::new(value));
                <R as std::borrow::Borrow<F::Output>>::borrow(leaked_ref)
            },
        )))
    }

    /// Prevent this call pattern from succeeding by explicitly panicking with a custom message.
    pub fn panics(self, message: impl Into<String>) -> QuantifyResponse<'p, F, O> {
        let message = message.into();

        self.responder(call_pattern::Responder::Panic(message))
    }

    /// Instruct this call pattern to invoke the [Unmock]ed function.
    pub fn unmocked(self) -> QuantifyResponse<'p, F, O>
    where
        F: Unmock,
    {
        self.responder(call_pattern::Responder::Unmock)
    }

    fn responder(mut self, responder: call_pattern::Responder<F>) -> QuantifyResponse<'p, F, O> {
        self.builder
            .get_mut()
            .responders
            .push(call_pattern::CallOrderResponder {
                response_index: self.response_index,
                responder,
            });
        QuantifyResponse {
            builder: self.builder,
            response_index: self.response_index,
            ordering: self.ordering,
        }
    }
}

/// Builder for defining how a call pattern gets verified.
pub struct QuantifyResponse<'p, F: MockFn, O> {
    builder: BuilderWrapper<'p, F>,
    response_index: usize,
    ordering: O,
}

impl<'p, F, O> QuantifyResponse<'p, F, O>
where
    F: MockFn + 'static,
    O: Ordering,
{
    /// Expect this call pattern to be called exactly once.
    pub fn once(mut self) -> QuantifiedResponse<'p, F, O, Exact> {
        self.pattern_call_counter()
            .add_to_minimum(1, counter::Exactness::Exact);
        self.into_exact(1)
    }

    /// Expect this call pattern to be called exactly the specified number of times.
    pub fn n_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, Exact> {
        self.pattern_call_counter()
            .add_to_minimum(times, counter::Exactness::Exact);
        self.into_exact(times)
    }

    /// Expect this call pattern to be called at least the specified number of times.
    pub fn at_least_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, AtLeast> {
        self.pattern_call_counter()
            .add_to_minimum(times, counter::Exactness::AtLeast);
        QuantifiedResponse {
            builder: self.builder,
            response_index: self.response_index + times,
            ordering: self.ordering,
            _repetition: AtLeast,
        }
    }

    /// Turn the call pattern into a stubbing clause, without any overall call order verification.
    pub fn in_any_order(self) -> Clause
    where
        O: Ordering<Kind = InAnyOrder>,
    {
        match self.builder {
            BuilderWrapper::Owned(builder) => Clause(ClausePrivate::Leaf(ClauseLeaf {
                dyn_mock_fn: DynMockFn::new::<F>(),
                kind: ClauseLeafKind::InAnyOrder(builder.into_dyn()),
            })),
            _ => panic!("Cannot expect a next call among group of call patterns"),
        }
    }

    fn pattern_call_counter(&mut self) -> &mut counter::CallCounter {
        &mut self.builder.get_mut().call_counter
    }

    fn into_exact(self, times: usize) -> QuantifiedResponse<'p, F, O, Exact> {
        QuantifiedResponse {
            builder: self.builder,
            response_index: self.response_index + times,
            ordering: self.ordering,
            _repetition: Exact,
        }
    }
}

/// An exactly quantified response, i.e. the number of times it is expected to respond is an exact number.
pub struct QuantifiedResponse<'p, F: MockFn, O, R> {
    builder: BuilderWrapper<'p, F>,
    response_index: usize,
    ordering: O,
    _repetition: R,
}

impl<'p, F, O, R> QuantifiedResponse<'p, F, O, R>
where
    F: MockFn + 'static,
    O: Ordering,
    R: Repetition,
{
    /// Prepare to set up a new response, which will take effect after the current response has been yielded.
    /// In order to make an output sequence, the preceding output must be exactly quantified.
    pub fn then(mut self) -> Match<'p, F, O>
    where
        R: Repetition<Kind = Exact>,
    {
        // Opening for a new response, which will be non-exactly quantified unless otherwise specified, set the exactness to AtLeastPlusOne now.
        // The reason it is AtLeastPlusOne is the additive nature.
        // We do not want to add anything to the number now, because it could be added to later in QuantifyResponse.
        // We just want to express that when using `then`, it should be called at least one time, if not `then` would be unnecessary.
        self.builder
            .get_mut()
            .call_counter
            .add_to_minimum(0, counter::Exactness::AtLeastPlusOne);

        Match {
            builder: self.builder,
            response_index: self.response_index,
            ordering: self.ordering,
        }
    }

    /// Turn this _exactly quantified_ definition into a [Clause] expectation.
    /// The clause can be included in a sequence of ordered clauses that specify calls to different functions that must be called in the exact order specified.
    ///
    /// # Example
    /// ```rust
    /// use unimock::*;
    ///
    /// #[unimock]
    /// trait Trait {
    ///     fn method(&self, arg: i32) -> &'static str;
    /// }
    ///
    /// let m = mock([
    ///     // the first call MUST be method(1) and it will return "a"
    ///     Trait__method.next_call(matching!(1)).returns_static("a").once().in_order(),
    ///     // the second call MUST be method(2) and it will return "b"
    ///     Trait__method.next_call(matching!(2)).returns_static("b").once().in_order(),
    ///     // there may be no more calls to this mock, as it has no stubs in it
    /// ]);
    ///
    /// assert_eq!("a", m.method(1));
    /// assert_eq!("b", m.method(2));
    /// ```
    pub fn in_order(self) -> Clause
    where
        O: Ordering<Kind = InOrder>,
        R: Repetition<Kind = Exact>,
    {
        match self.builder {
            BuilderWrapper::Owned(builder) => Clause(ClausePrivate::Leaf(ClauseLeaf {
                dyn_mock_fn: DynMockFn::new::<F>(),
                kind: ClauseLeafKind::InOrder(builder.into_dyn()),
            })),
            _ => panic!(),
        }
    }

    /// Turn the call pattern into a stubbing clause, without any overall call order verification.
    pub fn in_any_order(self) -> Clause
    where
        O: Ordering<Kind = InAnyOrder>,
    {
        match self.builder {
            BuilderWrapper::Owned(builder) => Clause(ClausePrivate::Leaf(ClauseLeaf {
                dyn_mock_fn: DynMockFn::new::<F>(),
                kind: ClauseLeafKind::InAnyOrder(builder.into_dyn()),
            })),
            _ => panic!(),
        }
    }
}
