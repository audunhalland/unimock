use crate::call_pattern::*;
use crate::clause;
use crate::property::*;
use crate::*;

use std::marker::PhantomData;
use std::panic;

pub(crate) struct DynCallPatternBuilder {
    pub input_matcher: DynInputMatcher,
    pub responders: Vec<DynCallOrderResponder>,
    pub count_expectation: counter::CallCountExpectation,
    pub current_response_index: usize,
}

impl DynCallPatternBuilder {
    pub fn new(input_matcher: DynInputMatcher) -> Self {
        Self {
            input_matcher,
            responders: vec![],
            count_expectation: Default::default(),
            current_response_index: 0,
        }
    }
}

enum BuilderWrapper<'p> {
    Borrowed(&'p mut DynCallPatternBuilder),
    Owned(DynCallPatternBuilder),
    Stolen,
}

impl<'p> BuilderWrapper<'p> {
    fn steal(&mut self) -> BuilderWrapper<'p> {
        let mut stolen = BuilderWrapper::Stolen;
        std::mem::swap(self, &mut stolen);
        stolen
    }

    fn get_mut(&mut self) -> &mut DynCallPatternBuilder {
        match self {
            Self::Borrowed(builder) => *builder,
            Self::Owned(builder) => builder,
            Self::Stolen => panic!("builder stolen"),
        }
    }

    fn push_responder(&mut self, responder: DynResponder) {
        let pattern_builder = self.get_mut();
        pattern_builder.responders.push(DynCallOrderResponder {
            response_index: pattern_builder.current_response_index,
            responder,
        });
    }

    /// Note: must be called after `push_responder`
    fn quantify(&mut self, times: usize, exactness: counter::Exactness) {
        let mut builder = self.get_mut();

        builder.count_expectation.add_to_minimum(times, exactness);
        builder.current_response_index += times;
    }
}

/// Builder for defining a series of cascading call patterns on a specific [MockFn].
pub struct Each<F: MockFn> {
    patterns: Vec<DynCallPatternBuilder>,
    mock_fn: PhantomData<F>,
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
        self.patterns.push(DynCallPatternBuilder::new(
            InputMatcher(Box::new(matching)).into_dyn(),
        ));

        Match {
            builder: BuilderWrapper::Borrowed(self.patterns.last_mut().unwrap()),
            mock_fn: PhantomData,
            ordering: InAnyOrder,
        }
    }

    pub(crate) fn new() -> Self {
        Self {
            patterns: vec![],
            mock_fn: PhantomData,
        }
    }

    pub(crate) fn into_clause(self) -> Clause {
        if self.patterns.is_empty() {
            panic!("Stub contained no call patterns");
        }

        Clause(clause::ClausePrivate::Leaf(clause::ClauseLeaf {
            dyn_mock_fn: DynMockFn::new::<F>(),
            kind: clause::ClauseLeafKind::Stub(self.patterns),
        }))
    }
}

/// A matched call pattern, ready for setting up a response.
pub struct Match<'p, F: MockFn, O: Ordering> {
    builder: BuilderWrapper<'p>,
    mock_fn: PhantomData<F>,
    ordering: O,
}

impl<'p, F, O> Match<'p, F, O>
where
    F: MockFn + 'static,
    O: Ordering,
{
    /// Create a new owned call pattern match.
    pub(crate) fn with_owned_builder(input_matcher: DynInputMatcher, ordering: O) -> Self {
        Match {
            builder: BuilderWrapper::Owned(DynCallPatternBuilder::new(input_matcher)),
            mock_fn: PhantomData,
            ordering,
        }
    }

    /// Specify the output of the call pattern by providing a value.
    /// The output type cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it.
    ///
    /// Unless explicitly configured on the returned [QuantifyValue], the return value specified here
    ///     can be returned only once, because this method does not require a [Clone] bound.
    /// To be able to return this value multiple times, call `.cloned()`, or quantify it explicitly.
    pub fn returns<V: Into<F::Output>>(self, value: V) -> QuantifyValue<'p, F, O>
    where
        F::Output: Send + Sync + Sized + 'static,
    {
        QuantifyValue {
            builder: self.builder,
            value: Some(value.into()),
            mock_fn: self.mock_fn,
            ordering: self.ordering,
        }
    }

    /// Specify the output of the call pattern by calling `Default::default()`.
    pub fn returns_default(mut self) -> Quantify<'p, F, O>
    where
        F::Output: Default,
    {
        self.builder.push_responder(
            ClosureResponder::<F> {
                func: Box::new(|_| Default::default()),
            }
            .into_dyn_responder(),
        );
        self.quantify()
    }

    /// Specify the output of the call to be a borrow of the provided value.
    /// This works well when the lifetime of the returned reference is the same as `self`.
    /// Using this for `'static` references will produce a runtime error. For static references, use [Match::returns_static].
    pub fn returns_ref<T>(mut self, value: T) -> Quantify<'p, F, O>
    where
        T: std::borrow::Borrow<F::Output> + Sized + Send + Sync + 'static,
    {
        self.builder.push_responder(
            BorrowableResponder::<F> {
                borrowable: Box::new(value),
            }
            .into_dyn_responder(),
        );
        self.quantify()
    }

    /// Specify the output of the call to be a reference to static value.
    /// This must be used when the returned reference in the mocked trait is `'static`.
    pub fn returns_static(mut self, value: &'static F::Output) -> Quantify<'p, F, O>
    where
        F::Output: Send + Sync + 'static,
    {
        self.builder.push_responder(
            StaticRefClosureResponder::<F> {
                func: Box::new(move |_| value),
            }
            .into_dyn_responder(),
        );
        self.quantify()
    }

    /// Specify the output of the call pattern by invoking the given closure that can then compute it based on input parameters.
    pub fn answers<A, R>(mut self, func: A) -> Quantify<'p, F, O>
    where
        A: (for<'i> Fn(<F as MockInputs<'i>>::Inputs) -> R) + Send + Sync + 'static,
        R: Into<F::Output>,
        F::Output: Sized,
    {
        self.builder.push_responder(
            ClosureResponder::<F> {
                func: Box::new(move |inputs| func(inputs).into()),
            }
            .into_dyn_responder(),
        );
        self.quantify()
    }

    /// Specify the output of the call pattern to be a static reference to leaked memory.
    ///
    /// The value may be based on the value of input parameters.
    ///
    /// This version will produce a new memory leak for _every invocation_ of the answer function.
    ///
    /// This method should only be used when computing a reference based
    /// on input parameters is necessary, which should not be a common use case.
    pub fn answers_leaked_ref<A, R>(mut self, func: A) -> Quantify<'p, F, O>
    where
        A: (for<'i> Fn(<F as MockInputs<'i>>::Inputs) -> R) + Send + Sync + 'static,
        R: std::borrow::Borrow<F::Output> + 'static,
        F::Output: Sized,
    {
        self.builder.push_responder(
            StaticRefClosureResponder::<F> {
                func: Box::new(move |inputs| {
                    let value = func(inputs);
                    let leaked_ref = Box::leak(Box::new(value));
                    <R as std::borrow::Borrow<F::Output>>::borrow(leaked_ref)
                }),
            }
            .into_dyn_responder(),
        );
        self.quantify()
    }

    /// Prevent this call pattern from succeeding by explicitly panicking with a custom message.
    pub fn panics(mut self, message: impl Into<String>) -> Quantify<'p, F, O> {
        self.builder
            .push_responder(DynResponder::Panic(message.into()));
        self.quantify()
    }

    /// Instruct this call pattern to invoke the [Unmock]ed function.
    pub fn unmocked(mut self) -> Quantify<'p, F, O>
    where
        F: Unmock,
    {
        self.builder.push_responder(DynResponder::Unmock);
        self.quantify()
    }

    fn quantify(self) -> Quantify<'p, F, O> {
        Quantify {
            builder: self.builder,
            mock_fn: PhantomData,
            ordering: self.ordering,
        }
    }
}

/// Builder for defining how a call pattern with an explicit return value gets verified with regards to quantification/counting.
pub struct QuantifyValue<'p, F, O>
where
    F: MockFn,
    F::Output: Sized + Send + Sync,
{
    builder: BuilderWrapper<'p>,
    value: Option<F::Output>,
    mock_fn: PhantomData<F>,
    ordering: O,
}

impl<'p, F, O> QuantifyValue<'p, F, O>
where
    F: MockFn,
    F::Output: Sized + Send + Sync,
    O: Copy,
{
    /// Keep this value unquantified, but configure it to be cloned before it gets returned.
    /// This way it can be returned more than once.
    ///
    /// This method returns nothing, because there is no more processing that can be done on an unquantified return value.
    pub fn cloned(mut self)
    where
        F::Output: Clone,
    {
        self.builder.push_responder(
            ValueResponder::<F> {
                stored_value: Box::new(StoredValueSlot(self.value.take().unwrap())),
            }
            .into_dyn_responder(),
        );
    }

    /// Expect this call pattern to be called exactly once.
    ///
    /// This is the only quantifier that works together with return values that don't implement [Clone].
    pub fn once(mut self) -> QuantifiedResponse<'p, F, O, Exact> {
        self.builder.push_responder(
            ValueResponder::<F> {
                stored_value: Box::new(StoredValueSlotOnce::new(self.value.take().unwrap())),
            }
            .into_dyn_responder(),
        );
        self.builder.quantify(1, counter::Exactness::Exact);
        QuantifiedResponse {
            builder: self.builder.steal(),
            mock_fn: PhantomData,
            ordering: self.ordering,
            _repetition: Exact,
        }
    }

    /// Expect this call pattern to be called exactly the specified number of times.
    pub fn n_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, Exact>
    where
        F::Output: Clone,
    {
        self.builder.push_responder(
            ValueResponder::<F> {
                stored_value: Box::new(StoredValueSlot(self.value.take().unwrap())),
            }
            .into_dyn_responder(),
        );
        self.builder.quantify(times, counter::Exactness::Exact);
        QuantifiedResponse {
            builder: self.builder.steal(),
            mock_fn: PhantomData,
            ordering: self.ordering,
            _repetition: Exact,
        }
    }

    /// Expect this call pattern to be called at least the specified number of times.
    pub fn at_least_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, AtLeast>
    where
        F::Output: Clone,
    {
        self.builder.push_responder(
            ValueResponder::<F> {
                stored_value: Box::new(StoredValueSlot(self.value.take().unwrap())),
            }
            .into_dyn_responder(),
        );
        self.builder.quantify(times, counter::Exactness::AtLeast);
        QuantifiedResponse {
            builder: self.builder.steal(),
            mock_fn: PhantomData,
            ordering: self.ordering,
            _repetition: AtLeast,
        }
    }

    /// Quantify this to exactly once, and return an ordered clause.
    pub fn in_order(self) -> Clause
    where
        O: Ordering<Kind = InOrder>,
    {
        self.once().in_order()
    }

    /// Turn this unquantified call pattern into an unordered clause.
    ///
    /// The pattern will be callable any number of times, by returning a cloned value.
    pub fn in_any_order(mut self) -> Clause
    where
        F::Output: Clone,
        O: Ordering<Kind = InAnyOrder>,
    {
        self.builder.push_responder(
            ValueResponder::<F> {
                stored_value: Box::new(StoredValueSlot(self.value.take().unwrap())),
            }
            .into_dyn_responder(),
        );
        match self.builder.steal() {
            BuilderWrapper::Owned(builder) => clause::ClauseLeaf {
                dyn_mock_fn: DynMockFn::new::<F>(),
                kind: clause::ClauseLeafKind::InAnyOrder(builder),
            }
            .into(),
            _ => panic!("Cannot expect a next call among group of call patterns"),
        }
    }
}

/// The drop implementation of this is intended to run when used in a stubbing clause,
/// when the call pattern is left unquantified by the user.
///
/// In that case, it is only able to return once, because no [Clone] bound has been
/// part of any construction step.
impl<'p, F, O> Drop for QuantifyValue<'p, F, O>
where
    F: MockFn,
    F::Output: Sized + Send + Sync,
{
    fn drop(&mut self) {
        if let Some(value) = self.value.take() {
            self.builder.push_responder(
                ValueResponder::<F> {
                    stored_value: Box::new(StoredValueSlotOnce::new(value)),
                }
                .into_dyn_responder(),
            );
        }
    }
}

/// Builder for defining how a call pattern gets verified with regards to quantification/counting.
pub struct Quantify<'p, F: MockFn, O> {
    builder: BuilderWrapper<'p>,
    mock_fn: PhantomData<F>,
    ordering: O,
}

impl<'p, F, O> Quantify<'p, F, O>
where
    F: MockFn + 'static,
    O: Ordering,
{
    /// Expect this call pattern to be called exactly once.
    pub fn once(mut self) -> QuantifiedResponse<'p, F, O, Exact> {
        self.builder.quantify(1, counter::Exactness::Exact);
        self.into_exact()
    }

    /// Expect this call pattern to be called exactly the specified number of times.
    pub fn n_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, Exact> {
        self.builder.quantify(times, counter::Exactness::Exact);
        self.into_exact()
    }

    /// Expect this call pattern to be called at least the specified number of times.
    pub fn at_least_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, AtLeast> {
        self.builder.quantify(times, counter::Exactness::AtLeast);
        QuantifiedResponse {
            builder: self.builder,
            mock_fn: PhantomData,
            ordering: self.ordering,
            _repetition: AtLeast,
        }
    }

    /// Quantify this to exactly once, and return an ordered clause.
    pub fn in_order(self) -> Clause
    where
        O: Ordering<Kind = InOrder>,
    {
        self.once().in_order()
    }

    /// Turn the call pattern into a stubbing clause, without any overall call order verification.
    pub fn in_any_order(self) -> Clause
    where
        O: Ordering<Kind = InAnyOrder>,
    {
        match self.builder {
            BuilderWrapper::Owned(builder) => clause::ClauseLeaf {
                dyn_mock_fn: DynMockFn::new::<F>(),
                kind: clause::ClauseLeafKind::InAnyOrder(builder),
            }
            .into(),
            _ => panic!("Cannot expect a next call among group of call patterns"),
        }
    }

    fn into_exact(self) -> QuantifiedResponse<'p, F, O, Exact> {
        QuantifiedResponse {
            builder: self.builder,
            mock_fn: PhantomData,
            ordering: self.ordering,
            _repetition: Exact,
        }
    }
}

/// An exactly quantified response, i.e. the number of times it is expected to respond is an exact number.
pub struct QuantifiedResponse<'p, F: MockFn, O, R> {
    builder: BuilderWrapper<'p>,
    mock_fn: PhantomData<F>,
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
            .count_expectation
            .add_to_minimum(0, counter::Exactness::AtLeastPlusOne);

        Match {
            builder: self.builder,
            mock_fn: PhantomData,
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
            BuilderWrapper::Owned(builder) => clause::ClauseLeaf {
                dyn_mock_fn: DynMockFn::new::<F>(),
                kind: clause::ClauseLeafKind::InOrder(builder),
            }
            .into(),
            _ => panic!(),
        }
    }

    /// Turn the call pattern into a stubbing clause, without any overall call order verification.
    pub fn in_any_order(self) -> Clause
    where
        O: Ordering<Kind = InAnyOrder>,
    {
        match self.builder {
            BuilderWrapper::Owned(builder) => clause::ClauseLeaf {
                dyn_mock_fn: DynMockFn::new::<F>(),
                kind: clause::ClauseLeafKind::InAnyOrder(builder),
            }
            .into(),
            _ => panic!(),
        }
    }
}
