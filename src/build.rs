use core::marker::PhantomData;

use crate::call_pattern::*;
use crate::clause::{self};
use crate::fn_mocker::PatternMatchMode;
use crate::output::{IntoCloneResponder, IntoOnceResponder, IntoResponse, Respond, StaticRef};
use crate::private::lib::vec;
use crate::property::*;
use crate::Clause;
use crate::*;

pub(crate) mod dyn_builder {
    use crate::output::ResponderError;
    use crate::private::lib::{vec, Vec};
    use crate::Responder;

    use crate::{
        call_pattern::{DynCallOrderResponder, DynInputMatcher, DynResponder},
        counter,
        fn_mocker::PatternMatchMode,
    };

    // note: appears in public trait signatures
    pub struct DynCallPatternBuilder {
        pub(crate) pattern_match_mode: PatternMatchMode,
        pub(crate) input_matcher: DynInputMatcher,
        pub(crate) responders: Vec<DynCallOrderResponder>,
        pub(crate) count_expectation: counter::CallCountExpectation,
        pub(crate) current_response_index: usize,
        pub(crate) responder_error: Option<ResponderError>,
    }

    impl DynCallPatternBuilder {
        pub(crate) fn new(
            pattern_match_mode: PatternMatchMode,
            input_matcher: DynInputMatcher,
        ) -> Self {
            Self {
                pattern_match_mode,
                input_matcher,
                responders: vec![],
                count_expectation: Default::default(),
                current_response_index: 0,
                responder_error: None,
            }
        }
    }

    pub(crate) enum DynBuilderWrapper<'p> {
        Borrowed(&'p mut DynCallPatternBuilder),
        Owned(DynCallPatternBuilder),
        Stolen,
    }

    impl<'p> DynBuilderWrapper<'p> {
        pub(super) fn steal(&mut self) -> DynBuilderWrapper<'p> {
            let mut stolen = DynBuilderWrapper::Stolen;
            core::mem::swap(self, &mut stolen);
            stolen
        }

        pub fn inner(&self) -> &DynCallPatternBuilder {
            match self {
                Self::Borrowed(builder) => builder,
                Self::Owned(builder) => builder,
                Self::Stolen => panic!("builder stolen"),
            }
        }

        pub fn inner_mut(&mut self) -> &mut DynCallPatternBuilder {
            match self {
                Self::Borrowed(builder) => builder,
                Self::Owned(builder) => builder,
                Self::Stolen => panic!("builder stolen"),
            }
        }

        pub fn push_responder_result(&mut self, result: Result<Responder, ResponderError>) {
            match result {
                Ok(responder) => self.push_responder(responder.0),
                Err(error) => {
                    let dyn_builder = self.inner_mut();
                    if dyn_builder.responder_error.is_none() {
                        dyn_builder.responder_error = Some(error);
                    }
                }
            }
        }

        pub fn push_responder(&mut self, responder: DynResponder) {
            let dyn_builder = self.inner_mut();
            dyn_builder.responders.push(DynCallOrderResponder {
                response_index: dyn_builder.current_response_index,
                responder,
            })
        }

        /// Note: must be called after `push_responder`
        pub fn quantify(&mut self, times: usize, exactness: counter::Exactness) {
            let mut builder = self.inner_mut();

            builder.count_expectation.add_to_minimum(times, exactness);
            builder.current_response_index += times;
        }

        pub fn into_owned(self) -> DynCallPatternBuilder {
            match self {
                Self::Owned(owned) => owned,
                _ => panic!("Tried to turn a non-owned pattern builder into owned"),
            }
        }
    }
}

use crate::private::lib::{String, ToString, Vec};
use dyn_builder::*;

/// Builder for defining a series of cascading call patterns on a specific [MockFn].
pub struct Each<F: MockFn> {
    patterns: Vec<dyn_builder::DynCallPatternBuilder>,
    mock_fn: PhantomData<F>,
}

impl<F> Each<F>
where
    F: MockFn,
{
    /// Define the next call pattern, given some input matcher.
    ///
    /// The new call pattern will be matched after any previously defined call patterns on the same [Each] instance.
    ///
    /// The method returns a [DefineMultipleResponses], which is used to define how unimock responds to the matched call.
    pub fn call<'e>(
        &'e mut self,
        matching_fn: &dyn Fn(&mut Matching<F>),
    ) -> DefineMultipleResponses<'e, F, InAnyOrder> {
        self.patterns.push(dyn_builder::DynCallPatternBuilder::new(
            PatternMatchMode::InAnyOrder,
            DynInputMatcher::from_matching_fn(matching_fn),
        ));

        DefineMultipleResponses {
            wrapper: dyn_builder::DynBuilderWrapper::Borrowed(self.patterns.last_mut().unwrap()),
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
}

impl<F> Clause for Each<F>
where
    F: MockFn,
{
    fn deconstruct(self, sink: &mut dyn clause::term::Sink) -> Result<(), String> {
        if self.patterns.is_empty() {
            return Err("Stub contained no call patterns".to_string());
        }

        for builder in self.patterns.into_iter() {
            sink.push(F::info(), builder)?;
        }

        Ok(())
    }
}

/// A matched call pattern, ready for defining a single response.
pub struct DefineResponse<'p, F: MockFn, O: Ordering> {
    wrapper: DynBuilderWrapper<'p>,
    mock_fn: PhantomData<F>,
    ordering: O,
}

impl<'p, F: MockFn, O: Ordering> DefineResponse<'p, F, O>
where
    <F::Response as Respond>::Type: Send + Sync,
{
    /// Specify the output of the call pattern by providing a value.
    /// The output type cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it.
    ///
    /// Unless explicitly configured on the returned [QuantifyReturnValue], the return value specified here
    ///     can be returned only once, because this method does not require a [Clone] bound.
    /// To be able to return this value multiple times, quantify it explicitly.
    pub fn returns<T>(self, value: T) -> QuantifyReturnValue<'p, F, T, O>
    where
        T: IntoOnceResponder<F::Response>,
    {
        QuantifyReturnValue {
            wrapper: self.wrapper,
            return_value: Some(value),
            mock_fn: self.mock_fn,
            ordering: self.ordering,
        }
    }
}

/// A matched call pattern, ready for defining multiple response, requiring return values to implement [Clone].
pub struct DefineMultipleResponses<'p, F: MockFn, O: Ordering> {
    wrapper: DynBuilderWrapper<'p>,
    mock_fn: PhantomData<F>,
    ordering: O,
}

impl<'p, F, O> DefineMultipleResponses<'p, F, O>
where
    F: MockFn + 'static,
    O: Ordering,
{
    /// Specify the output of the call pattern by providing a value.
    /// The output type cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it, and [Clone] because it should be able to be returned multiple times.
    pub fn returns<V: IntoCloneResponder<F::Response>>(mut self, value: V) -> Quantify<'p, F, O> {
        self.wrapper
            .push_responder_result(value.into_clone_responder::<F>());
        self.quantify()
    }
}

macro_rules! define_response_common_impl {
    ($typename:ident) => {
        impl<'p, F, O> $typename<'p, F, O>
        where
            F: MockFn,
            O: Ordering,
        {
            /// Create a new owned call pattern match.
            pub(crate) fn with_owned_builder(
                input_matcher: DynInputMatcher,
                pattern_match_mode: PatternMatchMode,
                ordering: O,
            ) -> Self {
                Self {
                    wrapper: DynBuilderWrapper::Owned(DynCallPatternBuilder::new(
                        pattern_match_mode,
                        input_matcher,
                    )),
                    mock_fn: PhantomData,
                    ordering,
                }
            }

            /// Specify the response of the call pattern by calling `Default::default()`.
            pub fn returns_default(mut self) -> Quantify<'p, F, O>
            where
                <F::Response as Respond>::Type: Default,
            {
                self.wrapper.push_responder(
                    FunctionResponder::<F> {
                        func: crate::private::lib::Box::new(|_, _| Default::default()),
                    }
                    .into_dyn_responder(),
                );
                self.quantify()
            }

            /// Specify the response of the call pattern by invoking the given closure that can then compute it based on input parameters.
            pub fn answers<C, R>(mut self, func: C) -> Quantify<'p, F, O>
            where
                C: (Fn(F::Inputs<'_>) -> R) + Send + Sync + 'static,
                R: IntoResponse<F::Response>,
            {
                self.wrapper.push_responder(
                    FunctionResponder::<F> {
                        func: crate::private::lib::Box::new(move |inputs, _ctx| {
                            func(inputs).into_response()
                        }),
                    }
                    .into_dyn_responder(),
                );
                self.quantify()
            }

            /// Specify the response of the call pattern by invoking the given closure that can then compute it based on input parameters.
            ///
            /// This variant passes an [AnswerContext] as the second parameter.
            pub fn answers_ctx<C, R>(mut self, func: C) -> Quantify<'p, F, O>
            where
                C: (Fn(F::Inputs<'_>, AnswerContext<'_, '_, '_, F>) -> R) + Send + Sync + 'static,
                R: IntoResponse<F::Response>,
            {
                self.wrapper.push_responder(
                    FunctionResponder::<F> {
                        func: crate::private::lib::Box::new(move |inputs, ctx| {
                            func(inputs, ctx).into_response()
                        }),
                    }
                    .into_dyn_responder(),
                );
                self.quantify()
            }

            /// Specify the response of the call pattern by invoking the given closure that supports mutating _one_ `&mut` parameter from the mocked signature.
            pub fn mutates<C, R>(mut self, func: C) -> Quantify<'p, F, O>
            where
                C: (Fn(&mut F::Mutation<'_>, F::Inputs<'_>) -> R) + Send + Sync + 'static,
                R: IntoResponse<F::Response>,
            {
                self.wrapper.push_responder(
                    FunctionResponder::<F> {
                        func: crate::private::lib::Box::new(move |inputs, ctx| {
                            func(ctx.mutation, inputs).into_response()
                        }),
                    }
                    .into_dyn_responder(),
                );
                self.quantify()
            }

            /// Specify the response of the call pattern to be a static reference to leaked memory.
            ///
            /// The value may be based on the value of input parameters.
            ///
            /// This version will produce a new memory leak for _every invocation_ of the answer function.
            ///
            /// This method should only be used when computing a reference based
            /// on input parameters is necessary, which should not be a common use case.
            pub fn answers_leaked_ref<C, R, T>(mut self, func: C) -> Quantify<'p, F, O>
            where
                F: MockFn<Response = StaticRef<T>>,
                C: (Fn(F::Inputs<'_>) -> R) + Send + Sync + 'static,
                R: core::borrow::Borrow<T> + 'static,
                T: 'static,
            {
                use crate::private::lib::Box;
                self.wrapper.push_responder(
                    FunctionResponder::<F> {
                        func: Box::new(move |inputs, _| {
                            let value = func(inputs);
                            let leaked_ref = Box::leak(Box::new(value));

                            <R as core::borrow::Borrow<T>>::borrow(leaked_ref)
                        }),
                    }
                    .into_dyn_responder(),
                );
                self.quantify()
            }

            /// Prevent this call pattern from succeeding by explicitly panicking with a custom message.
            pub fn panics(mut self, message: impl Into<String>) -> Quantify<'p, F, O> {
                let message = message.into();
                self.wrapper.push_responder(DynResponder::Panic(message));
                self.quantify()
            }

            /// Instruct this call pattern to invoke its corresponding `unmocked` function.
            ///
            /// For this to work, the mocked trait must be configured with an `unmock_with=[..]` parameter.
            /// If unimock doesn't find a way to unmock the function, this will panic when the function is called.
            pub fn unmocked(mut self) -> Quantify<'p, F, O> {
                self.wrapper.push_responder(DynResponder::Unmock);
                self.quantify()
            }

            /// Instruct this call pattern to invoke the method's default implementation.
            ///
            /// If the method has no default implementation, the method will panic when called.
            pub fn default_implementation(mut self) -> Quantify<'p, F, O> {
                self.wrapper.push_responder(DynResponder::CallDefaultImpl);
                self.quantify()
            }

            fn quantify(self) -> Quantify<'p, F, O> {
                Quantify {
                    wrapper: self.wrapper,
                    mock_fn: PhantomData,
                    ordering: self.ordering,
                }
            }
        }
    };
}

define_response_common_impl!(DefineResponse);
define_response_common_impl!(DefineMultipleResponses);

/// Builder for defining how a call pattern with an explicit return value gets verified with regards to quantification/counting.
pub struct QuantifyReturnValue<'p, F, T, O>
where
    F: MockFn,
    T: IntoOnceResponder<F::Response>,
{
    pub(crate) wrapper: DynBuilderWrapper<'p>,
    return_value: Option<T>,
    mock_fn: PhantomData<F>,
    ordering: O,
}

impl<'p, F, T, O> QuantifyReturnValue<'p, F, T, O>
where
    F: MockFn,
    T: IntoOnceResponder<F::Response>,
    O: Copy,
{
    /// Expect this call pattern to be matched exactly once.
    ///
    /// This is the only quantifier that works together with return values that don't implement [Clone].
    pub fn once(mut self) -> QuantifiedResponse<'p, F, O, Exact> {
        self.wrapper
            .push_responder_result(self.return_value.take().unwrap().into_once_responder::<F>());
        self.wrapper.quantify(1, counter::Exactness::Exact);
        QuantifiedResponse {
            wrapper: self.wrapper.steal(),
            mock_fn: PhantomData,
            ordering: self.ordering,
            _repetition: Exact,
        }
    }

    /// Expect this call pattern to be matched exactly the specified number of times.
    pub fn n_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, Exact>
    where
        T: IntoCloneResponder<F::Response>,
    {
        self.wrapper.push_responder_result(
            self.return_value
                .take()
                .unwrap()
                .into_clone_responder::<F>(),
        );
        self.wrapper.quantify(times, counter::Exactness::Exact);
        QuantifiedResponse {
            wrapper: self.wrapper.steal(),
            mock_fn: PhantomData,
            ordering: self.ordering,
            _repetition: Exact,
        }
    }

    /// Expect this call pattern to be matched at least the specified number of times.
    ///
    /// This only works for call patterns matched in any ordered.
    /// Strictly ordered call patterns must have exact quantification.
    pub fn at_least_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, AtLeast>
    where
        T: IntoCloneResponder<F::Response>,
        O: Ordering<Kind = InAnyOrder>,
    {
        self.wrapper.push_responder_result(
            self.return_value
                .take()
                .unwrap()
                .into_clone_responder::<F>(),
        );
        self.wrapper.quantify(times, counter::Exactness::AtLeast);
        QuantifiedResponse {
            wrapper: self.wrapper.steal(),
            mock_fn: PhantomData,
            ordering: self.ordering,
            _repetition: AtLeast,
        }
    }
}

impl<'p, F, T, O> Clause for QuantifyReturnValue<'p, F, T, O>
where
    F: MockFn,
    T: IntoOnceResponder<F::Response>,
    O: Copy + Ordering,
{
    fn deconstruct(self, sink: &mut dyn clause::term::Sink) -> Result<(), String> {
        self.once().deconstruct(sink)
    }
}

/// The drop implementation of this is intended to run when used in a stubbing clause,
/// when the call pattern is left unquantified by the user.
///
/// In that case, it is only able to return once, because no [Clone] bound has been
/// part of any construction step.
impl<'p, F, T, O> Drop for QuantifyReturnValue<'p, F, T, O>
where
    F: MockFn,
    T: IntoOnceResponder<F::Response>,
{
    fn drop(&mut self) {
        if let Some(return_value) = self.return_value.take() {
            self.wrapper
                .push_responder_result(return_value.into_once_responder::<F>());
        }
    }
}

/// Builder for defining how a call pattern gets verified with regards to quantification/counting.
pub struct Quantify<'p, F: MockFn, O> {
    pub(crate) wrapper: DynBuilderWrapper<'p>,
    mock_fn: PhantomData<F>,
    ordering: O,
}

impl<'p, F, O> Quantify<'p, F, O>
where
    F: MockFn,
    O: Ordering,
{
    /// Expect this call pattern to be matched exactly once.
    pub fn once(mut self) -> QuantifiedResponse<'p, F, O, Exact> {
        self.wrapper.quantify(1, counter::Exactness::Exact);
        self.into_exact()
    }

    /// Expect this call pattern to be matched exactly the specified number of times.
    pub fn n_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, Exact> {
        self.wrapper.quantify(times, counter::Exactness::Exact);
        self.into_exact()
    }

    /// Expect this call pattern to be matched at least the specified number of times.
    pub fn at_least_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, AtLeast> {
        self.wrapper.quantify(times, counter::Exactness::AtLeast);
        QuantifiedResponse {
            wrapper: self.wrapper,
            mock_fn: PhantomData,
            ordering: self.ordering,
            _repetition: AtLeast,
        }
    }

    fn into_exact(self) -> QuantifiedResponse<'p, F, O, Exact> {
        QuantifiedResponse {
            wrapper: self.wrapper,
            mock_fn: PhantomData,
            ordering: self.ordering,
            _repetition: Exact,
        }
    }
}

impl<'p, F, O> Clause for Quantify<'p, F, O>
where
    F: MockFn,
    O: Ordering,
{
    fn deconstruct(mut self, sink: &mut dyn clause::term::Sink) -> Result<(), String> {
        if self.wrapper.inner().pattern_match_mode == PatternMatchMode::InOrder {
            self.wrapper.quantify(1, counter::Exactness::Exact);
        }

        sink.push(F::info(), self.wrapper.into_owned())
    }
}

/// An exactly quantified response, i.e. the number of times it is expected to respond is an exact number.
pub struct QuantifiedResponse<'p, F: MockFn, O, R> {
    wrapper: DynBuilderWrapper<'p>,
    mock_fn: PhantomData<F>,
    ordering: O,
    _repetition: R,
}

impl<'p, F, O, R> QuantifiedResponse<'p, F, O, R>
where
    F: MockFn,
    O: Ordering,
    R: Repetition,
{
    /// Prepare to set up a new response, which will take effect after the current response has been yielded.
    /// In order to make an output sequence, the preceding output must be exactly quantified.
    pub fn then(mut self) -> DefineMultipleResponses<'p, F, O>
    where
        R: Repetition<Kind = Exact>,
    {
        // Opening for a new response, which will be non-exactly quantified unless otherwise specified, set the exactness to AtLeastPlusOne now.
        // The reason it is AtLeastPlusOne is the additive nature.
        // We do not want to add anything to the number now, because it could be added to later in Quantify.
        // We just want to express that when using `then`, it should be matched at least one time, if not `then` would be unnecessary.
        self.wrapper
            .inner_mut()
            .count_expectation
            .add_to_minimum(0, counter::Exactness::AtLeastPlusOne);

        DefineMultipleResponses {
            wrapper: self.wrapper,
            mock_fn: PhantomData,
            ordering: self.ordering,
        }
    }
}

impl<'p, F, O, R> Clause for QuantifiedResponse<'p, F, O, R>
where
    F: MockFn,
    O: Ordering,
    R: Repetition,
{
    fn deconstruct(self, sink: &mut dyn clause::term::Sink) -> Result<(), String> {
        sink.push(F::info(), self.wrapper.into_owned())
    }
}

/// AnswerContext represents known information in the current mocking context.
pub struct AnswerContext<'u, 'm0, 'm1, F: MockFn> {
    pub(crate) unimock: &'u Unimock,

    /// The mutation of the currently executing mock function.
    pub mutation: &'m0 mut F::Mutation<'m1>,
}

impl<'u, 'm0, 'm1, F: MockFn> AnswerContext<'u, 'm0, 'm1, F> {
    /// Construct a new unimock instance as a clone of the one currently in the context.
    pub fn clone_instance(&self) -> Unimock {
        self.unimock.clone()
    }
}
