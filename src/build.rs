use core::marker::PhantomData;

use crate::alloc::vec;
use crate::alloc::{String, ToString, Vec};
use crate::call_pattern::*;
use crate::fn_mocker::PatternMatchMode;
use crate::output::{IntoReturn, IntoReturnOnce, Return, ReturnDefault};
use crate::private::{AnswerClosure, AnswerClosureInner};
use crate::property::*;
use crate::responder::{Answerer, DynResponder, IntoReturner};
use crate::*;
use dyn_builder::*;

pub(crate) mod dyn_builder {
    use crate::alloc::{vec, Vec};
    use crate::output::OutputError;
    use crate::responder::Returner;
    use crate::MockFn;

    use crate::{
        call_pattern::{DynCallOrderResponder, DynInputMatcher},
        counter,
        fn_mocker::PatternMatchMode,
        responder::DynResponder,
    };

    // note: appears in public trait signatures
    pub struct DynCallPatternBuilder {
        pub(crate) pattern_match_mode: PatternMatchMode,
        pub(crate) input_matcher: DynInputMatcher,
        pub(crate) responders: Vec<DynCallOrderResponder>,
        pub(crate) count_expectation: counter::CallCountExpectation,
        pub(crate) current_response_index: usize,
        pub(crate) responder_error: Option<OutputError>,
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

        pub fn push_returner_result<F: MockFn>(
            &mut self,
            result: Result<Returner<F>, OutputError>,
        ) {
            match result {
                Ok(responder) => self.push_responder(responder.into_dyn_responder()),
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
            let builder = self.inner_mut();

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

impl<'p, F: MockFn, O: Ordering> DefineResponse<'p, F, O> {
    /// Specify the output of the call pattern by providing a value.
    /// The output type cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it.
    ///
    /// The value can be anything that can be converted [Into] the mock function output.
    ///
    /// Unless explicitly configured on the returned [QuantifyReturnValue], the return value specified here
    ///     can be returned only once, because this method does not require a [Clone] bound.
    /// To be able to return this value multiple times, quantify it explicitly.
    ///
    /// # Example
    /// Using `returns(&str)` for a method that outputs `String`:
    /// ```
    /// # use unimock::*;
    /// #[unimock(api=TraitMock)]
    /// trait Trait {
    ///     fn func(&self) -> String;
    /// }
    ///
    /// let u = Unimock::new(
    ///     TraitMock::func
    ///         .next_call(matching!())
    ///         .returns("hello")
    /// );
    ///
    /// assert_eq!("hello", u.func());
    /// ```
    pub fn returns<T>(self, value: T) -> QuantifyReturnValue<'p, F, T, O>
    where
        T: IntoReturnOnce<F::OutputKind>,
        <<F as MockFn>::OutputKind as Kind>::Return: IntoReturner<F>,
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
    ///
    /// The value can be anything that can be converted [Into] the mock function output.
    ///
    /// # Example
    /// ```
    /// # use unimock::*;
    /// #[unimock(api=TraitMock)]
    /// trait Trait {
    ///     fn get(&self) -> i32;
    /// }
    ///
    /// let u = Unimock::new(
    ///     TraitMock::get
    ///         .each_call(matching!())
    ///         .returns(13)
    /// );
    ///
    /// assert_eq!(13, u.get());
    /// assert_eq!(13, u.get());
    /// ```
    pub fn returns<T>(mut self, value: T) -> Quantify<'p, F, O>
    where
        T: IntoReturn<F::OutputKind>,
        <<F as MockFn>::OutputKind as Kind>::Return: IntoReturner<F>,
    {
        self.wrapper
            .push_returner_result(value.into_return().map(|r| r.into_returner()));
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
            ///
            /// # Example
            #[doc = concat!("\
```
# use unimock::*;
#[unimock(api=TraitMock)]
trait Trait {
    fn get(&self) -> Option<String>;
}

let u = Unimock::new(
    TraitMock::get
        .next_call(matching!())
        .returns_default()
);

assert_eq!(None, u.get());
```
",
)]
            pub fn returns_default(mut self) -> Quantify<'p, F, O>
            where
                F::OutputKind: Return,
                <F::OutputKind as Return>::Type: ReturnDefault<F::OutputKind>,
                <<F as MockFn>::OutputKind as Kind>::Return: IntoReturner<F>,
            {
                let default = <<F::OutputKind as Return>::Type as ReturnDefault<
                                                                                F::OutputKind,
                                                                            >>::return_default();
                self.wrapper
                    .push_returner_result(Ok(default.into_returner()));
                self.quantify()
            }

            /// Specify the response of the call pattern by applying the given function that can then compute it based on input parameters.
            ///
            /// The applied function can respond with types that don't implement [Send] and [Sync].
            ///
            /// The function signature has the same signature as the trait function that it mocks, including `self`.
            /// The `self` argument is included because of a potential need for calling .e.g. [`make_ref`](crate::Unimock::make_ref) on it.
            ///
            /// # Example
            #[doc = concat!("\
```
# use unimock::*;
use std::cell::Cell;

#[unimock(api=TraitMock)]
trait Trait {
    fn get_cell(&self, input: i32) -> Cell<i32>;
}

let u = Unimock::new(
    TraitMock::get_cell
        .next_call(matching!(84))
        .answers(&|_, input| Cell::new(input / 2))
);

assert_eq!(Cell::new(42), u.get_cell(84));
```
",
)]
            /// # Parameter mutation
            #[doc = concat!("\
```
# use unimock::*;
use std::cell::Cell;

#[unimock(api=TraitMock)]
trait Trait {
    fn mutate(&self, input: &mut i32);
}

let u = Unimock::new(
    TraitMock::mutate
        .next_call(matching!(41))
        .answers(&|_, input| {
            *input += 1;
        })
);

let mut number = 41;
u.mutate(&mut number);
assert_eq!(number, 42);
```
",
)]
            /// # Borrow from self
            #[doc = concat!("\
```
# use unimock::*;
#[unimock(api=TraitMock)]
trait Trait {
    fn get(&self) -> &u32;
}

let u = Unimock::new(
    TraitMock::get
        .next_call(matching!())
        .answers(&|u| u.make_ref(42))
);

assert_eq!(u.get(), &42);
```
",
)]
            /// # Recursion
            #[doc = concat!("\
```
# use unimock::*;
#[unimock(api=FibMock)]
trait Fib {
    fn fib(&self, input: i32) -> i32;
}

let u = Unimock::new((
    FibMock::fib
        .each_call(matching!(1 | 2))
        .returns(1),
    FibMock::fib
        .each_call(matching!(_))
        .answers(&|u, input| {
            u.fib(input - 1) + u.fib(input - 2)
        })
));

assert_eq!(55, u.fib(10));
```
",
)]
            pub fn answers(mut self, answer_fn: &'static F::AnswerFn) -> Quantify<'p, F, O> {
                self.wrapper
                    .push_responder(Answerer::<F> { answer_closure: AnswerClosure(AnswerClosureInner::Ref(answer_fn)) }.into_dyn_responder());
                self.quantify()
            }

            /// Specify the response of the call pattern by invoking the given closure that can then compute it based on input parameters.
            #[doc = concat!("\
```
# use unimock::*;
use std::sync::{Arc, Mutex};

#[unimock(api=TraitMock)]
trait Trait {
    fn get(&self) -> i32;
}

let mutex: Arc<Mutex<i32>> = Arc::new(Mutex::new(0));

let u = Unimock::new(
    TraitMock::get
        .each_call(matching!())
        .answers_arc({
            let mutex = mutex.clone();
            Arc::new(move |_, _| {
                *mutex.lock().unwrap()
            })
        })
);

assert_eq!(0, u.get());
*mutex.lock().unwrap() = 42;
assert_eq!(42, u.get());
```
",
            )]
            pub fn answers_arc(
                mut self,
                answer_fn: crate::alloc::Arc<F::AnswerFn>,
            ) -> Quantify<'p, F, O> {
                self.wrapper
                    .push_responder(Answerer::<F> { answer_closure: AnswerClosure(AnswerClosureInner::Arc(answer_fn)) }.into_dyn_responder());
                self.quantify()
            }

            /// Prevent this call pattern from succeeding by explicitly panicking with a custom message.
            pub fn panics(mut self, message: impl Into<String>) -> Quantify<'p, F, O> {
                let message = message.into();
                self.wrapper
                    .push_responder(DynResponder::Panic(message.into()));
                self.quantify()
            }

            /// Instruct this call pattern to invoke its corresponding `unmocked` function.
            ///
            /// For this to work, the mocked trait must be configured with an `unmock_with=[..]` parameter.
            /// If unimock doesn't find a way to unmock the function, this will panic when the function is called.
            #[doc = concat!("\
```
# use unimock::*;
#[unimock(api=FibMock, unmock_with=[my_fibonacci])]
trait Fib {
    fn fib(&self, input: i32) -> i32;
}

// note: no recursion guard:
fn my_fibonacci(f: &impl Fib, input: i32) -> i32 {
    f.fib(input - 1) + f.fib(input - 2)
}

let u = Unimock::new((
    FibMock::fib
        .each_call(matching!(1 | 2))
        .returns(1),
    FibMock::fib
        .each_call(matching!(_))
        .applies_unmocked()
));

assert_eq!(55, u.fib(10));
```
",
)]
            pub fn applies_unmocked(mut self) -> Quantify<'p, F, O> {
                self.wrapper.push_responder(DynResponder::Unmock);
                self.quantify()
            }

            /// Instruct this call pattern to invoke the method's default implementation.
            ///
            /// If the method has no default implementation, the method will panic when called.
            ///
            /// It is not required to pre-register calls to a default-implemented methods like this,
            /// but it's useful for _verifying_ that they are in fact called.
            ///
            /// # Example
            #[doc = concat!("\
```
# use unimock::*;
#[unimock(api=TraitMock)]
trait Trait {
    fn required(&self, input: i32) -> i32;
    fn provided(&self) -> i32 {
        self.required(0)
    }
}

let u = Unimock::new((
    TraitMock::provided
        .next_call(matching!())
        .applies_default_impl(),
    // the above call delegates to the required method:
    TraitMock::required
        .next_call(matching!(0))
        .returns(42)
));

assert_eq!(42, u.provided());
```
",
            )]
            pub fn applies_default_impl(mut self) -> Quantify<'p, F, O> {
                self.wrapper.push_responder(DynResponder::ApplyDefaultImpl);
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
    T: IntoReturnOnce<F::OutputKind>,
    <<F as MockFn>::OutputKind as Kind>::Return: IntoReturner<F>,
{
    pub(crate) wrapper: DynBuilderWrapper<'p>,
    return_value: Option<T>,
    mock_fn: PhantomData<F>,
    ordering: O,
}

impl<'p, F, T, O> QuantifyReturnValue<'p, F, T, O>
where
    F: MockFn,
    T: IntoReturnOnce<F::OutputKind>,
    O: Copy,
    <<F as MockFn>::OutputKind as Kind>::Return: IntoReturner<F>,
{
    /// Expect this call pattern to be matched exactly once.
    ///
    /// This is the only quantifier that works together with return values that don't implement [Clone].
    pub fn once(mut self) -> QuantifiedResponse<'p, F, O, Exact>
    where
        <<F as MockFn>::OutputKind as Kind>::Return: IntoReturner<F>,
    {
        self.wrapper.push_returner_result(
            self.return_value
                .take()
                .unwrap()
                .into_return_once()
                .map(|r| r.into_returner()),
        );
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
        T: IntoReturn<F::OutputKind>,
        <<F as MockFn>::OutputKind as Kind>::Return: IntoReturner<F>,
    {
        self.wrapper.push_returner_result(
            self.return_value
                .take()
                .unwrap()
                .into_return()
                .map(|r| r.into_returner()),
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
        T: IntoReturn<F::OutputKind>,
        O: Ordering<Kind = InAnyOrder>,
        <<F as MockFn>::OutputKind as Kind>::Return: IntoReturner<F>,
    {
        self.wrapper.push_returner_result(
            self.return_value
                .take()
                .unwrap()
                .into_return()
                .map(|r| r.into_returner()),
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
    T: IntoReturnOnce<F::OutputKind>,
    O: Copy + Ordering,
    <<F as MockFn>::OutputKind as Kind>::Return: IntoReturner<F>,
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
    T: IntoReturnOnce<F::OutputKind>,
    <<F as MockFn>::OutputKind as Kind>::Return: IntoReturner<F>,
{
    fn drop(&mut self) {
        if let Some(return_value) = self.return_value.take() {
            self.wrapper
                .push_returner_result(return_value.into_return_once().map(|r| r.into_returner()));
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
    pub fn at_least_times(mut self, times: usize) -> QuantifiedResponse<'p, F, O, AtLeast>
    where
        O: Ordering<Kind = InAnyOrder>,
    {
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
