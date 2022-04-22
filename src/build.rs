use std::panic;

use crate::counter::*;
use crate::mock::TypedMockImpl;
use crate::*;

/// A program clause for mock construction.
#[must_use]
pub struct Clause(pub(crate) ClauseKind);

pub(crate) enum ClauseKind {
    Stub(mock::DynImpl),
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
    /// The method returns a [ResponseBuilder], which is used to define how unimock responds to the matched call.
    pub fn call<'c, M>(&'c mut self, matching: M) -> ResponseBuilder<'c, F>
    where
        M: (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync + RefUnwindSafe + 'static,
    {
        borrow_new_matched_call_mut(&mut self.mock_impl, Box::new(matching))
    }

    pub(crate) fn new(input_debugger: mock::InputDebugger<F>) -> Self {
        Self {
            mock_impl: mock::TypedMockImpl::with_input_debugger(input_debugger),
        }
    }

    pub(crate) fn to_clause(self) -> Clause {
        Clause(ClauseKind::Stub(mock::DynImpl(Box::new(self.mock_impl))))
    }
}

fn borrow_new_matched_call_mut<'a, F: MockFn + 'static>(
    mock_impl: &'a mut TypedMockImpl<F>,
    input_matcher: Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync + RefUnwindSafe>,
) -> ResponseBuilder<'a, F> {
    mock_impl.patterns.push(mock::CallPattern {
        input_matcher,
        call_counter: counter::CallCounter::new(counter::CountExpectation::None),
        responder: mock::Responder::Error,
    });

    ResponseBuilder {
        pattern: PatternWrapper::Grouped(mock_impl.patterns.last_mut().unwrap()),
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

/// A builder for setting up the response for a matched call pattern.
pub struct ResponseBuilder<'p, F: MockFn> {
    pattern: PatternWrapper<'p, F>,
}

impl<'p, F> ResponseBuilder<'p, F>
where
    F: MockFn + 'static,
{
    /// Specify the output of the call pattern by providing a value.
    /// The output type must implement [Clone] and cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it.
    pub fn returns(mut self, value: impl Into<F::Output>) -> VerificationBuilder<'p, F>
    where
        F::Output: Send + Sync + Clone + RefUnwindSafe + 'static,
    {
        let value = value.into();
        self.pattern.get_mut().responder =
            mock::Responder::Closure(Box::new(move |_| value.clone()));
        self.next_state()
    }

    /// Specify the output of the call pattern by calling `Default::default()`.
    pub fn returns_default(mut self) -> VerificationBuilder<'p, F>
    where
        F::Output: Default,
    {
        self.pattern.get_mut().responder =
            mock::Responder::Closure(Box::new(|_| Default::default()));
        self.next_state()
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters.
    pub fn answers<A, R>(mut self, func: A) -> VerificationBuilder<'p, F>
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
    pub fn returns_leak<T>(mut self, value: impl Into<T>) -> VerificationBuilder<'p, F>
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
    pub fn answers_leak<A, R, O>(mut self, func: A) -> VerificationBuilder<'p, F>
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
    pub fn panics(mut self, message: impl Into<String>) -> VerificationBuilder<'p, F> {
        let message = message.into();
        self.pattern.get_mut().responder =
            mock::Responder::Closure(Box::new(move |_| panic!("{}", message)));
        self.next_state()
    }

    /// Instruct this call pattern to invoke the [Unmock]ed function.
    pub fn unmocked(mut self) -> VerificationBuilder<'p, F>
    where
        F: Unmock,
    {
        self.pattern.get_mut().responder = mock::Responder::Unmock;
        self.next_state()
    }

    /// Create a new standalone call pattern.
    ///
    /// A standalone call pattern is the only call pattern in a mock impl,
    /// in addition it owns its own mock impl.
    pub(crate) fn new_standalone(mock_impl: mock::TypedMockImpl<F>) -> Self {
        Self {
            pattern: PatternWrapper::Standalone(mock_impl),
        }
    }

    fn next_state(self) -> VerificationBuilder<'p, F> {
        VerificationBuilder {
            pattern: self.pattern,
        }
    }
}

/// Builder for defining how a call pattern gets verified.
pub struct VerificationBuilder<'p, F: MockFn> {
    pattern: PatternWrapper<'p, F>,
}

impl<'p, F> VerificationBuilder<'p, F>
where
    F: MockFn + 'static,
{
    /// Expect this call pattern to be called exactly once.
    pub fn once(mut self) -> Self {
        self.pattern
            .get_mut()
            .call_counter
            .set_expectation(CountExpectation::Exactly(1));
        self
    }

    /// Expect this call pattern to be called exactly the specified number of times.
    pub fn exactly(mut self, times: usize) -> Self {
        self.pattern
            .get_mut()
            .call_counter
            .set_expectation(CountExpectation::Exactly(times));
        self
    }

    /// Expect this call pattern to be called at least the specified number of times.
    pub fn at_least(mut self, times: usize) -> Self {
        self.pattern
            .get_mut()
            .call_counter
            .set_expectation(CountExpectation::AtLeast(times));
        self
    }

    /// Expect the call to happen in the same order that the resulting clause appears in the list of clauses.
    pub fn in_order(self) -> Clause {
        match self.pattern {
            PatternWrapper::Standalone(mock_impl) => {
                Clause(ClauseKind::Stub(mock::DynImpl(Box::new(mock_impl))))
            }
            _ => panic!("Cannot expect a next call among group of call patterns"),
        }
    }
}
