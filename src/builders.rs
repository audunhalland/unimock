use std::panic;

use crate::counter::*;
use crate::mock::TypedMockImpl;
use crate::*;

/// Program clause for mock building
#[must_use]
pub struct Clause {
    pub(crate) dyn_impl: mock::DynImpl,
}

pub struct GroupEach<F: MockFn> {
    mock_impl: mock::TypedMockImpl<F>,
}

impl<F> GroupEach<F>
where
    F: MockFn + 'static,
{
    pub(crate) fn new(input_debugger: mock::InputDebugger<F>) -> Self {
        Self {
            mock_impl: mock::TypedMockImpl::with_input_debugger(input_debugger),
        }
    }

    pub(crate) fn to_clause(self) -> Clause {
        Clause {
            dyn_impl: mock::DynImpl(Box::new(self.mock_impl)),
        }
    }

    pub fn call<'c, M>(&'c mut self, matching: M) -> MatchedCall<'c, F>
    where
        M: (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync + 'static,
    {
        borrow_new_matched_call_mut(&mut self.mock_impl, Box::new(matching))
    }
}

fn borrow_new_matched_call_mut<'a, F: MockFn + 'static>(
    mock_impl: &'a mut TypedMockImpl<F>,
    input_matcher: Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync>,
) -> MatchedCall<'a, F> {
    mock_impl.patterns.push(mock::CallPattern {
        input_matcher,
        call_counter: counter::CallCounter::new(counter::CountExpectation::None),
        responder: mock::Responder::Error,
    });

    MatchedCall {
        kind: CallKind::Grouped(mock_impl.patterns.last_mut().unwrap()),
    }
}

pub(crate) enum CallKind<'p, F: MockFn> {
    Grouped(&'p mut mock::CallPattern<F>),
    Standalone(mock::TypedMockImpl<F>),
}

pub struct MatchedCall<'p, F: MockFn> {
    kind: CallKind<'p, F>,
}

impl<'p, F> MatchedCall<'p, F>
where
    F: MockFn + 'static,
{
    pub(crate) fn new_standalone(mock_impl: mock::TypedMockImpl<F>) -> Self {
        Self {
            kind: CallKind::Standalone(mock_impl),
        }
    }

    /// Specify the output of the call pattern by providing a value.
    /// The output type must implement [Clone] and cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it.
    pub fn returns(mut self, value: impl Into<F::Output>) -> Self
    where
        F::Output: Send + Sync + Clone + 'static,
    {
        let value = value.into();
        self.pattern_mut().responder = mock::Responder::Closure(Box::new(move |_| value.clone()));
        self
    }

    /// Specify the output of the call pattern by calling `Default::default()`.
    pub fn returns_default(mut self) -> Self
    where
        F::Output: Default,
    {
        self.pattern_mut().responder = mock::Responder::Closure(Box::new(|_| Default::default()));
        self
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters.
    pub fn answers<A, R>(mut self, func: A) -> Self
    where
        A: (for<'i> Fn(F::Inputs<'i>) -> R) + Send + Sync + 'static,
        R: Into<F::Output>,
    {
        self.pattern_mut().responder =
            mock::Responder::Closure(Box::new(move |inputs| func(inputs).into()));
        self
    }

    /// Specify the output of the call pattern to be a static reference to the
    /// passed owned value, by leaking its memory. This version leaks the value once.
    pub fn returns_leak<T>(mut self, value: impl Into<T>) -> Self
    where
        F::Output: Send + Sync + Copy + LeakInto<Owned = T> + 'static,
    {
        let leaked = <F::Output as LeakInto>::leak_into(value.into());
        self.pattern_mut().responder = mock::Responder::Closure(Box::new(move |_| leaked));
        self
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters, then create a static reference
    /// to it by leaking. Note that this version will produce a new memory leak for
    /// _every invocation_ of the answer function.
    pub fn answers_leak<A, R, O>(mut self, func: A) -> Self
    where
        A: (for<'i> Fn(F::Inputs<'i>) -> R) + Send + Sync + 'static,
        R: Into<O>,
        F::Output: LeakInto<Owned = O>,
    {
        self.pattern_mut().responder = mock::Responder::Closure(Box::new(move |inputs| {
            let owned_output = func(inputs).into();
            <F::Output as LeakInto>::leak_into(owned_output)
        }));
        self
    }

    /// Prevent this call pattern from succeeding by explicitly panicking with a custom message.
    pub fn panics(mut self, message: impl Into<String>) -> Self {
        let message = message.into();
        self.pattern_mut().responder =
            mock::Responder::Closure(Box::new(move |_| panic!("{}", message)));
        self
    }

    /// Instruct this call pattern to invoke the [Unmock]ed function.
    pub fn unmock(mut self) -> Self
    where
        F: Unmock,
    {
        self.pattern_mut().responder = mock::Responder::Unmock;
        self
    }

    /// Expect this call pattern to never be called.
    pub fn never(mut self) -> Self {
        self.pattern_mut()
            .call_counter
            .set_expectation(CountExpectation::Exactly(0));
        self
    }

    /// Expect this call pattern to be called exactly once.
    pub fn once(mut self) -> Self {
        self.pattern_mut()
            .call_counter
            .set_expectation(CountExpectation::Exactly(1));
        self
    }

    /// Expect this call pattern to be called exactly the specified number of times.
    pub fn times(mut self, times: usize) -> Self {
        self.pattern_mut()
            .call_counter
            .set_expectation(CountExpectation::Exactly(times));
        self
    }

    /// Expect this call pattern to be called at least the specified number of times.
    pub fn at_least(mut self, times: usize) -> Self {
        self.pattern_mut()
            .call_counter
            .set_expectation(CountExpectation::AtLeast(times));
        self
    }

    pub fn in_order(self) -> Clause {
        match self.kind {
            CallKind::Standalone(mock_impl) => Clause {
                dyn_impl: mock::DynImpl(Box::new(mock_impl)),
            },
            _ => panic!("Cannot expect a next call among group of call patterns"),
        }
    }

    fn pattern_mut(&mut self) -> &mut mock::CallPattern<F> {
        match &mut self.kind {
            CallKind::Grouped(p) => *p,
            CallKind::Standalone(mock_impl) => mock_impl.patterns.last_mut().unwrap(),
        }
    }
}
