use crate::counter::*;
use crate::mock;
use crate::*;

///
/// Builder for defining call patterns that will be recognized on a mock.
///
pub struct Each<A: Api> {
    pub(crate) patterns: Vec<mock::CallPattern<A>>,
    pub(crate) input_debugger: mock::InputDebugger<A>,
}

impl<A: Api + 'static> Each<A> {
    pub(crate) fn new() -> Self {
        Self {
            patterns: vec![],
            input_debugger: mock::InputDebugger::new(),
        }
    }

    /// Set up a call pattern.
    /// The `matching` function receives a tuple representing the call arguments
    /// at hand. Its return value determines whether the defined call pattern matches the given arguments.
    ///
    /// Designed to work well with the [matching] macro. This version requires all inputs to implement [Debug],
    /// in order to print useful error messages.
    ///
    /// # Example
    ///
    /// ```rust
    /// #![feature(generic_associated_types)]
    /// use unimock::*;
    /// struct Foo;
    /// impl Api for Foo {
    ///     /* ... */
    ///     # type Inputs<'i> = (String);
    ///     # type Output = ();
    ///     # const N_INPUTS: u8 = 1;
    ///     # const NAME: &'static str = "Foo";
    /// }
    ///
    /// fn test() {
    ///     let mock = mock(Foo, |each| { each.call(matching!("value")).returns_default(); });
    /// }
    /// ```
    pub fn call<'b, F>(&'b mut self, matching: F) -> Call<'b, A>
    where
        F: (for<'i> Fn(&A::Inputs<'i>) -> bool) + Send + Sync + 'static,
        for<'i> A::Inputs<'i>: std::fmt::Debug,
    {
        if self.input_debugger.func.is_none() {
            self.input_debugger.func = Some(Box::new(|args| format!("{:?}", args)));
        }

        self.nodebug_call(matching)
    }

    /// Set up a call pattern, without requiring inputs to implement [Debug].
    /// As a result, mocking runtime errors will contain less useful information.
    pub fn nodebug_call<'b, F>(&'b mut self, matching: F) -> Call<'b, A>
    where
        F: (for<'i> Fn(&A::Inputs<'i>) -> bool) + Send + Sync + 'static,
    {
        let pat_index = self.patterns.len();
        self.patterns.push(mock::CallPattern {
            pat_index,
            arg_matcher: Some(Box::new(matching)),
            call_counter: counter::CallCounter::new(counter::CountExpectation::None),
            responder: mock::Responder::Panic,
        });

        Call {
            pattern: self.patterns.last_mut().unwrap(),
        }
    }
}

///
/// Builder for configuring a specific call pattern.
///
pub struct Call<'b, A: Api> {
    pattern: &'b mut mock::CallPattern<A>,
}

impl<'b, A> Call<'b, A>
where
    A: Api + 'static,
{
    /// Specify the output of the call pattern by providing a value.
    /// The output type must implement [Clone] and cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it.
    pub fn returns(self, value: impl Into<A::Output>) -> Self
    where
        A::Output: Send + Sync + Clone + 'static,
    {
        let value = value.into();
        self.pattern.responder = mock::Responder::Closure(Box::new(move |_| value.clone()));
        self
    }

    /// Specify the output of the call pattern by calling `Default::default()`.
    pub fn returns_default(self) -> Self
    where
        A::Output: Default,
    {
        self.pattern.responder = mock::Responder::Closure(Box::new(|_| Default::default()));
        self
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters.
    pub fn answers<F, R>(self, f: F) -> Self
    where
        F: (for<'i> Fn(A::Inputs<'i>) -> R) + Send + Sync + 'static,
        R: Into<A::Output>,
    {
        self.pattern.responder = mock::Responder::Closure(Box::new(move |inputs| f(inputs).into()));
        self
    }

    /// Specify the output of the call pattern to be a static reference to the
    /// passed owned value, by leaking its memory. This version leaks the value once.
    pub fn returns_leak<T>(self, value: impl Into<T>) -> Self
    where
        A::Output: Send + Sync + Copy + LeakOutput<Owned = T> + 'static,
    {
        let leaked = <A::Output as LeakOutput>::leak(value.into());
        self.pattern.responder = mock::Responder::Closure(Box::new(move |_| leaked));
        self
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters, then create a static reference
    /// to it by leaking. Note that this version will produce a new memory leak for
    /// _every invocation_ of the answer function.
    pub fn answers_leak<F, R, O>(self, f: F) -> Self
    where
        F: (for<'i> Fn(A::Inputs<'i>) -> R) + Send + Sync + 'static,
        R: Into<O>,
        A::Output: LeakOutput<Owned = O>,
    {
        self.pattern.responder = mock::Responder::Closure(Box::new(move |inputs| {
            let owned_output = f(inputs).into();
            <A::Output as LeakOutput>::leak(owned_output)
        }));
        self
    }

    /// Prevent this call pattern from succeeding by explicitly panicking with a custom message.
    pub fn panics(self, message: impl Into<String>) -> Self {
        let message = message.into();
        self.pattern.responder = mock::Responder::Closure(Box::new(move |_| panic!("{}", message)));
        self
    }

    pub fn calls_archetype(self) -> Self
//where
    //    A: Archetype,
    {
        self.pattern.responder = mock::Responder::Fallthrough;
        self
    }

    /// Expect this call pattern to never be called.
    pub fn never(self) -> Self {
        self.pattern
            .call_counter
            .set_expectation(CountExpectation::Exactly(0));
        self
    }

    /// Expect this call pattern to be called exactly once.
    pub fn once(self) -> Self {
        self.pattern
            .call_counter
            .set_expectation(CountExpectation::Exactly(1));
        self
    }

    /// Expect this call pattern to be called exactly the specified number of times.
    pub fn times(self, times: usize) -> Self {
        self.pattern
            .call_counter
            .set_expectation(CountExpectation::Exactly(times));
        self
    }

    /// Expect this call pattern to be called at least the specified number of times.
    pub fn at_least(self, times: usize) -> Self {
        self.pattern
            .call_counter
            .set_expectation(CountExpectation::AtLeast(times));
        self
    }
}
