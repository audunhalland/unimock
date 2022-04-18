use crate::counter::*;
use crate::mock;
use crate::*;

///
/// Builder for defining call patterns that will be recognized on a mock.
///
pub struct Each<F: VirtualFn> {
    pub(crate) patterns: Vec<mock::CallPattern<F>>,
    pub(crate) input_debugger: mock::InputDebugger<F>,
}

impl<F: VirtualFn + 'static> Each<F> {
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
    /// impl VirtualFn for Foo {
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
    pub fn call<'b, M>(&'b mut self, matching: M) -> Call<'b, F>
    where
        M: (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync + 'static,
        for<'i> F::Inputs<'i>: std::fmt::Debug,
    {
        if self.input_debugger.func.is_none() {
            self.input_debugger.func = Some(Box::new(|args| format!("{:?}", args)));
        }

        self.nodebug_call(matching)
    }

    /// Set up a call pattern, without requiring inputs to implement [Debug].
    /// As a result, mocking runtime errors will contain less useful information.
    pub fn nodebug_call<'b, M>(&'b mut self, matching: M) -> Call<'b, F>
    where
        M: (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync + 'static,
    {
        let pat_index = self.patterns.len();
        self.patterns.push(mock::CallPattern {
            pat_index,
            arg_matcher: Some(Box::new(matching)),
            call_counter: counter::CallCounter::new(counter::CountExpectation::None),
            responder: mock::Responder::Error,
        });

        Call {
            pattern: self.patterns.last_mut().unwrap(),
        }
    }
}

///
/// Builder for configuring a specific call pattern.
///
pub struct Call<'b, F: VirtualFn> {
    pattern: &'b mut mock::CallPattern<F>,
}

impl<'b, F> Call<'b, F>
where
    F: VirtualFn + 'static,
{
    /// Specify the output of the call pattern by providing a value.
    /// The output type must implement [Clone] and cannot contain non-static references.
    /// It must also be [Send] and [Sync] because unimock needs to store it.
    pub fn returns(self, value: impl Into<F::Output>) -> Self
    where
        F::Output: Send + Sync + Clone + 'static,
    {
        let value = value.into();
        self.pattern.responder = mock::Responder::Closure(Box::new(move |_| value.clone()));
        self
    }

    /// Specify the output of the call pattern by calling `Default::default()`.
    pub fn returns_default(self) -> Self
    where
        F::Output: Default,
    {
        self.pattern.responder = mock::Responder::Closure(Box::new(|_| Default::default()));
        self
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters.
    pub fn answers<A, R>(self, func: A) -> Self
    where
        A: (for<'i> Fn(F::Inputs<'i>) -> R) + Send + Sync + 'static,
        R: Into<F::Output>,
    {
        self.pattern.responder =
            mock::Responder::Closure(Box::new(move |inputs| func(inputs).into()));
        self
    }

    /// Specify the output of the call pattern to be a static reference to the
    /// passed owned value, by leaking its memory. This version leaks the value once.
    pub fn returns_leak<T>(self, value: impl Into<T>) -> Self
    where
        F::Output: Send + Sync + Copy + LeakInto<Owned = T> + 'static,
    {
        let leaked = <F::Output as LeakInto>::leak_into(value.into());
        self.pattern.responder = mock::Responder::Closure(Box::new(move |_| leaked));
        self
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters, then create a static reference
    /// to it by leaking. Note that this version will produce a new memory leak for
    /// _every invocation_ of the answer function.
    pub fn answers_leak<A, R, O>(self, func: A) -> Self
    where
        A: (for<'i> Fn(F::Inputs<'i>) -> R) + Send + Sync + 'static,
        R: Into<O>,
        F::Output: LeakInto<Owned = O>,
    {
        self.pattern.responder = mock::Responder::Closure(Box::new(move |inputs| {
            let owned_output = func(inputs).into();
            <F::Output as LeakInto>::leak_into(owned_output)
        }));
        self
    }

    /// Prevent this call pattern from succeeding by explicitly panicking with a custom message.
    pub fn panics(self, message: impl Into<String>) -> Self {
        let message = message.into();
        self.pattern.responder = mock::Responder::Closure(Box::new(move |_| panic!("{}", message)));
        self
    }

    /// Instruct this call pattern to invoke the [Original] function.
    pub fn invokes_original(self) -> Self
    where
        F: Original,
    {
        self.pattern.responder = mock::Responder::InvokeOriginal;
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
