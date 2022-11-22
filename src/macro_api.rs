use crate::debug;
use crate::output::Output;
use crate::{call_pattern::MatchingFn, call_pattern::MatchingFnDebug, *};

/// The evaluation of a [MockFn].
///
/// Used to tell trait implementations whether to do perform their own evaluation of a call.
///
/// The output is generic, because both owned and referenced output are supported.
pub enum Evaluation<'u, 'i, F: MockFn> {
    /// Function evaluated to its output.
    Evaluated(<F::Output<'u> as Output<'u, F::Response>>::Type),
    /// Function not yet evaluated.
    Skipped(F::Inputs<'i>),
}

impl<'u, 'i, F: MockFn> Evaluation<'u, 'i, F> {
    /// Unwrap the `Evaluated` variant, or panic.
    /// The unimock instance must be passed in order to register that an eventual panic happened.
    pub fn unwrap(self, unimock: &Unimock) -> <F::Output<'u> as Output<'u, F::Response>>::Type {
        match self {
            Self::Evaluated(output) => output,
            Self::Skipped(_) => panic!(
                "{}",
                unimock
                    .shared_state
                    .prepare_panic(error::MockError::CannotUnmock { name: F::NAME })
            ),
        }
    }
}

/// A builder for argument matchers.
pub struct Matching<F: MockFn> {
    pub(crate) mock_fn: std::marker::PhantomData<F>,
    pub(crate) matching_fn: Option<MatchingFn<F>>,
    pub(crate) matching_fn_debug: Option<MatchingFnDebug<F>>,
    pub(crate) pat_debug: Option<debug::InputMatcherDebug>,
}

impl<F> Matching<F>
where
    F: MockFn,
{
    pub(crate) fn new() -> Self {
        Self {
            mock_fn: std::marker::PhantomData,
            matching_fn: None,
            matching_fn_debug: None,
            pat_debug: None,
        }
    }

    /// Set the matching function, with debug capabilities.
    ///
    /// The function should accept a reference to inputs as argument, and return a boolean answer representing match or no match.
    #[inline]
    pub fn func_debug<M>(&mut self, matching_fn: M)
    where
        M: (for<'i> Fn(&F::Inputs<'i>, &mut MatchDebug) -> bool) + Send + Sync + 'static,
    {
        self.matching_fn_debug = Some(MatchingFnDebug(Box::new(matching_fn)));
    }

    /// Set the matching function.
    ///
    /// The function should accept a reference to inputs as argument, and return a boolean answer representing match or no match.
    #[inline]
    pub fn func<M>(&mut self, matching_fn: M)
    where
        M: (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync + 'static,
    {
        self.matching_fn = Some(MatchingFn(Box::new(matching_fn)));
    }

    /// Register debug info on the matching builder.
    ///
    /// This way, a mismatch may be easier to debug, as the debug info can be printed as part of panic messages.
    pub fn pat_debug(&mut self, pat_debug: &'static str, file: &'static str, line: u32) {
        self.pat_debug = Some(debug::InputMatcherDebug {
            pat_debug,
            file,
            line,
        });
    }
}

pub struct MatchDebug {}

impl MatchDebug {
    pub(crate) fn new() -> Self {
        Self {}
    }

    pub fn debug(&self) -> bool {
        false
    }
}

/// Evaluate a [MockFn] given some inputs, to produce its output.
#[track_caller]
pub fn eval<'u, 'i, F>(unimock: &'u Unimock, inputs: F::Inputs<'i>) -> Evaluation<'u, 'i, F>
where
    F: MockFn + 'static,
{
    unimock.handle_error(eval::eval(&unimock.shared_state, inputs))
}

/// Trait for computing the proper [std::fmt::Debug] representation of a value.
pub trait ProperDebug {
    /// Format a debug representation.
    fn unimock_try_debug(&self) -> String;
}

/// Fallback trait (using autoref specialization) for returning `"?"` when the implementing value does not implement [std::fmt::Debug].
pub trait NoDebug {
    /// Format a debug representation.
    fn unimock_try_debug(&self) -> String;
}

// Autoref specialization:
// https://github.com/dtolnay/case-studies/blob/master/autoref-specialization/README.md

impl<T: std::fmt::Debug> ProperDebug for T {
    fn unimock_try_debug(&self) -> String {
        format!("{:?}", self)
    }
}

impl<T> NoDebug for &T {
    fn unimock_try_debug(&self) -> String {
        "?".to_string()
    }
}

/// Take a vector of strings, comma separate and put within parentheses.
pub fn format_inputs(inputs: &[String]) -> String {
    let joined = inputs.join(", ");
    format!("({})", joined)
}

/// Convert any type implementing `AsRef<str>` to a `&str`.
pub fn as_str_ref<T>(input: &T) -> &str
where
    T: AsRef<str>,
{
    input.as_ref()
}

/// Convert any type implementing `AsRef<[I]>` to a `&[I]`.
pub fn as_slice<T, I>(input: &T) -> &[I]
where
    T: AsRef<[I]>,
{
    input.as_ref()
}
