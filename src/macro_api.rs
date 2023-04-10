use crate::call_pattern::InputIndex;
use crate::debug;
use crate::lib::{vec, Box, String, Vec};
use crate::mismatch::{Mismatch, MismatchKind};
use crate::output::Output;
use crate::{call_pattern::MatchingFn, *};

/// The evaluation of a [MockFn].
///
/// Used to tell trait implementations whether to do perform their own evaluation of a call.
///
/// The output is generic, because both owned and referenced output are supported.
#[non_exhaustive]
pub enum Evaluation<'u, 'i, F: MockFn> {
    /// Function evaluated to its output.
    Evaluated(<F::Output<'u> as Output<'u, F::Response>>::Type),
    /// Function not yet evaluated, should be unmocked.
    Unmocked(F::Inputs<'i>),
    /// Function not yet evaluated, should call default implementation.
    CallDefaultImpl(F::Inputs<'i>),
}

impl<'u, 'i, F: MockFn> Evaluation<'u, 'i, F> {
    /// Unwrap the `Evaluated` variant, or panic.
    /// The unimock instance must be passed in order to register that an eventual panic happened.
    #[track_caller]
    pub fn unwrap(self, unimock: &Unimock) -> <F::Output<'u> as Output<'u, F::Response>>::Type {
        let error = match self {
            Self::Evaluated(output) => return output,
            Self::Unmocked(_) => error::MockError::CannotUnmock { info: F::info() },
            Self::CallDefaultImpl(_) => error::MockError::NoDefaultImpl { info: F::info() },
        };

        panic!("{}", unimock.shared_state.prepare_panic(error))
    }
}

/// A builder for argument matchers.
pub struct Matching<F: MockFn> {
    pub(crate) mock_fn: core::marker::PhantomData<F>,
    pub(crate) matching_fn: Option<MatchingFn<F>>,
    pub(crate) matcher_debug: Option<debug::InputMatcherDebug>,
}

impl<F> Matching<F>
where
    F: MockFn,
{
    pub(crate) fn new() -> Self {
        Self {
            mock_fn: core::marker::PhantomData,
            matching_fn: None,
            matcher_debug: None,
        }
    }

    /// Set the matching function, with debug capabilities.
    ///
    /// The function should accept a reference to inputs as argument, and return a boolean answer representing match or no match.
    ///
    /// The function also receives a [MismatchReporter]
    #[inline]
    pub fn func<M>(&mut self, matching_fn: M)
    where
        M: (for<'i> Fn(&F::Inputs<'i>, &mut MismatchReporter) -> bool) + Send + Sync + 'static,
    {
        self.matching_fn = Some(MatchingFn(Box::new(matching_fn)));
    }

    /// Register debug info on the matching builder.
    ///
    /// This way, a mismatch may be easier to debug, as the debug info can be printed as part of panic messages.
    pub fn pat_debug(&mut self, pat_debug: &'static str, file: &'static str, line: u32) {
        self.matcher_debug = Some(debug::InputMatcherDebug {
            pat_debug,
            file,
            line,
        });
    }
}

/// A reporter used in call pattern matchers in case of mismatched inputs.
///
/// This is a diagnostics tool leading to higher quality error messages.
///
/// Used by the [matching] macro.
pub struct MismatchReporter {
    enabled: bool,
    pub(crate) mismatches: Vec<(InputIndex, Mismatch)>,
}

impl MismatchReporter {
    pub(crate) fn new_enabled() -> Self {
        Self {
            enabled: true,
            mismatches: crate::lib::vec![],
        }
    }

    pub(crate) fn new_disabled() -> Self {
        Self {
            enabled: false,
            mismatches: vec![],
        }
    }

    /// Whether debugging is enabled
    pub fn enabled(&self) -> bool {
        self.enabled
    }

    /// Register failure to match a pattern
    pub fn pat_fail(
        &mut self,
        input_index: usize,
        actual: Option<impl Into<String>>,
        expected: Option<impl Into<String>>,
    ) {
        self.mismatches.push((
            InputIndex(input_index),
            Mismatch {
                kind: MismatchKind::Pattern,
                actual: actual.map(|dbg| dbg.into()),
                expected: expected.map(|dbg| dbg.into()),
            },
        ));
    }

    /// Register failure to match a pattern
    pub fn eq_fail(
        &mut self,
        input_index: usize,
        actual: Option<impl Into<String>>,
        expected: Option<impl Into<String>>,
    ) {
        self.mismatches.push((
            InputIndex(input_index),
            Mismatch {
                kind: MismatchKind::Eq,
                actual: actual.map(|dbg| dbg.into()),
                expected: expected.map(|dbg| dbg.into()),
            },
        ));
    }

    /// Register failure for an ne check
    pub fn ne_fail(
        &mut self,
        input_index: usize,
        actual: Option<impl Into<String>>,
        expected: Option<impl Into<String>>,
    ) {
        self.mismatches.push((
            InputIndex(input_index),
            Mismatch {
                kind: MismatchKind::Ne,
                actual: actual.map(|dbg| dbg.into()),
                expected: expected.map(|dbg| dbg.into()),
            },
        ));
    }
}

/// Evaluate a [MockFn] given some inputs, to produce its output.
#[track_caller]
pub fn eval<'u, 'i, F>(
    unimock: &'u Unimock,
    inputs: F::Inputs<'i>,
    mutation: &mut F::Mutation<'_>,
) -> Evaluation<'u, 'i, F>
where
    F: MockFn + 'static,
{
    unimock.handle_error(eval::eval(unimock, inputs, mutation))
}

/// The DefaultImplDelegator is a struct
/// used for implementing only non-default methods of traits.
///
/// It is used as part of the infrastructure for supporting default implementation fallbacks in Unimock.
pub struct DefaultImplDelegator {
    pub(crate) unimock: Unimock,
}

impl From<Unimock> for DefaultImplDelegator {
    fn from(unimock: Unimock) -> Self {
        Self { unimock }
    }
}

impl AsRef<Unimock> for DefaultImplDelegator {
    fn as_ref(&self) -> &Unimock {
        &self.unimock
    }
}

impl AsMut<Unimock> for DefaultImplDelegator {
    fn as_mut(&mut self) -> &mut Unimock {
        &mut self.unimock
    }
}

impl core::fmt::Display for DefaultImplDelegator {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        <Unimock as core::fmt::Display>::fmt(&self.unimock, f)
    }
}

impl core::fmt::Debug for DefaultImplDelegator {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        <Unimock as core::fmt::Debug>::fmt(&self.unimock, f)
    }
}

/// Trait for computing the proper [std::fmt::Debug] representation of a value.
pub trait ProperDebug {
    /// Optionally format a debug representation.
    fn unimock_try_debug(&self) -> Option<String>;
}

/// Fallback trait (using autoref specialization) for returning [None] when the implementing value does not implement [std::fmt::Debug].
pub trait NoDebug {
    /// Optionally format a debug representation.
    fn unimock_try_debug(&self) -> Option<String>;
}

// Autoref specialization:
// https://github.com/dtolnay/case-studies/blob/master/autoref-specialization/README.md

impl<T: core::fmt::Debug> ProperDebug for T {
    fn unimock_try_debug(&self) -> Option<String> {
        Some(crate::lib::format!("{self:?}"))
    }
}

impl<T> NoDebug for &T {
    fn unimock_try_debug(&self) -> Option<String> {
        None
    }
}

/// Convert any type implementing `AsRef<str>` to a `&str`.
#[inline]
pub fn as_str_ref<T>(input: &T) -> &str
where
    T: AsRef<str>,
{
    input.as_ref()
}

/// Convert any type implementing `AsRef<[I]>` to a `&[I]`.
#[inline]
pub fn as_slice<T, I>(input: &T) -> &[I]
where
    T: AsRef<[I]>,
{
    input.as_ref()
}

/// Shorthand for converting any `T: AsRef<U>` to `&U`
#[inline]
pub fn as_ref<T, U>(input: &T) -> &U
where
    T: AsRef<U>,
{
    <T as AsRef<U>>::as_ref(input)
}

/// Shorthand for converting any `T: AsMut<U>` to `&mut U`
#[inline]
pub fn as_mut<T, U>(input: &mut T) -> &mut U
where
    T: AsMut<U>,
{
    <T as AsMut<U>>::as_mut(input)
}
