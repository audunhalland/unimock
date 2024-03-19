use core::ops::Deref;

use crate::alloc::{vec, Arc, String, Vec};
use crate::call_pattern::InputIndex;
use crate::mismatch::{Mismatch, MismatchKind};
use crate::output::GetOutput;
use crate::{call_pattern::MatchingFn, *};

pub use crate::default_impl_delegator::*;

/// The result of a [MockFn] evaluation.
#[doc(hidden)]
pub enum Eval<'u, 'i, F: MockFn> {
    /// An output should be returned.
    Return(<<<F as MockFn>::OutputKind as Kind>::Return as GetOutput>::Output<'u>),
    /// Mock implementation should continue to evaluate the inputs
    Continue(Continuation<F>, F::Inputs<'i>),
}

/// The continuation of an unevaluated [MockFn].
///
/// Used to tell trait implementations whether to do perform their own evaluation of a call.
#[non_exhaustive]
pub enum Continuation<F: MockFn> {
    /// Answer function should be applied
    Answer(AnswerClosure<F>),
    /// Unmocked implementation should be invoked
    Unmock,
    /// Default implementation should be invoked
    CallDefaultImpl,
}

impl<F: MockFn> Continuation<F> {
    /// Unwrap the `Evaluated` variant, or panic.
    /// The unimock instance must be passed in order to register that an eventual panic happened.
    #[track_caller]
    pub fn report(self, unimock: &Unimock) -> ! {
        let error = match self {
            Self::Answer(..) => error::MockError::NotAnswered { info: F::info() },
            Self::Unmock => error::MockError::CannotUnmock { info: F::info() },
            Self::CallDefaultImpl => error::MockError::NoDefaultImpl { info: F::info() },
        };

        unimock.induce_panic(error)
    }
}

#[doc(hidden)]
pub struct AnswerClosure<F: MockFn>(pub(crate) AnswerClosureInner<F>);

pub(crate) enum AnswerClosureInner<F: MockFn> {
    Ref(&'static F::AnswerFn),
    Arc(Arc<F::AnswerFn>),
}

impl<F: MockFn> Clone for AnswerClosure<F> {
    fn clone(&self) -> Self {
        AnswerClosure(match &self.0 {
            AnswerClosureInner::Ref(answer_fn) => AnswerClosureInner::Ref(*answer_fn),
            AnswerClosureInner::Arc(arc) => AnswerClosureInner::Arc(arc.clone()),
        })
    }
}

impl<F: MockFn> Deref for AnswerClosure<F> {
    type Target = F::AnswerFn;

    fn deref(&self) -> &Self::Target {
        match &self.0 {
            AnswerClosureInner::Ref(answer_fn) => answer_fn,
            AnswerClosureInner::Arc(answer_fn) => answer_fn.as_ref(),
        }
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
            mismatches: vec![],
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
pub fn eval<'u, 'i, F>(unimock: &'u Unimock, inputs: F::Inputs<'i>) -> Eval<'u, 'i, F>
where
    F: MockFn + 'static,
{
    unimock.handle_error(eval::eval(unimock, inputs))
}

/// Clone a Unimock instance
pub fn clone_unimock(unimock: &Unimock) -> Unimock {
    unimock.clone()
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
        Some(crate::alloc::format!("{self:?}"))
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

pub struct MutexIsh<T> {
    #[cfg(feature = "std")]
    inner: ::std::sync::Mutex<T>,

    #[cfg(all(feature = "spin-lock", not(feature = "std")))]
    inner: ::spin::Mutex<T>,

    #[cfg(not(any(feature = "std", feature = "spin-lock")))]
    inner: core::cell::RefCell<T>,
}

#[cfg(feature = "std")]
impl<T> MutexIsh<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: ::std::sync::Mutex::new(value),
        }
    }

    pub fn locked<U>(&self, func: impl FnOnce(&mut T) -> U) -> U {
        let mut lock = self.inner.lock().unwrap();
        func(&mut *lock)
    }
}

#[cfg(all(feature = "spin-lock", not(feature = "std")))]
impl<T> MutexIsh<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: ::spin::Mutex::new(value),
        }
    }

    pub fn locked<U>(&self, func: impl FnOnce(&mut T) -> U) -> U {
        let mut lock = self.inner.lock();
        func(&mut *lock)
    }
}

#[cfg(not(any(feature = "std", feature = "spin-lock")))]
impl<T> MutexIsh<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: core::cell::RefCell::new(value),
        }
    }

    pub fn locked<U>(&self, func: impl FnOnce(&mut T) -> U) -> U {
        func(&mut self.inner.borrow_mut())
    }
}
