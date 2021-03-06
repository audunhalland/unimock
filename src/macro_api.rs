use crate::*;

/// The evaluation of a [MockFn].
///
/// Used to tell trait implementations whether to do perform their own evaluation of a call.
///
/// The output is generic, because both owned and referenced output are supported.
pub enum Evaluation<'i, O, F: MockFn> {
    /// Function evaluated to its output.
    Evaluated(O),
    /// Function not yet evaluated.
    Skipped(<F as MockInputs<'i>>::Inputs),
}

impl<'i, O, F: MockFn> Evaluation<'i, O, F> {
    /// Unwrap the `Evaluated` variant, or panic.
    /// The unimock instance must be passed in order to register that an eventual panic happened.
    pub fn unwrap(self, unimock: &Unimock) -> O {
        match self {
            Self::Evaluated(output) => output,
            Self::Skipped(_) => panic!(
                "{}",
                unimock.prepare_panic(error::MockError::CannotUnmock { name: F::NAME })
            ),
        }
    }
}

/// Evaluate a [MockFn] given some inputs, to produce its output.
pub fn eval<'i, F>(
    unimock: &Unimock,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> macro_api::Evaluation<'i, F::Output, F>
where
    F: MockFn + 'static,
    F::Output: Sized,
{
    unimock.handle_error(eval::EvalCtx::new::<F>(&unimock.shared_state).eval_sized(inputs))
}

/// Evaluate a [MockFn] given some inputs, to produce its output, where output is borrowed from `self`.
pub fn eval_borrowed<'u, 'i, F>(
    unimock: &'u Unimock,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> macro_api::Evaluation<'i, &'u F::Output, F>
where
    F: MockFn + 'static,
{
    unimock.handle_error(
        eval::EvalCtx::new::<F>(&unimock.shared_state).eval_unsized_self_borrowed(inputs),
    )
}

/// Evaluate a [MockFn] given some inputs, to produce its output, where output is borrowed from a parameter that is not self.
pub fn eval_borrowed_param<'u, 'i, F>(
    unimock: &'u Unimock,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> macro_api::Evaluation<'i, &'i F::Output, F>
where
    F: MockFn + 'static,
{
    unimock.handle_error(
        eval::EvalCtx::new::<F>(&unimock.shared_state)
            .eval_unsized_static_ref(inputs, error::Lender::Param),
    )
}

/// Evaluate a [MockFn] given some inputs, to produce its output, where output is a static reference to `F::Output`.
pub fn eval_static_ref<'i, F>(
    unimock: &Unimock,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> macro_api::Evaluation<'i, &'static F::Output, F>
where
    F: MockFn + 'static,
{
    unimock.handle_error(
        eval::EvalCtx::new::<F>(&unimock.shared_state)
            .eval_unsized_static_ref(inputs, error::Lender::Static),
    )
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
