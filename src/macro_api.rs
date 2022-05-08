use crate::*;

/// The evaluation of a [MockFn].
///
/// Used to tell trait implementations whether to do perform their own
/// evaluation of a call.
///
/// The output is generic, because both owned and referenced output are supported.
pub enum Evaluation<'i, O, F: MockFn> {
    /// Function evaluated to its output.
    Evaluated(O),
    /// Function not yet evaluated.
    Skipped(F::Inputs<'i>),
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

/// Convert any type implementing `AsRef<str>` to a `&str`.
pub fn as_str_ref<T>(input: &T) -> &str
where
    T: AsRef<str>,
{
    input.as_ref()
}
