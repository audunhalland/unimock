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

// Debugging
// TODO: Could avoid creating the string upfront?

/// Debugger for MockFn inputs.
pub trait DebugInputs {
    /// Create a debug string from function inputs.
    fn unimock_debug(&self, n_inputs: u8) -> String;
}

/// Debugger for MockFn inputs which do not implement [std::fmt::Debug].
pub trait NoDebugInputs {
    /// Create a debug string from function inputs.
    fn unimock_debug(&self, n_inputs: u8) -> String;
}

impl<T: std::fmt::Debug> DebugInputs for T {
    fn unimock_debug(&self, n_inputs: u8) -> String {
        match n_inputs {
            1 => format!("({:?})", self),
            _ => format!("{:?}", self),
        }
    }
}

impl<T> NoDebugInputs for &T {
    fn unimock_debug(&self, n_inputs: u8) -> String {
        let inner = (0..n_inputs)
            .into_iter()
            .map(|_| "_")
            .collect::<Vec<_>>()
            .join(", ");

        format!("({})", inner)
    }
}

/// Convert any type implementing `AsRef<str>` to a `&str`.
pub fn as_str_ref<T>(input: &T) -> &str
where
    T: AsRef<str>,
{
    input.as_ref()
}
