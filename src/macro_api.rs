use crate::*;

/// The conditional evaluation result of a [MockFn].
///
/// Used to tell trait implementations whether to do perform their own
/// evaluation of a call.
pub enum ConditionalEval<'i, F: MockFn>
where
    F::Output: Sized,
{
    /// Function evaluated to its output.
    Yes(F::Output),
    /// Function not yet evaluated.
    No(F::Inputs<'i>),
}

/// Convert any type implementing `AsRef<str>` to a `&str`.
pub fn as_str_ref<T>(input: &T) -> &str
where
    T: AsRef<str>,
{
    input.as_ref()
}
