//! Traits for modelling the output of MockFns used with `returns`.

use crate::alloc::Box;

pub(crate) mod deep;
pub(crate) mod lending;
pub(crate) mod mut_lending;
pub(crate) mod owning;
pub(crate) mod shallow;
pub(crate) mod static_ref;

#[derive(Debug)]
#[doc(hidden)]
pub enum OutputError {
    OwnershipRequired,
    NoMutexApi,
}

type OutputResult<T> = Result<T, OutputError>;

/// This trait bounds various higher order type categories used as responses.
pub trait Kind: 'static {
    /// A type used to store return values
    type Return: GetOutput;
}

/// A [Kind] that may be used with `returns` combinators.
pub trait Return: Kind {
    /// Type of the return value, as stored inside Unimock.
    type Type: 'static;
}

/// A type from which it is possible to produce an output,
/// without consuming the value.
pub trait GetOutput: Sized + 'static {
    /// The output this value produces
    type Output<'u>
    where
        Self: 'u;

    /// Produce an output
    fn output(&self) -> Option<Self::Output<'_>>;
}

/// A type that can be converted into a [Kind::Return] that can be returned one time.
pub trait IntoReturnOnce<K: Kind> {
    #[doc(hidden)]
    fn into_return_once(self) -> OutputResult<K::Return>;
}

/// A type that can be converted into a [Kind::Return] that can be returned any number of times.
pub trait IntoReturn<K: Kind>: IntoReturnOnce<K> {
    #[doc(hidden)]
    fn into_return(self) -> OutputResult<K::Return>;
}

/// A type that can be returned by its [Default] implementation.
pub trait ReturnDefault<K: Kind> {
    #[doc(hidden)]
    fn return_default() -> K::Return;
}

/// A "marker" for mutable types
pub struct Mutable<T>(pub(crate) T);

pub use deep::Deep;
pub use lending::Lending;
pub use mut_lending::MutLending;
pub use owning::Owning;
pub use shallow::Shallow;
pub use static_ref::StaticRef;
