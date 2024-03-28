use core::marker::PhantomData;

use super::*;

/// A type category for values lent out by Unimock.
#[doc(hidden)]
pub struct MutLending<T: ?Sized + 'static>(core::marker::PhantomData<fn() -> T>);

impl<T: ?Sized + 'static> Kind for MutLending<T> {
    type Return = MutLent<T>;
}

pub struct MutLent<T: ?Sized>(PhantomData<T>);

impl<T: ?Sized + 'static> GetOutput for MutLent<T> {
    type Output<'u> = &'u mut T
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        None
    }
}
