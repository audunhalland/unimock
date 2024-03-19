use core::borrow::Borrow;

use super::*;

/// A type category for values lent out by Unimock.
#[doc(hidden)]
pub struct Lending<T: ?Sized + 'static>(core::marker::PhantomData<fn() -> T>);

impl<T: ?Sized + 'static> Kind for Lending<T> {
    type Return = Lent<T>;
}

impl<T: ?Sized + Send + Sync + 'static> Return for Lending<T> {
    type Type = Box<dyn Borrow<T> + Send + Sync>;
}

impl<T0, T: ?Sized + 'static> IntoReturnOnce<Lending<T>> for T0
where
    T0: Borrow<T> + Send + Sync + 'static,
{
    fn into_return_once(self) -> OutputResult<Lent<T>> {
        Ok(Lent(Box::new(self)))
    }
}

impl<T0, T: ?Sized + 'static> IntoReturn<Lending<T>> for T0
where
    T0: Borrow<T> + Send + Sync + 'static,
{
    fn into_return(self) -> OutputResult<Lent<T>> {
        Ok(Lent(Box::new(self)))
    }
}

pub struct Lent<T: ?Sized>(Box<dyn Borrow<T> + Send + Sync>);

impl<T: ?Sized + 'static> GetOutput for Lent<T> {
    type Output<'u> = &'u T
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        Some(self.0.as_ref().borrow())
    }
}
