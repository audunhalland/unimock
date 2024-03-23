use crate::output::*;

/// A type category for owned, 'static non-generic values
#[doc(hidden)]
pub struct Owning<T>(core::marker::PhantomData<fn() -> T>);

impl<T: 'static> Kind for Owning<T> {
    type Return = Owned<T>;
}

impl<T: 'static> Return for Owning<T> {
    type Type = T;
}

impl<T0, T: Send + Sync + 'static> IntoReturnOnce<Owning<T>> for T0
where
    T0: Into<T>,
{
    #[cfg(any(feature = "std", feature = "spin-lock"))]
    fn into_return_once(self) -> OutputResult<Owned<T>> {
        let mutex = crate::private::MutexIsh::new(Some(self.into()));
        Ok(Owned(Box::new(move || {
            mutex.locked(|option| option.take())
        })))
    }

    #[cfg(not(any(feature = "std", feature = "spin-lock")))]
    fn into_return_once(self) -> OutputResult<Owned<T>> {
        Err(OutputError::NoMutexApi)
    }
}

impl<T0, T> IntoReturn<Owning<T>> for T0
where
    T: Clone + Send + Sync + 'static,
    T0: Into<T>,
{
    fn into_return(self) -> OutputResult<Owned<T>> {
        let value = self.into();
        Ok(Owned(Box::new(move || Some(value.clone()))))
    }
}

impl<T: Default + 'static> ReturnDefault<Owning<T>> for T {
    fn return_default() -> <Owning<T> as Kind>::Return {
        Owned(Box::new(|| Some(T::default())))
    }
}

pub struct Owned<T>(Box<dyn Fn() -> Option<T> + Send + Sync + 'static>);

impl<T: 'static> GetOutput for Owned<T> {
    type Output<'u> = T where Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        (*self.0)()
    }
}
