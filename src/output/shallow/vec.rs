//! Covers a shallow Vec<&T>

use core::borrow::Borrow;

use crate::alloc::Vec;
use crate::output::*;

type Mix<T> = Shallow<Vec<&'static T>>;

type Response<T> = Vec<Box<dyn Borrow<T> + Send + Sync>>;

impl<T: ?Sized> Kind for Mix<T> {
    type Return = Response<T>;
}

impl<T: ?Sized + 'static> GetOutput for Response<T> {
    type Output<'u> = Vec<&'u T>
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        Some(self.iter().map(|el| el.as_ref().borrow()).collect())
    }
}

macro_rules! into {
    ($trait:ident, $method:ident) => {
        impl<T0, T: ?Sized + 'static> $trait<Mix<T>> for Vec<T0>
        where
            T0: Borrow<T> + Send + Sync + 'static,
        {
            fn $method(self) -> OutputResult<Response<T>> {
                Ok(self
                    .into_iter()
                    .map(|el| -> Box<dyn Borrow<T> + Send + Sync> { Box::new(el) })
                    .collect())
            }
        }
    };
}

into!(IntoReturnOnce, into_return_once);
into!(IntoReturn, into_return);
