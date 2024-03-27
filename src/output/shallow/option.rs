use core::borrow::Borrow;

use crate::output::*;

type Ref<T> = Shallow<Option<&'static T>>;
type Mut<T> = Shallow<Option<&'static mut T>>;

type RefResponse<T> = Option<Box<dyn Borrow<T> + Send + Sync>>;
type MutResponse<T> = Option<Mutable<Box<dyn Borrow<T> + Send + Sync>>>;

impl<T: ?Sized> Kind for Ref<T> {
    type Return = RefResponse<T>;
}

impl<T: ?Sized> Kind for Mut<T> {
    type Return = MutResponse<T>;
}

impl<T: ?Sized> Return for Ref<T> {
    type Type = Option<Box<dyn Borrow<T> + Send + Sync>>;
}

impl<T: ?Sized + 'static> GetOutput for RefResponse<T> {
    type Output<'u> = Option<&'u T>
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        Some(self.as_ref().map(|val| val.as_ref().borrow()))
    }
}

impl<T: ?Sized + 'static> GetOutput for MutResponse<T> {
    type Output<'u> = Option<&'u mut T>
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        None
    }
}

macro_rules! into {
    ($trait:ident, $method:ident) => {
        impl<T0, T: ?Sized + 'static> $trait<Ref<T>> for Option<T0>
        where
            T0: Borrow<T> + Send + Sync + 'static,
        {
            fn $method(self) -> OutputResult<RefResponse<T>> {
                Ok(match self {
                    Some(val) => Some(Box::new(val)),
                    None => None,
                })
            }
        }
    };
}

into!(IntoReturnOnce, into_return_once);
into!(IntoReturn, into_return);
