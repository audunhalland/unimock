//! Covers a shallow Result<&T, E>

use core::borrow::Borrow;

use crate::output::{owning::Owned, *};

type Ref<T, E> = Shallow<Result<&'static T, E>>;
type Mut<T, E> = Shallow<Result<&'static mut T, E>>;

type RefResponse<T, E> = Result<Box<dyn Borrow<T> + Send + Sync>, Owned<E>>;
type MutResponse<T, E> = Result<Mutable<Box<dyn Borrow<T> + Send + Sync>>, Owned<E>>;

impl<T: ?Sized, E: 'static> Kind for Ref<T, E> {
    type Return = RefResponse<T, E>;
}

impl<T: ?Sized, E: 'static> Kind for Mut<T, E> {
    type Return = MutResponse<T, E>;
}

impl<T: ?Sized, E: 'static> Return for Ref<T, E>
where
    Owning<E>: Return,
{
    type Type = Result<Box<dyn Borrow<T> + Send + Sync>, <Owning<E> as Return>::Type>;
}

impl<T: ?Sized + 'static, E: 'static> GetOutput for RefResponse<T, E> {
    type Output<'u> = Result<&'u T, E>
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        Some(match self {
            Ok(val) => Ok(val.as_ref().borrow()),
            Err(err) => Err(err.output()?),
        })
    }
}

impl<T: ?Sized + 'static, E: 'static> GetOutput for MutResponse<T, E> {
    type Output<'u> = Result<&'u mut T, E>
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        match self {
            Ok(_) => None,
            Err(err) => Some(Err(err.output()?)),
        }
    }
}

macro_rules! into {
    ($trait:ident, $method:ident) => {
        impl<T0, T: ?Sized + 'static, E0, E: 'static> $trait<Ref<T, E>> for Result<T0, E0>
        where
            T0: Borrow<T> + Send + Sync + 'static,
            E0: $trait<Owning<E>>,
        {
            fn $method(self) -> OutputResult<RefResponse<T, E>> {
                Ok(match self {
                    Ok(val) => Ok(Box::new(val)),
                    Err(err) => Err(err.$method()?),
                })
            }
        }
    };
}

into!(IntoReturnOnce, into_return_once);
into!(IntoReturn, into_return);
