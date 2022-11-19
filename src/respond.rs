use crate::as_owned::AsOwned;
use crate::call_pattern::{
    BorrowResponder, DynResponder, OwnedResponder, StoredValueSlot, StoredValueSlotOnce,
};
use crate::output::{Borrowed, Complex, Mixed, Output, Owned, StaticRef};
use crate::{MockFn, Responder};

/// Outputs that can respond at least once implement this trait.
pub trait RespondOnce<F: MockFn> {
    // Create a responder that can answer _at least_ once
    #[doc(hidden)]
    fn responder(output: <F::Output as Output>::Type) -> Responder;
}

/// Outputs that can respond any number of times implement this trait.
pub trait Respond<F: MockFn>: RespondOnce<F> {
    // Create a responder that can answer any number of times
    #[doc(hidden)]
    fn responder(output: <F::Output as Output>::Type) -> Responder;
}

impl<F, T> RespondOnce<F> for Owned<T>
where
    F: MockFn<Output = Self>,
    T: Send + Sync + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(DynResponder::new_owned::<F>(output))
    }
}

impl<F, T> Respond<F> for Owned<T>
where
    F: MockFn<Output = Self>,
    T: Clone + Send + Sync + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(
            OwnedResponder::<F> {
                stored_value: Box::new(StoredValueSlot(output)),
            }
            .into_dyn_responder(),
        )
    }
}

impl<F, T> RespondOnce<F> for Borrowed<T>
where
    F: MockFn<Output = Self>,
    T: ?Sized + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(BorrowResponder::<F> { borrowable: output }.into_dyn_responder())
    }
}

impl<F, T> Respond<F> for Borrowed<T>
where
    F: MockFn<Output = Self>,
    T: ?Sized + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(BorrowResponder::<F> { borrowable: output }.into_dyn_responder())
    }
}

impl<F, T> RespondOnce<F> for StaticRef<T>
where
    F: MockFn<Output = Self>,
    T: ?Sized + Send + Sync + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(BorrowResponder::<F> { borrowable: output }.into_dyn_responder())
    }
}

impl<F, T> Respond<F> for StaticRef<T>
where
    F: MockFn<Output = Self>,
    T: ?Sized + Send + Sync + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(BorrowResponder::<F> { borrowable: output }.into_dyn_responder())
    }
}

impl<F, T> RespondOnce<F> for Mixed<T>
where
    F: MockFn<Output = Self>,
    T: AsOwned<'static>,
    <T as AsOwned<'static>>::Owned: Send + Sync,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(
            OwnedResponder::<F> {
                stored_value: Box::new(StoredValueSlotOnce::new(output)),
            }
            .into_dyn_responder(),
        )
    }
}

impl<F, T> Respond<F> for Mixed<T>
where
    F: MockFn<Output = Self>,
    T: AsOwned<'static>,
    <T as AsOwned<'static>>::Owned: Clone + Send + Sync,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(
            OwnedResponder::<F> {
                stored_value: Box::new(StoredValueSlot(output)),
            }
            .into_dyn_responder(),
        )
    }
}

impl<F, T> RespondOnce<F> for Complex<T>
where
    Self: Output,
    <Self as Output>::Type: Send + Sync,
    F: MockFn<Output = Self>,
    T: 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(
            OwnedResponder::<F> {
                stored_value: Box::new(StoredValueSlotOnce::new(output)),
            }
            .into_dyn_responder(),
        )
    }
}

impl<F, T> Respond<F> for Complex<T>
where
    Self: Output,
    <Self as Output>::Type: Clone + Send + Sync,
    F: MockFn<Output = Self>,
    T: 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(
            OwnedResponder::<F> {
                stored_value: Box::new(StoredValueSlotOnce::new(output)),
            }
            .into_dyn_responder(),
        )
    }
}
