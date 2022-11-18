use crate::as_owned::AsOwned;
use crate::call_pattern::{
    BorrowResponder2, OwnedResponder2, StoredValueSlot, StoredValueSlotOnce,
};
use crate::output::{Borrowed, Mixed, Output, Owned, StaticRef};
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

trait MakeMultiResponder<F: MockFn> {}

impl<F, T> RespondOnce<F> for Owned<T>
where
    F: MockFn<Output = Self>,
    T: Send + Sync + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(
            OwnedResponder2::<F> {
                stored_value: Box::new(StoredValueSlotOnce::new(output)),
            }
            .into_dyn_responder(),
        )
    }
}

impl<F, T> Respond<F> for Owned<T>
where
    F: MockFn<Output = Self>,
    T: Clone + Send + Sync + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(
            OwnedResponder2::<F> {
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
        Responder(BorrowResponder2::<F> { borrowable: output }.into_dyn_responder())
    }
}

impl<F, T> Respond<F> for Borrowed<T>
where
    F: MockFn<Output = Self>,
    T: ?Sized + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(BorrowResponder2::<F> { borrowable: output }.into_dyn_responder())
    }
}

impl<F, T> RespondOnce<F> for StaticRef<T>
where
    F: MockFn<Output = Self>,
    T: ?Sized + Send + Sync + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(BorrowResponder2::<F> { borrowable: output }.into_dyn_responder())
    }
}

impl<F, T> Respond<F> for StaticRef<T>
where
    F: MockFn<Output = Self>,
    T: ?Sized + Send + Sync + 'static,
{
    fn responder(output: <<F as MockFn>::Output as Output>::Type) -> Responder {
        Responder(BorrowResponder2::<F> { borrowable: output }.into_dyn_responder())
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
            OwnedResponder2::<F> {
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
            OwnedResponder2::<F> {
                stored_value: Box::new(StoredValueSlot(output)),
            }
            .into_dyn_responder(),
        )
    }
}
