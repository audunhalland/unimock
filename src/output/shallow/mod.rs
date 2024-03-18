mod option;
mod result;
mod vec;

/// An output Kind where T is a generic type and some of the parameters are borrows.
///
/// The `T` is interpreted as a type directly.
///
/// e.g. `Shallow<Option<&i64>>`
#[doc(hidden)]
pub struct Shallow<T: ?Sized>(core::marker::PhantomData<fn() -> T>);
