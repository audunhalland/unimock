mod option;
mod poll;
mod result;
mod tuples;
mod vec;

/// An output Kind where T is a generic type and some of the parameters are borrows.
///
/// The `T` is interpreted as a type that takes another [crate::output::Kind].
/// e.g.:
///
/// `Deep<Option<Owned<i32>>>`
///
/// This way, Deep kinds can be nested indefinitely.
#[doc(hidden)]
pub struct Deep<T>(core::marker::PhantomData<T>);
