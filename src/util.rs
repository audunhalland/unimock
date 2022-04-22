/// Conveniently leak some value to produce a static reference.
pub trait Leak {
    fn leak(self) -> &'static Self;
}

impl<T: 'static> Leak for T {
    fn leak(self) -> &'static Self {
        Box::leak(Box::new(self))
    }
}

///
/// Internal trait implemented by references that allows transforming from `&T` to `&'static T`
/// by leaking memory.
///
/// The trait is implemented for all `&T`. It allows functions to refer to the non-referenced owned value `T`,
/// and leak that.
///
pub trait LeakInto {
    type Owned: 'static;

    fn leak_into(value: Self::Owned) -> Self;
}

impl<T: 'static> LeakInto for &T {
    type Owned = T;

    fn leak_into(value: Self::Owned) -> Self {
        Box::leak(Box::new(value))
    }
}
