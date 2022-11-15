pub trait Unborrow {
    type Unborrowed;

    fn reborrow(value: &Self::Unborrowed) -> Self;
}

impl Unborrow for &str {
    type Unborrowed = String;

    fn reborrow(value: &Self::Unborrowed) -> Self {
        // value.as_str()
        panic!()
    }
}

impl<T> Unborrow for &[T] {
    type Unborrowed = Vec<T>;

    fn reborrow(value: &Self::Unborrowed) -> Self {
        // value.as_slice()
        panic!()
    }
}

impl<T: Unborrow> Unborrow for Option<T> {
    type Unborrowed = Option<T::Unborrowed>;

    fn reborrow(value: &Self::Unborrowed) -> Self {
        value.as_ref().map(|inner| <T as Unborrow>::reborrow(inner))
    }
}

impl<T: Unborrow, E: Unborrow> Unborrow for Result<T, E> {
    type Unborrowed = Result<T::Unborrowed, E::Unborrowed>;

    fn reborrow(value: &Self::Unborrowed) -> Self {
        match value {
            Ok(value) => Ok(<T as Unborrow>::reborrow(value)),
            Err(err) => Err(<E as Unborrow>::reborrow(err)),
        }
    }
}

impl<T: Unborrow> Unborrow for std::ops::Range<T> {
    type Unborrowed = std::ops::Range<T::Unborrowed>;

    fn reborrow(value: &Self::Unborrowed) -> Self {
        let start = <T as Unborrow>::reborrow(&value.start);
        let end = <T as Unborrow>::reborrow(&value.end);

        start..end
    }
}

macro_rules! unb_identity {
    ($i:ident) => {
        impl Unborrow for $i {
            type Unborrowed = $i;

            fn reborrow(value: &Self::Unborrowed) -> Self {
                *value
            }
        }

        impl Unborrow for &$i {
            type Unborrowed = $i;

            fn reborrow(value: &Self::Unborrowed) -> Self {
                // value
                panic!()
            }
        }
    };
}

unb_identity!(u8);
unb_identity!(u16);
unb_identity!(u32);
unb_identity!(u64);
unb_identity!(u128);
unb_identity!(usize);
unb_identity!(i8);
unb_identity!(i16);
unb_identity!(i32);
unb_identity!(i64);
unb_identity!(i128);
unb_identity!(char);
