pub trait Unborrow<'a> {
    type Unborrowed;

    fn reborrow(value: &'a Self::Unborrowed) -> Self;
}

impl<'a> Unborrow<'a> for &'a str {
    type Unborrowed = String;

    fn reborrow(value: &'a Self::Unborrowed) -> Self {
        value.as_str()
    }
}

impl<'a, T> Unborrow<'a> for &'a [T] {
    type Unborrowed = Vec<T>;

    fn reborrow(value: &'a Self::Unborrowed) -> Self {
        value.as_slice()
    }
}

impl<'a, T: Unborrow<'a>> Unborrow<'a> for Option<T> {
    type Unborrowed = Option<T::Unborrowed>;

    fn reborrow(value: &'a Self::Unborrowed) -> Self {
        value.as_ref().map(|inner| <T as Unborrow>::reborrow(inner))
    }
}

impl<'a, T: Unborrow<'a>, E: Unborrow<'a>> Unborrow<'a> for Result<T, E> {
    type Unborrowed = Result<T::Unborrowed, E::Unborrowed>;

    fn reborrow(value: &'a Self::Unborrowed) -> Self {
        match value {
            Ok(value) => Ok(<T as Unborrow>::reborrow(value)),
            Err(err) => Err(<E as Unborrow>::reborrow(err)),
        }
    }
}

impl<'a, T: Unborrow<'a>> Unborrow<'a> for std::ops::Range<T> {
    type Unborrowed = std::ops::Range<T::Unborrowed>;

    fn reborrow(value: &'a Self::Unborrowed) -> Self {
        let start = <T as Unborrow>::reborrow(&value.start);
        let end = <T as Unborrow>::reborrow(&value.end);

        start..end
    }
}

macro_rules! unb_identity {
    ($i:ident) => {
        impl<'a> Unborrow<'a> for $i {
            type Unborrowed = $i;

            fn reborrow(value: &Self::Unborrowed) -> Self {
                *value
            }
        }

        impl<'a> Unborrow<'a> for &'a $i {
            type Unborrowed = $i;

            fn reborrow(value: &'a Self::Unborrowed) -> Self {
                value
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
