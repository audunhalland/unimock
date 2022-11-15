use std::{
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
};

pub trait Possess<'a> {
    type Possessed;

    fn reborrow(value: &'a Self::Possessed) -> Self;
}

impl<'a> Possess<'a> for &'a str {
    type Possessed = String;

    fn reborrow(value: &'a Self::Possessed) -> Self {
        value.as_str()
    }
}

impl<'a> Possess<'a> for &'a Path {
    type Possessed = PathBuf;

    fn reborrow(value: &'a Self::Possessed) -> Self {
        value.as_ref()
    }
}

impl<'a> Possess<'a> for &'a OsStr {
    type Possessed = OsString;

    fn reborrow(value: &'a Self::Possessed) -> Self {
        value.as_os_str()
    }
}

impl<'a, T> Possess<'a> for &'a [T] {
    type Possessed = Vec<T>;

    fn reborrow(value: &'a Self::Possessed) -> Self {
        value.as_slice()
    }
}

impl<'a, T: Possess<'a>> Possess<'a> for Option<T> {
    type Possessed = Option<T::Possessed>;

    fn reborrow(value: &'a Self::Possessed) -> Self {
        value.as_ref().map(|inner| <T as Possess>::reborrow(inner))
    }
}

impl<'a, T: Possess<'a>, E: Possess<'a>> Possess<'a> for Result<T, E> {
    type Possessed = Result<T::Possessed, E::Possessed>;

    fn reborrow(value: &'a Self::Possessed) -> Self {
        match value {
            Ok(value) => Ok(<T as Possess>::reborrow(value)),
            Err(err) => Err(<E as Possess>::reborrow(err)),
        }
    }
}

impl<'a, T: Possess<'a>> Possess<'a> for std::ops::Range<T> {
    type Possessed = std::ops::Range<T::Possessed>;

    fn reborrow(value: &'a Self::Possessed) -> Self {
        let start = <T as Possess>::reborrow(&value.start);
        let end = <T as Possess>::reborrow(&value.end);

        start..end
    }
}

macro_rules! possess_identity {
    ($i:ident) => {
        impl<'a> Possess<'a> for $i {
            type Possessed = $i;

            fn reborrow(value: &Self::Possessed) -> Self {
                *value
            }
        }

        impl<'a> Possess<'a> for &'a $i {
            type Possessed = $i;

            fn reborrow(value: &'a Self::Possessed) -> Self {
                value
            }
        }
    };
}

possess_identity!(u8);
possess_identity!(u16);
possess_identity!(u32);
possess_identity!(u64);
possess_identity!(u128);
possess_identity!(usize);
possess_identity!(i8);
possess_identity!(i16);
possess_identity!(i32);
possess_identity!(i64);
possess_identity!(i128);
possess_identity!(char);
