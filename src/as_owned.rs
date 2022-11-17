use std::{
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
    time::Duration,
};

pub trait AsOwned<'a> {
    type Owned: 'static;

    fn from_owned(value: &'a Self::Owned) -> Self;
}

impl<'a> AsOwned<'a> for &'a str {
    type Owned = String;

    fn from_owned(value: &'a Self::Owned) -> Self {
        value.as_str()
    }
}

impl<'a> AsOwned<'a> for &'a Path {
    type Owned = PathBuf;

    fn from_owned(value: &'a Self::Owned) -> Self {
        value.as_ref()
    }
}

impl<'a> AsOwned<'a> for &'a OsStr {
    type Owned = OsString;

    fn from_owned(value: &'a Self::Owned) -> Self {
        value.as_os_str()
    }
}

impl<'a, T: 'static> AsOwned<'a> for &'a [T] {
    type Owned = Vec<T>;

    fn from_owned(value: &'a Self::Owned) -> Self {
        value.as_slice()
    }
}

impl<'a, T: AsOwned<'a>> AsOwned<'a> for Option<T> {
    type Owned = Option<T::Owned>;

    fn from_owned(value: &'a Self::Owned) -> Self {
        value
            .as_ref()
            .map(|inner| <T as AsOwned>::from_owned(inner))
    }
}

impl<'a, T: AsOwned<'a>, E: AsOwned<'a>> AsOwned<'a> for Result<T, E> {
    type Owned = Result<T::Owned, E::Owned>;

    fn from_owned(value: &'a Self::Owned) -> Self {
        match value {
            Ok(value) => Ok(<T as AsOwned>::from_owned(value)),
            Err(err) => Err(<E as AsOwned>::from_owned(err)),
        }
    }
}

impl<'a, T: AsOwned<'a>> AsOwned<'a> for std::ops::Range<T> {
    type Owned = std::ops::Range<T::Owned>;

    fn from_owned(value: &'a Self::Owned) -> Self {
        let start = <T as AsOwned>::from_owned(&value.start);
        let end = <T as AsOwned>::from_owned(&value.end);

        start..end
    }
}

macro_rules! impl_identity {
    ($i:tt) => {
        impl<'a> AsOwned<'a> for $i {
            type Owned = $i;

            fn from_owned(value: &Self::Owned) -> Self {
                value.clone()
            }
        }

        impl<'a> AsOwned<'a> for &'a $i {
            type Owned = $i;

            fn from_owned(value: &'a Self::Owned) -> Self {
                value
            }
        }
    };
}

impl_identity!(());
impl_identity!(bool);
impl_identity!(u8);
impl_identity!(u16);
impl_identity!(u32);
impl_identity!(u64);
impl_identity!(u128);
impl_identity!(usize);
impl_identity!(i8);
impl_identity!(i16);
impl_identity!(i32);
impl_identity!(i64);
impl_identity!(i128);
impl_identity!(char);
impl_identity!(isize);
impl_identity!(f32);
impl_identity!(f64);
impl_identity!(String);
impl_identity!(PathBuf);
impl_identity!(OsString);
impl_identity!(Duration);
