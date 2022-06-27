#![feature(generic_associated_types)]
#![feature(type_alias_impl_trait)]

use unimock::*;

#[unimock]
trait Foobar {
    type Fut<'a>: ::core::future::Future<Output = i32> + Send
    where
        Self: 'a;
    fn foobar<'a>(&'a self, arg: i32) -> Self::Fut<'a>;
}
