use unimock::*;

#[unimock]
trait GenericReturn<T> {
    fn generic_return(&self) -> T;
}
