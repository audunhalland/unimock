use crate::debug;
use crate::error::{MockError, MockResult};
use crate::output::{ComplexOutput, Output, OwnedOutput, RefOutput, StaticRefOutput, StoreOutput};
use crate::*;

use std::any::Any;
use std::borrow::Borrow;
use std::sync::Mutex;

#[derive(Clone, Copy)]
pub(crate) struct PatIndex(pub usize);

impl std::fmt::Display for PatIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub(crate) type AnyBox = Box<dyn Any + Send + Sync + 'static>;

fn downcast_box<'b, T: 'static>(any_box: &'b AnyBox, name: &'static str) -> MockResult<&'b T> {
    any_box.downcast_ref().ok_or(MockError::Downcast { name })
}

pub(crate) struct CallPattern {
    pub input_matcher: DynInputMatcher,
    pub responders: Vec<DynCallOrderResponder>,
    pub ordered_call_index_range: std::ops::Range<usize>,
    pub call_counter: counter::CallCounter,
}

impl CallPattern {
    pub fn match_inputs<F: MockFn>(&self, inputs: &F::Inputs<'_>) -> MockResult<bool> {
        let func: &MatchingFn<F> =
            downcast_box(self.input_matcher.get_dyn_matching_fn(F::NAME)?, F::NAME)?;

        Ok((func.0)(inputs))
    }

    pub fn debug_location(&self, pat_index: PatIndex) -> debug::CallPatternLocation {
        if let Some(debug) = self.input_matcher.pat_debug {
            debug::CallPatternLocation::Debug(debug)
        } else {
            debug::CallPatternLocation::PatIndex(pat_index)
        }
    }

    pub fn next_responder(&self) -> Option<&DynResponder> {
        find_responder_by_call_index(&self.responders, self.call_counter.fetch_add())
    }
}

pub(crate) struct DynInputMatcher {
    pub(crate) func: Option<AnyBox>,
    pub(crate) pat_debug: Option<debug::InputMatcherDebug>,
}

impl DynInputMatcher {
    pub fn from_matching_fn<F: MockFn>(matching_fn: &dyn Fn(&mut Matching<F>)) -> Self {
        let mut builder = Matching::new();
        matching_fn(&mut builder);

        Self {
            func: match builder.matching_fn {
                Some(matching_fn) => Some(Box::new(matching_fn)),
                None => None,
            },
            pat_debug: builder.pat_debug,
        }
    }

    fn get_dyn_matching_fn(&self, name: &'static str) -> MockResult<&AnyBox> {
        match &self.func {
            Some(any_box) => Ok(any_box),
            None => Err(MockError::NoMatcherFunction { name }),
        }
    }
}

pub(crate) struct MatchingFn<F: MockFn>(
    #[allow(clippy::type_complexity)]
    pub  Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync>,
);

impl<F: MockFn> MatchingFn<F> {}

pub(crate) struct DynCallOrderResponder {
    pub response_index: usize,
    pub responder: DynResponder,
}

pub(crate) enum DynResponder {
    Value(DynValueResponder),
    Borrowable(DynBorrowableResponder),
    Closure(DynClosureResponder),
    StaticRefClosure(DynStaticRefClosureResponder),
    Panic(String),
    Unmock,
}

pub(crate) struct DynValueResponder(AnyBox);
pub(crate) struct DynBorrowableResponder(AnyBox);
pub(crate) struct DynClosureResponder(AnyBox);
pub(crate) struct DynStaticRefClosureResponder(AnyBox);

impl DynValueResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&ValueResponder<F>> {
        downcast_box(&self.0, F::NAME)
    }
}

impl DynBorrowableResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&BorrowableResponder<F>> {
        downcast_box(&self.0, F::NAME)
    }
}

impl DynClosureResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&ClosureResponder<F>> {
        downcast_box(&self.0, F::NAME)
    }
}

impl DynStaticRefClosureResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&StaticRefClosureResponder<F>> {
        downcast_box(&self.0, F::NAME)
    }
}

pub(crate) trait IntoDynResponder: Sized {
    fn into_dyn_responder(self) -> DynResponder;
}

pub(crate) struct ValueResponder<F: MockFn> {
    pub stored_value: Box<dyn CloneOrTakeOrBorrow<F::Output>>,
}
pub(crate) struct BorrowableResponder<F: MockFn> {
    pub borrowable: Box<dyn Borrow<F::Output> + Send + Sync>,
}
pub(crate) struct ClosureResponder<F: MockFn> {
    #[allow(clippy::type_complexity)]
    pub func: Box<dyn (for<'i> Fn(F::Inputs<'i>) -> F::Output) + Send + Sync>,
}
pub(crate) struct StaticRefClosureResponder<F: MockFn> {
    #[allow(clippy::type_complexity)]
    pub func: Box<dyn (for<'i> Fn(F::Inputs<'i>) -> &'static F::Output) + Send + Sync>,
}

impl<F: MockFn> IntoDynResponder for ValueResponder<F> {
    fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Value(DynValueResponder(Box::new(self)))
    }
}

impl<F: MockFn> IntoDynResponder for BorrowableResponder<F> {
    fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Borrowable(DynBorrowableResponder(Box::new(self)))
    }
}

impl<F: MockFn> IntoDynResponder for ClosureResponder<F> {
    fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Closure(DynClosureResponder(Box::new(self)))
    }
}

impl<F: MockFn> IntoDynResponder for StaticRefClosureResponder<F> {
    fn into_dyn_responder(self) -> DynResponder {
        DynResponder::StaticRefClosure(DynStaticRefClosureResponder(Box::new(self)))
    }
}

pub(crate) struct DynCallOrderResponder2 {
    pub response_index: usize,
    pub responder: DynResponder2,
}

pub(crate) enum DynResponder2 {
    Owned(DynOwnedResponder2),
    Ref(DynRefResponder2),
    StaticRefClosure(DynStaticRefClosureResponder2),
    ComplexValue(DynComplexValueResponder2),
}

pub(crate) struct DynOwnedResponder2(AnyBox);
pub(crate) struct DynRefResponder2(AnyBox);
pub(crate) struct DynStaticRefClosureResponder2(AnyBox);
pub(crate) struct DynComplexValueResponder2(AnyBox);

impl DynComplexValueResponder2 {
    pub fn downcast<F: MockFn2>(&self) -> MockResult<&ComplexValueResponder2<F>>
    where
        for<'u> F::Output<'u>: StoreOutput<'u>,
    {
        downcast_box(&self.0, "FAKE NAME")
    }
}

pub(crate) struct OwnedResponder2<F: MockFn2>
where
    for<'u> F::Output<'u>: OwnedOutput<'u>,
{
    pub stored_value: Box<dyn CloneOrTakeOrBorrow<<F::Output<'static> as Output<'static>>::Type>>,
}

pub(crate) struct RefResponder2<F: MockFn2>
where
    for<'u> F::Output<'u>: RefOutput<'u>,
{
    pub borrowable: Box<dyn Borrow<<F::Output<'static> as Output<'static>>::Type> + Send + Sync>,
}

pub(crate) struct StaticRefClosureResponder2<F: MockFn2>
where
    for<'u> F::Output<'u>: StaticRefOutput,
{
    #[allow(clippy::type_complexity)]
    pub func: Box<
        dyn (for<'i> Fn(F::Inputs<'i>) -> <F::Output<'static> as Output<'static>>::Type)
            + Send
            + Sync,
    >,
}

pub(crate) struct ComplexValueResponder2<F: MockFn2>
where
    for<'u> F::Output<'u>: StoreOutput<'u>,
{
    pub stored_value:
        Box<dyn CloneOrTakeOrBorrow<<F::Output<'static> as StoreOutput<'static>>::Stored>>,
}

impl<F: MockFn2> OwnedResponder2<F>
where
    for<'u> F::Output<'u>: OwnedOutput<'u>,
{
    pub fn into_dyn_responder(self) -> DynResponder2 {
        DynResponder2::Owned(DynOwnedResponder2(Box::new(self)))
    }
}

impl<F: MockFn2> RefResponder2<F>
where
    for<'u> F::Output<'u>: RefOutput<'u>,
{
    pub fn into_dyn_responder(self) -> DynResponder2 {
        DynResponder2::Ref(DynRefResponder2(Box::new(self)))
    }
}

impl<F: MockFn2> StaticRefClosureResponder2<F>
where
    for<'u> F::Output<'u>: StaticRefOutput,
{
    pub fn into_dyn_responder(self) -> DynResponder2 {
        DynResponder2::StaticRefClosure(DynStaticRefClosureResponder2(Box::new(self)))
    }
}

impl<F: MockFn2> ComplexValueResponder2<F>
where
    for<'u> F::Output<'u>: ComplexOutput<'u>,
{
    pub fn into_dyn_responder(self) -> DynResponder2 {
        DynResponder2::ComplexValue(DynComplexValueResponder2(Box::new(self)))
    }
}

pub(crate) trait CloneOrTakeOrBorrow<T: ?Sized>: Send + Sync {
    fn box_take_or_clone(&self) -> Option<Box<T>>;

    fn borrow_stored(&self) -> &T;
}

pub(crate) struct StoredValueSlot<T>(pub T);

impl<T: Clone + Send + Sync> CloneOrTakeOrBorrow<T> for StoredValueSlot<T> {
    fn box_take_or_clone(&self) -> Option<Box<T>> {
        Some(Box::new(self.0.clone()))
    }

    fn borrow_stored(&self) -> &T {
        &self.0
    }
}

pub(crate) struct StoredValueSlotOnce<T> {
    initial_value: Mutex<Option<T>>,
    borrowed_value: lazycell::AtomicLazyCell<T>,
}

impl<T> StoredValueSlotOnce<T> {
    pub fn new(value: T) -> Self {
        Self {
            initial_value: Mutex::new(Some(value)),
            borrowed_value: lazycell::AtomicLazyCell::new(),
        }
    }
}

impl<T: Send + Sync> CloneOrTakeOrBorrow<T> for StoredValueSlotOnce<T> {
    fn box_take_or_clone(&self) -> Option<Box<T>> {
        let mut lock = self.initial_value.lock().unwrap();
        lock.take().map(|value| Box::new(value))
    }

    fn borrow_stored(&self) -> &T {
        if let Some(value) = self.borrowed_value.borrow() {
            return value;
        }

        {
            let mut lock = self.initial_value.lock().unwrap();
            if let Some(value) = lock.take() {
                if self.borrowed_value.fill(value).is_err() {
                    panic!("Tried to set borrowed value twice");
                }
            }
        }

        self.borrowed_value
            .borrow()
            .expect("Tried to borrow a value that has already been taken")
    }
}

fn find_responder_by_call_index(
    responders: &[DynCallOrderResponder],
    call_index: usize,
) -> Option<&DynResponder> {
    if responders.is_empty() {
        return None;
    }

    let index_result =
        responders.binary_search_by(|responder| responder.response_index.cmp(&call_index));

    Some(match index_result {
        Ok(index) => &responders[index].responder,
        Err(insert_index) => &responders[insert_index - 1].responder,
    })
}

#[cfg(test)]
mod tests {
    use crate::output::Complex;

    use super::*;

    #[test]
    fn should_select_responder_with_lower_call_index() {
        let responders = vec![
            DynCallOrderResponder {
                response_index: 0,
                responder: DynResponder::Panic("0".to_string()),
            },
            DynCallOrderResponder {
                response_index: 5,
                responder: DynResponder::Panic("5".to_string()),
            },
        ];

        fn find_msg(responders: &[DynCallOrderResponder], call_index: usize) -> Option<&str> {
            find_responder_by_call_index(responders, call_index).map(|responder| match responder {
                DynResponder::Panic(msg) => msg.as_str(),
                _ => panic!(),
            })
        }

        assert_eq!(find_msg(&[], 42), None);
        assert_eq!(find_msg(&responders, 0), Some("0"));
        assert_eq!(find_msg(&responders, 4), Some("0"));
        assert_eq!(find_msg(&responders, 5), Some("5"));
        assert_eq!(find_msg(&responders, 7), Some("5"));
    }

    #[test]
    fn can_convert_complex_back_to_borrowed() {
        struct Test;

        impl MockFn2 for Test {
            type Inputs<'i> = ();
            type Output<'u> = Complex<'u, Option<&'u str>>;
        }

        let q = Test.some_call().returns(Some("fancy".to_string()));
        let call_pattern_builder = q.builder.inner();
        let resp = &call_pattern_builder.responders2[0];
        let dyn_complex_resp = match &resp.responder {
            DynResponder2::ComplexValue(c) => c,
            _ => panic!(),
        };

        let complex_resp = match dyn_complex_resp.downcast::<Test>() {
            Ok(r) => r,
            Err(_) => panic!(),
        };
        let borrowed_stored: &Option<String> = complex_resp.stored_value.borrow_stored();
        let reborrowed = util_reborrow_complex::<Test>(borrowed_stored).unwrap();

        assert_eq!(Some("fancy"), reborrowed);
    }

    fn util_reborrow_complex<'u, F: MockFn2>(
        stored: &'u <F::Output<'u> as StoreOutput<'u>>::Stored,
    ) -> Option<<F::Output<'u> as Output<'u>>::Type>
    where
        F::Output<'u>: StoreOutput<'u>,
    {
        <F::Output<'u> as StoreOutput<'u>>::reborrow_complex(stored)
    }
}
