use crate::call_pattern::{DynCallPattern, DynResponder, PatIndex};
use crate::error;
use crate::error::{MockError, MockResult};
use crate::macro_api::Evaluation;
use crate::mock_impl::{DynMockImpl, PatternMatchMode};
use crate::{DynMockFn, SharedState};
use crate::{FallbackMode, MockFn, MockInputs};

use std::borrow::Borrow;

enum Eval<C> {
    Continue(C),
    Unmock,
}

struct DynCall<'a> {
    mock_fn: DynMockFn,
    debug_inputs: &'a dyn Fn() -> String,
}

pub(crate) fn eval_sized<'i, F: MockFn + 'static>(
    state: &SharedState,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> MockResult<Evaluation<'i, F::Output, F>>
where
    F::Output: Sized,
{
    let call = DynCall {
        mock_fn: DynMockFn::new::<F>(),
        debug_inputs: &|| F::debug_inputs(&inputs),
    };

    match eval_responder::<F>(&call, state, &inputs)? {
        Eval::Continue((pat_index, responder)) => match responder {
            DynResponder::Value(inner) => {
                Ok(Evaluation::Evaluated(*inner.downcast::<F>().0.box_clone()))
            }
            DynResponder::Closure(inner) => {
                Ok(Evaluation::Evaluated(inner.downcast::<F>().0(inputs)))
            }
            DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
            responder => Err(sized_error(&call, pat_index, responder)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

#[inline(never)]
fn sized_error(call: &DynCall<'_>, pat_index: PatIndex, responder: &DynResponder) -> MockError {
    match responder {
        DynResponder::StaticRefClosure(_) | DynResponder::Borrowable(_) => {
            MockError::TypeMismatchExpectedOwnedInsteadOfBorrowed {
                name: call.mock_fn.name,
                inputs_debug: (call.debug_inputs)(),
                pat_index,
            }
        }
        DynResponder::Panic(msg) => MockError::ExplicitPanic {
            name: call.mock_fn.name,
            inputs_debug: (call.debug_inputs)(),
            pat_index,
            msg: msg.clone(),
        },
        _ => panic!("Responder error not handled"),
    }
}

pub(crate) fn eval_unsized_self_borrowed<'u, 'i, F: MockFn + 'static>(
    state: &'u SharedState,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> MockResult<Evaluation<'i, &'u F::Output, F>> {
    let call = DynCall {
        mock_fn: DynMockFn::new::<F>(),
        debug_inputs: &|| F::debug_inputs(&inputs),
    };

    match eval_responder::<F>(&call, state, &inputs)? {
        Eval::Continue((pat_index, responder)) => match responder {
            DynResponder::Value(inner) => Ok(Evaluation::Evaluated(
                inner.downcast::<F>().0.borrow_stored(),
            )),
            DynResponder::StaticRefClosure(inner) => {
                Ok(Evaluation::Evaluated(inner.downcast::<F>().0(inputs)))
            }
            DynResponder::Borrowable(inner) => {
                let borrowable: &dyn Borrow<<F as MockFn>::Output> =
                    inner.downcast::<F>().0.as_ref();
                let borrow = borrowable.borrow();
                Ok(Evaluation::Evaluated(borrow))
            }
            DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
            responder => Err(unsized_self_borrowed_error(&call, pat_index, responder)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

#[inline(never)]
fn unsized_self_borrowed_error(
    call: &DynCall<'_>,
    pat_index: PatIndex,
    responder: &DynResponder,
) -> MockError {
    match responder {
        DynResponder::Closure(_) => MockError::CannotBorrowValueProducedByClosure {
            name: call.mock_fn.name,
            inputs_debug: (call.debug_inputs)(),
            pat_index,
        },
        DynResponder::Panic(msg) => MockError::ExplicitPanic {
            name: call.mock_fn.name,
            inputs_debug: (call.debug_inputs)(),
            pat_index,
            msg: msg.clone(),
        },
        _ => panic!("Responder error not handled"),
    }
}

pub(crate) fn eval_unsized_static_ref<'i, F: MockFn + 'static>(
    state: &SharedState,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> MockResult<Evaluation<'i, &'static F::Output, F>> {
    let call = DynCall {
        mock_fn: DynMockFn::new::<F>(),
        debug_inputs: &|| F::debug_inputs(&inputs),
    };

    match eval_responder::<F>(&call, state, &inputs)? {
        Eval::Continue((pat_index, responder)) => match responder {
            DynResponder::StaticRefClosure(inner) => {
                Ok(Evaluation::Evaluated(inner.downcast::<F>().0(inputs)))
            }
            DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
            responder => Err(unsized_static_ref_error(&call, pat_index, responder)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

#[inline(never)]
fn unsized_static_ref_error(
    call: &DynCall<'_>,
    pat_index: PatIndex,
    responder: &DynResponder,
) -> MockError {
    match responder {
        DynResponder::Value(_) => MockError::CannotBorrowValueStatically {
            name: call.mock_fn.name,
            inputs_debug: (call.debug_inputs)(),
            pat_index,
        },
        DynResponder::Closure(_) => MockError::CannotBorrowValueProducedByClosure {
            name: call.mock_fn.name,
            inputs_debug: (call.debug_inputs)(),
            pat_index,
        },
        DynResponder::Borrowable(_) => MockError::CannotBorrowValueStatically {
            name: call.mock_fn.name,
            inputs_debug: (call.debug_inputs)(),
            pat_index,
        },
        DynResponder::Panic(msg) => MockError::ExplicitPanic {
            name: call.mock_fn.name,
            inputs_debug: (call.debug_inputs)(),
            pat_index,
            msg: msg.clone(),
        },
        _ => panic!("Responder error not handled"),
    }
}

fn eval_responder<'u, 'i, F: MockFn + 'static>(
    call: &DynCall<'_>,
    state: &'u SharedState,
    inputs: &<F as MockInputs<'i>>::Inputs,
) -> MockResult<Eval<(PatIndex, &'u DynResponder)>> {
    match eval_mock_op(call, state)? {
        Eval::Continue(dyn_mock_impl) => {
            let matched_pattern = match dyn_mock_impl.pattern_match_mode {
                PatternMatchMode::InAnyOrder => dyn_mock_impl
                    .call_patterns
                    .iter()
                    .enumerate()
                    .filter_map(
                        |(pat_index, pattern)| match pattern.match_inputs::<F>(inputs) {
                            Ok(false) => None,
                            Ok(true) => Some(Ok((PatIndex(pat_index), pattern))),
                            Err(err) => Some(Err(err)),
                        },
                    )
                    .next()
                    .transpose()?,
                PatternMatchMode::InOrder => {
                    try_select_in_order_call_pattern(call, state, dyn_mock_impl, &|pattern| {
                        pattern.match_inputs::<F>(inputs)
                    })?
                }
            };

            select_next_responder(call, state, matched_pattern)
        }
        Eval::Unmock => Ok(Eval::Unmock),
    }
}

#[inline(never)]
fn select_next_responder<'u>(
    call: &DynCall<'_>,
    state: &'u SharedState,
    matched_pattern: Option<(PatIndex, &'u DynCallPattern)>,
) -> MockResult<Eval<(PatIndex, &'u DynResponder)>> {
    match matched_pattern {
        Some((pat_index, pattern)) => match pattern.next_responder() {
            Some(responder) => Ok(Eval::Continue((pat_index, responder))),
            None => Err(MockError::NoOutputAvailableForCallPattern {
                name: call.mock_fn.name,
                inputs_debug: (call.debug_inputs)(),
                pat_index,
            }),
        },
        None => match state.fallback_mode {
            FallbackMode::Error => Err(MockError::NoMatchingCallPatterns {
                name: call.mock_fn.name,
                inputs_debug: (call.debug_inputs)(),
            }),
            FallbackMode::Unmock => Ok(Eval::Unmock),
        },
    }
}

#[inline(never)]
fn eval_mock_op<'u>(
    call: &DynCall<'_>,
    state: &'u SharedState,
) -> MockResult<Eval<&'u DynMockImpl>> {
    match state.impls.get(&call.mock_fn.type_id) {
        None => match state.fallback_mode {
            FallbackMode::Error => Err(MockError::NoMockImplementation {
                name: call.mock_fn.name,
            }),
            FallbackMode::Unmock => Ok(Eval::Unmock),
        },
        Some(dyn_impl) => Ok(Eval::Continue(dyn_impl)),
    }
}

#[inline(never)]
fn try_select_in_order_call_pattern<'u>(
    call: &DynCall<'_>,
    state: &'u SharedState,
    mock_impl: &'u DynMockImpl,
    match_inputs: &dyn Fn(&DynCallPattern) -> MockResult<bool>,
) -> MockResult<Option<(PatIndex, &'u DynCallPattern)>> {
    // increase call index here, because stubs should not influence it:
    let global_call_index = state
        .next_call_index
        .fetch_add(1, std::sync::atomic::Ordering::SeqCst);

    let (pat_index, pattern) = mock_impl
        .call_patterns
        .iter()
        .enumerate()
        .find(|(_, pattern)| {
            pattern.call_index_range.start <= global_call_index
                && pattern.call_index_range.end > global_call_index
        })
        .ok_or_else(|| MockError::CallOrderNotMatchedForMockFn {
            name: call.mock_fn.name,
            inputs_debug: (call.debug_inputs)(),
            actual_call_order: error::CallOrder(global_call_index),
            expected_ranges: mock_impl
                .call_patterns
                .iter()
                .map(|pattern| std::ops::Range {
                    start: pattern.call_index_range.start + 1,
                    end: pattern.call_index_range.end + 1,
                })
                .collect(),
        })?;

    if !match_inputs(pattern)? {
        return Err(MockError::InputsNotMatchedInCallOrder {
            name: call.mock_fn.name,
            inputs_debug: (call.debug_inputs)(),
            actual_call_order: error::CallOrder(global_call_index),
            pat_index: PatIndex(pat_index),
        });
    }

    Ok(Some((PatIndex(pat_index), pattern)))
}
