use crate::error;
use crate::error::MockError;
use crate::macro_api::Evaluation;
use crate::mock_impl::{CallPattern, DynMockImpl, PatternMatchMode, Responder, TypedMockImpl};
use crate::{FallbackMode, MockFn, MockInputs};

use std::any::Any;
use std::borrow::Borrow;
use std::sync::atomic::AtomicUsize;

enum Eval<C> {
    Continue(C),
    Unmock,
}

pub(crate) fn eval_sized<'i, F: MockFn + 'static>(
    dyn_impl: Option<&'i DynMockImpl>,
    inputs: <F as MockInputs<'i>>::Inputs,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Evaluation<'i, F::Output, F>, MockError>
where
    F::Output: Sized,
{
    match eval_responder(dyn_impl, &inputs, call_index, fallback_mode)? {
        Eval::Continue((pat_index, responder)) => match responder {
            Responder::Value(stored) => Ok(Evaluation::Evaluated(*stored.box_clone())),
            Responder::Closure(closure) => Ok(Evaluation::Evaluated(closure(inputs))),
            Responder::StaticRefClosure(_) | Responder::Borrowable(_) => {
                Err(MockError::TypeMismatchExpectedOwnedInsteadOfBorrowed {
                    name: F::NAME,
                    inputs_debug: F::debug_inputs(&inputs),
                    pat_index,
                })
            }
            Responder::Panic(msg) => Err(MockError::ExplicitPanic {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
                msg: msg.clone(),
            }),
            Responder::Unmock => Ok(Evaluation::Skipped(inputs)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

pub(crate) fn eval_unsized_self_borrowed<'i, 's: 'i, F: MockFn + 'static>(
    dyn_impl: Option<&'s DynMockImpl>,
    inputs: <F as MockInputs<'i>>::Inputs,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Evaluation<'i, &'s F::Output, F>, MockError> {
    match eval_responder::<F>(dyn_impl, &inputs, call_index, fallback_mode)? {
        Eval::Continue((pat_index, responder)) => match responder {
            Responder::Value(stored) => Ok(Evaluation::Evaluated(stored.borrow_stored())),
            Responder::Closure(_) => Err(MockError::CannotBorrowValueProducedByClosure {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            Responder::StaticRefClosure(closure) => Ok(Evaluation::Evaluated(closure(inputs))),
            Responder::Borrowable(borrowable) => {
                let borrowable: &dyn Borrow<<F as MockFn>::Output> = borrowable.as_ref();
                let borrow = borrowable.borrow();
                Ok(Evaluation::Evaluated(borrow))
            }
            Responder::Panic(msg) => Err(MockError::ExplicitPanic {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
                msg: msg.clone(),
            }),
            Responder::Unmock => Ok(Evaluation::Skipped(inputs)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

pub(crate) fn eval_unsized_static_ref<'i, 's: 'i, F: MockFn + 'static>(
    dyn_impl: Option<&'s DynMockImpl>,
    inputs: <F as MockInputs<'i>>::Inputs,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Evaluation<'i, &'static F::Output, F>, MockError> {
    match eval_responder::<F>(dyn_impl, &inputs, call_index, fallback_mode)? {
        Eval::Continue((pat_index, responder)) => match responder {
            Responder::Value(_) => Err(MockError::CannotBorrowValueStatically {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            Responder::Closure(_) => Err(MockError::CannotBorrowValueProducedByClosure {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            Responder::StaticRefClosure(closure) => Ok(Evaluation::Evaluated(closure(inputs))),
            Responder::Borrowable(_) => Err(MockError::CannotBorrowValueStatically {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            Responder::Panic(msg) => Err(MockError::ExplicitPanic {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
                msg: msg.clone(),
            }),
            Responder::Unmock => Ok(Evaluation::Skipped(inputs)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

fn eval_responder<'i, 's: 'i, F: MockFn + 'static>(
    dyn_impl: Option<&'s DynMockImpl>,
    inputs: &<F as MockInputs<'i>>::Inputs,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Eval<(usize, &'s Responder<F>)>, MockError> {
    match eval_type_erased_mock_impl(dyn_impl, F::NAME, fallback_mode)? {
        Eval::Continue((any, pattern_match_mode)) => {
            let typed_impl = any
                .downcast_ref::<TypedMockImpl<F>>()
                .ok_or_else(|| MockError::Downcast { name: F::NAME })?;

            if typed_impl.patterns.is_empty() {
                return Err(MockError::NoRegisteredCallPatterns {
                    name: F::NAME,
                    inputs_debug: F::debug_inputs(inputs),
                });
            }

            match match_pattern(pattern_match_mode, typed_impl, inputs, call_index)? {
                Some((pat_index, pattern)) => match select_responder_for_call(pattern) {
                    Some(responder) => Ok(Eval::Continue((pat_index, responder))),
                    None => Err(MockError::NoOutputAvailableForCallPattern {
                        name: F::NAME,
                        inputs_debug: F::debug_inputs(inputs),
                        pat_index,
                    }),
                },
                None => match fallback_mode {
                    FallbackMode::Error => Err(MockError::NoMatchingCallPatterns {
                        name: F::NAME,
                        inputs_debug: F::debug_inputs(inputs),
                    }),
                    FallbackMode::Unmock => Ok(Eval::Unmock),
                },
            }
        }
        Eval::Unmock => Ok(Eval::Unmock),
    }
}

#[inline(never)]
fn eval_type_erased_mock_impl<'s>(
    dyn_impl: Option<&'s DynMockImpl>,
    name: &'static str,
    fallback_mode: FallbackMode,
) -> Result<Eval<(&'s dyn Any, PatternMatchMode)>, MockError> {
    match dyn_impl {
        None => match fallback_mode {
            FallbackMode::Error => Err(MockError::NoMockImplementation { name }),
            FallbackMode::Unmock => Ok(Eval::Unmock),
        },
        Some(dyn_impl) => {
            dyn_impl
                .has_applications
                .store(true, std::sync::atomic::Ordering::SeqCst);

            Ok(Eval::Continue((
                dyn_impl.typed_impl.as_ref().as_any(),
                dyn_impl.pattern_match_mode,
            )))
        }
    }
}

fn match_pattern<'i, 's, F: MockFn>(
    pattern_match_mode: PatternMatchMode,
    mock_impl: &'s TypedMockImpl<F>,
    inputs: &<F as MockInputs<'i>>::Inputs,
    call_index: &AtomicUsize,
) -> Result<Option<(usize, &'s CallPattern<F>)>, MockError> {
    match pattern_match_mode {
        PatternMatchMode::InOrder => {
            // increase call index here, because stubs should not influence it:
            let current_call_index = call_index.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

            let (pat_index, pattern_by_call_index) = mock_impl
                .patterns
                .iter()
                .enumerate()
                .find(|(_, pattern)| {
                    pattern.call_index_range.start <= current_call_index
                        && pattern.call_index_range.end > current_call_index
                })
                .ok_or_else(|| MockError::CallOrderNotMatchedForMockFn {
                    name: F::NAME,
                    inputs_debug: F::debug_inputs(inputs),
                    actual_call_order: error::CallOrder(current_call_index),
                    expected_ranges: mock_impl
                        .patterns
                        .iter()
                        .map(|pattern| std::ops::Range {
                            start: pattern.call_index_range.start + 1,
                            end: pattern.call_index_range.end + 1,
                        })
                        .collect(),
                })?;

            if !(pattern_by_call_index.input_matcher)(inputs) {
                return Err(MockError::InputsNotMatchedInCallOrder {
                    name: F::NAME,
                    inputs_debug: F::debug_inputs(inputs),
                    actual_call_order: error::CallOrder(current_call_index),
                    pat_index,
                });
            }

            Ok(Some((pat_index, pattern_by_call_index)))
        }
        PatternMatchMode::InAnyOrder => Ok(mock_impl
            .patterns
            .iter()
            .enumerate()
            .find(|(_, pattern)| (*pattern.input_matcher)(inputs))),
    }
}

fn select_responder_for_call<F: MockFn>(pat: &CallPattern<F>) -> Option<&Responder<F>> {
    let call_index = pat.call_counter.fetch_add();

    let mut responder = None;

    for call_index_responder in pat.responders.iter() {
        if call_index_responder.response_index > call_index {
            break;
        }

        responder = Some(&call_index_responder.responder)
    }

    responder
}
