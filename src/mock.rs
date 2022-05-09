use crate::error::MockError;
use crate::macro_api::Evaluation;
use crate::*;

use std::any::{Any, TypeId};
use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::panic::RefUnwindSafe;

pub(crate) struct MockAssembler {
    pub impls: HashMap<TypeId, DynMockImpl>,
    current_call_index: usize,
}

pub(crate) enum AssembleError {
    IncompatiblePatternMatchMode {
        name: &'static str,
        old_mode: PatternMatchMode,
        new_mode: PatternMatchMode,
    },
    MockHasNoExactExpectation {
        name: &'static str,
    },
}

impl AssembleError {
    pub fn to_string(&self) -> String {
        match self {
            AssembleError::IncompatiblePatternMatchMode {
                name,
                old_mode,
                new_mode,
            } => {
                format!("A clause {name} has already been registered as a {old_mode:?}, but got re-registered as a {new_mode:?}. They cannot be mixed.")
            }
            AssembleError::MockHasNoExactExpectation { name } => {
                format!("{name} mock has no exact count expectation, which is needed for a mock.")
            }
        }
    }
}

impl MockAssembler {
    pub fn new() -> Self {
        Self {
            impls: HashMap::new(),
            current_call_index: 0,
        }
    }
}

pub(crate) struct DynMockImpl {
    typed_impl: Box<dyn TypeErasedMockImpl + Send + Sync + RefUnwindSafe + 'static>,
    pattern_match_mode: PatternMatchMode,
    has_applications: AtomicBool,
}

impl DynMockImpl {
    #[inline(never)]
    pub fn new_full_cascade(
        typed_impl: Box<dyn TypeErasedMockImpl + Send + Sync + RefUnwindSafe + 'static>,
    ) -> DynMockImpl {
        DynMockImpl {
            typed_impl,
            pattern_match_mode: PatternMatchMode::InAnyOrder,
            has_applications: AtomicBool::new(false),
        }
    }

    #[inline(never)]
    pub fn new_strict_order(
        typed_impl: Box<dyn TypeErasedMockImpl + Send + Sync + RefUnwindSafe + 'static>,
    ) -> DynMockImpl {
        DynMockImpl {
            typed_impl,
            pattern_match_mode: PatternMatchMode::InOrder,
            has_applications: AtomicBool::new(false),
        }
    }

    pub fn assemble_into(mut self, assembler: &mut MockAssembler) -> Result<(), AssembleError> {
        let description = self.typed_impl.describe();

        match assembler.impls.entry(description.type_id) {
            Entry::Occupied(mut entry) => {
                if entry.get().pattern_match_mode != self.pattern_match_mode {
                    return Err(AssembleError::IncompatiblePatternMatchMode {
                        name: description.name,
                        old_mode: entry.get().pattern_match_mode,
                        new_mode: self.pattern_match_mode,
                    });
                }

                self.typed_impl.assemble(
                    Some(entry.get_mut()),
                    self.pattern_match_mode,
                    &mut assembler.current_call_index,
                )?;
            }
            Entry::Vacant(entry) => {
                self.typed_impl.assemble(
                    None,
                    self.pattern_match_mode,
                    &mut assembler.current_call_index,
                )?;

                entry.insert(self);
            }
        }

        Ok(())
    }

    pub fn verify(&self, errors: &mut Vec<MockError>) {
        if !self
            .has_applications
            .load(std::sync::atomic::Ordering::SeqCst)
        {
            errors.push(error::MockError::MockNeverCalled {
                name: self.typed_impl.describe().name,
            });
        }

        self.typed_impl.verify(errors);
    }
}

pub(crate) trait TypeErasedMockImpl: Any {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn describe(&self) -> Description;

    fn assemble(
        &mut self,
        target: Option<&mut DynMockImpl>,
        pattern_match_mode: PatternMatchMode,
        assembler_call_index: &mut usize,
    ) -> Result<(), AssembleError>;

    fn verify(&self, errors: &mut Vec<MockError>);
}

pub(crate) struct Description {
    type_id: TypeId,
    name: &'static str,
}

enum Eval<C> {
    Continue(C),
    Unmock,
}

pub(crate) fn eval_sized<'i, F: MockFn + 'static>(
    dyn_impl: Option<&'i DynMockImpl>,
    inputs: F::Inputs<'i>,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Evaluation<'i, F::Output, F>, MockError>
where
    F::Output: Sized,
{
    match eval_responder(dyn_impl, &inputs, call_index, fallback_mode)? {
        Eval::Continue((_, responder)) => match responder {
            Responder::Closure(closure) => Ok(Evaluation::Evaluated(closure(inputs))),
            Responder::StaticRefClosure(_) => panic!(),
            Responder::Borrowable(_) => panic!(),
            Responder::Panic(msg) => panic!("{}", msg),
            Responder::Unmock => Ok(Evaluation::Skipped(inputs)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

pub(crate) fn eval_unsized_self_borrowed<'i, 's: 'i, F: MockFn + 'static>(
    dyn_impl: Option<&'s DynMockImpl>,
    inputs: F::Inputs<'i>,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Evaluation<'i, &'s F::Output, F>, MockError> {
    match eval_responder::<F>(dyn_impl, &inputs, call_index, fallback_mode)? {
        Eval::Continue((_, responder)) => match responder {
            Responder::Closure(_) => panic!(), // FIXME
            Responder::StaticRefClosure(closure) => Ok(Evaluation::Evaluated(closure(inputs))),
            Responder::Borrowable(borrowable) => {
                let borrowable: &dyn Borrow<<F as MockFn>::Output> = borrowable.as_ref();
                let borrow = borrowable.borrow();
                Ok(Evaluation::Evaluated(borrow))
            }
            Responder::Panic(msg) => panic!("{}", msg),
            Responder::Unmock => Ok(Evaluation::Skipped(inputs)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

pub(crate) fn eval_unsized_static_ref<'i, 's: 'i, F: MockFn + 'static>(
    dyn_impl: Option<&'s DynMockImpl>,
    inputs: F::Inputs<'i>,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Evaluation<'i, &'static F::Output, F>, MockError> {
    match eval_responder::<F>(dyn_impl, &inputs, call_index, fallback_mode)? {
        Eval::Continue((pat_index, responder)) => match responder {
            Responder::Closure(_) => panic!(), // FIXME
            Responder::StaticRefClosure(closure) => Ok(Evaluation::Evaluated(closure(inputs))),
            Responder::Borrowable(_) => Err(MockError::CannotBorrowValueStatically {
                name: F::NAME,
                pat_index,
            }),
            Responder::Panic(msg) => panic!("{}", msg),
            Responder::Unmock => Ok(Evaluation::Skipped(inputs)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

fn eval_responder<'i, 's: 'i, F: MockFn + 'static>(
    dyn_impl: Option<&'s DynMockImpl>,
    inputs: &F::Inputs<'i>,
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
                    inputs_debug: typed_impl.debug_inputs(&inputs),
                });
            }

            match match_pattern(pattern_match_mode, typed_impl, inputs, call_index)? {
                Some((pat_index, pattern)) => match select_responder_for_call(pattern) {
                    Some(responder) => Ok(Eval::Continue((pat_index, responder))),
                    None => Err(MockError::NoOutputAvailableForCallPattern {
                        name: F::NAME,
                        inputs_debug: typed_impl.debug_inputs(&inputs),
                        pat_index,
                    }),
                },
                None => match fallback_mode {
                    FallbackMode::Error => Err(MockError::NoMatchingCallPatterns {
                        name: F::NAME,
                        inputs_debug: typed_impl.debug_inputs(&inputs),
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
    inputs: &F::Inputs<'i>,
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
                    inputs_debug: mock_impl.debug_inputs(inputs),
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
                    inputs_debug: mock_impl.debug_inputs(inputs),
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

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub(crate) enum PatternMatchMode {
    /// Each new call starts at the first call pattern, tries to
    /// match it and then goes on to the next one until success.
    InAnyOrder,
    /// Each new call starts off where the previous one ended.
    /// E.g. match pattern[0] 1 time, match pattern[1] 3 times, etc.
    InOrder,
}

pub(crate) struct TypedMockImpl<F: MockFn> {
    pub input_debugger: InputDebugger<F>,
    pub patterns: Vec<CallPattern<F>>,
}

impl<F: MockFn> TypedMockImpl<F> {
    /// A standalone mock, used in the building stage.
    pub fn new_standalone(
        input_debugger: InputDebugger<F>,
        input_matcher: Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync + RefUnwindSafe>,
    ) -> Self {
        let mut mock_impl = Self::with_input_debugger(input_debugger);
        mock_impl.patterns.push(mock::CallPattern {
            input_matcher,
            call_index_range: Default::default(),
            call_counter: counter::CallCounter::default(),
            responders: vec![],
        });
        mock_impl
    }

    pub fn with_input_debugger(input_debugger: InputDebugger<F>) -> Self {
        Self {
            input_debugger,
            patterns: vec![],
        }
    }

    fn debug_inputs<'i>(&self, inputs: &F::Inputs<'i>) -> String {
        self.input_debugger
            .debug_input_as_tuple(inputs, F::N_INPUTS)
    }
}

impl<F: MockFn + 'static> TypeErasedMockImpl for TypedMockImpl<F> {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn describe(&self) -> Description {
        Description {
            type_id: TypeId::of::<F>(),
            name: F::NAME,
        }
    }

    fn assemble(
        &mut self,
        merge_target: Option<&mut DynMockImpl>,
        pattern_match_mode: PatternMatchMode,
        assembler_call_index: &mut usize,
    ) -> Result<(), AssembleError> {
        match pattern_match_mode {
            PatternMatchMode::InOrder => {
                if self.patterns.len() != 1 {
                    panic!("Input mock should only have one pattern");
                }

                for pattern in self.patterns.iter_mut() {
                    let exact_count = pattern
                        .call_counter
                        .get_expected_exact_count()
                        .ok_or(AssembleError::MockHasNoExactExpectation { name: F::NAME })?;

                    pattern.call_index_range.start = *assembler_call_index;
                    pattern.call_index_range.end = *assembler_call_index + exact_count;

                    *assembler_call_index = pattern.call_index_range.end;
                }
            }
            _ => {}
        }

        if let Some(merge_target) = merge_target {
            let existing_impl = merge_target
                .typed_impl
                .as_any_mut()
                .downcast_mut::<Self>()
                .unwrap();

            existing_impl.patterns.append(&mut self.patterns);

            existing_impl
                .input_debugger
                .steal_debug_if_necessary(&mut self.input_debugger);
        }

        Ok(())
    }

    fn verify(&self, errors: &mut Vec<MockError>) {
        for (pat_index, pattern) in self.patterns.iter().enumerate() {
            pattern.call_counter.verify(F::NAME, pat_index, errors);
        }
    }
}

pub(crate) struct CallPattern<F: MockFn> {
    pub input_matcher: Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync + RefUnwindSafe>,
    pub call_index_range: std::ops::Range<usize>,
    pub call_counter: counter::CallCounter,
    pub responders: Vec<CallOrderResponder<F>>,
}

pub(crate) struct CallOrderResponder<F: MockFn> {
    pub response_index: usize,
    pub responder: Responder<F>,
}

pub(crate) enum Responder<F: MockFn> {
    Closure(Box<dyn (for<'i> Fn(F::Inputs<'i>) -> F::Output) + Send + Sync + RefUnwindSafe>),
    StaticRefClosure(
        Box<dyn (for<'i> Fn(F::Inputs<'i>) -> &'static F::Output) + Send + Sync + RefUnwindSafe>,
    ),
    Borrowable(Box<dyn Borrow<F::Output> + Send + Sync + RefUnwindSafe>),
    Panic(String),
    Unmock,
}

pub(crate) struct InputDebugger<F: MockFn> {
    pub debug_func:
        Option<Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> String) + Send + Sync + RefUnwindSafe>>,
}

impl<F: MockFn> InputDebugger<F> {
    pub fn new_nodebug() -> Self {
        Self { debug_func: None }
    }

    pub fn new_debug() -> Self
    where
        for<'i> F::Inputs<'i>: std::fmt::Debug,
    {
        Self {
            debug_func: Some(Box::new(|args| format!("{:?}", args))),
        }
    }

    pub fn steal_debug_if_necessary(&mut self, other: &mut InputDebugger<F>) {
        if self.debug_func.is_none() {
            self.debug_func = other.debug_func.take()
        }
    }

    pub fn debug_input_as_tuple<'i>(&self, inputs: &F::Inputs<'i>, n_args: u8) -> String {
        if let Some(func) = self.debug_func.as_ref() {
            let debug = func(inputs);
            match n_args {
                1 => format!("({})", debug),
                _ => debug,
            }
        } else {
            anonymous_inputs_debug(n_args)
        }
    }
}

fn anonymous_inputs_debug(n_args: u8) -> String {
    let inner = (0..n_args)
        .into_iter()
        .map(|_| "_")
        .collect::<Vec<_>>()
        .join(", ");

    format!("({})", inner)
}
