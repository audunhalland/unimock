use crate::error::MockError;
use crate::*;

use std::any::{Any, TypeId};
use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

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
    pub typed_impl: Box<dyn TypeErasedMockImpl + Send + Sync + 'static>,
    pub pattern_match_mode: PatternMatchMode,
    pub has_applications: AtomicBool,
}

impl DynMockImpl {
    #[inline(never)]
    pub fn new_full_cascade(
        typed_impl: Box<dyn TypeErasedMockImpl + Send + Sync + 'static>,
    ) -> DynMockImpl {
        DynMockImpl {
            typed_impl,
            pattern_match_mode: PatternMatchMode::InAnyOrder,
            has_applications: AtomicBool::new(false),
        }
    }

    #[inline(never)]
    pub fn new_strict_order(
        typed_impl: Box<dyn TypeErasedMockImpl + Send + Sync + 'static>,
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
    pub patterns: Vec<CallPattern<F>>,
}

impl<F: MockFn> TypedMockImpl<F> {
    /// A standalone mock, used in the building stage.
    pub fn new_standalone(
        input_matcher: Box<dyn (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync>,
    ) -> Self {
        let mut mock_impl = Self::new();
        mock_impl.patterns.push(mock_impl::CallPattern {
            input_matcher,
            call_index_range: Default::default(),
            call_counter: counter::CallCounter::default(),
            responders: vec![],
        });
        mock_impl
    }

    pub fn new() -> Self {
        Self { patterns: vec![] }
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
    pub input_matcher: Box<dyn (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync>,
    pub call_index_range: std::ops::Range<usize>,
    pub call_counter: counter::CallCounter,
    pub responders: Vec<CallOrderResponder<F>>,
}

pub(crate) struct CallOrderResponder<F: MockFn> {
    pub response_index: usize,
    pub responder: Responder<F>,
}

pub(crate) enum Responder<F: MockFn> {
    Value(Box<dyn StoredValue<F::Output>>),
    Borrowable(Box<dyn Borrow<F::Output> + Send + Sync>),
    Closure(Box<dyn (for<'i> Fn(<F as MockInputs<'i>>::Inputs) -> F::Output) + Send + Sync>),
    StaticRefClosure(
        Box<dyn (for<'i> Fn(<F as MockInputs<'i>>::Inputs) -> &'static F::Output) + Send + Sync>,
    ),
    Panic(String),
    Unmock,
}

pub trait StoredValue<T: ?Sized>: Send + Sync {
    fn box_clone(&self) -> Box<T>;

    fn borrow_stored(&self) -> &T;
}

pub(crate) struct StoredValueSlot<T>(pub T);

impl<T: Clone + Send + Sync> StoredValue<T> for StoredValueSlot<T> {
    fn box_clone(&self) -> Box<T> {
        Box::new(self.0.clone())
    }

    fn borrow_stored(&self) -> &T {
        &self.0
    }
}
