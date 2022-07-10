use crate::assemble::{AssembleError, MockAssembler};
use crate::call_pattern::CallPattern;
use crate::error::MockError;
use crate::*;

use std::any::{Any, TypeId};
use std::collections::hash_map::Entry;

pub(crate) struct DynMockImpl {
    pub typed_impl: Box<dyn TypeErasedMockImpl + Send + Sync + 'static>,
    pub pattern_match_mode: PatternMatchMode,
}

impl DynMockImpl {
    #[inline(never)]
    pub fn new(
        typed_impl: Box<dyn TypeErasedMockImpl + Send + Sync + 'static>,
        mode: PatternMatchMode,
    ) -> DynMockImpl {
        DynMockImpl {
            typed_impl,
            pattern_match_mode: mode,
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
                    Some(entry.get_mut().typed_impl.as_any_mut()),
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
        let n_calls = self.typed_impl.verify(errors);

        if n_calls.0 == 0 {
            errors.push(error::MockError::MockNeverCalled {
                name: self.typed_impl.describe().name,
            });
        }
    }
}

pub(crate) trait TypeErasedMockImpl: Any {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn describe(&self) -> Description;

    fn assemble(
        &mut self,
        target: Option<&mut dyn Any>,
        pattern_match_mode: PatternMatchMode,
        assembler_call_index: &mut usize,
    ) -> Result<(), AssembleError>;

    fn verify(&self, errors: &mut Vec<MockError>) -> counter::NCalls;
}

pub(crate) trait TypeErasedCallPattern: Any {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
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
    // Invariant: Must be non-empty:
    patterns: Vec<CallPattern<F>>,
}

impl<F: MockFn> TypedMockImpl<F> {
    pub(crate) fn from_stub_patterns(patterns: Vec<CallPattern<F>>) -> Self {
        if patterns.is_empty() {
            panic!("Stub contained no call patterns");
        }

        Self { patterns }
    }

    pub(crate) fn from_pattern(pattern: CallPattern<F>) -> Self {
        Self {
            patterns: vec![pattern],
        }
    }

    pub(crate) fn patterns(&self) -> &[CallPattern<F>] {
        self.patterns.as_ref()
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
        merge_target: Option<&mut dyn Any>,
        pattern_match_mode: PatternMatchMode,
        assembler_call_index: &mut usize,
    ) -> Result<(), AssembleError> {
        match pattern_match_mode {
            PatternMatchMode::InOrder => {
                if self.patterns.len() != 1 {
                    panic!("Input mock should only have one pattern");
                }

                for pattern in self.patterns.iter_mut() {
                    pattern
                        .non_generic
                        .assemble_setup_call_range(assembler_call_index, F::NAME)?;
                }
            }
            _ => {}
        }

        if let Some(merge_target) = merge_target {
            let existing_impl = merge_target.downcast_mut::<Self>().unwrap();

            existing_impl.patterns.append(&mut self.patterns);
        }

        Ok(())
    }

    fn verify(&self, errors: &mut Vec<MockError>) -> counter::NCalls {
        let mut total_calls = 0;

        for (pat_index, pattern) in self.patterns.iter().enumerate() {
            total_calls += pattern
                .non_generic
                .call_counter
                .verify(F::NAME, pat_index, errors)
                .0;
        }

        counter::NCalls(total_calls)
    }
}
