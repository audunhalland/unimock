use crate::call_pattern::{DynCallPattern, DynCallPatternBuilder};
use crate::mock_impl::{self, DynMockImpl, PatternMatchMode};

use std::any::TypeId;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub(crate) struct DynMockFn {
    type_id: TypeId,
    name: &'static str,
}

impl DynMockFn {
    pub fn new<F: crate::MockFn>() -> Self {
        Self {
            type_id: TypeId::of::<F>(),
            name: F::NAME,
        }
    }
}

pub(crate) struct AssembleInput {
    pub dyn_mock_fn: DynMockFn,
    pub kind: AssembleInputKind,
}

pub(crate) enum AssembleInputKind {
    Stub(Vec<DynCallPatternBuilder>),
    InAnyOrder(DynCallPatternBuilder),
    InOrder(DynCallPatternBuilder),
}

impl AssembleInputKind {
    fn pattern_match_mode(&self) -> mock_impl::PatternMatchMode {
        match self {
            Self::Stub(_) | Self::InAnyOrder(_) => mock_impl::PatternMatchMode::InAnyOrder,
            Self::InOrder(_) => mock_impl::PatternMatchMode::InOrder,
        }
    }

    fn into_pattern_builders(self) -> Vec<DynCallPatternBuilder> {
        match self {
            Self::Stub(builders) => builders,
            Self::InAnyOrder(builder) => vec![builder],
            Self::InOrder(builder) => vec![builder],
        }
    }
}

pub(crate) struct MockAssembler {
    pub impls: HashMap<TypeId, DynMockImpl>,
    pub current_call_index: usize,
}

impl MockAssembler {
    pub fn add(&mut self, input: AssembleInput) -> Result<(), AssembleError> {
        let pattern_match_mode = input.kind.pattern_match_mode();
        let mut call_patterns = input
            .kind
            .into_pattern_builders()
            .into_iter()
            .map(|builder| self.build_call_pattern(builder, &input.dyn_mock_fn, pattern_match_mode))
            .collect::<Result<Vec<_>, AssembleError>>()?;

        match self.impls.entry(input.dyn_mock_fn.type_id) {
            Entry::Occupied(mut entry) => {
                if entry.get().pattern_match_mode != pattern_match_mode {
                    return Err(AssembleError::IncompatiblePatternMatchMode {
                        name: entry.get().name,
                        old_mode: entry.get().pattern_match_mode,
                        new_mode: pattern_match_mode,
                    });
                }

                entry.get_mut().call_patterns.append(&mut call_patterns);
            }
            Entry::Vacant(entry) => {
                entry.insert(DynMockImpl {
                    call_patterns,
                    name: input.dyn_mock_fn.name,
                    pattern_match_mode,
                });
            }
        }

        Ok(())
    }

    fn build_call_pattern(
        &mut self,
        builder: DynCallPatternBuilder,
        dyn_mock_fn: &DynMockFn,
        pattern_match_mode: mock_impl::PatternMatchMode,
    ) -> Result<DynCallPattern, AssembleError> {
        let mut call_index_range: std::ops::Range<usize> = Default::default();

        match pattern_match_mode {
            PatternMatchMode::InOrder => {
                let exact_count = builder.call_counter.get_expected_exact_count().ok_or(
                    AssembleError::MockHasNoExactExpectation {
                        name: dyn_mock_fn.name,
                    },
                )?;

                call_index_range.start = self.current_call_index;
                call_index_range.end = self.current_call_index + exact_count;

                self.current_call_index = call_index_range.end;
            }
            _ => {}
        };

        Ok(builder.build(call_index_range))
    }
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
