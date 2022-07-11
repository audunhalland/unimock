use crate::*;

impl<I: IntoIterator<Item = Clause>> From<I> for Clause {
    fn from(clauses: I) -> Self {
        Clause(ClausePrivate::Tree(clauses.into_iter().collect()))
    }
}

impl From<ClauseLeaf> for Clause {
    fn from(leaf: ClauseLeaf) -> Self {
        Self(ClausePrivate::Leaf(leaf))
    }
}

pub(crate) enum ClausePrivate {
    Leaf(ClauseLeaf),
    Tree(Vec<Clause>),
}

pub(crate) struct ClauseLeaf {
    pub dyn_mock_fn: DynMockFn,
    pub kind: ClauseLeafKind,
}

pub(crate) enum ClauseLeafKind {
    Stub(Vec<call_pattern::DynCallPatternBuilder>),
    InAnyOrder(call_pattern::DynCallPatternBuilder),
    InOrder(call_pattern::DynCallPatternBuilder),
}

impl ClauseLeafKind {
    pub fn pattern_match_mode(&self) -> mock_impl::PatternMatchMode {
        match self {
            Self::Stub(_) | Self::InAnyOrder(_) => mock_impl::PatternMatchMode::InAnyOrder,
            Self::InOrder(_) => mock_impl::PatternMatchMode::InOrder,
        }
    }

    pub fn into_pattern_builders(self) -> Vec<call_pattern::DynCallPatternBuilder> {
        match self {
            Self::Stub(builders) => builders,
            Self::InAnyOrder(builder) => vec![builder],
            Self::InOrder(builder) => vec![builder],
        }
    }
}
