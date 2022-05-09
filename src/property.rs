/// Trait for describing expected call ordering.
pub trait Ordering {
    /// The kind of ordering.
    type Kind;
}

/// Marker type for calls that can be matched in any order.
pub struct InAnyOrder;

/// Marker type for calls that must be matched in a specific order.
pub struct InOrder;

impl Ordering for InAnyOrder {
    type Kind = Self;
}

impl Ordering for InOrder {
    type Kind = Self;
}

/// Trait for describing the repetition class of output sequences
pub trait Repetition {
    /// The kind of repetition.
    type Kind;
}

/// Marker type for output sequences of finite, exact size.
pub struct Exact;

/// Marker type for output sequences of potentially infinite size.
pub struct AtLeast;

impl Repetition for Exact {
    type Kind = Self;
}

impl Repetition for AtLeast {
    type Kind = Self;
}
