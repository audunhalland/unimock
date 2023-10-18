use core::pin::Pin;

use crate::private::lib::{Arc, Rc};
use crate::{private, Unimock};

/// The DefaultImplDelegator is a struct
/// used for implementing only non-default methods of traits.
///
/// It is used as part of the infrastructure for supporting default implementation fallbacks in Unimock.
#[derive(Clone)]
pub struct DefaultImplDelegator {
    pub(crate) unimock: Unimock,
}

impl DefaultImplDelegator {
    /// Construct a default impl delegator from a unimock instance.
    ///
    /// This method exists as a helper in case of type inference problems with `From<T>`.
    pub fn __from_unimock(unimock: Unimock) -> Self {
        Self { unimock }
    }
}

impl AsRef<Unimock> for DefaultImplDelegator {
    fn as_ref(&self) -> &Unimock {
        &self.unimock
    }
}

impl AsMut<Unimock> for DefaultImplDelegator {
    fn as_mut(&mut self) -> &mut Unimock {
        &mut self.unimock
    }
}

#[cfg(feature = "mock-core")]
impl core::fmt::Display for DefaultImplDelegator {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        <Unimock as core::fmt::Display>::fmt(&self.unimock, f)
    }
}

#[cfg(feature = "mock-core")]
impl core::fmt::Debug for DefaultImplDelegator {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        <Unimock as core::fmt::Debug>::fmt(&self.unimock, f)
    }
}

pub trait DelegateToDefaultImpl {
    type Delegator;

    fn to_delegator(self) -> Self::Delegator;

    fn from_delegator(delegator: Self::Delegator) -> Self;
}

impl DelegateToDefaultImpl for Unimock {
    type Delegator = DefaultImplDelegator;

    fn to_delegator(self) -> Self::Delegator {
        DefaultImplDelegator::__from_unimock(self)
    }

    fn from_delegator(delegator: Self::Delegator) -> Self {
        delegator.unimock
    }
}

impl DelegateToDefaultImpl for Rc<Unimock> {
    type Delegator = Rc<DefaultImplDelegator>;

    fn to_delegator(self) -> Self::Delegator {
        Rc::new(DefaultImplDelegator::__from_unimock((*self).clone()))
    }

    fn from_delegator(delegator: Self::Delegator) -> Self {
        Rc::new(delegator.unimock.clone())
    }
}

impl DelegateToDefaultImpl for Arc<Unimock> {
    type Delegator = Arc<DefaultImplDelegator>;

    fn to_delegator(self) -> Self::Delegator {
        Arc::new(DefaultImplDelegator::__from_unimock((*self).clone()))
    }

    fn from_delegator(delegator: Self::Delegator) -> Self {
        Arc::new(delegator.unimock.clone())
    }
}

impl<'u> DelegateToDefaultImpl for Pin<&'u mut Unimock> {
    type Delegator = Pin<&'u mut DefaultImplDelegator>;

    fn to_delegator(self) -> Self::Delegator {
        let unimock = self.get_mut();
        let unimock_clone = unimock.clone();

        unimock.default_impl_delegator_cell.get_or_init(|| {
            private::lib::Box::new(DefaultImplDelegator::__from_unimock(unimock_clone))
        });

        let delegator = unimock.default_impl_delegator_cell.get_mut().unwrap();

        Pin::new(delegator)
    }

    fn from_delegator(delegator: Self::Delegator) -> Self {
        let delegator = delegator.get_mut();
        Pin::new(&mut delegator.unimock)
    }
}
