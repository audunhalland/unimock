use core::any::Any;
use once_cell::sync::OnceCell;

use crate::alloc::Box;

/// A collection that can store values, but never remove them until dropped.
///
/// This allows retaining shared references to inserted values.
#[derive(Default)]
pub struct ValueChain {
    root: OnceCell<Node>,
}

impl ValueChain {
    pub fn push<T: Any + Send + Sync>(&self, value: T) -> &T {
        self.push_value(Value::Send(Box::new(value)))
            .downcast_ref::<T>()
            .unwrap()
    }

    pub fn push_mut<T: Any + Send + Sync>(&mut self, value: T) -> &mut T {
        self.push_value_mut(Value::Send(Box::new(value)))
            .downcast_mut::<T>()
            .unwrap()
    }
}

#[cfg(feature = "fragile")]
impl ValueChain {
    pub fn push_fragile<T: Any>(&self, value: T) -> &T {
        self.push_value(Value::Fragile(fragile::Fragile::new(Box::new(value))))
            .downcast_ref::<T>()
            .unwrap()
    }

    pub fn push_fragile_mut<T: Any>(&mut self, value: T) -> &mut T {
        self.push_value_mut(Value::Fragile(fragile::Fragile::new(Box::new(value))))
            .downcast_mut::<T>()
            .unwrap()
    }
}

impl ValueChain {
    fn push_value(&self, value: Value) -> &Value {
        let node = self.push_node(Node::new(value));

        &node.value
    }

    fn push_value_mut(&mut self, value: Value) -> &mut Value {
        // note: There is no need for keeping the old chain.
        // All those references are out of scope when add_mut is called.
        self.root = Node::new(value).into();

        &mut self.root.get_mut().unwrap().value
    }

    fn push_node(&self, mut new_node: Node) -> &Node {
        let mut cell = &self.root;
        loop {
            match cell.try_insert(new_node) {
                Ok(new_node) => {
                    return new_node;
                }
                Err((parent_node, node)) => {
                    new_node = node;
                    cell = &parent_node.next;
                }
            }
        }
    }
}

impl Drop for ValueChain {
    fn drop(&mut self) {
        if let Some(node) = self.root.take() {
            drop(node.value);
            let mut cell = node.next;

            while let Some(node) = cell.take() {
                drop(node.value);
                cell = node.next;
            }
        }
    }
}

struct Node {
    value: Value,
    next: Box<OnceCell<Node>>,
}

impl Node {
    fn new(value: Value) -> Self {
        Self {
            value,
            next: Default::default(),
        }
    }
}

enum Value {
    Send(Box<dyn Any + Send + Sync>),
    #[cfg(feature = "fragile")]
    Fragile(fragile::Fragile<Box<dyn Any>>),
}

impl Value {
    fn downcast_ref<T: 'static>(&self) -> Option<&T> {
        match self {
            Self::Send(any) => any.downcast_ref::<T>(),
            #[cfg(feature = "fragile")]
            Self::Fragile(fragile) => fragile.get().downcast_ref::<T>(),
        }
    }

    fn downcast_mut<T: 'static>(&mut self) -> Option<&mut T> {
        match self {
            Self::Send(any) => any.downcast_mut::<T>(),
            #[cfg(feature = "fragile")]
            Self::Fragile(fragile) => fragile.get_mut().downcast_mut::<T>(),
        }
    }
}

#[test]
fn it_works() {
    let value_chain = ValueChain::default();
    let first = value_chain.push(1);
    let second = value_chain.push("");
    let third = value_chain.push(42.0);

    assert_eq!(&1, first);
    assert_eq!(&"", second);
    assert_eq!(&42.0, third);
}

#[test]
fn it_works_mut() {
    let mut value_chain = ValueChain::default();
    let first = value_chain.push_mut(1);
    *first += 1;
}
