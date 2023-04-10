use core::any::Any;
use once_cell::sync::OnceCell;

use crate::private::lib::Box;

/// A collection that can store values, but never remove them until dropped.
///
/// This allows retaining shared references to inserted values.
#[derive(Default)]
pub struct ValueChain {
    root_node: OnceCell<Node>,
}

impl ValueChain {
    pub fn add<T: Any + Send + Sync>(&self, value: T) -> &T {
        let node = self.push_node(Node::new(value));

        node.value.as_ref().downcast_ref::<T>().unwrap()
    }

    fn push_node(&self, mut new_node: Node) -> &Node {
        let mut cell = &self.root_node;
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
        if let Some(node) = self.root_node.take() {
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
    value: Box<dyn Any + Send + Sync>,
    next: Box<OnceCell<Node>>,
}

impl Node {
    pub fn new<T: Any + Send + Sync>(value: T) -> Self {
        Self {
            value: Box::new(value),
            next: Default::default(),
        }
    }
}

#[test]
fn it_works() {
    let value_chain = ValueChain::default();
    let first = value_chain.add(1);
    let second = value_chain.add("");
    let third = value_chain.add(42.0);

    assert_eq!(&1, first);
    assert_eq!(&"", second);
    assert_eq!(&42.0, third);
}
