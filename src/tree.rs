use std::{fmt, ops::Range};

#[derive(Clone)]
pub struct Tree<T> {
    nodes: Vec<Node<T>>,
    range: Range<usize>,
}

impl<T> Tree<T> {
    pub fn build<I, F, J>(initials: I, mut generator: F) -> Self
    where
        I: IntoIterator<Item = T>,
        F: FnMut(&T) -> J,
        J: IntoIterator<Item = T>,
    {
        let mut nodes = initials.into_iter().map(Node::new_root).collect::<Vec<_>>();
        let range = 0..nodes.len();
        let mut idx = 0;
        while idx < nodes.len() {
            let start = nodes.len();
            nodes.extend(
                generator(&nodes[idx].value)
                    .into_iter()
                    .map(|value| Node::new(value, idx)),
            );
            let end = nodes.len();
            nodes[idx].children = start..end;
            idx += 1;
        }
        Self { nodes, range }
    }

    fn as_subtree(&self) -> SubTree<T> {
        SubTree {
            nodes: &self.nodes,
            range: self.range.clone(),
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Tree<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.as_subtree(), f)
    }
}

#[derive(Clone)]
pub struct SubTree<'a, T> {
    nodes: &'a [Node<T>],
    range: Range<usize>,
}

impl<T: fmt::Debug> fmt::Debug for SubTree<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map()
            .entries(self.nodes[self.range.clone()].iter().map(|node| {
                (
                    &node.value,
                    SubTree {
                        nodes: self.nodes,
                        range: node.children.clone(),
                    },
                )
            }))
            .finish()
    }
}

#[derive(Debug, Clone)]
struct Node<T> {
    parent: Option<usize>,
    children: Range<usize>,
    value: T,
}

impl<T> Node<T> {
    fn new(value: T, parent: usize) -> Self {
        Self {
            parent: Some(parent),
            children: 0..0,
            value,
        }
    }
    fn new_root(value: T) -> Self {
        Self {
            parent: None,
            children: 0..0,
            value,
        }
    }
}
