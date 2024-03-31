use std::{fmt, ops::Range};

#[derive(Clone, Eq, PartialEq)]
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

    pub fn build_two_stage<I, U, F, J>(initials: I, mut generator: F) -> Self
    where
        I: IntoIterator<Item = U>,
        F: FnMut(U) -> (T, J),
        J: IntoIterator<Item = U>,
    {
        let mut staging = initials.into_iter().map(Node::new_root).collect::<Vec<_>>();
        let mut staging_2 = Vec::new();
        let range = 0..staging.len();
        let mut nodes = Vec::new();
        while !staging.is_empty() {
            let children_start = nodes.len() + staging.len();
            for node in staging.drain(..) {
                let parent_idx = nodes.len();
                let (mut parent, children_iter) = node.map_tuple_first(&mut generator);
                let start = children_start + staging_2.len();
                staging_2.extend(
                    children_iter.into_iter()
                    .map(|value| Node::new(value, parent_idx))
                );
                let end = children_start + staging_2.len();
                parent.children = start..end;
                nodes.push(parent);
            }
            std::mem::swap(&mut staging, &mut staging_2);
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

#[derive(Debug, Clone, Eq, PartialEq)]
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

    fn map_tuple_first<F, U, V>(self, func: F) -> (Node<U>, V) where F: FnOnce(T) -> (U, V) {
        let (value, other) = func(self.value);
        (Node { value, parent: self.parent, children: self.children }, other)
    }
}

#[cfg(test)]
mod tests {
    use super::Tree;

    #[test]
    fn build_tree() {
        let tree_1 = Tree::build([4], |&x| 0..x);
        let tree_2 = Tree::build_two_stage([4], |x| (x, 0..x));

        assert_eq!(tree_1, tree_2);
    }
}
