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
                    children_iter
                        .into_iter()
                        .map(|value| Node::new(value, parent_idx)),
                );
                let end = children_start + staging_2.len();
                parent.children = start..end;
                nodes.push(parent);
            }
            std::mem::swap(&mut staging, &mut staging_2);
        }
        Self { nodes, range }
    }

    /// Generates a new tree by replacing each edge with a node.
    pub fn contract<F, U>(self, mut contraction: F) -> Tree<U>
    where
        F: FnMut(&T, &T) -> U,
    {
        let root_len = self.range.len();
        let mut nodes = Vec::with_capacity(self.nodes.len() - root_len);
        for parent in &self.nodes {
            for child in &self.nodes[parent.children.clone()] {
                let grandchildren = if child.children.is_empty() {
                    0..0
                } else {
                    (child.children.start - root_len)..(child.children.end - root_len)
                };
                let new_child = Node {
                    parent: child.parent.and_then(|idx| idx.checked_sub(root_len)),
                    children: grandchildren,
                    value: contraction(&parent.value, &child.value),
                };
                nodes.push(new_child);
            }
        }
        let range = 0..(self.range.map(|idx| self.nodes[idx].children.len()).sum());
        Tree { nodes, range }
    }

    pub fn as_subtree(&self) -> SubTree<T> {
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

pub struct SubTree<'a, T> {
    nodes: &'a [Node<T>],
    range: Range<usize>,
}

impl<'a, T> SubTree<'a, T> {
    pub fn all_paths(&self) -> TreePathsIter<'a, T, impl FnMut(&'a T) -> bool> {
        let idx = Some(self.first_leaf(self.range.start));
        TreePathsIter {
            tree: self.clone(),
            idx,
            pred: |_| true,
        }
    }

    pub fn all_matching_paths<F>(&self, pred: F) -> TreePathsIter<'a, T, F>
    where
        F: FnMut(&'a T) -> bool,
    {
        let idx = Some(self.first_leaf(self.range.start));
        TreePathsIter {
            tree: self.clone(),
            idx,
            pred,
        }
    }

    fn first_leaf(&self, root: usize) -> usize {
        self.first_matching_leaf(root, |_| true).unwrap()
    }

    fn first_matching_leaf<F>(&self, mut root: usize, mut pred: F) -> Option<usize>
    where
        F: FnMut(&'a T) -> bool,
    {
        loop {
            let node = &self.nodes[root];
            let children = &node.children;
            if children.is_empty() {
                if pred(&node.value) {
                    return Some(root);
                }
                return self.next_matching_leaf(root, pred);
            }
            root = children.start
        }
    }

    fn next_leaf(&self, current: usize) -> Option<usize> {
        self.next_matching_leaf(current, |_| true)
    }

    fn next_matching_leaf<F>(&self, mut current: usize, pred: F) -> Option<usize>
    where
        F: FnMut(&'a T) -> bool,
    {
        loop {
            let parent = self.nodes[current].parent;
            let siblings = if let Some(parent) = parent {
                &self.nodes[parent].children
            } else {
                &self.range
            };
            if current < siblings.end - 1 {
                return self.first_matching_leaf(current + 1, pred);
            }
            // else: current is the last sibling of parent; get the next leaf after parent, or
            // `None` if current is a root
            current = parent?;
        }
    }
}

impl<T> Clone for SubTree<'_, T> {
    fn clone(&self) -> Self {
        Self {
            nodes: self.nodes,
            range: self.range.clone(),
        }
    }
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

pub struct TreeReversePathIter<'a, T> {
    nodes: &'a [Node<T>],
    idx: Option<usize>,
}

impl<'a, T> Iterator for TreeReversePathIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let node = &self.nodes[self.idx?];
        self.idx = node.parent;
        Some(&node.value)
    }
}

pub struct TreePathsIter<'a, T, F> {
    tree: SubTree<'a, T>,
    idx: Option<usize>,
    pred: F,
}

impl<'a, T, F: FnMut(&'a T) -> bool> Iterator for TreePathsIter<'a, T, F> {
    type Item = TreeReversePathIter<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.idx?;
        let it = TreeReversePathIter {
            nodes: self.tree.nodes,
            idx: Some(idx),
        };
        self.idx = self.tree.next_matching_leaf(idx, &mut self.pred);
        Some(it)
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

    fn map_tuple_first<F, U, V>(self, func: F) -> (Node<U>, V)
    where
        F: FnOnce(T) -> (U, V),
    {
        let (value, other) = func(self.value);
        (
            Node {
                value,
                parent: self.parent,
                children: self.children,
            },
            other,
        )
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

    #[test]
    fn contraction() {
        let tree_1 = Tree::build(0..4, |&x| 0..x);
        let tree_2 = tree_1.contract(|x, y| format!("{x}-{y}"));
        println!("{tree_2:#?}");
    }
}
