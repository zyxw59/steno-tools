use std::{fmt, ops::Range};

#[derive(Clone, Eq, PartialEq)]
pub struct Tree<T> {
    nodes: Vec<Node>,
    values: Vec<T>,
    range: Range<usize>,
}

impl<T> Tree<T> {
    pub fn build<I, F, J>(initials: I, generator: F) -> Self
    where
        I: IntoIterator<Item = T>,
        F: FnMut(&T) -> J,
        J: IntoIterator<Item = T>,
    {
        Self::build_with_leaf_validation(initials, generator, |_| true)
    }

    pub fn build_with_leaf_validation<I, G, J, L>(
        initials: I,
        mut generator: G,
        mut validator: L,
    ) -> Self
    where
        I: IntoIterator<Item = T>,
        G: FnMut(&T) -> J,
        J: IntoIterator<Item = T>,
        L: FnMut(&T) -> bool,
    {
        let mut values = initials.into_iter().collect::<Vec<_>>();
        let mut nodes = vec![Node::EMPTY_ROOT; values.len()];
        let range = 0..nodes.len();
        let mut idx = 0;
        while idx < nodes.len() {
            let start = values.len();
            values.extend(generator(&values[idx]));
            let end = values.len();
            // run the validator on leaf nodes
            if start == end && !validator(&values[idx]) {
                invalidate_node(&mut nodes, idx);
            }
            nodes.resize(end, Node::new(idx));
            nodes[idx].set_children(start..end);
            idx += 1;
        }
        Self {
            nodes,
            values,
            range,
        }
    }

    /// Generates a new tree by replacing each edge with a node.
    pub fn contract<F, U>(self, mut contraction: F) -> Tree<U>
    where
        F: FnMut(&T, &T) -> U,
    {
        let root_len = self.range.len();
        let mut nodes = Vec::with_capacity(self.nodes.len() - root_len);
        let mut values = Vec::with_capacity(self.nodes.len() - root_len);
        for (parent, parent_value) in self.nodes.iter().zip(&self.values) {
            for (child, child_value) in self.nodes[parent.children.clone()]
                .iter()
                .zip(&self.values[parent.children.clone()])
            {
                let new_child = child.subtract(root_len);
                nodes.push(new_child);
                values.push(contraction(parent_value, child_value));
            }
        }
        let range = 0..(self.range.map(|idx| self.nodes[idx].children.len()).sum());
        Tree {
            nodes,
            values,
            range,
        }
    }

    pub fn roots_with_indices(&self) -> impl Iterator<Item = (usize, &T)> {
        self.range_with_indices(self.range.clone())
    }

    pub fn children_with_indices(&self, parent: usize) -> impl Iterator<Item = (usize, &T)> {
        self.range_with_indices(self.nodes[parent].children.clone())
    }

    fn range_with_indices(&self, range: Range<usize>) -> impl Iterator<Item = (usize, &T)> {
        range.map(|i| (i, &self.values[i]))
    }

    pub fn as_subtree(&self) -> SubTree<T> {
        SubTree {
            nodes: &self.nodes,
            values: &self.values,
            range: self.range.clone(),
        }
    }

    pub fn all_paths<'tree, C>(&'tree mut self) -> impl Iterator<Item = C> + 'tree
    where
        C: FromIterator<&'tree T> + 'tree,
    {
        MappedTreePathsIter2 {
            nodes: &mut self.nodes,
            values: &self.values,
            range: self.range.clone(),
            func: collect_tree_path,
        }
    }

    pub fn all_paths_with<'tree, F, U>(&'tree mut self, func: F) -> impl Iterator<Item = U> + 'tree
    where
        F: for<'iter> FnMut(TreePathIter2<'iter, 'tree, T>) -> U + 'tree,
    {
        MappedTreePathsIter2 {
            nodes: &mut self.nodes,
            values: &self.values,
            range: self.range.clone(),
            func,
        }
    }
}

fn invalidate_node(nodes: &mut [Node], idx: usize) {
    nodes[idx].is_valid = false;
    if let Some(parent) = nodes[idx].parent {
        let parent_node = &mut nodes[parent];
        parent_node.invalid_children += 1;
        if parent_node.invalid_children >= parent_node.children.len() {
            invalidate_node(nodes, parent)
        }
    }
}

fn collect_tree_path<'iter, 'tree, T, C>(tree_paths: TreePathIter2<'iter, 'tree, T>) -> C
where
    C: FromIterator<&'tree T>,
{
    tree_paths.collect()
}

impl<T: fmt::Debug> fmt::Debug for Tree<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.as_subtree(), f)
    }
}

pub struct SubTree<'a, T> {
    nodes: &'a [Node],
    values: &'a [T],
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
            let value = &self.values[root];
            let children = &node.children;
            if children.is_empty() {
                if pred(value) {
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
            values: self.values,
            range: self.range.clone(),
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for SubTree<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map()
            .entries(
                self.nodes[self.range.clone()]
                    .iter()
                    .zip(&self.values[self.range.clone()])
                    .filter(|(node, _value)| node.is_valid)
                    .map(|(node, value)| {
                        (
                            value,
                            SubTree {
                                nodes: self.nodes,
                                values: self.values,
                                range: node.children.clone(),
                            },
                        )
                    }),
            )
            .finish()
    }
}

pub struct TreeReversePathIter<'a, T> {
    nodes: &'a [Node],
    values: &'a [T],
    idx: Option<usize>,
}

impl<'a, T> Iterator for TreeReversePathIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let node = &self.nodes[self.idx?];
        let value = &self.values[self.idx?];
        self.idx = node.parent;
        Some(value)
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
            values: self.tree.values,
            idx: Some(idx),
        };
        self.idx = self.tree.next_matching_leaf(idx, &mut self.pred);
        Some(it)
    }
}

pub struct MappedTreePathsIter2<'tree, T, F> {
    nodes: &'tree mut [Node],
    values: &'tree [T],
    range: Range<usize>,
    func: F,
}

impl<'tree, T, F, C> Iterator for MappedTreePathsIter2<'tree, T, F>
where
    F: for<'iter> FnMut(TreePathIter2<'iter, 'tree, T>) -> C,
{
    type Item = C;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.range.is_empty() {
                return None;
            }
            if !self.nodes[self.range.start].is_valid {
                self.range.start += 1;
            } else {
                break
            }
        }
        Some((self.func)(TreePathIter2 {
            nodes: self.nodes,
            values: self.values,
            current_idx: Some(self.range.start),
            root_idx: &mut self.range.start,
        }))
    }
}

pub struct TreePathIter2<'iter, 'tree, T> {
    nodes: &'iter mut [Node],
    values: &'tree [T],
    root_idx: &'iter mut usize,
    current_idx: Option<usize>,
}

impl<T> TreePathIter2<'_, '_, T> {
    fn increment_first_ancestor(&mut self, mut idx: usize) {
        while let Some(parent) = self.nodes[idx].parent {
            loop {
                let node = &mut self.nodes[parent];
                node.iter_first_child += 1;
                if node.iter_first_child >= node.children.end {
                    break;
                }
                // this node still has valid children left, so we can return early
                if self.nodes[node.iter_first_child].is_valid {
                    return;
                }
            }
            idx = parent;
        }
        *self.root_idx += 1;
    }
}

impl<'iter, 'tree, T> Iterator for TreePathIter2<'iter, 'tree, T> {
    type Item = &'tree T;

    fn next(&mut self) -> Option<Self::Item> {
        let current_idx = self.current_idx?;
        let node = &self.nodes[current_idx];
        eprintln!("{current_idx}: {node:?}");
        let value = &self.values[current_idx];
        if node.children.is_empty() {
            // we've reached a leaf, update state for the next iterator and mark this one complete
            self.increment_first_ancestor(current_idx);
            self.current_idx = None;
        } else {
            self.current_idx = Some(node.iter_first_child);
        }

        Some(value)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Node {
    parent: Option<usize>,
    iter_first_child: usize,
    children: Range<usize>,
    is_valid: bool,
    invalid_children: usize,
}

impl Node {
    const EMPTY_ROOT: Self = Self {
        parent: None,
        is_valid: true,
        iter_first_child: 0,
        children: 0..0,
        invalid_children: 0,
    };

    fn new(parent: usize) -> Self {
        Self {
            parent: Some(parent),
            ..Self::EMPTY_ROOT
        }
    }

    fn subtract(&self, offset: usize) -> Self {
        let children = if self.children.is_empty() {
            0..0
        } else {
            (self.children.start - offset) .. (self.children.end - offset)
        };
        Self {
            parent: self.parent.and_then(|idx| idx.checked_sub(offset)),
            iter_first_child: children.start,
            children,
            ..*self
        }
    }

    fn new_maybe_root(parent: Option<usize>) -> Self {
        Self {
            parent,
            ..Self::EMPTY_ROOT
        }
    }

    fn set_children(&mut self, children: Range<usize>) {
        self.iter_first_child = children.start;
        self.children = children;
    }
}

#[cfg(test)]
mod tests {
    use super::Tree;

    #[test]
    fn contraction() {
        let tree_1 = Tree::build(0..4, |&x| 0..x);
        let tree_2 = tree_1.contract(|x, y| format!("{x}-{y}"));
        println!("{tree_2:#?}");
    }
}
