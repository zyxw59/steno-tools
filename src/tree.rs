use std::{fmt, ops::Range};

use slab::Slab;

pub struct Tree2<T> {
    slab: Slab<Node2<T>>,
    roots: Option<(TreeIdx, TreeIdx)>,
}

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct TreeIdx(usize);

impl fmt::Debug for TreeIdx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl<T> Tree2<T> {
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
        let mut this = Self::new();
        for value in initials {
            this.insert(value, None);
        }

        let mut next_idx = this.roots.map(|(first, _)| first);
        while let Some(idx) = next_idx {
            let value = this.get(idx).unwrap();
            for child in generator(value) {
                this.insert(child, Some(idx));
            }
            next_idx = this.as_ref().next_dfs(idx);
            let value = this.get(idx).unwrap();
            if this.slab[idx.0].children.is_none() && !validator(value) {
                this.remove_branch(idx);
            }
        }

        this
    }

    pub fn contract<F, U>(self, mut contraction: F) -> Tree2<U>
    where
        F: FnMut(&T, &T) -> U,
    {
        let mut first_root = None;
        let mut last_root = None;
        let slab = self
            .slab
            .iter()
            .filter_map(|(i, node)| {
                let parent_idx = node.parent?;
                let parent = &self.slab[parent_idx.0];
                let mut edge = node
                    .as_ref()
                    .map_value(|child| contraction(&parent.value, child));
                if parent.parent.is_none() {
                    edge.parent = None;
                    // stitch this with neigboring cousins
                    if edge.prev_sibling.is_none() {
                        edge.prev_sibling = self.prev_cousin(TreeIdx(i));
                    }
                    if edge.next_sibling.is_none() {
                        edge.next_sibling = self.next_cousin(TreeIdx(i));
                    }
                    // if it's still a first or last sibling, set the appropriate `root`
                    if edge.prev_sibling.is_none() {
                        first_root = Some(TreeIdx(i));
                    }
                    if edge.next_sibling.is_none() {
                        last_root = Some(TreeIdx(i));
                    }
                }
                Some((i, edge))
            })
            .collect();

        Tree2 {
            slab,
            roots: first_root.and_then(|first| Some((first, last_root?))),
        }
    }

    pub fn map<F, U>(self, mut func: F) -> Tree2<U>
    where
        F: FnMut(T) -> U,
    {
        Tree2 {
            slab: self
                .slab
                .into_iter()
                .map(|(idx, node)| (idx, node.map_value(&mut func)))
                .collect(),
            roots: self.roots,
        }
    }

    pub fn get(&self, idx: TreeIdx) -> Option<&T> {
        self.slab.get(idx.0).map(|node| &node.value)
    }

    pub fn insert(&mut self, value: T, parent: Option<TreeIdx>) -> TreeIdx {
        let key = TreeIdx(self.slab.insert(Node2 {
            value,
            parent,
            children: None,
            prev_sibling: None,
            next_sibling: None,
        }));
        let prev_sibling = if let Some(parent) = parent {
            let parent = self.slab.get_mut(parent.0).expect("invalid parent");
            append_child(&mut parent.children, key)
        } else {
            append_child(&mut self.roots, key)
        };
        if let Some(prev_sibling) = prev_sibling {
            self.slab[prev_sibling.0].next_sibling = Some(key);
            self.slab[key.0].prev_sibling = Some(prev_sibling);
        }
        key
    }

    pub fn remove(&mut self, key: TreeIdx) -> Option<T> {
        self.remove_node(key).map(|node| node.value)
    }

    pub fn children(&self, idx: TreeIdx) -> ChildrenIter<'_, T> {
        let current = self.slab[idx.0].first_child();
        ChildrenIter {
            tree: self,
            current,
        }
    }

    fn remove_node(&mut self, key: TreeIdx) -> Option<Node2<T>> {
        let node = self.slab.try_remove(key.0)?;
        if let Some(next_sibling) = node.next_sibling {
            self.slab[next_sibling.0].prev_sibling = node.prev_sibling;
        }
        if let Some(prev_sibling) = node.prev_sibling {
            self.slab[prev_sibling.0].next_sibling = node.next_sibling;
        }
        let siblings = if let Some(parent) = node.parent {
            &mut self.slab[parent.0].children
        } else {
            &mut self.roots
        };
        *siblings = match *siblings {
            Some((first, last)) if first == key && last == key => None,
            Some((first, last)) if first == key => Some((node.next_sibling.unwrap(), last)),
            Some((first, last)) if last == key => Some((first, node.prev_sibling.unwrap())),
            children => children,
        };
        Some(node)
    }

    fn remove_branch(&mut self, leaf: TreeIdx) {
        let mut idx = leaf;
        while let Some(parent) = self.remove_node(idx).unwrap().parent {
            if self.slab[parent.0].children.is_some() {
                return;
            }
            idx = parent;
        }
    }

    fn next_cousin(&self, key: TreeIdx) -> Option<TreeIdx> {
        let node = self.slab.get(key.0)?;
        node.next_sibling.or_else(|| {
            self.as_ref()
                .sibling_nodes(node.parent?, self.roots?.1)
                .skip(1)
                .find_map(|(_, node)| node.first_child())
        })
    }

    fn prev_cousin(&self, key: TreeIdx) -> Option<TreeIdx> {
        let node = self.slab.get(key.0)?;
        node.prev_sibling.or_else(|| {
            self.as_ref()
                .sibling_nodes(self.roots?.0, node.parent?)
                .rev()
                .skip(1)
                .find_map(|(_, node)| node.last_child())
        })
    }

    fn new() -> Self {
        Self {
            slab: Slab::new(),
            roots: None,
        }
    }

    pub fn as_ref(&self) -> TreeRef<T> {
        TreeRef {
            slab: &self.slab,
            roots: self.roots,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Tree2<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.as_ref(), f)
    }
}

fn append_child(children: &mut Option<(TreeIdx, TreeIdx)>, new_child: TreeIdx) -> Option<TreeIdx> {
    match children {
        Some((_, last)) => Some(std::mem::replace(last, new_child)),
        None => {
            *children = Some((new_child, new_child));
            None
        }
    }
}

pub struct TreeRef<'a, T> {
    slab: &'a Slab<Node2<T>>,
    roots: Option<(TreeIdx, TreeIdx)>,
}

impl<T> Copy for TreeRef<'_, T> {}
impl<T> Clone for TreeRef<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> TreeRef<'a, T> {
    pub fn subtrees(self) -> SubTreeIter<'a, T> {
        SubTreeIter {
            iter: self.node_iter(),
        }
    }

    pub fn map<F, U>(self, mut func: F) -> Tree2<U>
    where
        F: FnMut(&'a T) -> U,
    {
        Tree2 {
            slab: self
                .slab
                .iter()
                .map(|(idx, node)| (idx, node.as_ref().map_value(&mut func)))
                .collect(),
            roots: self.roots,
        }
    }

    pub fn paths(self) -> TreePaths<'a, T> {
        TreePaths {
            iter: self.map(|value| value),
        }
    }

    fn child_nodes(self, parent: TreeIdx) -> SiblingNodeIter<'a, T> {
        SiblingNodeIter {
            slab: self.slab,
            roots: self.slab.get(parent.0).and_then(|node| node.children),
        }
    }

    fn sibling_nodes(self, first: TreeIdx, last: TreeIdx) -> SiblingNodeIter<'a, T> {
        SiblingNodeIter {
            slab: self.slab,
            roots: Some((first, last)),
        }
    }

    fn node_iter(self) -> SiblingNodeIter<'a, T> {
        SiblingNodeIter {
            slab: self.slab,
            roots: self.roots,
        }
    }

    fn dfs_index_iter(self) -> DfsIndexIter<'a, T> {
        DfsIndexIter {
            tree: self,
            current: self.first_child(),
        }
    }

    fn next_dfs(self, key: TreeIdx) -> Option<TreeIdx> {
        let node = self.slab.get(key.0)?;
        node.children
            .map(|(first, _)| first)
            .or(node.next_sibling)
            .or_else(|| self.next_dfs_2(node.parent?))
    }

    fn next_dfs_2(self, key: TreeIdx) -> Option<TreeIdx> {
        let node = self.slab.get(key.0)?;
        node.next_sibling.or_else(|| self.next_dfs_2(node.parent?))
    }

    pub fn first_child(self) -> Option<TreeIdx> {
        self.roots.map(|(first, _)| first)
    }

    fn debug(self) -> DebugTreeRef<'a, T> {
        DebugTreeRef(self)
    }
}

impl<T: fmt::Debug> fmt::Debug for TreeRef<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.subtrees()).finish()
    }
}

struct DebugTreeRef<'a, T>(TreeRef<'a, T>);

impl<T: fmt::Debug> fmt::Debug for DebugTreeRef<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Tree")
            .field("roots", &self.0.roots)
            .field("slab", &DebugTreeSlab(self.0.slab))
            .finish()
    }
}

struct DebugTreeSlab<'a, T>(&'a Slab<Node2<T>>);

impl<T: fmt::Debug> fmt::Debug for DebugTreeSlab<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.0).finish()
    }
}

#[derive(Debug)]
struct Node2<T> {
    value: T,
    children: Option<(TreeIdx, TreeIdx)>,
    prev_sibling: Option<TreeIdx>,
    next_sibling: Option<TreeIdx>,
    parent: Option<TreeIdx>,
}

impl<T> Node2<T> {
    fn as_ref(&self) -> Node2<&T> {
        Node2 {
            value: &self.value,
            children: self.children,
            prev_sibling: self.prev_sibling,
            next_sibling: self.next_sibling,
            parent: self.parent,
        }
    }

    fn map_value<U>(self, func: impl FnOnce(T) -> U) -> Node2<U> {
        Node2 {
            value: func(self.value),
            children: self.children,
            prev_sibling: self.prev_sibling,
            next_sibling: self.next_sibling,
            parent: self.parent,
        }
    }

    fn first_child(&self) -> Option<TreeIdx> {
        self.children.map(|(first, _)| first)
    }

    fn last_child(&self) -> Option<TreeIdx> {
        self.children.map(|(first, _)| first)
    }
}

pub struct ChildrenIter<'a, T> {
    tree: &'a Tree2<T>,
    current: Option<TreeIdx>,
}

impl<'a, T> Iterator for ChildrenIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.current?;
        let node = &self.tree.slab[idx.0];
        self.current = node.next_sibling;
        Some(&node.value)
    }
}

pub struct SubTreeIter<'a, T> {
    iter: SiblingNodeIter<'a, T>,
}

impl<'a, T> Iterator for SubTreeIter<'a, T> {
    type Item = (&'a T, TreeRef<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        let (_, node) = self.iter.next()?;
        Some((
            &node.value,
            TreeRef {
                slab: self.iter.slab,
                roots: node.children,
            },
        ))
    }
}

struct SiblingNodeIter<'a, T> {
    slab: &'a Slab<Node2<T>>,
    roots: Option<(TreeIdx, TreeIdx)>,
}

impl<'a, T> Iterator for SiblingNodeIter<'a, T> {
    type Item = (TreeIdx, &'a Node2<T>);

    fn next(&mut self) -> Option<Self::Item> {
        let (first, last) = self.roots?;
        let node = &self.slab[first.0];
        self.roots = node.next_sibling.map(|first| (first, last));
        Some((first, node))
    }
}

impl<'a, T> DoubleEndedIterator for SiblingNodeIter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let (first, last) = self.roots?;
        let node = &self.slab[last.0];
        self.roots = node.prev_sibling.map(|last| (first, last));
        Some((last, node))
    }
}

struct DfsIndexIter<'a, T> {
    tree: TreeRef<'a, T>,
    current: Option<TreeIdx>,
}

impl<'a, T> Iterator for DfsIndexIter<'a, T> {
    type Item = TreeIdx;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.current?;
        self.current = self.tree.next_dfs(idx);
        Some(idx)
    }
}

pub struct TreePaths<'tree, T> {
    iter: Tree2<&'tree T>,
}

impl<'tree, T> TreePaths<'tree, T> {
    pub fn next_path<'iter>(&'iter mut self) -> Option<PathIter<'iter, 'tree, T>> {
        let current_idx = self.iter.as_ref().first_child()?;
        Some(PathIter {
            iter: &mut self.iter,
            current_idx: Some(current_idx),
        })
    }

    pub fn with_collect<C>(self) -> impl Iterator<Item = C> + 'tree
    where
        C: FromIterator<&'tree T>,
    {
        self.with(|iter| iter.collect())
    }

    pub fn with<F, U>(self, func: F) -> MappedTreePathsIter<'tree, T, F>
    where
        F: for<'iter> FnMut(PathIter<'iter, 'tree, T>) -> U,
    {
        MappedTreePathsIter { iter: self, func }
    }
}

pub struct MappedTreePathsIter<'tree, T, F> {
    iter: TreePaths<'tree, T>,
    func: F,
}

impl<'tree, T, F, U> Iterator for MappedTreePathsIter<'tree, T, F>
where
    F: for<'iter> FnMut(PathIter<'iter, 'tree, T>) -> U,
{
    type Item = U;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next_path().map(&mut self.func)
    }
}

pub struct PathIter<'iter, 'tree, T> {
    iter: &'iter mut Tree2<&'tree T>,
    current_idx: Option<TreeIdx>,
}

impl<'iter, 'tree, T> Iterator for PathIter<'iter, 'tree, T> {
    type Item = &'tree T;

    fn next(&mut self) -> Option<Self::Item> {
        let current_idx = self.current_idx?;
        let node = &self.iter.slab[current_idx.0];
        let value = node.value;
        self.current_idx = node.first_child();
        if self.current_idx.is_none() {
            // we've reached a leaf, update state for the next iterator and mark this one complete
            self.iter.remove_branch(current_idx);
        }

        Some(value)
    }
}

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
                break;
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
            (self.children.start - offset)..(self.children.end - offset)
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
    use super::Tree2;

    #[test]
    fn contraction() {
        let tree_1 = Tree2::build_with_leaf_validation(0..4, |&x| 0..x, |_| true);
        let tree_1_old = super::Tree::build(0..4, |&x| 0..x);
        assert_eq!(format!("{tree_1:?}"), format!("{tree_1_old:?}"));
        let tree_2 = tree_1.contract(|x, y| format!("{x}-{y}"));
        let tree_2_old = tree_1_old.contract(|x, y| format!("{x}-{y}"));
        assert_eq!(format!("{tree_2:?}"), format!("{tree_2_old:?}"));
        eprintln!("{:#?}", tree_2.as_ref().debug());
        for idx in tree_2.as_ref().dfs_index_iter() {
            let node = &tree_2.slab[idx.0];
            if let Some(parent) = node.parent {
                let parent_matches = tree_2
                    .as_ref()
                    .child_nodes(parent)
                    .any(|(child_idx, _)| child_idx == idx);
                if !parent_matches {
                    panic!("{idx:?} claims {parent:?} as a parent");
                }
            }
        }
    }
}
