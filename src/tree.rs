use std::fmt;

use slab::Slab;

pub struct Tree<T> {
    slab: Slab<Node<T>>,
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

impl<T> Tree<T> {
    pub fn build_with_leaf_validation<I, J>(
        initials: I,
        mut generator: impl FnMut(&T) -> J,
        mut validator: impl FnMut(&T) -> bool,
    ) -> Self
    where
        I: IntoIterator<Item = T>,
        J: IntoIterator<Item = T>,
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

    pub fn contract<U>(self, mut contraction: impl FnMut(&T, &T) -> U) -> Tree<U> {
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

        Tree {
            slab,
            roots: first_root.and_then(|first| Some((first, last_root?))),
        }
    }

    pub fn map<U>(self, mut func: impl FnMut(T) -> U) -> Tree<U> {
        Tree {
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
        let key = TreeIdx(self.slab.insert(Node {
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

    fn remove_node(&mut self, key: TreeIdx) -> Option<Node<T>> {
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

impl<T: fmt::Debug> fmt::Debug for Tree<T> {
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
    slab: &'a Slab<Node<T>>,
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

    pub fn map<U>(self, mut func: impl FnMut(&'a T) -> U) -> Tree<U> {
        Tree {
            slab: self
                .slab
                .iter()
                .map(|(idx, node)| (idx, node.as_ref().map_value(&mut func)))
                .collect(),
            roots: self.roots,
        }
    }

    pub fn multi_map<I, U>(self, generator: impl for<'b> FnMut(&'b T, Option<&U>) -> I) -> Tree<U>
    where
        I: IntoIterator<Item = U>,
    {
        let empty = Tree::new();
        self.multi_map_with_graft(empty.as_ref(), generator)
    }

    pub fn multi_map_with_graft<I, U>(
        self,
        graft: TreeRef<'a, T>,
        mut generator: impl FnMut(&'a T, Option<&U>) -> I,
    ) -> Tree<U>
    where
        I: IntoIterator<Item = U>,
    {
        use itertools::Either;

        let mut intermediate = Tree::new();
        for (idx, node) in self.node_iter() {
            for value in generator(&node.value, None) {
                intermediate.insert((Either::Left(idx), value), None);
            }
        }

        let mut next_idx = intermediate.roots.map(|(first, _)| first);
        while let Some(new_parent_idx) = next_idx {
            let &(parent_idx, _) = intermediate.get(new_parent_idx).unwrap();
            let mut child_nodes = match parent_idx {
                Either::Left(parent_idx) => self.child_nodes(parent_idx),
                Either::Right(parent_idx) => graft.child_nodes(parent_idx),
            };
            let mut is_graft = parent_idx.is_right();
            // leaf in the original tree: add a graft
            if child_nodes.is_empty() && !is_graft {
                child_nodes = graft.node_iter();
                is_graft = true;
            }
            let is_leaf = child_nodes.is_empty();
            for (idx, node) in child_nodes {
                let idx = if is_graft {
                    Either::Right(idx)
                } else {
                    Either::Left(idx)
                };
                let (_, parent_value) = intermediate.get(new_parent_idx).unwrap();
                for value in generator(&node.value, Some(parent_value)) {
                    intermediate.insert((idx, value), Some(new_parent_idx));
                }
            }
            next_idx = intermediate.as_ref().next_dfs(new_parent_idx);
            // only allow leaf nodes where the original tree had leaves
            if intermediate.slab[new_parent_idx.0].children.is_none() && !is_leaf {
                intermediate.remove_branch(new_parent_idx);
            }
        }

        intermediate.map(|(_, value)| value)
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

    #[cfg(test)]
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

    #[cfg(test)]
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

struct DebugTreeSlab<'a, T>(&'a Slab<Node<T>>);

impl<T: fmt::Debug> fmt::Debug for DebugTreeSlab<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.0).finish()
    }
}

#[derive(Debug)]
struct Node<T> {
    value: T,
    children: Option<(TreeIdx, TreeIdx)>,
    prev_sibling: Option<TreeIdx>,
    next_sibling: Option<TreeIdx>,
    parent: Option<TreeIdx>,
}

impl<T> Node<T> {
    fn as_ref(&self) -> Node<&T> {
        Node {
            value: &self.value,
            children: self.children,
            prev_sibling: self.prev_sibling,
            next_sibling: self.next_sibling,
            parent: self.parent,
        }
    }

    fn map_value<U>(self, func: impl FnOnce(T) -> U) -> Node<U> {
        Node {
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
    tree: &'a Tree<T>,
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
    slab: &'a Slab<Node<T>>,
    roots: Option<(TreeIdx, TreeIdx)>,
}

impl<T> SiblingNodeIter<'_, T> {
    pub fn is_empty(&self) -> bool {
        self.roots.is_none()
    }
}

impl<'a, T> Iterator for SiblingNodeIter<'a, T> {
    type Item = (TreeIdx, &'a Node<T>);

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
    iter: Tree<&'tree T>,
}

impl<'tree, T> TreePaths<'tree, T> {
    pub fn next_path<'iter>(&'iter mut self) -> Option<PathIter<'iter, 'tree, T>> {
        let current_idx = self.iter.as_ref().first_child()?;
        Some(PathIter {
            iter: &mut self.iter,
            current_idx: Some(current_idx),
        })
    }

    pub fn with_collect_copied<C>(
        self,
    ) -> MappedTreePathsIter<'tree, T, impl for<'iter> FnMut(PathIter<'iter, 'tree, T>) -> C>
    where
        T: Copy,
        C: FromIterator<T>,
    {
        self.with(|iter| iter.copied().collect())
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
    iter: &'iter mut Tree<&'tree T>,
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

#[cfg(test)]
mod tests {
    use super::Tree;

    #[test]
    fn contraction() {
        let tree_1 = Tree::build_with_leaf_validation(0..4, |&x| 0..x, |_| true);
        let tree_2 = tree_1.contract(|x, y| format!("{x}-{y}"));
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
