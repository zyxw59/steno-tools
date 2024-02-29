use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    rc::Rc,
};

use serde::{Deserialize, Serialize};

#[derive(Default, Debug)]
pub struct Dictionary {
    outlines: BTreeMap<Outline, Word>,
    words: BTreeMap<Word, BTreeSet<Outline>>,
}

impl Dictionary {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn outlines(&self) -> &BTreeMap<Outline, Word> {
        &self.outlines
    }

    pub fn words(&self) -> &BTreeMap<Word, BTreeSet<Outline>> {
        &self.words
    }

    pub fn insert(&mut self, word: Word, outline: Outline) -> Result<(), Word> {
        match self.outlines.entry(outline.clone()) {
            Entry::Occupied(entry) => {
                if entry.get() != &word {
                    return Err(entry.get().clone());
                } else {
                    return Ok(());
                }
            }
            Entry::Vacant(entry) => entry.insert(word.clone()),
        };
        self.words.entry(word.clone()).or_default().insert(outline);
        Ok(())
    }

    pub fn remove_outline(&mut self, outline: &Outline) -> Option<Word> {
        let word = self.outlines.remove(outline)?;
        if let Entry::Occupied(mut entry) = self.words.entry(word.clone()) {
            entry.get_mut().remove(outline);
            if entry.get().is_empty() {
                entry.remove();
            }
        }
        Some(word)
    }

    pub fn num_outlines(&self) -> usize {
        self.outlines.len()
    }

    pub fn num_words(&self) -> usize {
        self.words.len()
    }
}

impl<'de> Deserialize<'de> for Dictionary {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let mut outlines = BTreeMap::<Outline, Word>::deserialize(de)?;
        let mut words = BTreeMap::<Word, BTreeSet<Outline>>::new();
        for (outline, word) in &mut outlines {
            let entry = words.entry(word.clone());
            if let Entry::Occupied(entry) = &entry {
                // dedup word Rc
                *word = entry.key().clone();
            }
            entry.or_default().insert(outline.clone());
        }
        Ok(Self { outlines, words })
    }
}

#[derive(Clone, Hash, Eq, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Outline(Rc<str>);
crate::fmt_impls!(Outline);
crate::deref_impls!(Outline as str);

impl<T: Into<Rc<str>>> From<T> for Outline {
    fn from(s: T) -> Self {
        Self(s.into())
    }
}

#[derive(Clone, Hash, Eq, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Word(Rc<str>);
crate::fmt_impls!(Word);
crate::deref_impls!(Word as str);

impl Word {
    pub fn categorize(&self) -> WordCategory {
        if self.0.starts_with("{^") {
            WordCategory::Suffix
        } else if self.0.ends_with("^}") {
            WordCategory::Prefix
        } else if self.0.contains('{') {
            WordCategory::Special
        } else if self.0.chars().all(char::is_lowercase) {
            WordCategory::Common
        } else {
            WordCategory::Name
        }
    }
}

impl<T: Into<Rc<str>>> From<T> for Word {
    fn from(s: T) -> Self {
        Self(s.into())
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, Ord, PartialEq, PartialOrd, clap::ValueEnum)]
pub enum WordCategory {
    Common,
    Name,
    Prefix,
    Suffix,
    Special,
}
