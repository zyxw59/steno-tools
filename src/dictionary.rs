use std::{
    collections::{btree_map::Entry, BTreeMap},
    fmt,
    rc::Rc,
};

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub struct Dictionary {
    outlines: IndexMap<Outline, Word>,
    words: BTreeMap<Word, Vec<Outline>>,
}

impl Dictionary {
    pub fn outlines(&self) -> &IndexMap<Outline, Word> {
        &self.outlines
    }

    pub fn words(&self) -> &BTreeMap<Word, Vec<Outline>> {
        &self.words
    }
}

impl<'de> Deserialize<'de> for Dictionary {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let mut outlines = IndexMap::<Outline, Word>::deserialize(de)?;
        let mut words = BTreeMap::<Word, Vec<Outline>>::new();
        for (outline, word) in &mut outlines {
            let entry = words.entry(word.clone());
            if let Entry::Occupied(entry) = &entry {
                // dedup word Rc
                *word = entry.key().clone();
            }
            entry.or_default().push(outline.clone());
        }
        Ok(Self { outlines, words })
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Outline(Rc<str>);

impl fmt::Display for Outline {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Word(Rc<str>);

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

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
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
