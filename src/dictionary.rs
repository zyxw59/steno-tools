use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    rc::Rc,
};

use serde::{Deserialize, Serialize};

use crate::chord::Outline;

#[derive(Default, Debug, Serialize)]
pub struct Dictionary {
    #[serde(flatten)]
    outlines: BTreeMap<Outline, Word>,
    #[serde(skip)]
    words: BTreeMap<Word, BTreeSet<Outline>>,
}

impl Dictionary {
    pub fn words(&self) -> &BTreeMap<Word, BTreeSet<Outline>> {
        &self.words
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
