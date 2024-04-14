use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::{
    chord::Outline,
    dictionary::Word,
    pronounce::{DictionaryEntry, Pronunciation},
};

#[derive(Default, Debug, Serialize)]
pub struct GeneratedDictionary {
    pub valid_outlines: Dictionary,
    pub conflicts: BTreeMap<Outline, BTreeSet<DictionaryEntry>>,
    pub no_pronunciation: Vec<Word>,
    pub no_outlines: Vec<NoOutline>,
}

impl GeneratedDictionary {
    /// Returns whether the outline was successfully inserted without any conflicts.
    pub fn insert(&mut self, outline: Outline, word: Word, pronunciation: Pronunciation) -> bool {
        let new_entry = DictionaryEntry {
            word,
            pronunciation,
        };
        match self.conflicts.entry(outline.clone()) {
            Entry::Occupied(mut conflicts_entry) => {
                conflicts_entry.get_mut().insert(new_entry);
                false
            }
            Entry::Vacant(conflicts_entry) => {
                if let Some(old_entry) = self.valid_outlines.insert(outline, new_entry.clone()) {
                    conflicts_entry.insert([old_entry, new_entry].into_iter().collect());
                    false
                } else {
                    true
                }
            }
        }
    }

    pub fn remove_conflicts_with_valid_alternatives(&mut self) {
        let mut removals = BTreeSet::new();
        for (outline, conflicts) in &mut self.conflicts {
            conflicts.retain(|entry| !self.valid_outlines.words.contains_key(&entry.word));
            if conflicts.len() <= 1 {
                if let Some(entry) = conflicts.pop_first() {
                    self.valid_outlines.insert(outline.clone(), entry);
                }
                removals.insert(outline.clone());
            }
        }
        self.conflicts
            .retain(|outline, _| !removals.contains(outline));
    }

    pub fn resolve_identical_conflicts(&mut self) {
        let mut removals = BTreeSet::new();
        for (outline, conflicts) in &mut self.conflicts {
            if conflicts.iter().map(|entry| &entry.word).all_equal() {
                self.valid_outlines
                    .insert(outline.clone(), conflicts.pop_first().unwrap());
                removals.insert(outline.clone());
            }
        }
        self.conflicts
            .retain(|outline, _| !removals.contains(outline));
    }

    pub fn remove_errors_with_valid_alternatives(&mut self) {
        self.no_outlines
            .retain(|entry| !self.valid_outlines.words.contains_key(&entry.word))
    }

    pub fn resolve_pairs(
        &mut self,
        mut f: impl FnMut(&Outline, &DictionaryEntry, &DictionaryEntry) -> Option<(Outline, Outline)>,
    ) {
        let mut insertions = BTreeMap::new();
        let mut removals = BTreeSet::new();
        for (outline, conflicts) in &self.conflicts {
            if conflicts.len() != 2 {
                continue;
            }
            // since there's exactly two we can just take first and last
            let entry_1 = conflicts.first().unwrap();
            let entry_2 = conflicts.last().unwrap();
            let Some((outline_1, outline_2)) = f(outline, entry_1, entry_2) else {
                continue;
            };
            // don't add this disambiguation if it would conflict with a different entry
            if !self.valid_outlines.outlines.contains_key(&outline_1)
                && !self.valid_outlines.outlines.contains_key(&outline_2)
            {
                removals.insert(outline.clone());
                insertions.insert(outline_1, entry_1.clone());
                insertions.insert(outline_2, entry_2.clone());
            }
        }
        self.conflicts
            .retain(|outline, _| !removals.contains(outline));
        for (outline, entry) in insertions {
            self.insert(outline, entry.word, entry.pronunciation);
        }
    }
}

#[derive(Default, Debug)]
pub struct Dictionary {
    outlines: BTreeMap<Outline, DictionaryEntry>,
    words: BTreeMap<Word, BTreeSet<Outline>>,
}

impl Dictionary {
    pub fn insert(
        &mut self,
        outline: Outline,
        new_entry: DictionaryEntry,
    ) -> Option<DictionaryEntry> {
        match self.outlines.entry(outline.clone()) {
            Entry::Occupied(old_entry) if old_entry.get().word != new_entry.word => {
                let old_word = &old_entry.get().word;
                if let Some(outlines) = self.words.get_mut(old_word) {
                    outlines.remove(&outline);
                    if outlines.is_empty() {
                        self.words.remove(old_word);
                    }
                }
                return Some(old_entry.remove());
            }
            Entry::Occupied(_) => {}
            Entry::Vacant(vacant) => {
                self.words
                    .entry(new_entry.word.clone())
                    .or_default()
                    .insert(outline);
                vacant.insert(new_entry);
            }
        }
        None
    }
}

impl Serialize for Dictionary {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        let mut map = ser.serialize_map(Some(self.outlines.len()))?;
        for (outline, entry) in &self.outlines {
            map.serialize_entry(outline, &entry.word)?;
        }
        map.end()
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct NoOutline {
    pub word: Word,
    pub pronunciation: Pronunciation,
    #[serde(
        serialize_with = "serialize_anyhow",
        deserialize_with = "deserialize_anyhow"
    )]
    pub error: anyhow::Error,
}

fn serialize_anyhow<S>(error: &anyhow::Error, ser: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    error.to_string().serialize(ser)
}

fn deserialize_anyhow<'de, D>(de: D) -> Result<anyhow::Error, D::Error>
where
    D: serde::Deserializer<'de>,
{
    String::deserialize(de).map(anyhow::Error::msg)
}

#[derive(Debug, Deserialize)]
pub struct Override {
    pub word: Word,
    pub pronunciation: Pronunciation,
    pub outline: Outline,
}
