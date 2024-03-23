use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::{chord::Outline, dictionary::Word, pronounce::Pronunciation, theory::PhoneticTheory};

#[derive(Default, Debug, Serialize)]
pub struct GeneratedDictionary {
    pub valid_outlines: Dictionary,
    pub conflicts: BTreeMap<Outline, BTreeSet<DictionaryEntry>>,
    pub no_pronunciation: Vec<Word>,
    pub no_outlines: Vec<NoOutline>,
}

impl GeneratedDictionary {
    pub fn insert(&mut self, outline: Outline, word: Word, pronunciation: Pronunciation) {
        let new_entry = DictionaryEntry {
            word,
            pronunciation,
        };
        match self.conflicts.entry(outline.clone()) {
            Entry::Occupied(mut conflicts_entry) => {
                conflicts_entry.get_mut().insert(new_entry);
            }
            Entry::Vacant(conflicts_entry) => {
                if let Some(old_entry) = self.valid_outlines.insert(outline, new_entry.clone()) {
                    conflicts_entry.insert([old_entry, new_entry].into_iter().collect());
                }
            }
        }
    }

    pub fn resolve_phonetic_conflicts(&mut self, theory: &PhoneticTheory) {
        let mut insertions = BTreeMap::new();
        let mut removals = BTreeSet::new();
        for (outline, conflicts) in &self.conflicts {
            if conflicts.len() != 2 {
                continue;
            }
            // since there's exactly two we can just take first and last
            let entry_1 = conflicts.first().unwrap();
            let entry_2 = conflicts.last().unwrap();
            if entry_1.pronunciation == entry_2.pronunciation {
                continue;
            }
            let Some((outline_1, outline_2)) = theory.disambiguate_phonetic(
                outline.clone(),
                &entry_1.pronunciation,
                &entry_2.pronunciation,
            ) else {
                continue;
            };
            removals.insert(outline.clone());
            insertions.insert(outline_1, entry_1.clone());
            insertions.insert(outline_2, entry_1.clone());
        }
        for outline in removals {
            self.conflicts.remove(&outline);
        }
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
                if let Some(outlines) = self.words.get_mut(&new_entry.word) {
                    outlines.remove(&outline);
                }
                return Some(old_entry.remove());
            }
            Entry::Occupied(_) => {}
            Entry::Vacant(vacant) => {
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

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize)]
pub struct DictionaryEntry {
    word: Word,
    pronunciation: Pronunciation,
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
