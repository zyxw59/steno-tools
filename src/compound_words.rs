use std::collections::{BTreeMap, BTreeSet};

use crate::pronounce::{self, DictionaryEntry};

#[derive(Debug, serde::Serialize)]
pub struct CompoundWords {
    entries: BTreeMap<DictionaryEntry, [DictionaryEntry; 2]>,
    ambiguous: BTreeMap<DictionaryEntry, BTreeSet<[DictionaryEntry; 2]>>,
}

impl From<&pronounce::Dictionary> for CompoundWords {
    fn from(dictionary: &pronounce::Dictionary) -> Self {
        let mut entries = BTreeMap::new();
        let mut ambiguous = BTreeMap::new();
        for entry in dictionary.entries() {
            let mut splits = get_splits(&entry, dictionary);
            if splits.len() > 1 {
                ambiguous.insert(entry, splits);
            } else if let Some(split) = splits.pop_first() {
                entries.insert(entry, split);
            }
        }
        Self { entries, ambiguous }
    }
}

fn get_splits(
    entry: &DictionaryEntry,
    dictionary: &pronounce::Dictionary,
) -> BTreeSet<[DictionaryEntry; 2]> {
    if entry.word.is_empty() {
        return BTreeSet::new();
    }
    (1..(entry.word.len() - 1))
        .filter_map(|i| get_one_split(entry, dictionary, i))
        .collect()
}

fn get_one_split(
    entry: &DictionaryEntry,
    dictionary: &pronounce::Dictionary,
    split_idx: usize,
) -> Option<[DictionaryEntry; 2]> {
    let (first_word, second_word) = entry.word.split_at(split_idx);
    let first_prons = dictionary
        .get(first_word)
        .iter()
        .filter(|pron| entry.pronunciation.starts_with(pron))
        .collect::<Vec<_>>();
    let second_prons = dictionary
        .get(second_word)
        .iter()
        .filter(|pron| entry.pronunciation.ends_with(pron))
        .collect::<Vec<_>>();

    // find a pair of pronunciations that add to make the original. we've already filtered to just
    // the pronunciations that are valid prefixes/suffixes of the original word, so we just need to
    // find a pair whose lengths add up
    let original_len = entry.pronunciation.len();
    for first_pron in first_prons {
        let first_len = first_pron.len();
        if let Some(&second_pron) = second_prons
            .iter()
            .find(|pron| first_len + pron.len() == original_len)
        {
            return Some([
                DictionaryEntry {
                    word: first_word.into(),
                    pronunciation: first_pron.clone(),
                },
                DictionaryEntry {
                    word: second_word.into(),
                    pronunciation: second_pron.clone(),
                },
            ]);
        }
    }
    None
}
