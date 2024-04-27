use std::collections::{BTreeMap, BTreeSet};

use crate::{
    dictionary::Word,
    pronounce::{self, DictionaryEntry, Pronunciation},
};

#[derive(Debug, serde::Serialize)]
pub struct CompoundWords {
    entries: BTreeMap<DictionaryEntry, [DictionaryEntry; 2]>,
    ambiguous: BTreeMap<DictionaryEntry, BTreeSet<[DictionaryEntry; 2]>>,
}

impl From<&pronounce::Dictionary> for CompoundWords {
    fn from(dictionary: &pronounce::Dictionary) -> Self {
        let mut entries = BTreeMap::new();
        let mut ambiguous = BTreeMap::new();
        for (word, pronunciation) in dictionary.entries() {
            let mut splits = get_splits(word, pronunciation, dictionary);
            let entry = DictionaryEntry {
                word: word.clone(),
                pronunciation: pronunciation.clone(),
            };
            if splits.len() > 1 {
                ambiguous.insert(entry, splits);
            } else if let Some(split) = splits.pop_first() {
                entries.insert(entry, split);
            }
        }
        Self { entries, ambiguous }
    }
}

pub fn get_unambiguous_split(
    word: &Word,
    pronunciation: &Pronunciation,
    dictionary: &pronounce::Dictionary,
) -> Option<[DictionaryEntry; 2]> {
    let mut splits = get_splits(word, pronunciation, dictionary);
    if splits.len() > 1 {
        None
    } else {
        splits.pop_first()
    }
}

fn get_splits(
    word: &Word,
    pronunciation: &Pronunciation,
    dictionary: &pronounce::Dictionary,
) -> BTreeSet<[DictionaryEntry; 2]> {
    if word.is_empty() {
        return BTreeSet::new();
    }
    (1..(word.len() - 1))
        .filter_map(|i| get_one_split(word, pronunciation, dictionary, i))
        .collect()
}

fn get_one_split(
    word: &Word,
    pronunciation: &Pronunciation,
    dictionary: &pronounce::Dictionary,
    split_idx: usize,
) -> Option<[DictionaryEntry; 2]> {
    let (first_word, second_word) = word.split_at(split_idx);
    let first_prons = dictionary
        .get(first_word)
        .iter()
        .filter(|pron| pronunciation.starts_with(pron))
        .collect::<Vec<_>>();
    let second_prons = dictionary
        .get(second_word)
        .iter()
        .filter(|pron| pronunciation.ends_with(pron))
        .collect::<Vec<_>>();

    // find a pair of pronunciations that add to make the original. we've already filtered to just
    // the pronunciations that are valid prefixes/suffixes of the original word, so we just need to
    // find a pair whose lengths add up
    let original_len = pronunciation.len();
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
