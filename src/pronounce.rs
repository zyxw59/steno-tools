use std::{cmp, collections::BTreeMap, fmt, io::BufRead, ops::Deref, rc::Rc};

use anyhow::Context;
use enumset::EnumSetType;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::dictionary::Word;

#[derive(Default, Debug, Clone, Deserialize)]
pub struct Dictionary {
    entries: BTreeMap<Word, Vec<Pronunciation>>,
}

impl Dictionary {
    pub fn load(reader: impl BufRead) -> anyhow::Result<Self> {
        let word_variant_pattern = regex::Regex::new(r"\([0-9]+\)$")?;
        let mut this = Self::default();
        for (idx, line) in reader.lines().enumerate() {
            let line = line.with_context(|| {
                format!("reading line {} from pronunciation dictionary", idx + 1)
            })?;
            // comments
            if line.starts_with(";;;") {
                continue;
            }
            let Some((word, prons)) = line.split_once("  ") else {
                eprintln!("line did not contain a pronunciation: {line}");
                continue;
            };
            let mut word = word_variant_pattern.replace(word, "");
            word.to_mut().make_ascii_lowercase();
            let entries = this.entries.entry(word.into()).or_default();
            entries.push(prons.into());
        }
        Ok(this)
    }

    pub fn get(&self, word: &str) -> &[Pronunciation] {
        self.entries
            .get(&*word.to_ascii_lowercase())
            .map(|ps| &**ps)
            .unwrap_or(&[])
    }

    pub fn entries(&self) -> impl Iterator<Item = DictionaryEntry> + '_ {
        self.entries.iter().flat_map(|(word, pronunciations)| {
            pronunciations.iter().map(|pronunciation| DictionaryEntry {
                word: word.clone(),
                pronunciation: pronunciation.clone(),
            })
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize)]
pub struct DictionaryEntry {
    pub word: Word,
    pub pronunciation: Pronunciation,
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Deserialize)]
#[serde(from = "SerdePronunciation")]
pub struct Pronunciation(Rc<[Phoneme]>);
crate::deref_impls!(Pronunciation as [Phoneme]);

impl Serialize for Pronunciation {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(ser)
    }
}

#[derive(Deserialize)]
#[serde(untagged)]
enum SerdePronunciation {
    String(String),
    List(Rc<[Phoneme]>),
}

impl From<&str> for Pronunciation {
    fn from(s: &str) -> Self {
        Self(s.split_whitespace().map(Phoneme::from).collect())
    }
}

impl From<SerdePronunciation> for Pronunciation {
    fn from(s: SerdePronunciation) -> Self {
        match s {
            SerdePronunciation::String(s) => s.as_str().into(),
            SerdePronunciation::List(l) => Self(l),
        }
    }
}

impl fmt::Display for Pronunciation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&PronunciationSlice(&self.0), f)
    }
}

impl fmt::Debug for Pronunciation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&PronunciationSlice(&self.0), f)
    }
}

pub struct PronunciationSlice<'w>(pub &'w [Phoneme]);
crate::deref_impls!(PronunciationSlice<'_> as [Phoneme]);

impl fmt::Display for PronunciationSlice<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some((first, rest)) = self.0.split_first() {
            f.write_str(first)?;
            for ph in rest {
                f.write_str(" ")?;
                f.write_str(ph)?;
            }
        }
        Ok(())
    }
}

impl fmt::Debug for PronunciationSlice<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("\"")?;
        if let Some((first, rest)) = self.0.split_first() {
            f.write_str(first)?;
            for ph in rest {
                f.write_str(" ")?;
                f.write_str(ph)?;
            }
        }
        f.write_str("\"")
    }
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Phoneme(smol_str::SmolStr);
crate::fmt_impls!(Phoneme);

impl Phoneme {
    pub fn stress(&self) -> Stress {
        match self.0.as_bytes().last() {
            Some(b'1') => Stress::Primary,
            Some(b'2') => Stress::Secondary,
            _ => Stress::None,
        }
    }

    fn ignore_stress(&self) -> &str {
        self.0.trim_end_matches(&['0', '1', '2'])
    }
}

impl Ord for Phoneme {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.ignore_stress().cmp(other.ignore_stress())
    }
}

impl PartialOrd for Phoneme {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Phoneme {}

impl PartialEq for Phoneme {
    fn eq(&self, other: &Self) -> bool {
        self.ignore_stress() == other.ignore_stress()
    }
}

impl<S: AsRef<str>> From<S> for Phoneme {
    fn from(s: S) -> Phoneme {
        Phoneme(SmolStr::new(s))
    }
}

impl Deref for Phoneme {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Ord, PartialOrd, EnumSetType, Deserialize, serde::Serialize)]
#[enumset(serialize_repr = "list")]
pub enum Stress {
    None,
    Secondary,
    Primary,
}
