use std::{collections::BTreeMap, fmt, rc::Rc};

use serde::{Deserialize, Serialize};

use crate::{
    chord::Chord,
    dictionary::{Outline, Word},
};

#[derive(Debug, Clone, Deserialize)]
pub struct Dictionary {
    entries: BTreeMap<Word, Vec<Pronunciation>>,
}

impl Dictionary {
    pub fn get(&self, word: &Word) -> &[Pronunciation] {
        self.entries.get(&*word.to_ascii_uppercase()).map(|ps| &**ps).unwrap_or(&[])
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct Pronunciation(Rc<[Phoneme]>);
crate::deref_impls!(Pronunciation as [Phoneme]);

impl fmt::Display for Pronunciation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0.join(" "), f)
    }
}

impl<'de> Deserialize<'de> for Pronunciation {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{Error, SeqAccess};
        struct Visitor;

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Pronunciation;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("an array of strings")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(Pronunciation(
                    v.split_whitespace()
                        .map(|segment| Phoneme(segment.into()))
                        .collect(),
                ))
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let size_hint = seq.size_hint().unwrap_or_default();
                let mut vec = Vec::with_capacity(size_hint);
                while let Some(phoneme) = seq.next_element::<Phoneme>()? {
                    vec.push(phoneme);
                }
                Ok(Pronunciation(vec.into()))
            }
        }

        de.deserialize_any(Visitor)
    }
}

#[derive(Clone, Hash, Eq, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Phoneme(Rc<str>);
crate::fmt_impls!(Phoneme);
crate::deref_impls!(Phoneme as str);

#[derive(Debug, Clone)]
pub struct Theory {
    vowels: BTreeMap<Pronunciation, Vec<Chord>>,
    onsets: BTreeMap<Pronunciation, Vec<Chord>>,
    codas: BTreeMap<Pronunciation, Vec<Chord>>,
    max_onset_len: usize,
    max_coda_len: usize,
}

impl<'de> Deserialize<'de> for Theory {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct RawTheory {
            vowels: BTreeMap<Pronunciation, Vec<Chord>>,
            onsets: BTreeMap<Pronunciation, Vec<Chord>>,
            codas: BTreeMap<Pronunciation, Vec<Chord>>,
        }

        impl From<RawTheory> for Theory {
            fn from(theory: RawTheory) -> Self {
                fn max_len<T>(map: &BTreeMap<Pronunciation, T>) -> usize {
                    map.keys().map(|p| p.len()).max().unwrap_or_default()
                }
                let max_vowel_len = max_len(&theory.vowels);
                let max_onset_len = max_len(&theory.onsets).max(max_vowel_len);
                let max_coda_len = max_len(&theory.codas).max(max_vowel_len);
                Self {
                    vowels: theory.vowels,
                    onsets: theory.onsets,
                    codas: theory.codas,
                    max_onset_len,
                    max_coda_len,
                }
            }
        }

        RawTheory::deserialize(de).map(Self::from)
    }
}

impl Theory {
    pub fn get_outline(&self, pronunciation: &Pronunciation) -> Option<Outline> {
        use std::fmt::Write;
        enum SyllablePosition {
            Onset,
            Coda,
        }
        let mut current_chord = Chord::empty();
        let mut syllable_position = SyllablePosition::Onset;
        let mut outline = String::new();
        let mut idx = 0;
        'outer: while idx < pronunciation.len() {
            match syllable_position {
                SyllablePosition::Onset => {
                    for len in (0..=self.max_onset_len).rev() {
                        if let Some(substr) = pronunciation.get(idx..idx + len) {
                            if let Some(chords) = self.get_onset(substr) {
                                for &chord in chords {
                                    if current_chord.before(chord) {
                                        current_chord |= chord;
                                        idx += len;
                                        continue 'outer;
                                    }
                                }
                            }
                            if let Some(chords) = self.get_vowel(substr) {
                                for &chord in chords {
                                    if current_chord.before(chord) {
                                        current_chord |= chord;
                                        syllable_position = SyllablePosition::Coda;
                                        idx += len;
                                        continue 'outer;
                                    }
                                }
                            }
                        }
                    }
                    return None;
                }
                SyllablePosition::Coda => {
                    for len in (0..=self.max_coda_len).rev() {
                        if let Some(substr) = pronunciation.get(idx..idx + len) {
                            if let Some(chords) = self.get_coda(substr) {
                                for &chord in chords {
                                    if current_chord.before(chord) {
                                        current_chord |= chord;
                                        idx += len;
                                        continue 'outer;
                                    }
                                }
                            }
                        }
                    }
                    let _ = write!(outline, "/{current_chord}");
                    current_chord = Chord::empty();
                    syllable_position = SyllablePosition::Onset;
                }
            }
        }
        let _ = write!(outline, "/{current_chord}");

        Some((&outline[1..]).into())
    }

    fn get_onset(&self, sounds: &[Phoneme]) -> Option<&[Chord]> {
        self.onsets.get(sounds).map(|ch| &**ch)
    }

    fn get_vowel(&self, sounds: &[Phoneme]) -> Option<&[Chord]> {
        self.vowels.get(sounds).map(|ch| &**ch)
    }

    fn get_coda(&self, sounds: &[Phoneme]) -> Option<&[Chord]> {
        self.codas.get(sounds).map(|ch| &**ch)
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::BufReader};

    use super::Theory;

    #[test]
    fn load_theory() -> anyhow::Result<()> {
        let _theory: Theory = serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        Ok(())
    }
}
