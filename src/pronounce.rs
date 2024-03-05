use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    rc::Rc,
};

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
        self.entries
            .get(&*word.to_ascii_uppercase())
            .map(|ps| &**ps)
            .unwrap_or(&[])
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

#[derive(Debug, Clone, Deserialize)]
pub struct Phonology<P: Ord> {
    onset_singles: BTreeSet<P>,
    onset_clusters: BTreeMap<P, BTreeSet<P>>,
    vowels: BTreeSet<P>,
}

impl<P: Ord> Phonology<P> {
    #[allow(unused)]
    pub fn syllabize_word<'p, 'w>(
        &'p self,
        word: &'w [P],
    ) -> anyhow::Result<SyllableIterator<'p, 'w, P>> {
        let (mut onset_start, mut coda_start) = self.syllabize_first(word, 0);
        if onset_start != 0 {
            return Err(anyhow::anyhow!(
                "failed to syllabize first {onset_start} phonemes"
            ));
        }
        Ok(SyllableIterator {
            onset_start,
            coda_start,
            word,
            phonology: self,
        })
    }

    /// Returns the index of the start of the next syllable, and the index after the vowel in that
    /// syllable.
    fn syllabize_first(&self, word: &[P], start_at: usize) -> (usize, usize) {
        // find the first vowel
        let Some(vowel_idx) = word[start_at..]
            .iter()
            .position(|ph| self.vowels.contains(ph))
        else {
            return (word.len(), word.len());
        };
        let vowel_idx = vowel_idx + start_at;
        // work backwards to find the maximally valid onset
        let mut onset = word[..vowel_idx].iter().enumerate().rev();
        let last_item = match onset.next() {
            Some((idx, ph)) if self.onset_singles.contains(ph) => (idx, ph),
            _ => return (vowel_idx, vowel_idx + 1),
        };
        let result = onset.try_fold(last_item, |(following_idx, following_ph), (idx, ph)| {
            if self
                .onset_clusters
                .get(ph)
                .map_or(false, |followers| followers.contains(following_ph))
            {
                Ok((idx, ph))
            } else {
                Err(following_idx)
            }
        });
        match result {
            Ok((idx, _)) | Err(idx) => (idx, vowel_idx + 1),
        }
    }
}

pub struct SyllableIterator<'p, 'w, P: Ord> {
    phonology: &'p Phonology<P>,
    word: &'w [P],
    onset_start: usize,
    coda_start: usize,
}

impl<'p, 'w, P: Ord> Iterator for SyllableIterator<'p, 'w, P> {
    type Item = &'w [P];

    fn next(&mut self) -> Option<Self::Item> {
        if self.onset_start >= self.word.len() {
            None
        } else {
            let (next_onset, next_coda) =
                self.phonology.syllabize_first(self.word, self.coda_start);
            let item = &self.word[self.onset_start..next_onset];
            self.onset_start = next_onset;
            self.coda_start = next_coda;
            Some(item)
        }
    }
}

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

    use test_case::test_case;

    use super::{Phonology, Theory};

    #[test]
    fn load_theory() -> anyhow::Result<()> {
        let _theory: Theory = serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        Ok(())
    }

    #[test_case("spun", &["spun"] ; "one syllable")]
    #[test_case("epsom", &["ep", "som"] ; "vowel initial")]
    #[test_case("malsprot", &["mal", "sprot"] ; "complex onset")]
    fn syllabification(word: &str, expected_syllables: &[&str]) -> anyhow::Result<()> {
        let phonology = Phonology {
            onset_singles: "ptksrlmn".bytes().collect(),
            onset_clusters: [(b'p', "rl"), (b't', "r"), (b'k', "rl"), (b's', "ptklmn")]
                .into_iter()
                .map(|(k, v)| (k, v.bytes().collect()))
                .collect(),
            vowels: "aeiou".bytes().collect(),
        };
        let actual_syllables = phonology
            .syllabize_word(word.as_bytes())?
            .map(std::str::from_utf8)
            .collect::<Result<Vec<_>, _>>()?;

        assert_eq!(actual_syllables, expected_syllables);
        Ok(())
    }
}
