use std::{
    cmp,
    collections::{BTreeMap, BTreeSet},
    fmt,
    ops::Range,
    rc::Rc,
};

use regex::Regex;
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

#[derive(Clone, Hash, Eq, PartialEq, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Phoneme(Rc<str>);
crate::fmt_impls!(Phoneme);
crate::deref_impls!(Phoneme as str);

impl<S: Into<Rc<str>>> From<S> for Phoneme {
    fn from(s: S) -> Phoneme {
        Phoneme(s.into())
    }
}

/// Order phonemes first by decreasing length order, then by normal string ordering. This is so
/// that when constructing regexes, the sorted order will produce a regex that preferentially
/// matches longer phonemes.
impl Ord for Phoneme {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.len()
            .cmp(&other.len())
            .reverse()
            .then_with(|| self.0.cmp(&other.0))
    }
}

impl PartialOrd for Phoneme {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct RawPhonology {
    onset_singles: BTreeSet<Phoneme>,
    onset_clusters: Vec<OnsetCluster>,
    consonants: BTreeSet<Phoneme>,
    vowels: BTreeSet<Phoneme>,
    primary_stress: Phoneme,
    secondary_stress: Phoneme,
}

#[derive(Debug, Clone, Deserialize)]
struct OnsetCluster {
    first: BTreeSet<Phoneme>,
    second: BTreeSet<Phoneme>,
}

impl TryFrom<RawPhonology> for Phonology {
    type Error = regex::Error;

    fn try_from(raw: RawPhonology) -> Result<Self, Self::Error> {
        fn pattern_from_set(
            set: impl IntoIterator<Item = Phoneme>,
            right_anchored: bool,
        ) -> Result<Regex, regex::Error> {
            let initial = if right_anchored {
                "(?:".to_owned()
            } else {
                String::new()
            };
            let mut pattern = set.into_iter().fold(initial, |mut pat, phon| {
                regex_syntax::escape_into(&phon, &mut pat);
                pat.push('|');
                pat
            });
            // remove trailing '|'
            pattern.pop();
            if right_anchored {
                pattern += ")$";
            }
            Regex::new(&pattern)
        }

        let onset_singles = pattern_from_set(raw.onset_singles, true)?;

        // map (second_consonant -> [first_consonants])
        let mut onset_clusters_map = BTreeMap::<_, BTreeSet<_>>::new();
        for cluster in raw.onset_clusters {
            for ph in cluster.second {
                onset_clusters_map
                    .entry(ph)
                    .or_default()
                    .extend(cluster.first.iter().cloned());
            }
        }
        let onset_clusters = onset_clusters_map
            .into_iter()
            .map(|(first, seconds)| pattern_from_set(seconds, true).map(|pat| (first, pat)))
            .collect::<Result<_, _>>()?;

        let stress_markers = [
            (raw.primary_stress, Stress::Primary),
            (raw.secondary_stress, Stress::Secondary),
        ]
        .into_iter()
        .collect::<BTreeMap<_, _>>();
        let stress_pattern = pattern_from_set(stress_markers.keys().cloned(), false)?;
        let consonants = pattern_from_set(raw.consonants, true)?;
        let vowels = pattern_from_set(raw.vowels, false)?;

        Ok(Phonology {
            onset_singles,
            onset_clusters,
            consonants,
            vowels,
            stress_pattern,
            stress_markers,
        })
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(try_from = "RawPhonology")]
pub struct Phonology {
    onset_singles: Regex,
    onset_clusters: BTreeMap<Phoneme, Regex>,
    consonants: Regex,
    vowels: Regex,
    stress_pattern: Regex,
    stress_markers: BTreeMap<Phoneme, Stress>,
}

impl Phonology {
    #[allow(unused)]
    pub fn syllabize_word<'p, 'w>(&'p self, word: &'w str) -> SyllableIterator<'p, 'w> {
        let mut next_stress_marker = self.next_stress_marker(word, 0);
        let prev_syllable = self.syllabize_one(word, 0);
        let prev_syllable_stress;
        let require_start;
        if next_stress_marker.1.start == 0 {
            prev_syllable_stress = next_stress_marker.0;
            require_start = Some(next_stress_marker.1.end);
            // get the *next* stress marker, since we've already passed this one by the first
            // syllable
            next_stress_marker = self.next_stress_marker(word, next_stress_marker.1.end);
        } else {
            require_start = Some(0);
            prev_syllable_stress = Stress::None;
        }

        SyllableIterator {
            phonology: self,
            word,
            next_stress_marker,
            prev_syllable,
            prev_syllable_stress,
            require_start,
        }
    }

    fn syllabize_one(&self, word: &str, start_at: usize) -> SyllableIndices {
        // find the first vowel
        let Some(vowel_match) = self.vowels.find_at(word, start_at) else {
            return SyllableIndices {
                onset: word.len(),
                vowel: word.len(),
                coda: word.len(),
            };
        };
        let vowel = vowel_match.start();
        let coda = vowel_match.end();
        // work backwards to find the maximally valid onset
        let mut onset = vowel;
        let mut regex = &self.onset_singles;
        while let Some(prev_in_cluster) = regex.find(&word[..onset]) {
            if let Some(consonant) = self.consonants.find(&word[..onset]) {
                if consonant.start() != prev_in_cluster.start() {
                    // there was another (longer) consonant that matched here
                    break;
                }
            }
            onset = prev_in_cluster.start();
            let Some(prev_regex) = self.onset_clusters.get(prev_in_cluster.as_str()) else {
                break;
            };
            regex = prev_regex;
        }
        SyllableIndices { onset, vowel, coda }
    }

    fn next_stress_marker(&self, word: &str, start_at: usize) -> (Stress, Range<usize>) {
        if let Some(m) = self.stress_pattern.find_at(word, start_at) {
            let stress = self
                .stress_markers
                .get(m.as_str())
                .expect("unexpected stress marker match");
            (*stress, m.range())
        } else {
            (Stress::None, word.len()..word.len())
        }
    }
}

/// Start indices of the onset, vowel, and coda of a syllable
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct SyllableIndices {
    onset: usize,
    vowel: usize,
    coda: usize,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Syllable<'w> {
    pub stress: Stress,
    pub onset: &'w str,
    pub vowel: &'w str,
    pub coda: &'w str,
}

impl fmt::Display for Syllable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.onset)?;
        f.write_str(self.vowel)?;
        f.write_str(self.coda)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum Stress {
    None,
    Secondary,
    Primary,
}

pub struct SyllableIterator<'p, 'w> {
    phonology: &'p Phonology,
    word: &'w str,
    next_stress_marker: (Stress, Range<usize>),
    prev_syllable: SyllableIndices,
    prev_syllable_stress: Stress,
    require_start: Option<usize>,
}

impl<'p, 'w> Iterator for SyllableIterator<'p, 'w> {
    type Item = anyhow::Result<Syllable<'w>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.prev_syllable.onset >= self.word.len() {
            None
        } else {
            let mut result = Ok(());
            if let Some(require_start) = self.require_start {
                if self.prev_syllable.onset > require_start {
                    result = Err(anyhow::anyhow!(
                        "invalid onset: {:?}",
                        &self.word[require_start..self.prev_syllable.onset]
                    ));
                }
            }

            let next_syllable = self
                .phonology
                .syllabize_one(self.word, self.prev_syllable.coda);

            let next_syllable_stress;
            let coda_end;
            if next_syllable.vowel >= self.next_stress_marker.1.end {
                self.require_start = Some(self.next_stress_marker.1.end);
                next_syllable_stress = self.next_stress_marker.0;
                coda_end = self.next_stress_marker.1.start;
                self.next_stress_marker = self
                    .phonology
                    .next_stress_marker(self.word, self.next_stress_marker.1.end);
            } else {
                self.require_start = None;
                next_syllable_stress = Stress::None;
                coda_end = self.prev_syllable.coda.max(next_syllable.onset);
            }
            let item = Syllable {
                stress: self.prev_syllable_stress,
                onset: &self.word[self.prev_syllable.onset..self.prev_syllable.vowel],
                vowel: &self.word[self.prev_syllable.vowel..self.prev_syllable.coda],
                coda: &self.word[self.prev_syllable.coda..coda_end],
            };
            self.prev_syllable = next_syllable;
            self.prev_syllable_stress = next_syllable_stress;
            Some(result.map(|()| item))
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
    use std::{collections::BTreeSet, fs::File, io::BufReader};

    use test_case::test_case;

    use super::{OnsetCluster, Phoneme, Phonology, RawPhonology, Theory};

    #[test]
    fn load_theory() -> anyhow::Result<()> {
        let _theory: Theory = serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        Ok(())
    }

    fn phoneme_set(s: &str) -> BTreeSet<Phoneme> {
        s.split_whitespace().map(|ph| Phoneme(ph.into())).collect()
    }

    #[test_case("sput", &["sput"] ; "one syllable")]
    #[test_case("epsol", &["ep", "sol"] ; "vowel initial")]
    #[test_case("talsprot", &["tal", "sprot"] ; "complex onset")]
    #[test_case("tʃitʃrek", &["tʃitʃ", "rek"] ; "longer consonants")]
    #[test_case("tejis", &["tej", "jis"] ; "diphthong onset overlap")]
    #[test_case("tejkis", &["tej", "kis"] ; "diphthong plus consonant")]
    #[test_case("ˈtara", &["ta", "ra"] ; "initial stress")]
    #[test_case("ˈata", &["a", "ta"] ; "initial stress without onset")]
    #[test_case("tasˈpal", &["tas", "pal"] ; "medial stress")]
    #[test_case("kajˈteraˌpat", &["kaj", "te", "ra", "pat"] ; "multiple stresses")]
    fn syllabification(word: &str, expected_syllables: &[&str]) -> anyhow::Result<()> {
        let phonology: Phonology = RawPhonology {
            onset_singles: phoneme_set("p t k tʃ s ʃ r l j"),
            onset_clusters: [
                OnsetCluster {
                    first: phoneme_set("p k"),
                    second: phoneme_set("r l"),
                },
                OnsetCluster {
                    first: phoneme_set("t ʃ"),
                    second: phoneme_set("r"),
                },
                OnsetCluster {
                    first: phoneme_set("s"),
                    second: phoneme_set("p t k l"),
                },
            ]
            .into_iter()
            .collect(),
            consonants: phoneme_set("p t k tʃ s ʃ r l j"),
            vowels: phoneme_set("a e i o u ej"),
            primary_stress: "ˈ".into(),
            secondary_stress: "ˌ".into(),
        }
        .try_into()?;
        let actual_syllables = phonology
            .syllabize_word(word)
            .map(|res| res.map(|s| s.to_string()))
            .collect::<Result<Vec<_>, _>>()?;

        assert_eq!(actual_syllables, expected_syllables);
        Ok(())
    }
}
