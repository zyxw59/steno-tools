use std::{
    borrow::Borrow,
    cmp,
    collections::{BTreeMap, BTreeSet},
    fmt,
    io::BufRead,
    ops,
    ops::Range,
    rc::Rc,
};

use itertools::Itertools;
use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::{
    chord::Chord,
    dictionary::{Outline, Word},
};

#[derive(Default, Debug, Clone, Deserialize)]
pub struct Dictionary {
    entries: BTreeMap<Word, Vec<Pronunciation>>,
}

impl Dictionary {
    pub fn load_csv(reader: impl BufRead) -> anyhow::Result<Self> {
        let mut this = Self::default();
        for line in reader.lines() {
            let line = line?;
            let Some((word, prons)) = line.split_once(',') else {
                continue;
            };
            let entries = this.entries.entry(word.into()).or_default();
            for p in prons.trim_matches('"').split(',') {
                let pron = p.trim().trim_matches('/');
                entries.push(pron.into());
            }
        }
        Ok(this)
    }

    pub fn get(&self, word: &Word) -> &[Pronunciation] {
        self.entries
            .get(&*word.to_ascii_lowercase())
            .map(|ps| &**ps)
            .unwrap_or(&[])
    }
}

pub type Pronunciation = Rc<str>;

#[derive(Clone, Hash, Eq, PartialEq, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Phoneme(Rc<str>);
crate::fmt_impls!(Phoneme);

impl<S: Into<Rc<str>>> From<S> for Phoneme {
    fn from(s: S) -> Phoneme {
        Phoneme(s.into())
    }
}

impl ops::Deref for Phoneme {
    type Target = PhonemeRef;

    fn deref(&self) -> &Self::Target {
        PhonemeRef::new(&self.0)
    }
}

impl Borrow<PhonemeRef> for Phoneme {
    fn borrow(&self) -> &PhonemeRef {
        self
    }
}

impl Ord for Phoneme {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        PhonemeRef::cmp(self, other)
    }
}

impl PartialOrd for Phoneme {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Hash, Eq, PartialEq)]
#[repr(transparent)]
pub struct PhonemeRef(str);

impl PhonemeRef {
    pub fn new(s: &str) -> &Self {
        unsafe { &*(s as *const str as *const Self) }
    }
}

impl ops::Deref for PhonemeRef {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Order phonemes first by decreasing length order, then by normal string ordering. This is so
/// that when constructing regexes, the sorted order will produce a regex that preferentially
/// matches longer phonemes.
impl Ord for PhonemeRef {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.0
            .len()
            .cmp(&other.0.len())
            .reverse()
            .then_with(|| self.0.cmp(&other.0))
    }
}

impl PartialOrd for PhonemeRef {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct PhoneticTheory {
    pub theory: Theory,
    pub phonology: Phonology,
}

impl PhoneticTheory {
    pub fn get_outline(&self, pronunciation: &str, spelling: &str) -> anyhow::Result<Outline> {
        let mut strokes = Vec::new();
        for syllable in self.phonology.syllabize_word(pronunciation) {
            let syllable = syllable?;
            let mut possible_strokes = vec![Chord::empty()];
            let mut next_strokes = Vec::new();
            for possible_chords in self.theory.onset_matches(syllable.onset()) {
                for st in possible_strokes.drain(..) {
                    for &ch in possible_chords {
                        if (st & ch).is_empty() & st.before_ignore_star(ch) {
                            next_strokes.push(st | ch);
                        }
                    }
                }
                std::mem::swap(&mut possible_strokes, &mut next_strokes);
            }
            if possible_strokes.is_empty() {
                return Err(anyhow::anyhow!("no strokes for onset {:?}", syllable.onset()));
            }
            for possible_chords in self.theory.vowel_matches(syllable.vowel()) {
                for st in possible_strokes.drain(..) {
                    for &ch in possible_chords {
                        if (st & ch).is_empty() & st.before_ignore_star(ch) {
                            next_strokes.push(st | ch);
                        }
                    }
                }
                std::mem::swap(&mut possible_strokes, &mut next_strokes);
            }
            if possible_strokes.is_empty() {
                return Err(anyhow::anyhow!("no strokes for vowel {:?}", syllable.vowel()));
            }
            for possible_chords in self.theory.coda_matches(syllable.coda()) {
                for st in possible_strokes.drain(..) {
                    for &ch in possible_chords {
                        if (st & ch).is_empty() & st.before_ignore_star(ch) {
                            next_strokes.push(st | ch);
                        }
                    }
                }
                std::mem::swap(&mut possible_strokes, &mut next_strokes);
            }
            if possible_strokes.is_empty() {
                return Err(anyhow::anyhow!("no strokes for coda {:?}", syllable.coda()));
            }
            let stroke = *possible_strokes.first().ok_or_else(|| {
                anyhow::anyhow!("no valid strokes found for syllable \"{syllable:#}\"")
            })?;
            strokes.push(stroke);
        }
        Ok(strokes.into_iter().join("/").into())
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct RawTheory {
    onsets: BTreeMap<Phoneme, Vec<Chord>>,
    vowels: BTreeMap<Phoneme, Vec<Chord>>,
    codas: BTreeMap<Phoneme, Vec<Chord>>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(try_from = "RawTheory")]
pub struct Theory {
    onset_pattern: Regex,
    vowel_pattern: Regex,
    coda_pattern: Regex,
    raw: RawTheory,
}

impl Theory {
    fn onset_matches<'t, 'w>(&'t self, onset: &'w str) -> MatchChordIter<'t, 'w> {
        MatchChordIter {
            matches: self.onset_pattern.find_iter(onset),
            map: &self.raw.onsets,
        }
    }

    fn vowel_matches<'t, 'w>(&'t self, vowel: &'w str) -> MatchChordIter<'t, 'w> {
        MatchChordIter {
            matches: self.vowel_pattern.find_iter(vowel),
            map: &self.raw.vowels,
        }
    }

    fn coda_matches<'t, 'w>(&'t self, coda: &'w str) -> MatchChordIter<'t, 'w> {
        MatchChordIter {
            matches: self.coda_pattern.find_iter(coda),
            map: &self.raw.codas,
        }
    }
}

struct MatchChordIter<'t, 'w> {
    matches: regex::Matches<'t, 'w>,
    map: &'t BTreeMap<Phoneme, Vec<Chord>>,
}

impl<'t, 'w> Iterator for MatchChordIter<'t, 'w> {
    type Item = &'t [Chord];

    fn next(&mut self) -> Option<Self::Item> {
        let m = self.matches.next()?;
        Some(
            self.map
                .get(PhonemeRef::new(m.as_str()))
                .map_or(&[], |v| &**v),
        )
    }
}

impl TryFrom<RawTheory> for Theory {
    type Error = regex::Error;

    fn try_from(raw: RawTheory) -> Result<Self, Self::Error> {
        let onset_pattern = pattern_from_set(raw.onsets.keys().cloned(), false)?;
        let vowel_pattern = pattern_from_set(raw.vowels.keys().cloned(), false)?;
        let coda_pattern = pattern_from_set(raw.codas.keys().cloned(), false)?;
        Ok(Self {
            onset_pattern,
            vowel_pattern,
            coda_pattern,
            raw,
        })
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
        let prev_stress_marker;
        if next_stress_marker.1.start == 0 {
            prev_syllable_stress = next_stress_marker.0;
            prev_stress_marker = Some(next_stress_marker.1.clone());
            // get the *next* stress marker, since we've already passed this one by the first
            // syllable
            next_stress_marker = self.next_stress_marker(word, next_stress_marker.1.end);
        } else {
            prev_stress_marker = Some(0..0);
            prev_syllable_stress = Stress::None;
        }

        SyllableIterator {
            phonology: self,
            word,
            next_stress_marker,
            prev_syllable,
            prev_syllable_stress,
            prev_stress_marker,
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
            let Some(prev_regex) = self
                .onset_clusters
                .get(PhonemeRef::new(prev_in_cluster.as_str()))
            else {
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
                .get(PhonemeRef::new(m.as_str()))
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
    word: &'w str,
    start: usize,
    indices: SyllableIndices,
    end: usize,
}

impl<'w> Syllable<'w> {
    pub fn onset(&self) -> &'w str {
        &self.word[self.indices.onset..self.indices.vowel]
    }

    pub fn vowel(&self) -> &'w str {
        &self.word[self.indices.vowel..self.indices.coda]
    }

    pub fn coda(&self) -> &'w str {
        &self.word[self.indices.coda..self.end]
    }

    pub fn as_str(&self) -> &'w str {
        &self.word[self.start..self.end]
    }
}

impl fmt::Display for Syllable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            f.write_str(&self.word[self.start..self.indices.onset])?;
            f.write_str(" ")?;
            f.write_str(self.onset())?;
            f.write_str(" ")?;
            f.write_str(self.vowel())?;
            f.write_str(" ")?;
            f.write_str(self.coda())?;
        } else {
            f.write_str(self.as_str())?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum Stress {
    None,
    Secondary,
    Primary,
}

impl fmt::Display for Stress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::None => Ok(()),
            Self::Secondary => f.write_str("ˌ"),
            Self::Primary => f.write_str("ˈ"),
        }
    }
}

pub struct SyllableIterator<'p, 'w> {
    phonology: &'p Phonology,
    word: &'w str,
    next_stress_marker: (Stress, Range<usize>),
    prev_syllable: SyllableIndices,
    prev_syllable_stress: Stress,
    prev_stress_marker: Option<Range<usize>>,
}

impl<'p, 'w> Iterator for SyllableIterator<'p, 'w> {
    type Item = anyhow::Result<Syllable<'w>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.prev_syllable.onset >= self.word.len() {
            None
        } else {
            let mut result = Ok(());
            let start;
            if let Some(prev_stress_marker) = self.prev_stress_marker.take() {
                start = prev_stress_marker.start;
                if self.prev_syllable.onset > prev_stress_marker.end {
                    result = Err(anyhow::anyhow!(
                        "invalid onset: {:?}",
                        &self.word[prev_stress_marker.end..self.prev_syllable.vowel]
                    ));
                }
            } else {
                start = self.prev_syllable.onset;
            }

            let next_syllable = self
                .phonology
                .syllabize_one(self.word, self.prev_syllable.coda);

            let next_syllable_stress;
            let coda_end;
            if next_syllable.vowel >= self.next_stress_marker.1.end {
                self.prev_stress_marker = Some(self.next_stress_marker.1.clone());
                next_syllable_stress = self.next_stress_marker.0;
                coda_end = self.next_stress_marker.1.start;
                self.next_stress_marker = self
                    .phonology
                    .next_stress_marker(self.word, self.next_stress_marker.1.end);
            } else {
                self.prev_stress_marker = None;
                next_syllable_stress = Stress::None;
                coda_end = self.prev_syllable.coda.max(next_syllable.onset);
            }
            let item = Syllable {
                stress: self.prev_syllable_stress,
                word: self.word,
                start,
                indices: self.prev_syllable,
                end: coda_end,
            };
            self.prev_syllable = next_syllable;
            self.prev_syllable_stress = next_syllable_stress;
            Some(result.map(|()| item))
        }
    }
}

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

#[cfg(test)]
mod tests {
    use std::{collections::BTreeSet, fs::File, io::BufReader};

    use test_case::test_case;

    use super::{OnsetCluster, Phoneme, PhoneticTheory, Phonology, RawPhonology};

    #[test]
    fn load_theory() -> anyhow::Result<()> {
        let _theory: PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
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
    #[test_case("ˈtara", &["ˈta", "ra"] ; "initial stress")]
    #[test_case("ˈata", &["ˈa", "ta"] ; "initial stress without onset")]
    #[test_case("tasˈpal", &["tas", "ˈpal"] ; "medial stress")]
    #[test_case("kajˈteraˌpat", &["kaj", "ˈte", "ra", "ˌpat"] ; "multiple stresses")]
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

    #[test_case("a", "ˈej", "AEU" ; "a")]
    fn word_to_outline(
        spelling: &str,
        pronunciation: &str,
        expected_outline: &str,
    ) -> anyhow::Result<()> {
        let theory: PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        let actual_outline = theory.get_outline(pronunciation, spelling)?;
        assert_eq!(&*actual_outline, expected_outline);
        Ok(())
    }
}
