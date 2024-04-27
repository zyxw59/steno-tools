use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    ops::Range,
};

use enumset::{EnumSet, EnumSetType};
use serde::Deserialize;

use crate::{
    pronounce::{Phoneme, Pronunciation, PronunciationSlice, Stress},
    tree::Tree,
};

#[derive(Debug, Clone, Deserialize)]
struct RawCluster {
    first: BTreeSet<Phoneme>,
    second: BTreeSet<Phoneme>,
}

#[derive(Debug, Default, Clone, Deserialize)]
#[serde(from = "Vec<RawCluster>")]
struct ClusterMap {
    map: BTreeMap<Phoneme, BTreeSet<Phoneme>>,
}

impl ClusterMap {
    /// Finds the maximal cluster within the range of indices in the word, anchored at the end.
    /// Returns the start index of the cluster.
    pub fn find(
        &self,
        word: &[Phoneme],
        range: Range<usize>,
        initial: Option<&BTreeSet<Phoneme>>,
    ) -> usize {
        let empty_set = BTreeSet::new();
        let mut allowed_phonemes = initial
            .or_else(|| self.map.get(&word[range.end]))
            .unwrap_or(&empty_set);
        word[range.clone()]
            .iter()
            .rposition(|ph| {
                if !allowed_phonemes.contains(ph) {
                    return true;
                }
                allowed_phonemes = self.map.get(ph).unwrap_or(&empty_set);
                false
            })
            .map(|idx| idx + 1 + range.start)
            .unwrap_or(range.start)
    }
}

impl From<Vec<RawCluster>> for ClusterMap {
    fn from(raw_clusters: Vec<RawCluster>) -> Self {
        // map (second_consonant -> [first_consonants])
        let mut map = BTreeMap::<_, BTreeSet<_>>::new();
        for cluster in raw_clusters {
            for ph in cluster.second {
                map.entry(ph)
                    .or_default()
                    .extend(cluster.first.iter().cloned());
            }
        }
        Self { map }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Deserialize)]
struct MultiSyllable {
    onset: Pronunciation,
    vowel: Phoneme,
    coda: Pronunciation,
    stress: Option<EnumSet<Stress>>,
    position: Option<EnumSet<SyllablePosition>>,
}

impl MultiSyllable {
    fn len(&self) -> usize {
        self.onset.len() + 1 + self.coda.len()
    }

    fn find(
        &self,
        word: &[Phoneme],
        starting_range: Range<usize>,
    ) -> Option<(SyllableIndices, usize)> {
        for onset in starting_range {
            if onset + self.len() > word.len() {
                break;
            }
            let vowel = onset + self.onset.len();
            let coda = vowel + 1;
            let end = coda + self.coda.len();
            let vowel_ph = &word[vowel];
            let indices = SyllableIndices {
                onset,
                vowel,
                coda,
                stress: vowel_ph.stress(),
            };
            let position = SyllablePosition::new(onset == 0, end == word.len());
            if word[onset..vowel] == *self.onset
                && vowel_ph == &self.vowel
                && self
                    .stress
                    .map(|st| st.contains(indices.stress))
                    .unwrap_or(true)
                && self
                    .position
                    .map(|pos| pos.contains(position))
                    .unwrap_or(true)
                && word[coda..end] == *self.coda
            {
                return Some((indices, end));
            }
        }
        None
    }
}

#[derive(Debug, EnumSetType, Deserialize, serde::Serialize)]
#[enumset(serialize_repr = "list")]
pub enum SyllablePosition {
    Only,
    Initial,
    Medial,
    Final,
}

impl SyllablePosition {
    fn new(is_start: bool, is_end: bool) -> Self {
        match (is_start, is_end) {
            (true, true) => SyllablePosition::Only,
            (true, false) => SyllablePosition::Initial,
            (false, true) => SyllablePosition::Final,
            (false, false) => SyllablePosition::Medial,
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Phonology {
    onset_singles: BTreeSet<Phoneme>,
    onset_clusters: ClusterMap,
    vowels: BTreeSet<Phoneme>,
    #[serde(default)]
    vowel_clusters: ClusterMap,
    #[serde(default)]
    multi_syllables: Vec<MultiSyllable>,
}

impl Phonology {
    fn syllabize_one(&self, word: &[Phoneme], start_at: usize) -> SyllableIndices {
        // find the first vowel
        let Some(mut vowel) = word[start_at..]
            .iter()
            .position(|ph| self.vowels.contains(ph))
        else {
            return SyllableIndices {
                stress: Stress::None,
                onset: word.len(),
                vowel: word.len(),
                coda: word.len(),
            };
        };
        vowel += start_at;
        let stress = word[vowel].stress();
        let coda = vowel + 1;
        vowel = self.vowel_clusters.find(word, start_at..vowel, None);
        // work backwards to find the maximally valid onset
        let onset = self
            .onset_clusters
            .find(word, start_at..vowel, Some(&self.onset_singles));
        SyllableIndices {
            stress,
            onset,
            vowel,
            coda,
        }
    }

    pub fn syllable_tree<'w>(&self, word: &'w [Phoneme]) -> Tree<Syllable<'w>> {
        let half_syllable_tree = Tree::build_with_leaf_validation(
            self.get_initial_syllables(word),
            |(prev, end)| {
                if prev.onset == word.len() {
                    None
                } else {
                    Some(self.get_next_syllables(word, end.unwrap_or(prev.coda), end.is_none()))
                }
                .into_iter()
                .flatten()
            },
            |(prev, _end)| prev.onset == word.len(),
        );
        half_syllable_tree.contract(|(prev, end), (next, _)| {
            let end = end.unwrap_or(prev.coda.max(next.onset));
            Syllable {
                word,
                indices: *prev,
                end,
            }
        })
    }

    fn get_initial_syllables<'p, 'w>(
        &'p self,
        word: &'w [Phoneme],
    ) -> impl Iterator<Item = (SyllableIndices, Option<usize>)> + Captures<(&'p (), &'w ())> {
        self.get_next_syllables(word, 0, false)
    }

    fn get_next_syllables<'p, 'w>(
        &'p self,
        word: &'w [Phoneme],
        start_at: usize,
        allow_coda: bool,
    ) -> impl Iterator<Item = (SyllableIndices, Option<usize>)> + Captures<(&'p (), &'w ())> {
        let plain_syllable = self.syllabize_one(word, start_at);
        let end = if allow_coda {
            plain_syllable.coda
        } else {
            start_at + 1
        };
        let others = self.find_multisyllable(word, start_at..end);
        let plain_syllable_iter = if allow_coda || plain_syllable.onset == start_at {
            Some((plain_syllable, None))
        } else {
            None
        };
        others.chain(plain_syllable_iter)
    }

    fn find_multisyllable<'p, 'w>(
        &'p self,
        word: &'w [Phoneme],
        starting_range: Range<usize>,
    ) -> impl Iterator<Item = (SyllableIndices, Option<usize>)> + Captures<(&'p (), &'w ())> {
        self.multi_syllables
            .iter()
            .filter_map(move |multi| multi.find(word, starting_range.clone()))
            .map(|(si, end)| (si, Some(end)))
    }
}

trait Captures<U> {}
impl<T: ?Sized, U> Captures<U> for T {}

/// Start indices of the onset, vowel, and coda of a syllable
#[derive(Clone, Copy, Eq, PartialEq)]
struct SyllableIndices {
    stress: Stress,
    onset: usize,
    vowel: usize,
    coda: usize,
}

impl fmt::Debug for SyllableIndices {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}..{}..{} ({:?})",
            self.onset, self.vowel, self.coda, self.stress
        )
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Syllable<'w> {
    word: &'w [Phoneme],
    indices: SyllableIndices,
    end: usize,
}

impl<'w> Syllable<'w> {
    pub fn range(&self) -> Range<usize> {
        self.start()..self.end
    }

    pub fn start(&self) -> usize {
        self.indices.onset
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn onset_range(&self) -> Range<usize> {
        self.indices.onset..self.indices.vowel
    }

    pub fn vowel_range(&self) -> Range<usize> {
        self.indices.vowel..self.indices.coda
    }

    pub fn vowel(&self) -> &'w [Phoneme] {
        &self.word[self.vowel_range()]
    }

    pub fn coda_range(&self) -> Range<usize> {
        self.indices.coda..self.end
    }

    pub fn as_slice(&self) -> &'w [Phoneme] {
        &self.word[self.indices.onset..self.end]
    }

    pub fn stress(&self) -> Stress {
        self.indices.stress
    }

    /// Skips the first `skip` phonemes of the syllable
    pub fn skip(mut self, skip: usize) -> Self {
        self.indices.onset += skip;
        self.indices.vowel = self.indices.vowel.max(self.indices.onset);
        self.indices.coda = self.indices.coda.max(self.indices.onset);
        self.end = self.end.max(self.indices.onset);
        self
    }

    pub fn word(&self) -> &'w [Phoneme] {
        self.word
    }

    /// Returns the part of the syllable up to the specified index into the word.
    pub fn until_index(&self, index: usize) -> &'w [Phoneme] {
        &self.word[self.indices.onset..index]
    }

    /// Returns the part of the syllable after the specified index into the word.
    pub fn after_index(&self, index: usize) -> &'w [Phoneme] {
        &self.word[index..self.end]
    }

    pub fn position(&self) -> SyllablePosition {
        SyllablePosition::new(self.indices.onset == 0, self.end == self.word.len())
    }
}

impl fmt::Display for Syllable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&PronunciationSlice(self.as_slice()), f)
    }
}

impl fmt::Debug for Syllable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&PronunciationSlice(self.as_slice()), f)
    }
}

pub struct SyllableIterator<'p, 'w> {
    phonology: &'p Phonology,
    word: &'w [Phoneme],
    prev_syllable: SyllableIndices,
}

impl<'p, 'w> Iterator for SyllableIterator<'p, 'w> {
    type Item = Syllable<'w>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.prev_syllable.onset >= self.word.len() {
            None
        } else {
            let next_syllable = self
                .phonology
                .syllabize_one(self.word, self.prev_syllable.coda);
            let end = self.prev_syllable.coda.max(next_syllable.onset);
            let item = Syllable {
                word: self.word,
                indices: self.prev_syllable,
                end,
            };
            self.prev_syllable = next_syllable;
            Some(item)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::BufReader};

    use itertools::Itertools;
    use test_case::test_case;

    use crate::{phonology::Pronunciation, theory::PhoneticTheory};

    #[test_case("S N UW1 T", &["S N UW1 T"] ; "snoot")]
    #[test_case("S EY1 IH0 NG", &["S EY1/IH0 NG", "S EY1/IH0 NG"] ; "saying")]
    #[test_case("B AE1 K Y AA2 R D", &["B AE1 K/Y AA2 R D"] ; "backyard")]
    #[test_case("K Y UW1 B", &["K Y UW1 B"] ; "cube")]
    #[test_case("IH0 K S P EH2 N D", &["IH0 K S/P EH2 N D", "IH0 K/S P EH2 N D"]; "expend")]
    #[test_case("IH0 K S CH EY2 N JH", &["IH0 K S/CH EY2 N JH", "IH0 K S/CH EY2 N JH"] ; "exchange")]
    #[test_case("D IH1 S T AH0 N T", &["D IH1 S/T AH0 N T", "D IH1/S T AH0 N T"] ; "distant")]
    #[test_case("L AH1 V AH0 B AH0 L", &[
        "L AH1 V/AH0 B AH0 L",
        "L AH1 V/AH0/B AH0 L",
        "L AH1/V AH0 B/AH0 L",
        "L AH1/V AH0/B AH0 L",
    ] ; "loveable")]
    fn syllabification(word: &str, expected_syllables: &[&str]) -> anyhow::Result<()> {
        let theory: PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        let phonology = theory.phonology;
        let pronunciation = Pronunciation::from(word);
        let tree = phonology.syllable_tree(&pronunciation);
        eprintln!("syllable tree: {tree:?}");
        let actual_syllables = tree
            .as_ref()
            .paths()
            .with(|path| path.map(ToString::to_string).join("/"))
            .collect::<Vec<_>>();
        assert_eq!(actual_syllables, expected_syllables);
        Ok(())
    }
}
