use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    ops::Range,
    rc::Rc,
};

use enumset::{EnumSet, EnumSetType};
use itertools::Itertools;
use regex::{RegexSet, RegexSetBuilder};
use serde::Deserialize;

use crate::{
    chord::{Chord, Outline},
    pronounce::{Phoneme, Pronunciation, PronunciationSlice, Stress},
    tree::Tree,
};

#[derive(Debug, Clone, Deserialize)]
pub struct PhoneticTheory {
    pub theory: Theory,
    pub phonology: Phonology,
}

impl PhoneticTheory {
    pub fn get_outline(&self, pronunciation: &[Phoneme]) -> anyhow::Result<Outline> {
        self.get_outline_tree(pronunciation)?
            .as_ref()
            .paths()
            .with_collect_copied()
            .next()
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "no outlines found for word {}",
                    PronunciationSlice(pronunciation)
                )
            })
    }

    pub fn get_outline_compound(&self, first: &[Phoneme], second: &[Phoneme]) -> anyhow::Result<Outline> {
        self.get_outline_tree_compound(first, second)?
            .as_ref()
            .paths()
            .with_collect_copied()
            .next()
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "no outlines found for word {} {}",
                    PronunciationSlice(first),
                    PronunciationSlice(second),
                )
            })
    }

    fn get_outline_tree(&self, pronunciation: &[Phoneme]) -> anyhow::Result<Tree<OutlinePiece>> {
        let syllables = self.phonology.syllable_tree(pronunciation);
        if syllables.is_empty() {
            return Err(anyhow::anyhow!(
                "no syllabification found for word {}",
                PronunciationSlice(pronunciation)
            ));
        }
        Ok(syllables.as_ref().multi_map(|syllable, prev_stroke| {
            self.outlines_for_syllable(prev_stroke.copied(), *syllable)
        }))
    }

    fn get_outline_tree_compound(
        &self,
        first: &[Phoneme],
        second: &[Phoneme],
    ) -> anyhow::Result<Tree<OutlinePiece>> {
        let first_syllables = self.phonology.syllable_tree(first);
        if first_syllables.is_empty() {
            return Err(anyhow::anyhow!(
                "no syllabification found for word {}",
                PronunciationSlice(first)
            ));
        }
        let second_syllables = self.phonology.syllable_tree(second);
        if second_syllables.is_empty() {
            return Err(anyhow::anyhow!(
                "no syllabification found for word {}",
                PronunciationSlice(second)
            ));
        }
        Ok(first_syllables
            .as_ref()
            .multi_map_with_graft(second_syllables.as_ref(), |syllable, prev_stroke| {
                self.outlines_for_syllable(prev_stroke.copied(), *syllable)
            }))
    }

    fn outlines_for_syllable(
        &self,
        prev: Option<OutlinePiece>,
        syllable: Syllable,
    ) -> impl IntoIterator<Item = OutlinePiece> {
        let mut next_outlines = Vec::new();
        self.prefixes_for_syllable(prev, syllable, &mut next_outlines);
        self.suffixes_for_syllable(prev, syllable, &mut next_outlines);

        next_outlines.extend(self.write_outs_for_syllable(prev, syllable));
        next_outlines
    }

    pub fn disambiguate_phonetic(
        &self,
        outline: Outline,
        pron_1: &[Phoneme],
        pron_2: &[Phoneme],
    ) -> Option<(Outline, Outline)> {
        let outlines_1 = self.get_outline_tree(pron_1).ok()?;
        let outlines_2 = self.get_outline_tree(pron_2).ok()?;
        disambiguate_iters(
            outline,
            outlines_1.as_ref().paths().with_collect_copied::<Outline>(),
            outlines_2.as_ref().paths().with_collect_copied::<Outline>(),
        )
    }

    pub fn disambiguate_spelling(
        &self,
        outline: Outline,
        spelling_1: &str,
        spelling_2: &str,
    ) -> Option<(Outline, Outline)> {
        let outlines_1 = self.spelling_options(outline.clone(), spelling_1);
        let outlines_2 = self.spelling_options(outline.clone(), spelling_2);
        disambiguate_iters(outline, outlines_1, outlines_2)
    }

    fn spelling_options(&self, outline: Outline, spelling: &str) -> Vec<Outline> {
        let mut possible_outlines = vec![outline];
        let mut next_outlines = Vec::new();
        for conflict_rule in &self.theory.spelling_conflicts {
            for outline in possible_outlines.drain(..) {
                if let Some(new_outline) = conflict_rule.apply(&outline, spelling) {
                    next_outlines.push(new_outline);
                } else {
                    next_outlines.push(outline);
                }
            }
            std::mem::swap(&mut possible_outlines, &mut next_outlines);
        }
        possible_outlines
    }

    fn prefixes_for_syllable(
        &self,
        prev: Option<OutlinePiece>,
        syllable: Syllable,
        next_outlines: &mut Vec<OutlinePiece>,
    ) {
        let linker = self.theory.get_linker(syllable);
        let st = prev.and_then(|op| op.is_prefix().then_some(op.stroke));
        let prev_skip = prev.map(|op| op.skip).unwrap_or(0);
        let prev_linker = if syllable.onset_range().is_empty() {
            prev.map(|piece| piece.linker).unwrap_or_default()
        } else {
            Chord::empty()
        };
        let (chords, skip) = self.theory.get_prefix(syllable, prev_skip);
        for &ch in chords {
            if let Some(st) = st {
                if (st & ch).is_empty() && st.before_ignore_star(ch) {
                    next_outlines.push(OutlinePiece {
                        stroke: st | ch,
                        linker,
                        replace_previous: true,
                        kind: OutlinePieceKind::Prefix,
                        skip,
                    })
                }
            } else {
                next_outlines.push(OutlinePiece {
                    stroke: ch | prev_linker, // TODO what if the linker would conflict,
                    linker,
                    replace_previous: false,
                    kind: OutlinePieceKind::Prefix,
                    skip,
                })
            }
        }
    }

    fn suffixes_for_syllable(
        &self,
        prev: Option<OutlinePiece>,
        syllable: Syllable,
        next_outlines: &mut Vec<OutlinePiece>,
    ) {
        let linker = self.theory.get_linker(syllable);
        let st = prev.map(|op| op.stroke);
        let prev_skip = prev.map(|op| op.skip).unwrap_or(0);
        let prev_linker = if syllable.onset_range().is_empty() {
            prev.map(|piece| piece.linker).unwrap_or_default()
        } else {
            Chord::empty()
        };
        let (chords, skip) = self.theory.get_suffix(syllable, prev_skip);
        for &ch in chords {
            if let Some(st) = st {
                if (st & ch).is_empty() && st.before_ignore_star(ch) {
                    next_outlines.push(OutlinePiece {
                        stroke: st | ch,
                        linker,
                        replace_previous: true,
                        kind: OutlinePieceKind::Suffix,
                        skip,
                    });
                } else {
                    // if the suffix would conflict, push it as a standalone
                    next_outlines.push(OutlinePiece {
                        stroke: ch | prev_linker, // TODO what if the linker would conflict
                        linker,
                        replace_previous: false,
                        kind: OutlinePieceKind::Suffix,
                        skip,
                    })
                }
            }
        }
    }

    fn write_outs_for_syllable(
        &self,
        prev: Option<OutlinePiece>,
        mut syllable: Syllable,
    ) -> Vec<OutlinePiece> {
        let linker = self.theory.get_linker(syllable);
        let prev_skip = prev.map(|op| op.skip).unwrap_or(0);
        syllable.skip(prev_skip);
        let st = prev.and_then(|op| op.is_prefix().then_some(op.stroke));
        let prev_linker = if syllable.onset_range().is_empty() {
            prev.map(|piece| piece.linker).unwrap_or_default()
        } else {
            Chord::empty()
        };
        let mut possible_outlines = vec![OutlinePiece {
            stroke: st.unwrap_or(prev_linker),
            linker,
            replace_previous: st.is_some(),
            kind: OutlinePieceKind::WriteOut,
            skip: 0,
        }];
        let mut next_outlines = Vec::new();
        for possible_chords in self.theory.onset_matches(syllable) {
            for outline in possible_outlines.drain(..) {
                let st = outline.stroke;
                for &ch in possible_chords {
                    if (st & ch).is_empty() && st.before_ignore_star(ch) {
                        next_outlines.push(outline.with_stroke(st | ch))
                    }
                }
            }
            std::mem::swap(&mut possible_outlines, &mut next_outlines);
        }

        for possible_chords in self.theory.vowel_matches(syllable) {
            for outline in possible_outlines.drain(..) {
                let st = outline.stroke;
                for &ch in possible_chords {
                    if (st & ch).is_empty() && st.before_ignore_star(ch) {
                        next_outlines.push(outline.with_stroke(st | ch))
                    }
                }
            }
            std::mem::swap(&mut possible_outlines, &mut next_outlines);
        }

        for possible_chords in self.theory.coda_matches(syllable) {
            for outline in possible_outlines.drain(..) {
                let st = outline.stroke;
                for &ch in possible_chords {
                    if (st & ch).is_empty() && st.before_ignore_star(ch) {
                        next_outlines.push(outline.with_stroke(st | ch))
                    }
                }
            }
            std::mem::swap(&mut possible_outlines, &mut next_outlines);
        }
        possible_outlines
    }
}

fn disambiguate_iters<T, U, I1, I2>(initial: T, left: I1, right: I2) -> Option<(T, T)>
where
    T: From<U> + PartialEq + Clone,
    I1: IntoIterator<Item = U>,
    I2: IntoIterator<Item = U>,
{
    let mut candidate_1 = initial.clone();
    let mut candidate_2 = initial.clone();
    for item in left.into_iter().zip_longest(right) {
        use itertools::EitherOrBoth;
        match item {
            EitherOrBoth::Both(left, right) => {
                candidate_1 = left.into();
                candidate_2 = right.into();
            }
            EitherOrBoth::Left(left) => {
                candidate_1 = left.into();
            }
            EitherOrBoth::Right(right) => {
                candidate_2 = right.into();
            }
        }
        if candidate_1 != candidate_2 {
            return Some((candidate_1, candidate_2));
        }
    }
    None
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct OutlinePiece {
    stroke: Chord,
    linker: Chord,
    replace_previous: bool,
    kind: OutlinePieceKind,
    skip: usize,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum OutlinePieceKind {
    Prefix,
    Suffix,
    WriteOut,
}

impl OutlinePiece {
    fn is_prefix(self) -> bool {
        self.kind == OutlinePieceKind::Prefix
    }

    fn with_stroke(self, stroke: Chord) -> Self {
        Self { stroke, ..self }
    }
}

impl FromIterator<OutlinePiece> for Outline {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = OutlinePiece>,
    {
        let mut outline = Vec::new();
        for piece in iter {
            if piece.replace_previous {
                *outline
                    .last_mut()
                    .expect("replace_previous set for initial stroke") = piece.stroke;
            } else {
                outline.push(piece.stroke)
            }
        }
        outline.into()
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Theory {
    onsets: PronunciationMap<OutputRules>,
    vowels: PronunciationMap<OutputRules>,
    codas: PronunciationMap<OutputRules>,
    #[serde(default)]
    linkers: BTreeMap<Pronunciation, Chord>,
    #[serde(default)]
    prefixes: PronunciationMap<Rc<[SyllableRule]>>,
    #[serde(default)]
    suffixes: PronunciationMap<Rc<[SyllableRule]>>,
    #[serde(default)]
    spelling_conflicts: Vec<SpellingConflict>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(from = "BTreeMap<Pronunciation, V>")]
struct PronunciationMap<V> {
    map: BTreeMap<Pronunciation, V>,
    min_key_len: usize,
    max_key_len: usize,
}

impl<V> Default for PronunciationMap<V> {
    fn default() -> Self {
        Self {
            map: Default::default(),
            min_key_len: 0,
            max_key_len: 0,
        }
    }
}

impl<V> From<BTreeMap<Pronunciation, V>> for PronunciationMap<V> {
    fn from(map: BTreeMap<Pronunciation, V>) -> Self {
        let (min_key_len, max_key_len) = map
            .keys()
            .map(|k| k.len())
            .minmax()
            .into_option()
            .unwrap_or((0, 0));
        Self {
            map,
            min_key_len,
            max_key_len,
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
enum OutputRules {
    Simple(Rc<[Chord]>),
    Complex(Rc<[OutputRule]>),
}

#[derive(Debug, Clone, Deserialize)]
struct OutputRule {
    prev: Option<Pronunciation>,
    prev_broad: Option<Pronunciation>,
    next: Option<Pronunciation>,
    next_broad: Option<Pronunciation>,
    stress: Option<EnumSet<Stress>>,
    chords: Rc<[Chord]>,
}

impl OutputRule {
    fn matches_at(&self, syllable: Syllable<'_>, at: Range<usize>) -> bool {
        [
            Self::matches_stress,
            Self::matches_prev,
            Self::matches_next,
            Self::matches_prev_broad,
            Self::matches_next_broad,
        ]
        .into_iter()
        .all(|f| f(self, syllable, at.clone()).unwrap_or(true))
    }

    fn matches_stress(&self, syllable: Syllable<'_>, _at: Range<usize>) -> Option<bool> {
        self.stress.map(|st| st.contains(syllable.indices.stress))
    }

    fn matches_prev(&self, syllable: Syllable<'_>, at: Range<usize>) -> Option<bool> {
        self.prev
            .as_ref()
            .map(|p| syllable.until_index(at.start).ends_with(p))
    }

    fn matches_next(&self, syllable: Syllable<'_>, at: Range<usize>) -> Option<bool> {
        self.next
            .as_ref()
            .map(|p| syllable.after_index(at.end).starts_with(p))
    }

    fn matches_prev_broad(&self, syllable: Syllable<'_>, at: Range<usize>) -> Option<bool> {
        self.prev_broad
            .as_ref()
            .map(|p| syllable.word[..at.start].ends_with(p))
    }

    fn matches_next_broad(&self, syllable: Syllable<'_>, at: Range<usize>) -> Option<bool> {
        self.next_broad
            .as_ref()
            .map(|p| syllable.word[at.end..].starts_with(p))
    }
}

#[derive(Debug, Clone, Deserialize)]
struct SyllableRule {
    prev: Option<Pronunciation>,
    next: Option<Pronunciation>,
    take_next: Option<Pronunciation>,
    stress: Option<EnumSet<Stress>>,
    position: Option<EnumSet<SyllablePosition>>,
    chords: Rc<[Chord]>,
}

impl SyllableRule {
    fn matches(&self, syllable: Syllable<'_>) -> bool {
        [
            Self::matches_stress,
            Self::matches_position,
            Self::matches_prev,
            Self::matches_next,
            Self::matches_take_next,
        ]
        .into_iter()
        .all(|f| f(self, syllable).unwrap_or(true))
    }

    fn matches_stress(&self, syllable: Syllable<'_>) -> Option<bool> {
        self.stress.map(|st| st.contains(syllable.indices.stress))
    }

    fn matches_position(&self, syllable: Syllable<'_>) -> Option<bool> {
        self.position.map(|pos| pos.contains(syllable.position()))
    }

    fn matches_prev(&self, syllable: Syllable<'_>) -> Option<bool> {
        self.prev
            .as_ref()
            .map(|p| syllable.word[..syllable.indices.onset].ends_with(p))
    }

    fn matches_next(&self, syllable: Syllable<'_>) -> Option<bool> {
        self.next
            .as_ref()
            .map(|p| syllable.word[syllable.end..].starts_with(p))
    }

    fn matches_take_next(&self, syllable: Syllable<'_>) -> Option<bool> {
        self.take_next
            .as_ref()
            .map(|p| syllable.word[syllable.end..].starts_with(p))
    }

    fn chords_and_skip(&self) -> (&[Chord], usize) {
        (&self.chords, self.take_next.as_ref().map_or(0, |s| s.len()))
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(try_from = "RawSpellingConflict")]
struct SpellingConflict {
    chord: Chord,
    pattern: RegexSet,
    replacements: Rc<[Chord]>,
}

impl SpellingConflict {
    fn apply(&self, outline: &[Chord], word: &str) -> Option<Outline> {
        let (idx, replace) = outline
            .iter()
            .enumerate()
            .find_map(|(idx, &st)| Some((idx, self.try_apply(st, word)?)))?;
        let mut outline = outline.to_owned();
        outline[idx] = replace;
        Some(outline.into())
    }

    fn try_apply(&self, stroke: Chord, word: &str) -> Option<Chord> {
        if !stroke.contains(self.chord) {
            return None;
        }
        let idx = self.pattern.matches(word).iter().next()?;
        stroke.try_replace(self.chord, self.replacements[idx])
    }
}

#[derive(Deserialize)]
struct RawSpellingConflict {
    chord: Chord,
    rules: Box<[RawSpellingRule]>,
}

#[derive(Deserialize)]
struct RawSpellingRule {
    pattern: Box<str>,
    replace: Chord,
}

impl TryFrom<RawSpellingConflict> for SpellingConflict {
    type Error = regex::Error;

    fn try_from(raw: RawSpellingConflict) -> Result<Self, Self::Error> {
        let pattern = RegexSetBuilder::new(raw.rules.iter().map(|rule| &rule.pattern)).build()?;
        let replacements = raw.rules.iter().map(|rule| rule.replace).collect();
        Ok(Self {
            chord: raw.chord,
            pattern,
            replacements,
        })
    }
}

impl Theory {
    fn onset_matches<'t, 'w>(&'t self, syllable: Syllable<'w>) -> MatchChordIter<'t, 'w> {
        MatchChordIter {
            map: &self.onsets,
            range: syllable.onset_range(),
            syllable,
        }
    }

    fn vowel_matches<'t, 'w>(&'t self, syllable: Syllable<'w>) -> MatchChordIter<'t, 'w> {
        MatchChordIter {
            map: &self.vowels,
            range: syllable.vowel_range(),
            syllable,
        }
    }

    fn coda_matches<'t, 'w>(&'t self, syllable: Syllable<'w>) -> MatchChordIter<'t, 'w> {
        MatchChordIter {
            map: &self.codas,
            range: syllable.coda_range(),
            syllable,
        }
    }

    fn get_prefix(&self, syllable: Syllable, skip: usize) -> (&[Chord], usize) {
        syllable
            .as_slice()
            .get(skip..)
            .and_then(|slice| self.prefixes.map.get(slice))
            .into_iter()
            .flat_map(|rules| &**rules)
            .find(|rule| rule.matches(syllable))
            .map(SyllableRule::chords_and_skip)
            .unwrap_or((&[], 0))
    }

    fn get_suffix(&self, syllable: Syllable, skip: usize) -> (&[Chord], usize) {
        syllable
            .as_slice()
            .get(skip..)
            .and_then(|slice| self.suffixes.map.get(slice))
            .into_iter()
            .flat_map(|rules| &**rules)
            .find(|rule| rule.matches(syllable))
            .map(SyllableRule::chords_and_skip)
            .unwrap_or((&[], 0))
    }

    fn get_linker(&self, syllable: Syllable) -> Chord {
        if syllable.coda_range().is_empty() {
            self.linkers
                .get(syllable.vowel())
                .copied()
                .unwrap_or_default()
        } else {
            Chord::empty()
        }
    }
}

struct MatchChordIter<'t, 'w> {
    map: &'t PronunciationMap<OutputRules>,
    syllable: Syllable<'w>,
    range: Range<usize>,
}

impl<'t, 'w> Iterator for MatchChordIter<'t, 'w> {
    type Item = &'t [Chord];

    fn next(&mut self) -> Option<Self::Item> {
        if self.range.is_empty() {
            return None;
        }
        let start = self.range.start;
        let max_len = self.map.max_key_len.min(self.range.end - self.range.start);
        // check prefixes in descending order
        for len in (self.map.min_key_len..=max_len).rev() {
            let slice = &self.syllable.word[start..start + len];
            let chords = match self.map.map.get(slice) {
                Some(OutputRules::Simple(chords)) => chords,
                Some(OutputRules::Complex(rules)) => {
                    if let Some(chords) = rules
                        .iter()
                        .find(|rule| rule.matches_at(self.syllable, start..start + len))
                        .map(|rule| &*rule.chords)
                    {
                        chords
                    } else {
                        continue;
                    }
                }
                None => continue,
            };
            self.range.start += len;
            return Some(chords);
        }
        // no matches
        self.range.start += 1;
        Some(&[])
    }
}

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
enum SyllablePosition {
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

    fn syllable_tree<'w>(&self, word: &'w [Phoneme]) -> Tree<Syllable<'w>> {
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
    fn onset_range(&self) -> Range<usize> {
        self.indices.onset..self.indices.vowel
    }

    fn vowel_range(&self) -> Range<usize> {
        self.indices.vowel..self.indices.coda
    }

    fn vowel(&self) -> &'w [Phoneme] {
        &self.word[self.vowel_range()]
    }

    fn coda_range(&self) -> Range<usize> {
        self.indices.coda..self.end
    }

    pub fn as_slice(&self) -> &'w [Phoneme] {
        &self.word[self.indices.onset..self.end]
    }

    /// Skips the first `skip` phonemes of the syllable
    fn skip(&mut self, skip: usize) {
        self.indices.onset += skip;
        self.indices.vowel = self.indices.vowel.max(self.indices.onset);
        self.indices.coda = self.indices.coda.max(self.indices.onset);
        self.end = self.end.max(self.indices.onset);
    }

    /// Returns the part of the syllable up to the specified index into the word.
    fn until_index(&self, index: usize) -> &'w [Phoneme] {
        &self.word[self.indices.onset..index]
    }

    /// Returns the part of the syllable after the specified index into the word.
    fn after_index(&self, index: usize) -> &'w [Phoneme] {
        &self.word[index..self.end]
    }

    fn position(&self) -> SyllablePosition {
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

    use super::{PhoneticTheory, Pronunciation};
    use crate::chord::Outline;

    #[test]
    fn load_theory() -> anyhow::Result<()> {
        let _theory: PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        Ok(())
    }

    #[test_case("S N UW1 T", &["S N UW1 T"] ; "snoot")]
    #[test_case("S EY1 IH0 NG", &["S EY1/IH0 NG"] ; "saying")]
    #[test_case("B AE1 K Y AA2 R D", &["B AE1 K/Y AA2 R D"] ; "backyard")]
    #[test_case("K Y UW1 B", &["K Y UW1 B"] ; "cube")]
    #[test_case("IH0 K S P EH2 N D", &["IH0 K S/P EH2 N D", "IH0 K/S P EH2 N D"]; "expend")]
    #[test_case("IH0 K S CH EY2 N JH", &["IH0 K S/CH EY2 N JH", "IH0 K S/CH EY2 N JH"] ; "exchange")]
    #[test_case("D IH1 S T AH0 N T", &["D IH1 S/T AH0 N T", "D IH1/S T AH0 N T"] ; "distant")]
    #[test_case("L AH1 V AH0 B AH0 L", &[
        "L AH1 V/AH0 B AH0 L",
        "L AH1 V/AH0/B AH0 L",
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

    #[test_case("EY1", "AEU" ; "a")]
    #[test_case("AO1 L", "AUL" ; "all")]
    #[test_case("Y AH1 NG", "KWRUPBG" ; "young")]
    #[test_case("EH1 M Y AH0 L EY2 T", "E/PHAOU/HRAEUT" ; "emulate")]
    #[test_case("M AE1 R IY0", "PHE/RAOE" ; "marry")]
    #[test_case("IH0 K S P EH2 N D", "KPEPBD" ; "expend")]
    #[test_case("IH0 K S CH EY2 N JH", "KPHAEUFPBG" ; "exchange")]
    #[test_case("D IH1 S T AH0 N T", "STKUPBT" ; "distant")]
    #[test_case("AE1 K SH AH0 N", "ABGS" ; "action")]
    #[test_case("G AH1 M P SH AH0 N", "TKPWUFRPGS" ; "gumption")]
    #[test_case("K AA1 N SH AH0 S", "K-RBS" ; "conscious")]
    #[test_case("D R AO1 IH0 NG", "TKRO/WEUPBG" ; "drawing")]
    fn word_to_outline(pronunciation: &str, expected_outline: &str) -> anyhow::Result<()> {
        let expected_outline = expected_outline.parse::<Outline>()?;
        let theory: PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        let outlines = theory.get_outline_tree(&Pronunciation::from(pronunciation))?;
        let outline_tree = outlines.as_ref().map(|piece| {
            if piece.replace_previous {
                format!("(-){}", piece.stroke)
            } else {
                format!("{}", piece.stroke)
            }
        });
        eprintln!("outlines: {outline_tree:?}");
        let actual_outline = outlines
            .as_ref()
            .paths()
            .with_collect_copied::<Outline>()
            .next()
            .expect("no outlines");
        assert_eq!(actual_outline, expected_outline);
        Ok(())
    }
}
