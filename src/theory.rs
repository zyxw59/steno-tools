use std::{collections::BTreeMap, ops::Range, rc::Rc};

use enumset::EnumSet;
use itertools::Itertools;
use regex::{RegexSet, RegexSetBuilder};
use serde::Deserialize;

use crate::{
    chord::{Chord, Outline},
    phonology::{Phonology, Syllable, SyllablePosition},
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
        outline_tree_iter(&self.get_outline_tree(pronunciation)?)
            .next()
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "no outlines found for word {}",
                    PronunciationSlice(pronunciation)
                )
            })
    }

    pub fn get_outline_compound(
        &self,
        first: &[Phoneme],
        second: &[Phoneme],
    ) -> anyhow::Result<Outline> {
        outline_tree_iter(&self.get_outline_tree_compound(first, second)?)
            .next()
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "no outlines found for word {} {}",
                    PronunciationSlice(first),
                    PronunciationSlice(second),
                )
            })
    }

    pub fn get_outlines_vec(&self, pronunciation: &[Phoneme]) -> anyhow::Result<Vec<Outline>> {
        Ok(outline_tree_iter(&self.get_outline_tree(pronunciation)?).collect())
    }

    fn get_outline_tree(&self, pronunciation: &[Phoneme]) -> anyhow::Result<Tree<OutlinePiece>> {
        let syllables = self.phonology.syllable_tree(pronunciation);
        #[cfg(test)]
        eprintln!("syllables: {syllables:#?}");
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
        let prev_skip = prev.map(|op| op.skip).unwrap_or(0);
        let syllable = syllable.skip(prev_skip);

        let mut next_outlines = Vec::new();
        next_outlines.extend(self.prefixes_for_syllable(prev, syllable));
        next_outlines.extend(self.suffixes_for_syllable(prev, syllable));

        let mut write_out_outlines = vec![self.initial_outline(prev, syllable)];
        self.write_outs_for_syllable(syllable, &mut write_out_outlines);
        next_outlines.extend(write_out_outlines);
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

    pub fn spelling_options(&self, outline: Outline, spelling: &str) -> Vec<Outline> {
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
    ) -> Vec<OutlinePiece> {
        let mut next_outlines = Vec::new();
        let linker = self.theory.get_linker(syllable);
        let st = prev.and_then(|op| op.is_prefix().then_some(op.stroke));
        let prev_linker = if syllable.onset_range().is_empty() {
            prev.map(|piece| piece.linker).unwrap_or_default()
        } else {
            Chord::empty()
        };
        let SyllableOutput { chords, skip, end } = self.theory.get_prefix(syllable);
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
        if end < syllable.end() {
            let syllable = syllable.skip(end - syllable.start());
            self.write_outs_for_syllable(syllable, &mut next_outlines);
        }
        next_outlines
    }

    fn suffixes_for_syllable(
        &self,
        prev: Option<OutlinePiece>,
        syllable: Syllable,
    ) -> Vec<OutlinePiece> {
        let mut next_outlines = Vec::new();
        let linker = self.theory.get_linker(syllable);
        let st = prev.map(|op| op.stroke);
        let prev_linker = if syllable.onset_range().is_empty() {
            prev.map(|piece| piece.linker).unwrap_or_default()
        } else {
            Chord::empty()
        };
        let SyllableOutput { chords, skip, end } = self.theory.get_suffix(syllable);
        for &ch in chords {
            match st {
                Some(st) if (st & ch).is_empty() && st.before_ignore_star(ch) => {
                    next_outlines.push(OutlinePiece {
                        stroke: st | ch,
                        linker,
                        replace_previous: true,
                        kind: OutlinePieceKind::Suffix,
                        skip,
                    });
                }
                _ => {
                    // if the suffix would conflict, or there's no preceding syllable, push it as a
                    // standalone
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
        if end < syllable.end() {
            let syllable = syllable.skip(end - syllable.start());
            self.write_outs_for_syllable(syllable, &mut next_outlines);
        }
        next_outlines
    }

    fn initial_outline(&self, prev: Option<OutlinePiece>, syllable: Syllable) -> OutlinePiece {
        let linker = self.theory.get_linker(syllable);
        let st = prev.and_then(|op| op.is_prefix().then_some(op.stroke));
        let prev_linker = if syllable.onset_range().is_empty() {
            prev.map(|piece| piece.linker).unwrap_or_default()
        } else {
            Chord::empty()
        };
        OutlinePiece {
            stroke: st.unwrap_or(prev_linker),
            linker,
            replace_previous: st.is_some(),
            kind: OutlinePieceKind::WriteOut,
            skip: 0,
        }
    }

    fn write_outs_for_syllable(
        &self,
        syllable: Syllable,
        possible_outlines: &mut Vec<OutlinePiece>,
    ) {
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
            std::mem::swap(possible_outlines, &mut next_outlines);
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
            std::mem::swap(possible_outlines, &mut next_outlines);
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
            std::mem::swap(possible_outlines, &mut next_outlines);
        }
    }
}

fn outline_tree_iter(outlines: &Tree<OutlinePiece>) -> impl Iterator<Item = Outline> + '_ {
    outlines.as_ref().paths().with_collect_copied()
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

impl RuleIter for OutputRules {
    type Output<'t> = &'t [Chord];

    fn matches_at<'t>(
        &'t self,
        syllable: Syllable,
        range: Range<usize>,
    ) -> Option<Self::Output<'t>> {
        match self {
            Self::Simple(chords) => Some(chords),
            Self::Complex(rules) => rules
                .iter()
                .find(|rule| rule.matches_at(syllable, range.clone()))
                .map(|rule| &*rule.chords),
        }
    }

    fn default_output<'t>() -> Self::Output<'t> {
        &[]
    }
}

#[derive(Debug, Clone, Deserialize)]
struct OutputRule {
    prev: Option<Pronunciation>,
    prev_broad: Option<Pronunciation>,
    next: Option<Pronunciation>,
    next_broad: Option<Pronunciation>,
    stress: Option<EnumSet<Stress>>,
    position: Option<EnumSet<SyllablePosition>>,
    chords: Rc<[Chord]>,
}

impl OutputRule {
    fn matches_at(&self, syllable: Syllable<'_>, at: Range<usize>) -> bool {
        [
            Self::matches_stress,
            Self::matches_position,
            Self::matches_prev,
            Self::matches_next,
            Self::matches_prev_broad,
            Self::matches_next_broad,
        ]
        .into_iter()
        .all(|f| f(self, syllable, at.clone()).unwrap_or(true))
    }

    fn matches_stress(&self, syllable: Syllable<'_>, _at: Range<usize>) -> Option<bool> {
        self.stress.map(|st| st.contains(syllable.stress()))
    }

    fn matches_position(&self, syllable: Syllable<'_>, _at: Range<usize>) -> Option<bool> {
        self.position.map(|pos| pos.contains(syllable.position()))
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
            .map(|p| syllable.word()[..at.start].ends_with(p))
    }

    fn matches_next_broad(&self, syllable: Syllable<'_>, at: Range<usize>) -> Option<bool> {
        self.next_broad
            .as_ref()
            .map(|p| syllable.word()[at.end..].starts_with(p))
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

impl RuleIter for Rc<[SyllableRule]> {
    type Output<'t> = (&'t [Chord], usize);

    fn matches_at<'t>(
        &'t self,
        syllable: Syllable,
        range: Range<usize>,
    ) -> Option<Self::Output<'t>> {
        self.iter()
            .find(|rule| rule.matches(syllable, range.clone()))
            .map(SyllableRule::chords_and_skip)
    }

    fn default_output<'t>() -> Self::Output<'t> {
        (&[], 0)
    }
}

impl SyllableRule {
    fn matches(&self, syllable: Syllable<'_>, range: Range<usize>) -> bool {
        [
            Self::matches_stress,
            Self::matches_position,
            Self::matches_prev,
            Self::matches_next,
            Self::matches_take_next,
        ]
        .into_iter()
        .all(|f| f(self, syllable, range.clone()).unwrap_or(true))
    }

    fn matches_stress(&self, syllable: Syllable<'_>, _range: Range<usize>) -> Option<bool> {
        self.stress.map(|st| st.contains(syllable.stress()))
    }

    fn matches_position(&self, syllable: Syllable<'_>, _range: Range<usize>) -> Option<bool> {
        self.position.map(|pos| pos.contains(syllable.position()))
    }

    fn matches_prev(&self, syllable: Syllable<'_>, range: Range<usize>) -> Option<bool> {
        self.prev
            .as_ref()
            .map(|p| syllable.word()[..range.start].ends_with(p))
    }

    fn matches_next(&self, syllable: Syllable<'_>, range: Range<usize>) -> Option<bool> {
        self.next
            .as_ref()
            .map(|p| syllable.word()[range.end..].starts_with(p))
    }

    fn matches_take_next(&self, syllable: Syllable<'_>, range: Range<usize>) -> Option<bool> {
        self.take_next
            .as_ref()
            .map(|p| syllable.word()[range.end..].starts_with(p))
    }

    fn chords_and_skip(&self) -> (&[Chord], usize) {
        (&self.chords, self.take_next.as_ref().map_or(0, |s| s.len()))
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(try_from = "RawSpellingConflict")]
struct SpellingConflict {
    chord: Chord,
    exclude: Chord,
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
        if !(stroke & self.exclude).is_empty() {
            return None;
        }
        let idx = self.pattern.matches(word).iter().next()?;
        stroke.try_replace(self.chord, self.replacements[idx])
    }
}

#[derive(Deserialize)]
struct RawSpellingConflict {
    chord: Chord,
    #[serde(default)]
    exclude: Chord,
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
            exclude: raw.exclude,
            pattern,
            replacements,
        })
    }
}

impl Theory {
    fn onset_matches<'t, 'w>(
        &'t self,
        syllable: Syllable<'w>,
    ) -> PronunciationMapIter<'t, 'w, OutputRules> {
        PronunciationMapIter {
            map: &self.onsets,
            range: syllable.onset_range(),
            syllable,
        }
    }

    fn vowel_matches<'t, 'w>(
        &'t self,
        syllable: Syllable<'w>,
    ) -> PronunciationMapIter<'t, 'w, OutputRules> {
        PronunciationMapIter {
            map: &self.vowels,
            range: syllable.vowel_range(),
            syllable,
        }
    }

    fn coda_matches<'t, 'w>(
        &'t self,
        syllable: Syllable<'w>,
    ) -> PronunciationMapIter<'t, 'w, OutputRules> {
        PronunciationMapIter {
            map: &self.codas,
            range: syllable.coda_range(),
            syllable,
        }
    }

    fn get_prefix(&self, syllable: Syllable) -> SyllableOutput {
        let mut it = PronunciationMapIter {
            map: &self.prefixes,
            range: syllable.range(),
            syllable,
        };

        let (chords, skip) = it.next().unwrap_or((&[], 0));
        SyllableOutput {
            chords,
            skip,
            end: it.range.start,
        }
    }

    fn get_suffix(&self, syllable: Syllable) -> SyllableOutput {
        let mut it = PronunciationMapIter {
            map: &self.suffixes,
            range: syllable.range(),
            syllable,
        };

        let (chords, skip) = it.next().unwrap_or((&[], 0));
        SyllableOutput {
            chords,
            skip,
            end: it.range.start,
        }
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

struct SyllableOutput<'t> {
    chords: &'t [Chord],
    skip: usize,
    end: usize,
}

struct PronunciationMapIter<'t, 'w, T> {
    map: &'t PronunciationMap<T>,
    syllable: Syllable<'w>,
    range: Range<usize>,
}

trait RuleIter {
    type Output<'t>
    where
        Self: 't;

    fn matches_at<'t>(
        &'t self,
        syllable: Syllable,
        range: Range<usize>,
    ) -> Option<Self::Output<'t>>;

    fn default_output<'t>() -> Self::Output<'t>;
}

impl<'t, 'w, T> Iterator for PronunciationMapIter<'t, 'w, T>
where
    T: RuleIter,
{
    type Item = T::Output<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.range.is_empty() {
            return None;
        }
        let start = self.range.start;
        let max_len = self.map.max_key_len.min(self.range.end - self.range.start);
        // check prefixes in descending order
        for len in (self.map.min_key_len..=max_len).rev() {
            let range = start..start + len;
            let slice = &self.syllable.word()[range.clone()];
            let Some(chords) = self
                .map
                .map
                .get(slice)
                .and_then(|rule| rule.matches_at(self.syllable, range))
            else {
                continue;
            };
            self.range.start += len;
            return Some(chords);
        }
        // no matches
        self.range.start += 1;
        Some(T::default_output())
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::BufReader};

    use test_case::test_case;

    use super::{PhoneticTheory, Pronunciation};
    use crate::chord::Outline;

    #[test]
    fn load_theory() -> anyhow::Result<()> {
        let _theory: PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        Ok(())
    }

    #[test_case("EY1", "AEU" ; "a")]
    #[test_case("AO1 L", "AUL" ; "all")]
    #[test_case("Y AH1 NG", "KWRUPBG" ; "young")]
    #[test_case("EH1 M Y AH0 L EY2 T", "E/PHAOU/HRAEUT" ; "emulate")]
    #[test_case("M AE1 R IY0", "PHA*EURD" ; "marry")]
    #[test_case("IH0 K S P EH2 N D", "KPEPBD" ; "expend")]
    #[test_case("IH0 K S CH EY2 N JH", "KPHAEUFRPBG" ; "exchange")]
    #[test_case("D IH1 S T AH0 N T", "STK-PBT" ; "distant")]
    #[test_case("AE1 K SH AH0 N", "ABGS" ; "action")]
    #[test_case("G AH1 M P SH AH0 N", "TKPWUFRPGS" ; "gumption")]
    #[test_case("K AA1 N SH AH0 S", "K-RBS" ; "conscious")]
    #[test_case("W IH DH D R AO1 AH0 L", "W*EUT/TKROL" ; "withdrawal")]
    #[test_case("S T EY1 SH AH0 N EH2 R IY0", "STAEUGS/A*EURD" ; "stationary")]
    #[test_case("K AA1 N F AH0 D EH2 N S", "K-F/TKEPBS" ; "confidence")]
    #[test_case("P IH1 JH AH0 N Z", "PEUGSZ" ; "pigeons")]
    #[test_case("K AO1 R M AH0 N", "KOR/PHUPB" ; "corpsman")]
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
        eprintln!("outlines: {outline_tree:#?}");
        let actual_outline = outlines
            .as_ref()
            .paths()
            .with_collect_copied::<Outline>()
            .next()
            .expect("no outlines");
        assert_eq!(actual_outline, expected_outline);
        Ok(())
    }

    #[test_case("N AO1 R TH", "W EH2 S T", "TPHO*RT/WEFT" ; "northwest")]
    fn compound_word_to_outline(
        first: &str,
        second: &str,
        expected_outline: &str,
    ) -> anyhow::Result<()> {
        let expected_outline = expected_outline.parse::<Outline>()?;
        let theory: PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        let outlines = theory
            .get_outline_tree_compound(&Pronunciation::from(first), &Pronunciation::from(second))?;
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
