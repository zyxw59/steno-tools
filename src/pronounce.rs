use std::{
    cmp,
    collections::{BTreeMap, BTreeSet},
    fmt,
    io::BufRead,
    ops::{Deref, Range},
    rc::Rc,
};

use anyhow::Context;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

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

    pub fn get(&self, word: &Word) -> &[Pronunciation] {
        self.entries
            .get(&*word.to_ascii_lowercase())
            .map(|ps| &**ps)
            .unwrap_or(&[])
    }
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
        fmt::Display::fmt(self, f)
    }
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Phoneme(smol_str::SmolStr);
crate::fmt_impls!(Phoneme);

impl Phoneme {
    fn stress(&self) -> Stress {
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

#[derive(Debug, Clone, Deserialize)]
pub struct PhoneticTheory {
    pub theory: Theory,
    pub phonology: Phonology,
}

impl PhoneticTheory {
    pub fn get_outline(
        &self,
        pronunciation: &[Phoneme],
        _spelling: &str,
    ) -> anyhow::Result<Outline> {
        let mut possible_outlines = vec![OutlineBuilder::default()];
        for syllable in self.phonology.syllabize_word(pronunciation)? {
            let mut next_outlines = Vec::new();
            self.prefixes_for_syllable(syllable, &possible_outlines, &mut next_outlines);
            self.suffixes_for_syllable(syllable, &possible_outlines, &mut next_outlines);

            let res = self.write_outs_for_syllable(syllable, &mut possible_outlines);
            next_outlines.extend(possible_outlines.drain(..).map(OutlineBuilder::push_empty));
            if next_outlines.is_empty() {
                res?;
            }
            possible_outlines = next_outlines;
        }
        // if an outline has a non-empty last_stroke, that means it ends in a prefix, which is not
        // valid; skip those outlines
        let outline = possible_outlines
            .into_iter()
            .find(|outline| outline.last_stroke.is_empty())
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "no outlines found for word {}",
                    PronunciationSlice(pronunciation)
                )
            })?;
        let mut stroke_iter = outline.into_iter();
        // skip empty stroke
        stroke_iter.next();
        let stroke_rev = stroke_iter.collect::<Vec<Chord>>();
        Ok(stroke_rev.into_iter().rev().join("/").into())
    }

    fn prefixes_for_syllable(
        &self,
        syllable: Syllable,
        possible_outlines: &[OutlineBuilder],
        next_outlines: &mut Vec<OutlineBuilder>,
    ) {
        for outline in possible_outlines {
            let st = outline.last_stroke;
            let (chords, skip) = self.theory.get_prefix(syllable, outline.skip);
            for &ch in chords {
                if (st & ch).is_empty() & st.before_ignore_star(ch) {
                    next_outlines.push(outline.with_stroke_and_skip(st | ch, skip))
                }
            }
        }
    }

    fn suffixes_for_syllable(
        &self,
        syllable: Syllable,
        possible_outlines: &[OutlineBuilder],
        next_outlines: &mut Vec<OutlineBuilder>,
    ) {
        for mut outline in possible_outlines {
            // skip over empty outlines
            if outline.last_stroke.is_empty() {
                if let Some(prev) = outline.rest.as_deref() {
                    outline = prev;
                } else {
                    // if the only outline is empty, it means we're at the start of the word
                    continue;
                }
            }
            let st = outline.last_stroke;
            let (chords, skip) = self.theory.get_suffix(syllable, outline.skip);
            for &ch in chords {
                if (st & ch).is_empty() & st.before_ignore_star(ch) {
                    next_outlines.push(outline.with_stroke(st | ch).push_empty_with_skip(skip));
                }
            }
        }
    }

    fn write_outs_for_syllable(
        &self,
        syllable: Syllable,
        possible_outlines: &mut Vec<OutlineBuilder>,
    ) -> anyhow::Result<()> {
        let mut next_outlines = Vec::new();
        for possible_chords in self.theory.onset_matches(syllable) {
            for outline in possible_outlines.drain(..) {
                if outline.skip > 0 {
                    next_outlines.push(outline.decrement_skip());
                    continue;
                }
                let st = outline.last_stroke;
                for &ch in possible_chords {
                    if (st & ch).is_empty() & st.before_ignore_star(ch) {
                        next_outlines.push(outline.with_stroke(st | ch))
                    }
                }
            }
            std::mem::swap(possible_outlines, &mut next_outlines);
        }
        if possible_outlines.is_empty() {
            return Err(anyhow::anyhow!(
                "no strokes for onset {}",
                PronunciationSlice(syllable.onset())
            ));
        }
        for possible_chords in self.theory.vowel_matches(syllable) {
            for outline in possible_outlines.drain(..) {
                if outline.skip > 0 {
                    next_outlines.push(outline.decrement_skip());
                    continue;
                }
                let st = outline.last_stroke;
                for &ch in possible_chords {
                    if (st & ch).is_empty() & st.before_ignore_star(ch) {
                        next_outlines.push(outline.with_stroke(st | ch))
                    }
                }
            }
            std::mem::swap(possible_outlines, &mut next_outlines);
        }
        if possible_outlines.is_empty() {
            return Err(anyhow::anyhow!(
                "no strokes for vowel {}",
                PronunciationSlice(syllable.vowel())
            ));
        }
        for possible_chords in self.theory.coda_matches(syllable) {
            for outline in possible_outlines.drain(..) {
                if outline.skip > 0 {
                    next_outlines.push(outline.decrement_skip());
                    continue;
                }
                let st = outline.last_stroke;
                for &ch in possible_chords {
                    if (st & ch).is_empty() & st.before_ignore_star(ch) {
                        next_outlines.push(outline.with_stroke(st | ch))
                    }
                }
            }
            std::mem::swap(possible_outlines, &mut next_outlines);
        }
        if possible_outlines.is_empty() {
            return Err(anyhow::anyhow!(
                "no strokes for coda {}",
                PronunciationSlice(syllable.coda())
            ));
        }
        Ok(())
    }
}

struct OutlineBuilder {
    last_stroke: Chord,
    rest: Option<Rc<OutlineBuilder>>,
    skip: usize,
    len: usize,
}

impl OutlineBuilder {
    fn with_stroke(&self, new_stroke: Chord) -> Self {
        OutlineBuilder {
            last_stroke: new_stroke,
            rest: self.rest.clone(),
            skip: 0,
            len: self.len,
        }
    }

    fn with_stroke_and_skip(&self, new_stroke: Chord, skip: usize) -> Self {
        OutlineBuilder {
            last_stroke: new_stroke,
            rest: self.rest.clone(),
            skip,
            len: self.len,
        }
    }

    fn decrement_skip(self) -> Self {
        Self {
            skip: self.skip - 1,
            ..self
        }
    }

    fn push_empty(self) -> Self {
        self.push_empty_with_skip(0)
    }

    fn push_empty_with_skip(self, skip: usize) -> Self {
        if self.last_stroke.is_empty() {
            Self {
                skip: self.skip + skip,
                ..self
            }
        } else {
            Self {
                last_stroke: Chord::empty(),
                len: self.len + 1,
                skip,
                rest: Some(Rc::new(self)),
            }
        }
    }
}

impl IntoIterator for OutlineBuilder {
    type IntoIter = OutlineBuilderIter;

    type Item = Chord;

    fn into_iter(self) -> Self::IntoIter {
        OutlineBuilderIter {
            outline: Some(Rc::new(self)),
        }
    }
}

impl Default for OutlineBuilder {
    fn default() -> Self {
        Self {
            last_stroke: Chord::empty(),
            rest: None,
            skip: 0,
            len: 1,
        }
    }
}

struct OutlineBuilderIter {
    outline: Option<Rc<OutlineBuilder>>,
}

impl Iterator for OutlineBuilderIter {
    type Item = Chord;

    fn next(&mut self) -> Option<Self::Item> {
        let outline = self.outline.take()?;
        let item = outline.last_stroke;
        self.outline = outline.rest.clone();
        Some(item)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl ExactSizeIterator for OutlineBuilderIter {
    fn len(&self) -> usize {
        self.outline.as_ref().map_or(0, |outline| outline.len)
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Theory {
    onsets: PronunciationMap<OutputRules>,
    vowels: PronunciationMap<OutputRules>,
    codas: PronunciationMap<OutputRules>,
    #[serde(default)]
    prefixes: PronunciationMap<Rc<[SyllableRule]>>,
    #[serde(default)]
    suffixes: PronunciationMap<Rc<[SyllableRule]>>,
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
    stress: Option<Stress>,
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
        self.stress.map(|st| st == syllable.indices.stress)
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
    stress: Option<Stress>,
    chords: Rc<[Chord]>,
}

impl SyllableRule {
    fn matches(&self, syllable: Syllable<'_>) -> bool {
        [
            Self::matches_stress,
            Self::matches_prev,
            Self::matches_next,
            Self::matches_take_next,
        ]
        .into_iter()
        .all(|f| f(self, syllable).unwrap_or(true))
    }

    fn matches_stress(&self, syllable: Syllable<'_>) -> Option<bool> {
        self.stress.map(|st| st == syllable.indices.stress)
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
pub struct RawPhonology {
    onset_singles: BTreeSet<Phoneme>,
    onset_clusters: Vec<OnsetCluster>,
    vowels: BTreeSet<Phoneme>,
}

#[derive(Debug, Clone, Deserialize)]
struct OnsetCluster {
    first: BTreeSet<Phoneme>,
    second: BTreeSet<Phoneme>,
}

impl From<RawPhonology> for Phonology {
    fn from(raw: RawPhonology) -> Self {
        // map (second_consonant -> [first_consonants])
        let mut onset_clusters = BTreeMap::<_, BTreeSet<_>>::new();
        for cluster in raw.onset_clusters {
            for ph in cluster.second {
                onset_clusters
                    .entry(ph)
                    .or_default()
                    .extend(cluster.first.iter().cloned());
            }
        }
        Phonology {
            onset_singles: raw.onset_singles,
            onset_clusters,
            vowels: raw.vowels,
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(try_from = "RawPhonology")]
pub struct Phonology {
    onset_singles: BTreeSet<Phoneme>,
    onset_clusters: BTreeMap<Phoneme, BTreeSet<Phoneme>>,
    vowels: BTreeSet<Phoneme>,
}

impl Phonology {
    #[allow(unused)]
    pub fn syllabize_word<'p, 'w>(
        &'p self,
        word: &'w [Phoneme],
    ) -> anyhow::Result<SyllableIterator<'p, 'w>> {
        let prev_syllable = self.syllabize_one(word, 0);
        if prev_syllable.onset != 0 {
            return Err(anyhow::anyhow!(
                "invalid onset: {}",
                PronunciationSlice(&word[..prev_syllable.vowel])
            ));
        }

        Ok(SyllableIterator {
            phonology: self,
            word,
            prev_syllable,
        })
    }

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
        // work backwards to find the maximally valid onset
        let mut allowed_phonemes = &self.onset_singles;
        let empty_set = BTreeSet::new();
        let onset = word[..vowel]
            .iter()
            .rposition(|ph| {
                if !allowed_phonemes.contains(ph) {
                    return true;
                }
                allowed_phonemes = self.onset_clusters.get(ph).unwrap_or(&empty_set);
                false
            })
            .map(|idx| idx + 1)
            .unwrap_or(0);
        SyllableIndices {
            stress,
            onset,
            vowel,
            coda,
        }
    }
}

/// Start indices of the onset, vowel, and coda of a syllable
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct SyllableIndices {
    stress: Stress,
    onset: usize,
    vowel: usize,
    coda: usize,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Syllable<'w> {
    word: &'w [Phoneme],
    indices: SyllableIndices,
    end: usize,
}

impl<'w> Syllable<'w> {
    pub fn onset(&self) -> &'w [Phoneme] {
        &self.word[self.onset_range()]
    }

    fn onset_range(&self) -> Range<usize> {
        self.indices.onset..self.indices.vowel
    }

    pub fn vowel(&self) -> &'w [Phoneme] {
        &self.word[self.vowel_range()]
    }

    fn vowel_range(&self) -> Range<usize> {
        self.indices.vowel..self.indices.coda
    }

    pub fn coda(&self) -> &'w [Phoneme] {
        &self.word[self.coda_range()]
    }

    fn coda_range(&self) -> Range<usize> {
        self.indices.coda..self.end
    }

    pub fn as_slice(&self) -> &'w [Phoneme] {
        &self.word[self.indices.onset..self.end]
    }

    /// Returns the part of the syllable up to the specified index into the word.
    fn until_index(&self, index: usize) -> &'w [Phoneme] {
        &self.word[self.indices.onset..index]
    }

    /// Returns the part of the syllable after the specified index into the word.
    fn after_index(&self, index: usize) -> &'w [Phoneme] {
        &self.word[index..self.end]
    }
}

impl fmt::Display for Syllable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&PronunciationSlice(self.as_slice()), f)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum Stress {
    None,
    Secondary,
    Primary,
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

    use test_case::test_case;

    use super::{PhoneticTheory, Pronunciation};

    #[test]
    fn load_theory() -> anyhow::Result<()> {
        let _theory: PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        Ok(())
    }

    #[test_case("S P UH T", &["S P UH T"] ; "one syllable")]
    #[test_case("EH P S AO L", &["EH P", "S AO L"] ; "vowel initial")]
    #[test_case("T AE L S P R AO T", &["T AE L", "S P R AO T"] ; "complex onset")]
    #[test_case("CH IH CH R EH K", &["CH IH CH", "R EH K"] ; "longer consonants")]
    #[test_case("T EY IH S", &["T EY", "EY IH S"] ; "consecutive vowels as linkers")]
    fn syllabification(word: &str, expected_syllables: &[&str]) -> anyhow::Result<()> {
        let theory: PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        let phonology = theory.phonology;
        let actual_syllables = phonology
            .syllabize_word(&Pronunciation::from(word))?
            .map(|s| s.to_string())
            .collect::<Vec<_>>();

        assert_eq!(actual_syllables, expected_syllables);
        Ok(())
    }

    #[test_case("a", "EY1", "AEU" ; "a")]
    #[test_case("all", "AO1 L", "AUL" ; "all")]
    #[test_case("young", "Y AH1 NG", "KWRUPBG" ; "young")]
    #[test_case("emulate", "EH1 M Y AH0 L EY2 T", "E/PHAOU/HRAEUT" ; "emulate")]
    #[test_case("marry", "M AE1 R IY0", "PHE/RAOE" ; "marry")]
    #[test_case("expend", "IH0 K S P EH2 N D", "KPEPBD" ; "expend")]
    #[test_case("exchange", "IH0 K S CH EY2 N JH", "KPHAEUFPBG" ; "exchange")]
    #[test_case("action", "AE1 K SH AH0 N", "ABGS" ; "action")]
    #[test_case("gumption", "G AH1 M P SH AH0 N", "TKPW*UPLGS" ; "gumption")]
    #[test_case("conscious", "K AA1 N SH AH0 S", "K-RBS" ; "conscious")]
    #[test_case("drawing", "D R AO1 IH0 NG", "TKRO/WEUPBG" ; "drawing")]
    fn word_to_outline(
        spelling: &str,
        pronunciation: &str,
        expected_outline: &str,
    ) -> anyhow::Result<()> {
        let theory: PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        let actual_outline = theory.get_outline(&Pronunciation::from(pronunciation), spelling)?;
        assert_eq!(&*actual_outline, expected_outline);
        Ok(())
    }
}
