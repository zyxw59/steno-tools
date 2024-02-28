use std::{collections::BTreeMap, rc::Rc, fmt};

use serde::{Deserialize, Serialize};

use crate::{dictionary::Word, chord::Chord};

#[derive(Debug, Clone, Deserialize)]
pub struct Dictionary {
    entries: BTreeMap<Word, Vec<Pronunciation>>,
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Pronunciation(Rc<[Phoneme]>);
crate::deref_impls!(Pronunciation as [Phoneme]);

impl fmt::Display for Pronunciation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0.join(" "), f)
    }
}

#[derive(Clone, Hash, Eq, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Phoneme(Rc<str>);
crate::fmt_impls!(Phoneme);
crate::deref_impls!(Phoneme as str);

#[derive(Debug, Clone, Deserialize)]
pub struct PhonemeMap {
    map: BTreeMap<Phoneme, Chord>,
}
