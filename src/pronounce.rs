use std::{collections::BTreeMap, fmt, rc::Rc};

use serde::{Deserialize, Serialize};

use crate::{chord::Chord, dictionary::Word};

#[derive(Debug, Clone, Deserialize)]
pub struct Dictionary {
    entries: BTreeMap<Word, Vec<Pronunciation>>,
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

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: SeqAccess<'de> {
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
pub struct TheoryRules {
    vowels: BTreeMap<Pronunciation, Vec<Chord>>,
    onsets: BTreeMap<Pronunciation, Vec<Chord>>,
    codas: BTreeMap<Pronunciation, Vec<Chord>>,
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::BufReader};

    use super::TheoryRules;

    #[test]
    fn load_theory() -> anyhow::Result<()> {
        let _theory: TheoryRules = serde_yaml::from_reader(BufReader::new(File::open("theory.yaml")?))?;
        Ok(())
    }
}
