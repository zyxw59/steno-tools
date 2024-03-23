use std::{fmt, rc::Rc, str::FromStr};

use serde::{Deserialize, Serialize};

bitflags::bitflags! {
    #[derive(Clone, Copy, Default, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Chord: u32 {
        const NUMBER  = 1 << 0;
        const CARET   = 1 << 1;
        const S_LEFT  = 1 << 2;
        const T_LEFT  = 1 << 3;
        const K_LEFT  = 1 << 4;
        const P_LEFT  = 1 << 5;
        const W_LEFT  = 1 << 6;
        const H_LEFT  = 1 << 7;
        const R_LEFT  = 1 << 8;
        const A       = 1 << 9;
        const O       = 1 << 10;
        const STAR    = 1 << 11;
        const E       = 1 << 12;
        const U       = 1 << 13;
        const F_RIGHT = 1 << 14;
        const R_RIGHT = 1 << 15;
        const P_RIGHT = 1 << 16;
        const B_RIGHT = 1 << 17;
        const L_RIGHT = 1 << 18;
        const G_RIGHT = 1 << 19;
        const T_RIGHT = 1 << 20;
        const S_RIGHT = 1 << 21;
        const D_RIGHT = 1 << 22;
        const Z_RIGHT = 1 << 23;
    }
}

impl Chord {
    fn flag_name(self) -> &'static str {
        match self {
            Self::NUMBER => "#",
            Self::CARET => "^",
            Self::S_LEFT => "S",
            Self::T_LEFT => "T",
            Self::K_LEFT => "K",
            Self::P_LEFT => "P",
            Self::W_LEFT => "W",
            Self::H_LEFT => "H",
            Self::R_LEFT => "R",
            Self::A => "A",
            Self::O => "O",
            Self::STAR => "*",
            Self::E => "E",
            Self::U => "U",
            Self::F_RIGHT => "F",
            Self::R_RIGHT => "R",
            Self::P_RIGHT => "P",
            Self::B_RIGHT => "B",
            Self::L_RIGHT => "L",
            Self::G_RIGHT => "G",
            Self::T_RIGHT => "T",
            Self::S_RIGHT => "S",
            Self::D_RIGHT => "D",
            Self::Z_RIGHT => "Z",
            _ => "",
        }
    }

    fn implicit_hyphen(self) -> bool {
        self.intersects(Self::A | Self::O | Self::STAR | Self::E | Self::U)
    }

    fn post_hyphen(self) -> bool {
        (Self::F_RIGHT
            | Self::R_RIGHT
            | Self::P_RIGHT
            | Self::B_RIGHT
            | Self::L_RIGHT
            | Self::G_RIGHT
            | Self::T_RIGHT
            | Self::S_RIGHT
            | Self::D_RIGHT
            | Self::Z_RIGHT)
            .contains(self)
    }

    /// Returns whether this chord is entirely before the other chord in steno order.
    pub fn before(self, other: Chord) -> bool {
        self.highest_flag_index() < other.lowest_flag_index()
    }

    /// Returns whether this chord is entirely before the other chord in steno order, ignoring
    /// star.
    pub fn before_ignore_star(self, other: Chord) -> bool {
        (self & !Chord::STAR).highest_flag_index() < (other & !Chord::STAR).lowest_flag_index()
    }

    fn highest_flag_index(self) -> i32 {
        (<Self as bitflags::Flags>::Bits::BITS - self.bits().leading_zeros()) as i32 - 1
    }

    fn lowest_flag_index(self) -> i32 {
        self.bits().trailing_zeros() as i32
    }
}

impl fmt::Debug for Chord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            for flag in Chord::all() {
                if self.contains(flag) {
                    f.write_str(flag.flag_name())?;
                } else {
                    f.write_str(" ")?;
                }
            }
            Ok(())
        } else {
            let mut hyphen = self.implicit_hyphen();
            for flag in *self {
                if !hyphen && flag.post_hyphen() {
                    f.write_str("-")?;
                    hyphen = true;
                }
                f.write_str(flag.flag_name())?;
            }
            Ok(())
        }
    }
}

impl fmt::Display for Chord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl FromStr for Chord {
    type Err = ChordParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut value = Chord::empty();
        let mut post_hyphen = false;
        for c in s.chars() {
            let add = match (c, post_hyphen) {
                ('#', false) => Chord::NUMBER,
                ('^', false) => Chord::CARET,
                ('S', false) => Chord::S_LEFT,
                ('T', false) => Chord::T_LEFT,
                ('K', false) => Chord::K_LEFT,
                ('P', false) => Chord::P_LEFT,
                ('W', false) => Chord::W_LEFT,
                ('H', false) => Chord::H_LEFT,
                ('R', false) => Chord::R_LEFT,
                ('-', false) => {
                    post_hyphen = true;
                    continue;
                }
                ('A', _) => Chord::A,
                ('O', _) => Chord::O,
                ('*', _) => Chord::STAR,
                ('E', _) => Chord::E,
                ('U', _) => Chord::U,
                ('F', true) => Chord::F_RIGHT,
                ('R', true) => Chord::R_RIGHT,
                ('P', true) => Chord::P_RIGHT,
                ('B', true) => Chord::B_RIGHT,
                ('L', true) => Chord::L_RIGHT,
                ('G', true) => Chord::G_RIGHT,
                ('T', true) => Chord::T_RIGHT,
                ('S', true) => Chord::S_RIGHT,
                ('D', true) => Chord::D_RIGHT,
                ('Z', true) => Chord::Z_RIGHT,

                ('#' | '^' | 'K' | 'W' | 'H' | '-', true)
                | ('F' | 'B' | 'L' | 'G' | 'D' | 'Z', false) => {
                    return Err(ChordParseError::OutOfOrder(c));
                }
                _ => return Err(ChordParseError::UnexpectedChar(c)),
            };
            if !value.before(add) {
                return Err(ChordParseError::OutOfOrder(c));
            }
            post_hyphen |= add.implicit_hyphen();
            value |= add;
        }
        Ok(value)
    }
}

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ChordParseError {
    #[error("Unexpected {0:?} in steno notation")]
    UnexpectedChar(char),
    #[error("{0:?} out-of-order in steno notation")]
    OutOfOrder(char),
}

impl ChordParseError {
    fn char(self) -> char {
        let (Self::UnexpectedChar(c) | Self::OutOfOrder(c)) = self;
        c
    }
}

impl<'de> Deserialize<'de> for Chord {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{Error, Unexpected};
        struct Visitor;

        const EXPECTED: &str = "#^STKPWHRAO*-EUFRPBLGTSDZ, in that order";

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Chord;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("a string representing steno notation")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                v.parse::<Chord>()
                    .map_err(|err| E::invalid_value(Unexpected::Char(err.char()), &EXPECTED))
            }
        }

        de.deserialize_str(Visitor)
    }
}

impl Serialize for Chord {
    fn serialize<S>(&self, se: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        se.serialize_str(&self.to_string())
    }
}

#[derive(Clone, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Outline(Rc<[Chord]>);
crate::deref_impls!(Outline as [Chord]);

impl fmt::Debug for Outline {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some((first, rest)) = self.0.split_first() {
            fmt::Debug::fmt(first, f)?;
            for chord in rest {
                f.write_str("/")?;
                fmt::Debug::fmt(chord, f)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Outline {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl FromStr for Outline {
    type Err = <Chord as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split('/').map(Chord::from_str).collect()
    }
}

impl FromIterator<Chord> for Outline {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Chord>,
    {
        Self(iter.into_iter().collect())
    }
}

impl<T> From<T> for Outline where Rc<[Chord]>: From<T> {
    fn from(val: T) -> Self {
        Self(val.into())
    }
}

impl Serialize for Outline {
    fn serialize<S>(&self, se: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        se.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Outline {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{Error, Unexpected};
        struct Visitor;

        const EXPECTED: &str = "a sequence of steno strokes separated by slashes";

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Outline;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("a string representing steno notation")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                v.parse::<Outline>()
                    .map_err(|err| E::invalid_value(Unexpected::Char(err.char()), &EXPECTED))
            }
        }

        de.deserialize_str(Visitor)
    }
}

#[cfg(test)]
mod tests {
    use test_case::test_case;

    use super::{Chord, ChordParseError};

    #[test_case("#^STKPWHRAO*EUFRPBLGTSDZ", Chord::all() ; "all")]
    #[test_case("ST-TS", Chord::S_LEFT | Chord::T_LEFT | Chord::T_RIGHT | Chord::S_RIGHT ; "hyphenated")]
    #[test_case("-TS", Chord::T_RIGHT | Chord::S_RIGHT ; "right hand only")]
    #[test_case("ST", Chord::S_LEFT | Chord::T_LEFT ; "left hand only")]
    fn chord_serde(original: &str, expected: Chord) -> anyhow::Result<()> {
        let parsed: Chord = original.parse()?;
        assert_eq!(parsed, expected);
        let formatted = parsed.to_string();
        assert_eq!(formatted, original);
        Ok(())
    }

    #[test_case("TS", ChordParseError::OutOfOrder('S') ; "transposition")]
    #[test_case("FR-F", ChordParseError::OutOfOrder('F') ; "pre hyphen")]
    #[test_case("QR", ChordParseError::UnexpectedChar('Q') ; "invalid char")]
    fn chord_invalid_serde(original: &str, expected: ChordParseError) {
        let actual = original.parse::<Chord>().unwrap_err();
        assert_eq!(actual, expected);
    }

    #[test_case("ST", "-TS", true, false ; "before")]
    #[test_case("SH", "KWR", false, false ; "overlap")]
    #[test_case("-FR", "*T", true, false ; "ignore star")]
    #[test_case("*FR", "*T", true, true ; "star conflicts")]
    fn compare_chords(
        left: &str,
        right: &str,
        before: bool,
        conflicts: bool,
    ) -> anyhow::Result<()> {
        let left: Chord = left.parse()?;
        let right: Chord = right.parse()?;
        assert_eq!(left.before_ignore_star(right), before);
        assert_eq!(!(left & right).is_empty(), conflicts);
        Ok(())
    }
}
