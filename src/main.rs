use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet},
    fs::File,
    io::{self, BufRead, BufReader, BufWriter},
    path::PathBuf,
};

use clap::Parser;
use serde_json::from_reader;

mod chord;
mod dictionary;
mod pronounce;
mod wrapper_impls;

use dictionary::{Dictionary, Outline, Word};
use pronounce::Pronunciation;

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    match args.command {
        Command::Compare(args) => args.execute(),
        Command::Categorize(args) => args.execute(),
        Command::GenerateOutlines(args) => args.execute(),
    }
}

#[derive(Debug, Parser)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, clap::Subcommand)]
enum Command {
    /// Compare two dictionary files
    Compare(Compare),
    /// Categorize the words in a dictionary file
    Categorize(Categorize),
    /// Generate outlines given a word list, pronunciation dictionary, and theory
    GenerateOutlines(GenerateOutlines),
}

#[derive(Debug, clap::Args)]
struct Compare {
    file_1: PathBuf,
    file_2: PathBuf,
}

impl Compare {
    fn execute(&self) -> anyhow::Result<()> {
        let dict_1: Dictionary = from_reader(BufReader::new(File::open(&self.file_1)?))?;
        let dict_2: Dictionary = from_reader(BufReader::new(File::open(&self.file_2)?))?;

        let words_1 = dict_1.words().keys().collect::<BTreeSet<_>>();
        let words_2 = dict_2.words().keys().collect::<BTreeSet<_>>();

        println!("Unique words");
        println!(
            "{} / {}",
            words_1.difference(&words_2).count(),
            words_2.difference(&words_1).count()
        );
        // for word in words_1.difference(&words_2) {
        //     println!("{word}");
        // }
        // for word in words_2.difference(&words_1) {
        //     println!("\t{word}");
        // }
        let common_words = words_1.intersection(&words_2);
        println!("\nUnique outlines");
        for &word in common_words {
            let outlines_1 = dict_1
                .words()
                .get(word)
                .into_iter()
                .flatten()
                .collect::<BTreeSet<_>>();
            let outlines_2 = dict_2
                .words()
                .get(word)
                .into_iter()
                .flatten()
                .collect::<BTreeSet<_>>();
            if outlines_1 != outlines_2 {
                println!("{word}:");
                for outline in outlines_1.difference(&outlines_2) {
                    println!("\t{outline}");
                }
                for outline in outlines_2.difference(&outlines_1) {
                    println!("\t\t{outline}");
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, clap::Args)]
struct Categorize {
    file: PathBuf,
    #[arg(short, long)]
    summarize: bool,
    print: Vec<dictionary::WordCategory>,
}

impl Categorize {
    fn execute(&self) -> anyhow::Result<()> {
        let dictionary: Dictionary = from_reader(BufReader::new(File::open(&self.file)?))?;
        let mut cat_map = BTreeMap::<_, BTreeSet<_>>::new();
        for word in dictionary.words().keys() {
            cat_map
                .entry(word.categorize())
                .or_default()
                .insert(word.clone());
        }
        if self.summarize || self.print.is_empty() {
            for (cat, words) in &cat_map {
                println!("{cat:?}: {} words", words.len());
            }
        }
        for cat in &self.print {
            println!("{cat:?}:");
            for word in cat_map.get(cat).into_iter().flatten() {
                println!("{word}");
            }
        }
        Ok(())
    }
}

#[derive(Debug, clap::Args)]
struct GenerateOutlines {
    wordlist: PathBuf,
    pronunciation_file: PathBuf,
    theory_file: PathBuf,
    out_file: Option<PathBuf>,
}

impl GenerateOutlines {
    fn execute(&self) -> anyhow::Result<()> {
        let pronunciation_dict =
            pronounce::Dictionary::load_csv(BufReader::new(File::open(&self.pronunciation_file)?))?;
        let theory: pronounce::PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open(&self.theory_file)?))?;
        let mut generated_dict = GeneratedDictionary::default();
        let words = BufReader::new(File::open(&self.wordlist)?)
            .lines()
            .map_while(Result::ok)
            .map(Word::from);
        for word in words {
            let prons = pronunciation_dict.get(&word);
            if prons.is_empty() {
                generated_dict.no_pronunciation.push(word.clone())
            }
            for pron in prons {
                match theory.get_outline(pron, &word) {
                    Ok(outline) => {
                        if let Err(conflict) = generated_dict
                            .valid_outlines
                            .insert(word.clone(), outline.clone())
                        {
                            let conflict_entry =
                                generated_dict.conflicts.entry(outline).or_default();
                            conflict_entry.insert(conflict);
                            conflict_entry.insert(word.clone());
                        }
                    }
                    Err(error) => {
                        generated_dict.no_outlines.push(NoOutline {
                            word: word.clone(),
                            pronunciation: pron.clone(),
                            error,
                        });
                    }
                }
            }
        }
        for outline in generated_dict.conflicts.keys() {
            generated_dict.valid_outlines.remove_outline(outline);
        }
        if let Some(out_path) = &self.out_file {
            serde_json::to_writer_pretty(BufWriter::new(File::create(out_path)?), &generated_dict)?;
        } else {
            serde_json::to_writer_pretty(io::stdout().lock(), &generated_dict)?;
        }
        Ok(())
    }
}

#[derive(Default, Debug, Deserialize, Serialize)]
pub struct GeneratedDictionary {
    pub valid_outlines: Dictionary,
    pub conflicts: BTreeMap<Outline, BTreeSet<Word>>,
    pub no_pronunciation: Vec<Word>,
    pub no_outlines: Vec<NoOutline>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct NoOutline {
    word: Word,
    pronunciation: Pronunciation,
    #[serde(
        serialize_with = "serialize_anyhow",
        deserialize_with = "deserialize_anyhow"
    )]
    error: anyhow::Error,
}

fn serialize_anyhow<S>(error: &anyhow::Error, ser: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    error.to_string().serialize(ser)
}

fn deserialize_anyhow<'de, D>(de: D) -> Result<anyhow::Error, D::Error>
where
    D: serde::Deserializer<'de>,
{
    String::deserialize(de).map(anyhow::Error::msg)
}
