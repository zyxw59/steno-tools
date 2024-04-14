use std::{
    collections::{BTreeMap, BTreeSet},
    fs::File,
    io::{self, BufRead, BufReader, BufWriter},
    path::PathBuf,
};

use clap::Parser;
use serde_json::from_reader;

mod chord;
mod compound_words;
mod dictionary;
mod generated;
mod pronounce;
mod theory;
mod tree;
mod wrapper_impls;

use dictionary::{Dictionary, Word};
use generated::{GeneratedDictionary, NoOutline, Override};

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    match args.command {
        Command::Compare(args) => args.execute(),
        Command::Categorize(args) => args.execute(),
        Command::GenerateOutlines(args) => args.execute(),
        Command::CompoundWords(args) => args.execute(),
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
    /// Generate a file listing compound words, using the specified pronunciation dictionary
    CompoundWords(CompoundWords),
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
    #[clap(short, long)]
    wordlist: PathBuf,
    #[clap(short, long = "pronunciations")]
    pronunciation_file: PathBuf,
    #[clap(short, long = "theory")]
    theory_file: PathBuf,
    #[clap(short = 'O', long = "overrides")]
    overrides_file: Option<PathBuf>,
    #[clap(short, long = "output")]
    out_file: Option<PathBuf>,
}

impl GenerateOutlines {
    fn execute(&self) -> anyhow::Result<()> {
        let mut pronunciation_dict =
            pronounce::Dictionary::load(BufReader::new(File::open(&self.pronunciation_file)?))?;
        let theory: theory::PhoneticTheory =
            serde_yaml::from_reader(BufReader::new(File::open(&self.theory_file)?))?;
        let mut generated_dict = GeneratedDictionary::default();
        let words = BufReader::new(File::open(&self.wordlist)?)
            .lines()
            .map_while(Result::ok)
            .map(Word::from)
            .collect::<BTreeSet<_>>();
        pronunciation_dict.retain_words(|word| words.contains(word));
        for word in words {
            let prons = pronunciation_dict.get(&word);
            if prons.is_empty() {
                generated_dict.no_pronunciation.push(word.clone())
            }
            for pron in prons {
                let outline_result = if let Some([first, second]) =
                    compound_words::get_unambiguous_split(&word, pron, &pronunciation_dict)
                {
                    theory.get_outline_compound(&first.pronunciation, &second.pronunciation)
                } else {
                    theory.get_outline(pron)
                };
                match outline_result {
                    Ok(outline) => {
                        generated_dict.insert(outline, word.clone(), pron.clone());
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
        generated_dict.resolve_pairs(|outline, entry_1, entry_2| {
            if entry_1.pronunciation == entry_2.pronunciation {
                return None;
            }
            // TODO: disambiguate compound words? there should be few enough that it's not actually
            // an issue
            theory.disambiguate_phonetic(
                outline.clone(),
                &entry_1.pronunciation,
                &entry_2.pronunciation,
            )
        });
        generated_dict.resolve_pairs(|outline, entry_1, entry_2| {
            if entry_1.word == entry_2.word {
                return None;
            }
            theory.disambiguate_spelling(outline.clone(), &entry_1.word, &entry_2.word)
        });
        if let Some(overrides_file) = &self.overrides_file {
            let overrides: Vec<Override> =
                serde_yaml::from_reader(BufReader::new(File::open(overrides_file)?))?;
            for entry in overrides {
                if !generated_dict.insert(
                    entry.outline.clone(),
                    entry.word.clone(),
                    entry.pronunciation.clone(),
                ) {
                    eprintln!("Warning: override {entry:?} was a conflict");
                }
            }
        }
        generated_dict.remove_conflicts_with_valid_alternatives();
        generated_dict.resolve_identical_conflicts();
        generated_dict.remove_errors_with_valid_alternatives();
        if let Some(out_path) = &self.out_file {
            serde_json::to_writer_pretty(BufWriter::new(File::create(out_path)?), &generated_dict)?;
        } else {
            serde_json::to_writer_pretty(io::stdout().lock(), &generated_dict)?;
        }
        Ok(())
    }
}

#[derive(Debug, clap::Args)]
struct CompoundWords {
    #[clap(short, long)]
    wordlist: Option<PathBuf>,
    #[clap(short, long = "pronunciations")]
    pronunciation_file: PathBuf,
    #[clap(short, long = "output")]
    out_file: Option<PathBuf>,
}

impl CompoundWords {
    fn execute(&self) -> anyhow::Result<()> {
        let words = if let Some(wordlist) = &self.wordlist {
            Some(
                BufReader::new(File::open(wordlist)?)
                    .lines()
                    .map_while(Result::ok)
                    .map(Word::from)
                    .collect::<BTreeSet<Word>>(),
            )
        } else {
            None
        };
        let mut pronunciation_dict =
            pronounce::Dictionary::load(BufReader::new(File::open(&self.pronunciation_file)?))?;
        if let Some(words) = words {
            pronunciation_dict.retain_words(|word| words.contains(word));
        }
        let compounds = compound_words::CompoundWords::from(&pronunciation_dict);
        if let Some(out_path) = &self.out_file {
            serde_yaml::to_writer(BufWriter::new(File::create(out_path)?), &compounds)?;
        } else {
            serde_yaml::to_writer(io::stdout().lock(), &compounds)?;
        }
        Ok(())
    }
}
