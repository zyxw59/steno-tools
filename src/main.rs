use std::{collections::{BTreeSet, BTreeMap}, fs::File, io::BufReader, path::PathBuf};

use clap::Parser;
use serde_json::from_reader;

mod dictionary;

use dictionary::Dictionary;

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    match args.command {
        Command::Compare(args) => args.execute(),
        Command::Categorize(args) => args.execute(),
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
        for word in common_words {
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
}

impl Categorize {
    fn execute(&self) -> anyhow::Result<()> {
        let dictionary: Dictionary = from_reader(BufReader::new(File::open(&self.file)?))?;
        let mut cat_map = BTreeMap::<_, BTreeSet<_>>::new();
        for word in dictionary.words().keys() {
            cat_map.entry(word.categorize()).or_default().insert(word.clone());
        }
        for (cat, words) in &cat_map {
            println!("{cat:?}: {} words", words.len());
        }
        Ok(())
    }
}
