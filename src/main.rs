use std::{collections::BTreeSet, fs::File, io::BufReader, path::PathBuf};

use clap::Parser;
use indexmap::IndexMap;
use itertools::Itertools;
use serde_json::from_reader;

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    match args.command {
        Command::Compare(args) => args.execute(),
    }
}

#[derive(Debug, Parser)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, clap::Subcommand)]
enum Command {
    Compare(Compare),
}

#[derive(Debug, clap::Args)]
struct Compare {
    file_1: PathBuf,
    file_2: PathBuf,
}

impl Compare {
    fn execute(&self) -> anyhow::Result<()> {
        let dict_1: IndexMap<String, String> =
            from_reader(BufReader::new(File::open(&self.file_1)?))?;
        let dict_2: IndexMap<String, String> =
            from_reader(BufReader::new(File::open(&self.file_2)?))?;
        let reverse_1 = dict_1
            .iter()
            .map(|(key, value)| (value, key))
            .into_grouping_map()
            .collect::<BTreeSet<_>>();
        let reverse_2 = dict_2
            .iter()
            .map(|(key, value)| (value, key))
            .into_grouping_map()
            .collect::<BTreeSet<_>>();
        let words_1 = dict_1.values().collect::<BTreeSet<_>>();
        let words_2 = dict_2.values().collect::<BTreeSet<_>>();

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
        let empty_set = BTreeSet::new();
        for word in common_words {
            let outlines_1 = reverse_1.get(word).unwrap_or(&empty_set);
            let outlines_2 = reverse_2.get(word).unwrap_or(&empty_set);
            if outlines_1 != outlines_2 {
                println!("{word}:");
                for outline in outlines_1.difference(outlines_2) {
                    println!("\t{outline}");
                }
                for outline in outlines_2.difference(outlines_1) {
                    println!("\t\t{outline}");
                }
            }
        }
        Ok(())
    }
}
