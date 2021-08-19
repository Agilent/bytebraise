use std::path::PathBuf;

use bytebraise::cooker_data::CookerDataBuilder;
use clap::Parser;

#[derive(Parser)]
struct Opts {
    file: PathBuf,
}

fn main() {
    let opts = Opts::parse();

    let cwd = std::env::current_dir().unwrap();
    std::env::set_current_dir(opts.file).unwrap();

    let data_builder = CookerDataBuilder::new();
    let _d = data_builder.parse_configuration_files().unwrap();
    std::env::set_current_dir(cwd).unwrap();
}
