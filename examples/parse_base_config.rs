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
    data_builder.parse_base_configuration().unwrap();

    std::env::set_current_dir(cwd).unwrap();
}
