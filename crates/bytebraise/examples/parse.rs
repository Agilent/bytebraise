use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use bytebraise::data_smart::DataSmart;
use bytebraise::evaluate::Evaluate;
use bytebraise_syntax::parser::parse_bitbake_from_str;
use clap::Parser;

#[derive(Parser)]
struct Opts {
    file: PathBuf,
}

fn main() {
    let opts = Opts::parse();
    let mut data = String::new();

    File::open(opts.file).unwrap().read_to_string(&mut data);

    let parsed = parse_bitbake_from_str(&data);

    println!("{:#?}", &parsed);

    let d = DataSmart::new();
    parsed.evaluate(&d).unwrap();
    println!("{d:?}");
}
