use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use bytebraise_datasmart::evaluate::Evaluate;
use bytebraise_datasmart::petgraph2::DataSmart;
use bytebraise_syntax::parser::parse_bitbake_from_str;
use clap::Parser;

use bytebraise_datasmart::get_var;

#[derive(Parser)]
struct Opts {
    file: PathBuf,
}

fn main() {
    let opts = Opts::parse();
    let mut data = String::new();

    File::open(opts.file).unwrap().read_to_string(&mut data).unwrap();

    let parsed = parse_bitbake_from_str(&data);

    println!("{:#?}", &parsed);

    let mut d = DataSmart::new();
    parsed.evaluate(&mut d).unwrap();
    println!("{d:#?}");

    let v = get_var!(&d, "BBLAYERS").unwrap();
    dbg!(v);
}
