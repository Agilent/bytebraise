use clap::Parser;
use bytebraise::cooker_data::CookerDataBuilder;
use bytebraise::data_smart::variable_contents::VariableContentsAccessors;

/// Port of bitbake-layers
#[derive(Parser)]
#[clap(version = "1.0")]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    ShowLayers(ShowLayers),
}

/// Show current configured layers
#[derive(Parser)]
struct ShowLayers {}

fn main() {
    let opts = Opts::parse();

    match opts.subcmd {
        SubCommand::ShowLayers(_show_layers) => {
            let mut data_builder = CookerDataBuilder::new();
            data_builder.parse_base_configuration().unwrap();

            let d = data_builder.data();
            let collections = d.get_var("BBFILE_COLLECTIONS").unwrap().as_string();

            for c in collections.split_whitespace() {
                let priority = d.get_var(&format!("BBFILE_PRIORITY_{}", c)).unwrap();
                eprintln!("{:?}", priority);

                let depends = d.get_var(&format!("LAYERDEPENDS_{}", c)).unwrap();
                eprintln!("{:?}", depends);

                let version = d.get_var(&format!("LAYERVERSION_{}", c)).unwrap();
                eprintln!("{:?}", version);

                println!();
            }

            panic!("{:#?}", collections);
        }
    }
}
