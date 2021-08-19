use clap::Parser;

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
        SubCommand::ShowLayers(_show_layers) => {}
    }
}
