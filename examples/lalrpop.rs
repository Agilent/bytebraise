use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub value);

fn run_case(input: &str) {
    let ret = value::ExpressionParser::new().parse(input);
    println!("{}:", input);
    println!("\t{:#?}", ret.unwrap());
}

fn main() {
    run_case("${A}${${${22}}}${B}${@OK}");
}
