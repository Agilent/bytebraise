use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Expr, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

/// Parsed input: `<target>, <value>`
struct MacroInput {
    target: Expr,
    _comma: Token![,],
    value: Expr,
}

impl Parse for MacroInput {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let target = input.parse()?;
        let _comma = input.parse()?;
        let value = input.parse()?;
        Ok(Self {
            target,
            _comma,
            value,
        })
    }
}

#[proc_macro]
pub fn m(item: TokenStream) -> TokenStream {
    // Expect exactly: `<target> , <expr>`
    let MacroInput { target, value, .. } = parse_macro_input!(item as MacroInput);

    quote!( #target.set(#value) ).into()
}
