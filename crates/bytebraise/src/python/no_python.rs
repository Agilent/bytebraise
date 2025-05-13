use crate::data_smart::variable_parse::VariableParse;
use crate::python::PYTHON_EXPANSION_REGEX;
use bytebraise_datasmart::errors::DataSmartResult;
use std::borrow::Cow;

pub fn handle_python<'a>(
    input: &'a Cow<'a, str>,
    _: &'a mut VariableParse,
) -> DataSmartResult<Cow<'a, str>> {
    if PYTHON_EXPANSION_REGEX.find(input.as_ref()).is_some() {
        anyhow::bail!(
            "built without Python support, but attempted to expand Python expression: {}",
            input
        );
    }
    Ok(Cow::Borrowed(input))
}
