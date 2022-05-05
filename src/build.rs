use crate::data_smart::variable_contents::VariableContents;
use crate::data_smart::{DataSmart, GetVarOptions};
use crate::ByteBraiseResult;
use std::collections::BTreeSet;
use std::convert::TryInto;

const BBTASKS: &'static str = "__BBTASKS";

pub fn add_task<T>(
    task: T,
    before: Vec<String>,
    after: Vec<String>,
    d: &DataSmart,
) -> ByteBraiseResult<()>
where
    T: ToString,
{
    let mut task = task.to_string();
    if !task.starts_with("do_") {
        task = "do_".to_string() + &task;
    }

    d.set_var_flag(task, "task", "1");
    //
    // let bbtasks: BTreeSet<VariableContents> = d
    //     .get_var_opt(BBTASKS, GetVarOptions::default().expand(false))?
    //     .unwrap()
    //     .try_into()
    //     .unwrap();

    Ok(())
}
