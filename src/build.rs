use crate::data_smart::DataSmart;
use crate::ByteBraiseResult;

const BBTASKS: &str = "__BBTASKS";

pub fn add_task<T>(
    task: T,
    _before: Vec<String>,
    _after: Vec<String>,
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
