use crate::data_smart::errors::DataSmartResult;
use crate::data_smart::variable_contents::{VariableContents, VariableContentsAccessors};
use crate::data_smart::{DataSmart, GetVarOptions};
use crate::parser::parser::parse_bitbake_from_str;
use crate::syntax::ast::nodes::{Assignment, Directive, Root, RootItem};
use crate::syntax::ast::AstToken;
use crate::syntax::syntax_kind::SyntaxKind;
use anyhow::Context;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use crate::utils::which;

pub trait Evaluate {
    fn evaluate(&self, d: &DataSmart) -> DataSmartResult<()>;
}

impl Evaluate for Root {
    fn evaluate(&self, d: &DataSmart) -> DataSmartResult<()> {
        // TODO provide option to disable?
        if d.get_var("TOPDIR")?.is_none() {
            let topdir = std::env::current_dir()?;
            d.set_var("TOPDIR", topdir.display().to_string())?;
        }

        for item in self.items() {
            item.evaluate(d)?;
        }

        Ok(())
    }
}

impl Evaluate for RootItem {
    fn evaluate(&self, d: &DataSmart) -> DataSmartResult<()> {
        match self {
            RootItem::Comment(_c) => Ok(()),
            RootItem::Assignment(e) => e.evaluate(d),
            RootItem::Directive(directive) => directive.evaluate(d),
            _ => unimplemented!(),
        }
    }
}

impl Evaluate for Directive {
    fn evaluate(&self, d: &DataSmart) -> DataSmartResult<()> {
        match self {
            Directive::Export(e) => {
                if let Some(assignment) = e.assignment().as_ref() {
                    evaluate_assignment_expression(d, assignment, true)
                } else {
                    let var = e.var();
                    // TODO: enforce at parse-time so we don't need to do it here
                    assert!(var.varflag().is_none(), "cannot export varflag");

                    let var = var.identifier();
                    // TODO: int 1, not string "1"
                    d.set_var_flag(var.syntax.text().to_string(), "export", "1")?;
                    Ok(())
                }
            }
            Directive::Include(i) => {
                let what_files = d.expand(i.value().text())?.unwrap_or_default();
                for file_name in what_files.split_whitespace() {
                    include_single_file(d, file_name)?;
                }
                Ok(())
            }
            _ => unimplemented!(),
        }
    }
}

impl Evaluate for Assignment {
    fn evaluate(&self, d: &DataSmart) -> DataSmartResult<()> {
        evaluate_assignment_expression(d, self, false)
    }
}

fn evaluate_assignment_expression(
    data: &DataSmart,
    expr: &Assignment,
    is_exported: bool,
) -> DataSmartResult<()> {
    let ident = expr.left();
    let key = ident.identifier().syntax.text().to_string();
    let mut varflag_text = ident.varflag().map(|v| v.value().to_string());
    let assigned_value = expr.right().value().to_string();

    let new_value: VariableContents;

    if is_exported {
        // TODO literal 1, not string "1"
        data.set_var_flag(&key, "export", "1")?;
    }

    fn _get_func(
        data: &DataSmart,
        key: &str,
        flag: Option<&str>,
    ) -> DataSmartResult<Option<VariableContents>> {
        let get_opts = GetVarOptions::default().no_weak_default(true).parsing(true);
        match flag {
            Some(flag) => data.get_var_flag_contents(key, flag, get_opts),
            None => data.get_var_opt(key, get_opts),
        }
    }

    match expr.op().syntax().kind() {
        SyntaxKind::Equals => new_value = assigned_value.into(),
        SyntaxKind::WeakEquals => {
            new_value = assigned_value.into();
            assert!(varflag_text.is_none(), "cannot weak assign variable flag");
            varflag_text = Some(String::from("_defaultval"));
        }

        SyntaxKind::DefaultEquals
        | SyntaxKind::PlusEquals
        | SyntaxKind::EqualsPlus
        | SyntaxKind::EqualsDot
        | SyntaxKind::DotEquals
        | SyntaxKind::ColonEquals => {
            let current_value = _get_func(data, key.as_str(), varflag_text.as_deref())
                .with_context(|| format!("failure getting value for {}", key))?;

            match expr.op().syntax().kind() {
                SyntaxKind::DefaultEquals => {
                    new_value = match current_value {
                        Some(val) => val,
                        None => assigned_value.into(),
                    }
                }
                SyntaxKind::ColonEquals => {
                    let e = data.create_copy();
                    new_value = e
                        .expand_with_varname(assigned_value.as_str(), format!("{}[:=]", key))
                        .with_context(|| format!("error expanding {}", &assigned_value))?
                        .unwrap()
                        .into();
                }
                SyntaxKind::PlusEquals => {
                    new_value =
                        format!("{} {}", current_value.as_string_or_empty(), assigned_value).into();
                }
                SyntaxKind::EqualsPlus => {
                    new_value =
                        format!("{} {}", assigned_value, current_value.as_string_or_empty()).into();
                }
                SyntaxKind::DotEquals => {
                    new_value =
                        format!("{}{}", current_value.as_string_or_empty(), assigned_value).into();
                }
                SyntaxKind::EqualsDot => {
                    new_value =
                        format!("{}{}", assigned_value, current_value.as_string_or_empty()).into();
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    };

    if let Some(flag) = varflag_text {
        data.set_var_flag(key, flag, new_value)?;
    } else {
        data.set_var_opt(key, new_value, true)?;
    }

    Ok(())
}

fn include_single_file<F: AsRef<Path>>(data: &DataSmart, file_name: F) -> DataSmartResult<()> {
    // TODO prevent recursion

    let mut file_name = file_name.as_ref().to_path_buf();
    if file_name.is_relative() {
        let dname = file_name.parent().unwrap();
        // TODO: don't use display(), just add flavor of |which| that takes slice?
        let abs_file_name = which(
            format!(
                "{}:{}",
                dname.display(),
                data.get_var("BBPATH").unwrap().as_string_or_empty()
            ),
            file_name,
            false,
            false,
        )?;

        // TODO mark dependency stuff

        if abs_file_name.is_none() {
            // TODO
            return Ok(());
            //unimplemented!();
        }

        file_name = abs_file_name.unwrap();
    }

    parse_config_file(&file_name, data)
        .with_context(|| format!("failure including file {:?}", &file_name))?;

    Ok(())
}

pub fn parse_config_file<F: AsRef<Path>>(file: F, d: &DataSmart) -> DataSmartResult<()> {
    let mut source = String::new();
    let mut file = file.as_ref().to_path_buf();

    // TODO extract to method, resolve_file

    if !file.is_absolute() {
        let bbpath = d.get_var("BBPATH")?.as_string();
        let newfn = which(&bbpath, &file, false, false)?;
        // TODO avoid clone
        let newfn = newfn
            .clone()
            .ok_or_else(|| anyhow::anyhow!("file {:?} not found in {}", &newfn, &bbpath))?;
        file = newfn;
    }

    if file.extension().map(|e| e.to_str().unwrap()) != Some("conf") {
        //anyhow::bail!("expected .conf file extension; file is: {:?}", &file);
    }

    println!("file: {:?}", &file);
    File::open(file)?.read_to_string(&mut source)?;
    let res = parse_bitbake_from_str(&*source);
    res.evaluate(d)
        .with_context(|| "failure to evaluate metadata")?;
    Ok(())
}
