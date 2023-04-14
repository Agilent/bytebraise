use std::collections::HashSet;
use std::convert::TryInto;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

use lazy_static::lazy_static;

use crate::data_smart::DataSmart;
use crate::ByteBraiseResult;

pub fn which<P: AsRef<str>, I: AsRef<Path>>(
    path: P,
    item: I,
    reversed: bool,
    executable: bool,
) -> ByteBraiseResult<Option<PathBuf>> {
    let paths: Box<dyn Iterator<Item = &str>> = match reversed {
        false => Box::new(path.as_ref().split(':')),
        true => Box::new(path.as_ref().rsplit(':')),
    };

    let ret = paths
        .map(|path| PathBuf::from(path).join(item.as_ref()))
        .find(|path| {
            if path.exists() {
                if executable {
                    let metadata = path.metadata().unwrap();
                    if (metadata.permissions().mode() & 0o111) == 0 {
                        return false;
                    }
                }
                return true;
            }
            false
        })
        .map(PathBuf::from)
        .map(|path| path.canonicalize().unwrap());

    Ok(ret)
}

pub fn contains<'a, V: AsRef<str>, C: AsRef<str>>(
    variable: V,
    check_values: C,
    true_value: &'a str,
    false_value: &'a str,
    d: &DataSmart,
) -> ByteBraiseResult<&'a str> {
    let variable = variable.as_ref();
    let check_values = check_values.as_ref();

    if let Some(val) = d.get_var(variable)? {
        let val: String = val.try_into().unwrap(); // TODO error

        let val: HashSet<_> = val.split_whitespace().collect();
        let check_values: HashSet<_> = check_values.split_whitespace().collect();

        return Ok(match check_values.is_subset(&val) {
            true => true_value,
            false => false_value,
        });
    }

    Ok(false_value)
}

lazy_static! {
    static ref PRESERVED_ENVVARS_EXPORTED: Vec<&'static str> = vec![
        "BB_TASKHASH",
        "HOME",
        "LOGNAME",
        "PATH",
        "PWD",
        "SHELL",
        "USER",
        "LC_ALL",
        "BBSERVER",
    ];
    static ref PRESERVED_ENVVARS: Vec<&'static str> = {
        let mut ret = PRESERVED_ENVVARS_EXPORTED.clone();
        ret.extend_from_slice(&[
            "BBPATH",
            "BB_PRESERVE_ENV",
            "BB_ENV_WHITELIST",
            "BB_ENV_EXTRAWHITE",
        ]);
        ret
    };
}

pub fn approved_variables() -> Vec<String> {
    if std::env::var("BB_PRESERVE_ENV").is_ok() {
        return std::env::vars().map(|k| k.0).collect();
    }

    let mut approved: HashSet<String>;
    if let Ok(allowlist) = std::env::var("BB_ENV_WHITELIST") {
        approved = allowlist.split_whitespace().map(String::from).collect();
        approved.insert(String::from("BB_ENV_WHITELIST"));
    } else {
        approved = PRESERVED_ENVVARS
            .clone()
            .iter()
            .map(|s| String::from(*s))
            .collect();
    }

    if let Ok(extra_allow) = std::env::var("BB_ENV_EXTRAWHITE") {
        approved.extend(extra_allow.split_whitespace().map(String::from));
        approved.insert(String::from("BB_ENV_EXTRAWHITE"));
    }

    approved.drain().collect()
}
