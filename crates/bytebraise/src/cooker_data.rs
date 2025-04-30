use anyhow::Context;
use std::env::VarError;
use std::path::PathBuf;

use crate::data_smart::variable_contents::VariableContentsAccessors;
use crate::data_smart::DataSmart;
use crate::syntax::ast::evaluate::{inherit, parse_config_file};
use crate::utils::{approved_variables, which};
use crate::ByteBraiseResult;

// TODO: allow passing in BBPATH from yb env
pub fn find_top_dir() -> ByteBraiseResult<Option<PathBuf>> {
    let d = DataSmart::new();
    let bbpath = match std::env::var("BBPATH") {
        Ok(val) => {
            d.set_var("BBPATH", val.clone())?;
            Some(val)
        }
        Err(VarError::NotPresent) => None,
        Err(other_err) => return Err(other_err.into()),
    };

    if let Some(layer_conf) = find_config_file("bblayers.conf", &d)? {
        return Ok(Some(PathBuf::from(
            layer_conf.parent().unwrap().parent().unwrap(),
        )));
    }

    if let Some(bbpath) = bbpath {
        if let Some(bitbakeconf) = which(bbpath, "conf/bitbake.conf", false, false)? {
            return Ok(Some(PathBuf::from(
                bitbakeconf.parent().unwrap().parent().unwrap(),
            )));
        }
    }

    Ok(None)
}

pub fn find_config_file(
    config_file_name: &str,
    d: &DataSmart,
) -> ByteBraiseResult<Option<PathBuf>> {
    let bbpath = d.get_var("BBPATH")?.as_string_or_empty();
    let bbpath_entries = bbpath.split(':').map(PathBuf::from);

    let cwd = std::env::current_dir()?;
    let path_ancestors = cwd.ancestors().map(PathBuf::from);

    for search_path in bbpath_entries.chain(path_ancestors) {
        let candidate_file = search_path.join("conf").join(config_file_name);
        if candidate_file.exists() {
            return Ok(Some(candidate_file.canonicalize().unwrap()));
        }
    }

    Ok(None)
}

pub struct CookerDataBuilder {
    basedata: DataSmart,
    data: DataSmart,
}

impl CookerDataBuilder {
    pub fn new() -> Self {
        // TODO BB_ORIGENV, worker vs server context, multiconfigs
        Self {
            basedata: DataSmart::new(),
            data: DataSmart::new(),
        }
    }

    pub fn parse_base_configuration(&self) -> ByteBraiseResult<()> {
        self.parse_configuration_files()?;

        //let valid: VariableContents = self.data.get_var("BB_INVALIDCONF")?.unwrap();
        //assert!(valid.as_or_default::<bool>());

        Ok(())
    }

    pub fn parse_configuration_files(&self) -> ByteBraiseResult<DataSmart> {
        let data = self.data.create_copy();

        // TODO multiconfig
        data.set_var("BB_CURRENT_MC", "default")?;

        // TODO prefiles

        if let Some(layerconf) = find_config_file("bblayers.conf", &data)? {
            data.set_var("TOPDIR", layerconf.parent().unwrap().parent().unwrap())?;
            parse_config_file(layerconf, &data)?;

            let layers: Vec<_> = data
                .get_var("BBLAYERS")?
                .as_string_or_empty()
                .split_whitespace()
                .map(PathBuf::from)
                .collect();

            let broken_layers = layers
                .iter()
                .filter(|path| !path.is_dir())
                .collect::<Vec<_>>();
            assert!(
                broken_layers.is_empty(),
                "these layers don't exist: {:?}",
                broken_layers
            );

            let approved_variables = approved_variables();
            for mut layer in layers {
                if layer.display().to_string().contains('~')
                    && approved_variables.contains(&String::from("HOME"))
                {
                    layer = layer.canonicalize().unwrap();
                }

                assert!(!layer.display().to_string().ends_with('/'));

                data.set_var("LAYERDIR", layer.clone())?;
                // TODO: use Python re?
                data.set_var("LAYERDIR_RE", regex::escape(layer.to_str().unwrap()))?;

                parse_config_file(layer.join("conf").join("layer.conf"), &data)?;

                data.expand_varref("LAYERDIR")?;
                data.expand_varref("LAYERDIR_RE")?;
            }

            data.del_var("LAYERDIR");
            data.del_var("LAYERDIR_RE");
        } else {
            panic!();
        }

        let _bbfiles_dynamic = data.get_var("BBFILES_DYNAMIC")?.as_string_or_empty();
        let _collections = data.get_var("BBFILE_COLLECTIONS")?.as_string_or_empty();

        let _layerseries = data.get_var("LAYERSERIES_CORENAMES")?.as_string_or_empty();

        //panic!("bbpath: {:?}", data.get_var("BBPATH")?);
        //panic!("{:?}", data.get_var("COREBASE")?);
        parse_config_file(PathBuf::from("conf").join("bitbake.conf"), &data);

        // TODO: post-files

        inherit("base.bbclass", &data)?;

        for bbclass in data
            .get_var("INHERIT")?
            .as_string_or_empty()
            .split_whitespace()
        {
            inherit(bbclass, &data).with_context(|| format!("unable to inherit {}", bbclass))?;
        }

        Ok(data)
    }
}
