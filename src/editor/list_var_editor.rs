use crate::parser::parse_bitbake_from_str;
use crate::syntax::ast::nodes::Root;
use crate::syntax::ast::{AstNode, AstToken};
use crate::syntax::ted::Position;
use crate::syntax::{make, ted};
use crate::ByteBraiseResult;
use anyhow::Context;
use maplit::hashset;
use std::collections::HashSet;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

enum EditActionKind {
    Set,
    Add,
    Remove,
}

// TODO: optionally make set
struct EditAction {
    values: HashSet<String>,
    kind: EditActionKind,
}

pub struct ListVarEditor {
    root: Root,
    path: PathBuf,
    var: String,
    actions: Vec<EditAction>,
}

impl ListVarEditor {
    pub fn from_file<P: AsRef<Path>>(path: P, var: String) -> ByteBraiseResult<Self> {
        let path = path.as_ref();

        let mut source = String::new();
        File::open(&path)
            .with_context(|| format!("failed to read {:?}", &path))?
            .read_to_string(&mut source)?;

        let root = parse_bitbake_from_str(&*source).clone_for_update();
        let assignments = root.identifier_assignments(&var).collect::<Vec<_>>();
        assert!(
            assignments.len() < 2,
            "editing a file with multiple assignments to same variable not supported yet"
        );

        Ok(Self {
            root,
            path: path.to_path_buf(),
            var,
            actions: vec![],
        })
    }

    pub fn add_value(&mut self, val: String) {
        self.actions.push(EditAction {
            kind: EditActionKind::Add,
            values: hashset![val],
        })
    }

    pub fn remove_value(&mut self, val: String) {
        self.actions.push(EditAction {
            kind: EditActionKind::Remove,
            values: hashset![val],
        })
    }

    pub fn commit(&mut self) -> ByteBraiseResult<()> {
        let assignments = self
            .root
            .identifier_assignments(&self.var)
            .collect::<Vec<_>>();

        // TODO split value
        let values: HashSet<String> = match assignments.len() {
            0 => hashset![],
            1 => assignments
                .first()
                .unwrap()
                .right()
                .lines()
                .map(|v| v.trim().to_owned())
                .filter(|v| !v.is_empty())
                .collect(),
            _ => unreachable!(),
        };

        let values = self
            .actions
            .iter()
            .fold(values, |mut values, action| match action.kind {
                EditActionKind::Set => action.values.clone(),
                EditActionKind::Add => {
                    values.extend(action.values.clone().into_iter());
                    values
                }
                EditActionKind::Remove => {
                    values.retain(|v| !action.values.contains(v));
                    values
                }
            });

        // TODO optional sort
        let mut values = values.into_iter().collect::<Vec<_>>();
        values.sort();

        let value_node = make::quoted_value_from_slice(&values);

        if assignments.is_empty() {
            let last_node = self.root.syntax().last_child_or_token().unwrap();

            // TODO operator
            let new_assignment = make::assignment(
                self.var.to_string(),
                String::from("?="),
                value_node.syntax().to_string(),
            )
            .clone_for_update();

            ted::insert(Position::after(last_node), new_assignment.syntax());
        } else {
            let assignment = assignments.first().unwrap();
            ted::replace(assignment.right().syntax(), value_node.syntax());
        }

        let mut f = OpenOptions::new()
            .write(true)
            .truncate(true)
            .open(&self.path)?;
        f.write_all(self.root.syntax().to_string().as_bytes())?;

        Ok(())
    }
}
