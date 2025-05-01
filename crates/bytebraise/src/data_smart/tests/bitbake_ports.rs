// Other macros rely on this
#[cfg(test)]
use crate::basic_datasmart_test;
#[cfg(test)]
use crate::data_smart::*;
#[cfg(test)]
use crate::ported_datasmart_concat_test;
#[cfg(test)]
use crate::ported_datasmart_expansion_test;
#[cfg(test)]
use crate::ported_datasmart_flag_test;
#[cfg(test)]
use crate::ported_datasmart_overrides_test;
#[cfg(test)]
use crate::ported_datasmart_test;

// DataExpansions
#[cfg(test)]
mod data_expansions {
    use std::assert_matches::assert_matches;

    use DataSmartError::RecursiveReferenceError;

    use super::*;

    ported_datasmart_test!(one_var, d, {
        assert_eq!(
            d.expand("${foo}")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "value_of_foo"
        );
    });

    ported_datasmart_test!(indirect_one_var, d, {
        assert_eq!(
            d.expand("${${foo}}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "value_of_'value_of_foo'"
        );
    });

    ported_datasmart_test!(indirect_and_another, d, {
        assert_eq!(
            d.expand("${${foo}} ${bar}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "value_of_'value_of_foo' value_of_bar"
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_test!(python_snippet, d, {
        assert_eq!(
            d.expand("${@5*12}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "60"
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_test!(expand_in_python_snippet, d, {
        assert_eq!(
            d.expand("${@'boo ' + '${foo}'}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "boo value_of_foo"
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_test!(python_snippet_getvar, d, {
        assert_eq!(
            d.expand("${@d.getVar('foo') + ' ${bar}'}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "value_of_foo value_of_bar"
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_test!(python_unexpanded, d, {
        d.set_var("bar", "${unsetvar}")?;
        let val = d
            .expand("${@d.getVar('foo') + ' ${bar}'}")?
            .ok_or(DataSmartError::UnwrapNoneError)?;
        assert_eq!(val, "${@d.getVar('foo') + ' ${unsetvar}'}");
    });

    #[cfg(feature = "python")]
    ported_datasmart_test!(python_snippet_syntax_error, d, {
        d.set_var("FOO", "${@foo = 5}")?;
        let val = d.get_var("FOO");
        use DataSmartError::PythonSyntaxError;
        assert_matches!(
            val.map_err(|e| e.downcast()),
            Err(Ok(PythonSyntaxError { .. }))
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_test!(python_snippet_runtime_error, d, {
        d.set_var("FOO", "${@int('test')}")?;
        let val = d.get_var("FOO");
        use pyo3::exceptions::PyValueError;
        use pyo3::{PyErr, PyResult, Python};
        let py_result: PyResult<_> = val.map_err(|e| e.downcast::<PyErr>().unwrap());
        Python::with_gil(
            |py| assert_matches!(py_result, Err(e) if e.is_instance_of::<PyValueError>(py)),
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_test!(python_snippet_error_path, d, {
        d.set_var("FOO", "foo value ${BAR}")?;
        d.set_var("BAR", "bar value ${@int('test')}")?;
        let val = d.get_var("FOO");
        use pyo3::exceptions::PyValueError;
        use pyo3::{PyErr, PyResult, Python};
        let py_result: PyResult<_> = val.map_err(|e| e.downcast::<PyErr>().unwrap());
        Python::with_gil(
            |py| assert_matches!(py_result, Err(e) if e.is_instance_of::<PyValueError>(py)),
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_test!(value_containing_value, d, {
        assert_eq!(
            d.expand("${@d.getVar('foo') + ' ${bar}'}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "value_of_foo value_of_bar"
        );
    });

    ported_datasmart_test!(reference_undefined_val, d, {
        assert_eq!(
            d.expand("${undefinedvar} meh")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "${undefinedvar} meh"
        );
    });

    ported_datasmart_test!(double_reference, d, {
        d.set_var("BAR", "bar value")?;
        d.set_var("FOO", "${BAR} foo ${BAR}")?;
        assert_eq!(
            d.get_var("FOO")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "bar value foo bar value"
        );
    });

    ported_datasmart_test!(direct_recursion, d, {
        d.set_var("FOO", "${FOO}")?;
        let val = d.get_var("FOO");
        assert_matches!(val.map_err(|e| e.downcast()), Err(Ok(RecursiveReferenceError { var })) if var == "FOO");
    });

    ported_datasmart_test!(indirect_recursion, d, {
        d.set_var("FOO", "${BAR}")?;
        d.set_var("BAR", "${BAZ}")?;
        d.set_var("BAZ", "${FOO}")?;
        let val = d.get_var("FOO");
        assert_matches!(val.map_err(|e| e.downcast()), Err(Ok(RecursiveReferenceError { var })) if var == "BAR");
    });

    #[cfg(feature = "python")]
    ported_datasmart_test!(recursion_exception, d, {
        d.set_var("FOO", "${BAR}")?;
        d.set_var("BAR", "${${@'FOO'}}")?;
        let val = d.get_var("FOO");
        assert_matches!(val.map_err(|e| e.downcast()), Err(Ok(RecursiveReferenceError { var })) if var == "BAR");
    });

    ported_datasmart_test!(incomplete_varexp_single_quotes, d, {
        d.set_var("FOO", "sed -i -e 's:IP{:I${:g' $pc")?;
        assert_eq!(
            d.get_var("FOO")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "sed -i -e 's:IP{:I${:g' $pc"
        );
    });

    // TOOD: irrelevant?
    // ported_datasmart_test!(nonstring, d,  {
    //     let mut d = d;
    //     d.set_var("TEST", 5, false);
    //     let val = d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?;
    //     assert_eq!(val, "5");
    // })

    ported_datasmart_test!(rename, d, {
        d.rename_var("foo", "newfoo")?;
        assert_eq!(
            d.get_var("newfoo")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "value_of_foo"
        );
        assert!(d.get_var("foo")?.is_none());
    });

    ported_datasmart_test!(deletion, d, {
        d.del_var("foo");
        assert!(d.get_var("foo")?.is_none());
    });

    ported_datasmart_test!(keys, d, {
        let mut keys = d.keys()?;
        let mut expected = vec!["value_of_foo", "foo", "bar"];
        keys.sort();
        expected.sort_unstable();
        assert_eq!(keys, expected);
    });

    ported_datasmart_test!(keys_deletion, d, {
        // TODO this is slightly different from the actual BitBake test - does it matter?
        d.del_var("bar");
        let mut keys = d.keys()?;
        let mut expected = vec!["value_of_foo", "foo", "bar"];
        keys.sort();
        expected.sort_unstable();
        assert_eq!(keys, expected);
    });
}

#[cfg(test)]
mod nested_expansions {
    use super::*;

    // TestNestedExpansions
    ported_datasmart_expansion_test!(refs, d, {
        assert_eq!(
            d.expand("${value_of_${foo}${bar}}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "187"
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_expansion_test!(in_python_ref, d, {
        assert_eq!(
            d.expand("${@'${foo}' + 'bar'}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "foobar"
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_expansion_test!(python_ref_in_ref, d, {
        assert_eq!(
            d.expand("${${@'f'+'o'+'o'}}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "foo"
        );
    });

    ported_datasmart_expansion_test!(deep_nesting, d, {
        let depth = 100;
        let expression = format!("{}foo{}", "${".repeat(depth), "}".repeat(depth));
        assert_eq!(
            d.expand(&expression)?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "foo"
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_expansion_test!(mixed, d, {
        assert_eq!(
            d.expand("${value_of_${@('${foo}'+'bar')[0:3]}${${@'BAR'.lower()}}}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "187"
        );
    });

    #[cfg(feature = "python")]
    ported_datasmart_expansion_test!(runtime, d, {
        assert_eq!(
            d.expand("${${@'value_of' + '_f'+'o'+'o'+'b'+'a'+'r'}}")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "187"
        );
    });
}

// TestMemoize - OMITTED (not applicable to Rust)

// TestConcat
#[cfg(test)]
mod concat {
    use super::*;

    ported_datasmart_concat_test!(prepend, d, {
        d.set_var("TEST", "${VAL}")?;
        d.prepend_var("TEST", "${FOO}:")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "foo:val"
        );
    });

    ported_datasmart_concat_test!(append, d, {
        d.set_var("TEST", "${VAL}")?;
        d.append_var("TEST", ":${BAR}")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "val:bar"
        );
    });

    ported_datasmart_concat_test!(multiple_append, d, {
        d.set_var("TEST", "${VAL}")?;
        d.prepend_var("TEST", "${FOO}:")?;
        d.append_var("TEST", ":val2")?;
        d.append_var("TEST", ":${BAR}")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "foo:val:val2:bar"
        );
    });
}

// TestConcat
#[cfg(test)]
mod concat_override {
    use super::*;

    ported_datasmart_concat_test!(prepend, d, {
        d.set_var("TEST", "${VAL}")?;
        d.set_var("TEST_prepend", "${FOO}:")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "foo:val"
        );
    });

    ported_datasmart_concat_test!(append, d, {
        d.set_var("TEST", "${VAL}")?;
        d.set_var("TEST_append", ":${BAR}")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "val:bar"
        );
    });

    ported_datasmart_concat_test!(multiple_append, d, {
        d.set_var("TEST", "${VAL}")?;
        d.set_var("TEST_prepend", "${FOO}:")?;
        d.set_var("TEST_append", ":val2")?;
        d.set_var("TEST_append", ":${BAR}")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "foo:val:val2:bar"
        );
    });

    ported_datasmart_concat_test!(append_unset, d, {
        d.set_var("TEST_prepend", "${FOO}:")?;
        d.set_var("TEST_append", ":val2")?;
        d.set_var("TEST_append", ":${BAR}")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "foo::val2:bar"
        );
    });

    ported_datasmart_concat_test!(remove, d, {
        d.set_var("TEST", "${VAL} ${BAR}")?;
        d.set_var("TEST_remove", "val")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            " bar"
        );
    });

    ported_datasmart_concat_test!(remove_cleared, d, {
        d.set_var("TEST", "${VAL} ${BAR}")?;
        d.set_var("TEST_remove", "val")?;
        d.set_var("TEST", "${VAL} ${BAR}")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "val bar"
        );
    });

    ported_datasmart_concat_test!(remove_inactive_override, d, {
        d.set_var("TEST", "${VAL} ${BAR}    123")?;
        d.set_var("TEST_remove_inactiveoverride", "val")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "val bar    123"
        );
    });

    ported_datasmart_concat_test!(doubleref_remove, d, {
        d.set_var("TEST", "${VAL} ${BAR}")?;
        d.set_var("TEST_remove", "val")?;
        d.set_var("TEST_TEST", "${TEST} ${TEST}")?;
        assert_eq!(
            d.get_var("TEST_TEST")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            " bar  bar"
        );
    });

    ported_datasmart_concat_test!(empty_remove, d, {
        d.set_var("TEST", "")?;
        d.set_var("TEST_remove", "val")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            ""
        );
    });

    ported_datasmart_concat_test!(remove_expansion, d, {
        d.set_var("BAR", "Z")?;
        d.set_var("TEST", "${BAR}/X Y")?;
        d.set_var("TEST_remove", "${BAR}/X")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            " Y"
        );
    });

    ported_datasmart_concat_test!(remove_expansion_items, d, {
        d.set_var("TEST", "A B C D")?;
        d.set_var("BAR", "B D")?;
        d.set_var("TEST_remove", "${BAR}")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "A  C "
        );
    });

    ported_datasmart_concat_test!(remove_preserve_whitespace, d, {
        d.set_var("TEST", " A B")?;
        d.set_var("TEST_remove", "C")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            " A B"
        );
    });

    ported_datasmart_concat_test!(remove_preserve_whitespace2, d, {
        d.set_var("TEST", " A B")?;
        d.set_var("TEST_remove", "B")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            " A "
        );
    });
}

#[cfg(test)]
mod overrides {
    use super::*;

    ported_datasmart_overrides_test!(no_override, d, {
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvalue"
        );
    });

    ported_datasmart_overrides_test!(one_override, d, {
        d.set_var("TEST_bar", "testvalue2")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvalue2"
        );
    });

    ported_datasmart_overrides_test!(one_override_unset, d, {
        d.set_var("TEST2_bar", "testvalue2")?;
        assert_eq!(
            d.get_var("TEST2")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvalue2"
        );
        let mut keys = d.keys()?;
        let mut expected = vec!["TEST", "TEST2", "OVERRIDES", "TEST2_bar"];
        keys.sort();
        expected.sort_unstable();
        assert_eq!(keys, expected);
    });

    ported_datasmart_overrides_test!(multiple_override, d, {
        d.set_var("TEST_bar", "testvalue2")?;
        d.set_var("TEST_local", "testvalue3")?;
        d.set_var("TEST_foo", "testvalue4")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvalue3"
        );
        let mut keys = d.keys()?;
        let mut expected = vec!["TEST", "TEST_foo", "OVERRIDES", "TEST_bar", "TEST_local"];
        keys.sort();
        expected.sort_unstable();
        assert_eq!(keys, expected);
    });

    ported_datasmart_overrides_test!(multiple_combined_overrides, d, {
        d.set_var("TEST_local_foo_bar", "testvalue3")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvalue3"
        );
    });

    ported_datasmart_overrides_test!(multiple_overrides_unset, d, {
        d.set_var("TEST2_local_foo_bar", "testvalue3")?;
        assert_eq!(
            d.get_var("TEST2")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvalue3"
        );
    });

    ported_datasmart_overrides_test!(keyexpansion_override, d, {
        d.set_var("LOCAL", "local")?;
        d.set_var("TEST_bar", "testvalue2")?;
        d.set_var("TEST_${LOCAL}", "testvalue3")?;
        d.set_var("TEST_foo", "testvalue4")?;
        d.expand_keys()?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvalue3"
        );
    });

    ported_datasmart_overrides_test!(rename_override, d, {
        d.set_var("ALTERNATIVE_ncurses-tools_class-target", "a")?;
        d.set_var("OVERRIDES", "class-target")?;
        d.rename_var(
            "ALTERNATIVE_ncurses-tools",
            "ALTERNATIVE_lib32-ncurses-tools",
        )?;
        assert_eq!(
            d.get_var("ALTERNATIVE_lib32-ncurses-tools")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "a"
        );
    });

    ported_datasmart_overrides_test!(underscore_override, d, {
        d.set_var("TEST_bar", "testvalue2")?;
        d.set_var("TEST_some_val", "testvalue3")?;
        d.set_var("TEST_foo", "testvalue4")?;
        d.set_var("OVERRIDES", "foo:bar:some_val")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvalue3"
        );
    });

    ported_datasmart_overrides_test!(remove_with_override, d, {
        d.set_var("TEST_bar", "testvalue2")?;
        d.set_var("TEST_some_val", "testvalue3 testvalue5")?;
        d.set_var("TEST_some_val_remove", "testvalue3")?;
        d.set_var("TEST_foo", "testvalue4")?;
        d.set_var("OVERRIDES", "foo:bar:some_val")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            " testvalue5"
        );
    });

    ported_datasmart_overrides_test!(append_and_override_1, d, {
        d.set_var("TEST_append", "testvalue2")?;
        d.set_var("TEST_bar", "testvalue3")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvalue3testvalue2"
        );
    });

    ported_datasmart_overrides_test!(append_and_override_2, d, {
        d.set_var("TEST_append_bar", "testvalue2")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvaluetestvalue2"
        );
    });

    ported_datasmart_overrides_test!(append_and_override_3, d, {
        d.set_var("TEST_bar_append", "testvalue2")?;
        assert_eq!(
            d.get_var("TEST")?.ok_or(DataSmartError::UnwrapNoneError)?,
            "testvalue2"
        );
    });

    ported_datasmart_overrides_test!(underscore_override_2, d, {
        d.set_var("TARGET_ARCH", "x86_64")?;
        d.set_var("PN", "test-${TARGET_ARCH}")?;
        d.set_var("VERSION", "1")?;
        d.set_var("VERSION_pn-test-${TARGET_ARCH}", "2")?;
        d.set_var("OVERRIDES", "pn-${PN}")?;
        d.expand_keys()?;
        assert_eq!(
            d.get_var("VERSION")?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "2"
        );
    });
}

#[cfg(test)]
mod key_expansion {
    // TODO need to get warnings in place first
}

#[cfg(test)]
mod flags {
    use super::*;

    ported_datasmart_flag_test!(setflag, d, {
        assert_eq!(
            d.get_var_flag_contents("foo", "flag1", GetVarOptions::default().expand(false))?
                .ok_or(DataSmartError::UnwrapNoneError)?,
            "value of flag1"
        );
    });

    ported_datasmart_flag_test!(delflag, d, {
        d.del_var_flag("foo", "flag2");
        assert!(
            d.get_var_flag_contents("foo", "flag2", GetVarOptions::default().expand(false))?
                .is_none()
        );
    });
}
