#[macro_export]
macro_rules! basic_datasmart_test {
    ($test_name:ident, $d:ident, $expression:block) => {
        paste::item! {
            #[test]
            fn [<test_ $test_name>]() -> DataSmartResult<()> {
                #[allow(unused_mut)]
                let mut $d = DataSmart::new();
                $expression
                Ok(())
            }
        }
    };
}

#[macro_export]
macro_rules! ported_datasmart_test {
    ($test_name:ident, $d:ident, $expression:block) => {
        basic_datasmart_test!($test_name, $d, {
            $d.set_var("foo", "value_of_foo")?;
            $d.set_var("bar", "value_of_bar")?;
            $d.set_var("value_of_foo", "value_of_'value_of_foo'")?;
            $expression
        });
    };
}

#[macro_export]
macro_rules! ported_datasmart_expansion_test {
    ($test_name:ident, $d:ident, $expression:block) => {
        basic_datasmart_test!($test_name, $d, {
            $d.set_var("foo", "foo")?;
            $d.set_var("bar", "bar")?;
            $d.set_var("value_of_foobar", "187")?;
            $expression
        });
    };
}

#[macro_export]
macro_rules! ported_datasmart_concat_test {
    ($test_name:ident, $d:ident, $expression:block) => {
        basic_datasmart_test!($test_name, $d, {
            $d.set_var("FOO", "foo")?;
            $d.set_var("VAL", "val")?;
            $d.set_var("BAR", "bar")?;
            $expression
        });
    };
}

#[macro_export]
macro_rules! ported_datasmart_overrides_test {
    ($test_name:ident, $d:ident, $expression:block) => {
        basic_datasmart_test!($test_name, $d, {
            $d.set_var("OVERRIDES", "foo:bar:local")?;
            $d.set_var("TEST", "testvalue")?;
            $expression
        });
    };
}

#[macro_export]
macro_rules! ported_datasmart_flag_test {
    ($test_name:ident, $d:ident, $expression:block) => {
        basic_datasmart_test!($test_name, $d, {
            $d.set_var("foo", "value of foo")?;
            $d.set_var_flag("foo", "flag1", "value of flag1")?;
            $d.set_var_flag("foo", "flag2", "value of flag2")?;
            $expression
        });
    };
}
