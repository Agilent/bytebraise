#[cfg(test)]
use crate::{evaluate::eval, macros::get_var};

#[test]
fn del_var() {
    let mut d = eval(
        r#"
TEST = "1"
TEST:a = "2"
TEST:a:b = "3"
"#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "1");
    assert_eq!(get_var!(&d, "TEST:a").unwrap(), "2");
    assert_eq!(get_var!(&d, "TEST:a:b").unwrap(), "3");
    d.del_var("TEST:a").unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "1");
    assert_eq!(get_var!(&d, "TEST:a"), None);
    assert_eq!(get_var!(&d, "TEST:a:b").unwrap(), "3");
}
