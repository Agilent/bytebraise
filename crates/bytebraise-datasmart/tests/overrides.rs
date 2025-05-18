use bytebraise_datasmart::evaluate::eval;
use bytebraise_datasmart::get_var;

#[test]
fn test_operators() {
    let d = eval(
        r#"
A[a] = "1"
        "#,
    );

    assert_eq!(get_var!(&d, "A").unwrap(), "c1");
}
