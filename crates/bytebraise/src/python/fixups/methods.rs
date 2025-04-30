pub fn fixup_oe_import(code: &mut String) {
    let mut lines = code.lines().map(String::from).collect::<Vec<_>>();
    let trimmed_lines = code.lines().map(|line| line.trim()).collect::<Vec<_>>();

    let sys_line = trimmed_lines
        .iter()
        .position(|line| line == &"import sys")
        .unwrap();
    lines.insert(sys_line, String::from("    import os"));

    let path_line = trimmed_lines
        .iter()
        .position(|line| line.starts_with("sys.path[0:0]") && line.contains("bbpath"))
        .unwrap();

    let s = path_line + 2;
    lines.splice(
        s..s,
        vec![
            String::from(
                r#"    sys.path.insert(0, os.path.join(d.getVar("COREBASE"), "bitbake", "lib"))"#,
            ),
            String::from("    import bb"),
        ],
    );

    *code = lines.join("\n");
}
