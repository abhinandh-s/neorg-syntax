#![allow(unused)]

use neorg_syntax::*;

fn main() {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let _doc_meta = r#"
    @document.meta
    @end
    "#;
    let source = include_str!("../examples/tests/heading.norg");

    let italics01 = "/ this is not italics/";
    let italics02 = "/this is not italics /";
    let italics03 = "/this is italics/";
    let italics04 = "/ this is not italics /";
    let heading = "!%/*this is a nested inline text*/%!";

    let mut p = Parser::new("* this is a heading\nThis");

    let cst = document(&mut p).display();
    println!("{cst}");
}
