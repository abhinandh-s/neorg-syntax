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
    
    let mut p = Parser::new("this is a ");

    // print_cst(source);

    // let mut emph_nodes = Vec::new();
    // collect_error_nodes(&p.nodes, &mut emph_nodes);
    // println!("\n== Found {} error nodes ==", emph_nodes.len());
    // for (i, emph) in emph_nodes.iter().enumerate() {
    //     println!("ERROR {}: {:?}", i, emph);
    // }
    //
    // let mut ranges = Vec::new();
    // collect_error_ranges(&p.nodes, source, &mut ranges);
    //
    // for range in &ranges {
    //     println!("LSP error range: {:?}", range);
    // }
}
