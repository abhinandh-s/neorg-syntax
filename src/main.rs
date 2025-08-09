use neorg_syntax::*;

fn main() {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let source = r##"--- this is a unord list"##;
    let mut p = Parser::new(source);

    let cst = document(&mut p);
    println!("{}", cst.display());

    let err = crate::get_errors(cst.clone());
    for i in err {
        println!("{i:?}");
    }

    let n = fmt_list(cst.clone());
    println!("{}", n.display());

    #[cfg(feature = "tower-lsp")]
    {
        cst.provide_semantic_tokens().iter().for_each(|s| {
            println!(
                "{},{},{},{},{}",
                s.delta_line, s.delta_start, s.length, s.token_type, s.token_modifiers_bitset
            );
        });
    }
}
