use neorg_syntax::*;

fn main() {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let source = r##"* Heading 1

this is /italic/ ds.

> this is quote 

i was soo fluminous.
"##;
    let mut p = Parser::new(source);

    let cst = document(&mut p);
    println!("{}", cst.display());

    let err = crate::get_errors(cst.clone());
    for i in err {
        println!("{i:?}");
    }

    #[cfg(feature = "tower-lsp")]
    {
        let mut hl = crate::highlight::Highlight::new(cst);
        let res = hl.get();
        println!("{:#?}", res);
    }
}
