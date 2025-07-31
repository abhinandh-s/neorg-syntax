use neorg_syntax::*;

fn main() {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let source = include_str!("../examples/tests/implemented.norg");
    let mut p = Parser::new(source);

    let cst = document(&mut p);
    println!("{}", cst.display());

    let err = crate::get_errors(cst);
    for i in err {
        println!("{i:?}");
    }
}
