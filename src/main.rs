use neorg_syntax::*;

fn main() {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let source = r##"sdasdas




*      this
-- this"##;

    let cst = cst!(source);
    println!("{}", cst.display());

    cst.collect_semantic_tokens().iter().for_each(|f| println!("{:?}", f));

    if let Some(formatted) = cst.format() {
        
        println!("Input:\n{}", source);
        println!("Formatted:\n{}", formatted);
    }
}
