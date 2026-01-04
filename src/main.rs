use neorg_syntax::*;

fn main() {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let source = r##"
** this is

15:15:47his is *this is somethi

> this is 

.56%^$%^$%^%#$!


sdasda




"##;

    println!("{}", cst!(source).display());

    /*
    cst.collect_semantic_tokens()
        .iter()
        .for_each(|f| println!("{:?}", f));
        if let Some(formatted) = cst.format() {
            println!("Input:\n{}", source);
            println!("Formatted:\n{}", formatted);
        }

    */
}
