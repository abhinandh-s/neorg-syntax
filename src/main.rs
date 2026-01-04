use neorg_syntax::*;

fn main() {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let source = r##"

        `this is */_-!^,bold,^!-_/* some`
        %this is */_-!^,bold,^!-_/* some%
        $this is */_-!^,bold,^!-_/* some$
        this is */_-!^,bold,^!-_/* some
    // {./README.md}[readme]
                                   
                                   "##;

    println!("{}", cst!(source).display());
}
