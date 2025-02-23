use neorg_syntax::parser::Parser;

fn main() {
    let input = "* Main Heading\nSome _italic_ text\n- List item\n@ metadata";
    let mut parser = Parser::new(input);
    parser.parse().children().for_each(|f| {
        if f.children().len() != 0 {
            f.children().for_each(|i| {
                println!("{:?}: {}", i, i.span());
            });
        }
        println!("{:?}: {}", f, f.span());
    });
}
