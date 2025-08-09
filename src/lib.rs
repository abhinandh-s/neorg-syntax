pub(crate) mod errors;

mod grammer;
#[cfg(feature = "tower-lsp")]
pub mod highlight;
mod kind;
mod lex;
mod node;
mod parser;
mod set;
mod span;

pub use grammer::*;
pub use kind::*;
pub use lex::*;
pub use node::*;
pub use parser::*;
pub use set::*;
pub use span::*;

#[macro_export]
macro_rules! hl {
    ($str:literal) => {
        Some($str)
    };
    () => {
        None
    };
}

/// If an Operation in this while loop took more than 1 second it will panic
#[macro_export]
macro_rules! looper {
    ($cond:expr, $body:block) => {{
        #[cfg(debug_assertions)]
        {
            let start = std::time::Instant::now();
            while $cond {
                if start.elapsed() >= std::time::Duration::from_secs(1) {
                    panic!("Operation took more than 1 sec, possible infinite loop.");
                }
                $body
            }
        }
        #[cfg(not(debug_assertions))]
        {
            while $cond {
                $body
            }
        }
    }};
}

#[cfg(debug_assertions)]
#[track_caller]
pub fn dbg_looper<B>(p: &mut Parser, body: &mut B)
where
    B: FnMut(),
{
    let start = std::time::Instant::now();
    while !p.is_at_eof() {
        if start.elapsed() >= std::time::Duration::from_secs(1) {
            panic!("Operation took more than 1 sec, possible infinite loop.");
        }
        let last_cursor = p.cursor;
        body();
        p.assert_movement(last_cursor);
    }
}

#[cfg(debug_assertions)]
#[track_caller]
pub fn looper<C, B>(cond: C, body: &mut B)
where
    C: Fn() -> bool,
    B: FnMut(),
{
    let start = std::time::Instant::now();
    while cond() {
        if start.elapsed() >= std::time::Duration::from_secs(1) {
            panic!("Operation took more than 1 sec, possible infinite loop.");
        }
        body()
    }
}

#[cfg(not(debug_assertions))]
#[track_caller]
pub fn looper<C, B>(cond: C, body: B)
where
    C: Fn() -> bool,
    B: Fn(),
{
    while cond() {
        body()
    }
}

#[macro_export]
macro_rules! assert_tree {
  ($test_name:ident, $parse_fn:ident, $input:literal) => {
        #[test]
        fn $test_name() {
            let snapshot_path = {
                let root = env!("CARGO_MANIFEST_DIR");
                std::path::Path::new(root)
                    .join("tests")
                    .join("snapshots")
            };

            let mut p = $crate::Parser::new($input);
            $parse_fn(&mut p);
            assert_eq!(p.nodes().len(), 1);

            let output = p.nodes()[0].display();

            // puts it in tests/snapshots/
            // and do not prepend path before snaps name
            insta::with_settings!({ snapshot_path => snapshot_path, prepend_module_to_snapshot => false }, {
                insta::assert_snapshot!(output);
            });
        }
    };

    ($dir_name:ident, $test_name:ident, $parse_fn:ident, $input:literal) => {
        #[test]
        fn $test_name() {
            let snapshot_path = {
                let root = env!("CARGO_MANIFEST_DIR");
                std::path::Path::new(root)
                    .join("tests")
                    .join("snapshots").join(stringify!($dir_name))
            };

            let mut p = $crate::Parser::new($input);
            $parse_fn(&mut p);
            assert_eq!(p.nodes().len(), 1);

            let output = p.nodes()[0].display();

            // puts it in tests/snapshots/
            // and do not prepend path before snaps name
            insta::with_settings!({ snapshot_path => snapshot_path, prepend_module_to_snapshot => false }, {
                insta::assert_snapshot!(output);
            });
        }
    };
}
