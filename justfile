dev:
 nix develop --command fish

snap:
  INSTA_UPDATE=always cargo test

clippy:
  cargo clippy

fmt:
  cargo fmt --all -v

run:
  cargo r -q

watch:
  cargo watch -c -x 'build --all'
