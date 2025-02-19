dev:
 nix develop --command fish

clippy:
  cargo clippy

fmt:
  cargo fmt --all -v

run:
  cargo r -q

watch:
  cargo watch -c -x 'build --all'
