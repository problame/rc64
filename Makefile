.PHONY: test run build format

test:
	cargo test --release --all-targets

run:
	cargo run --release

build:
	cargo build --release

format:
	cargo fmt
