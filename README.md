## TODOs

- [ ] check ASL behavior: on the real hw, it writes to memory twice:
  - once original value
  - then ASLed value
  - Sometimes used to acknowledge interrupts to VIC
- [ ] Boulder demo brown font
- [ ] Ninja demo (switches text mode & bitmap mode)
- [ ] Run pacman demo (there is one that requires sprites, one that doesn't)
- [ ] Fill Highlights section

## Highlights

- [x] Passes Klaus Dormann's 6502 functional test suite (no BCD mode)

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

Above licensing does not apply to the vendored 6502 functional test suite in the sub-directory `dormann_6502_functional_test_suite`, which is licensed under *GPLv3* (see `dormann_6502_functional_test_suite/vendor_upstream_6502_functional_test_suite_fe99e5616243a1bdbceaf5907390ce4443de7db0.tar`)

## Performance Measurement

```

# make sure Cargo.toml has profile.release.debug = true
cargo build --release

NUM_CYCLES=50000000

# host instructions / guest cycles
perf stat  target/release/rc64 \
    --exit-after-cycles $NUM_CYCLES --disable-clock-freq-limit \
    PATH_TO_PRG
# => compute yourself

# flame graph
perf record -D 5000 --call-graph=dwarf  target/release/rc64 \
    --exit-after-cycles $NUM_CYCLES --disable-clock-freq-limit \
    PATH_TO_PRG
cargo install inferno
perf script| inferno-collapse-perf | inferno-flamegraph > flamegraph.svg

```