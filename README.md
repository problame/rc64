## TODOs

Notes from the final presentation

- [ ] SDC instruction
- [ ] run the "klaus" 6502 test suite
  - failure is encoded by a self-jumping instruction (the tightest loop you can get)
  - success??? => ask C++ group
- [ ] check ASL behavior: on the real hw, it writes to memory twice:
  - once original value
  - then ASLed value
  - Sometimes used to acknowledge interrupts to VIC
- [ ] Boulder demo brown font
- [ ] Ninja demo (switches text mode & bitmap mode)
- [ ] Run pacman demo (there is one that requires sprites, one that doesn't)

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

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