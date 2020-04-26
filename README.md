## Introduction

Have a look at our [final presentation](https://docs.google.com/presentation/d/1JdwHQDP4C2LULtR_oQqrcnfqxXkiI-fZZ7yfw_HaI9g/edit?usp=sharing) for an introduction.

## TODO

- [ ] Joystick configuration (boulder dash needs Joystick 1, joy.basic needs Joystick 2)

## Highlights

- [x] MOS6510 implementation passes Klaus Dormann's 6502 functional test suite (no BCD mode)
  - just run `cargo test --release`
- [x] Built-in Monitor Monitor
  - PC and EA breakpoints
  - Raster-IRQ breakpoints
  - Raster-beam visualization
  - MOS and VIC state inspection
  - Memory inspection & manipulation
  - Instruction tracing
- [x] PRG Autostarter - just pass the path to the PRG you want to start on the command line
- [x] Cycle-accurate instruction execution duration
  - architectural behavior of each instruction applied in first cycle, though
- [x] Joystick Support - try it with `./rc64/prg_tests/joy.prg`
- [x] Rust Memory-Safety

### What Works And What Doesn't

- [x] Kernal Boot-up & BASIC prompt input
- [x] Start and run BASIC programs in `./rc64/prg_tests`
- [x] Raster IRQ demos in `./rc64/asm_tests`
- [x] Boulder PRG
- [ ] Ninja PRG
  - no sprite support
- [ ] Pacman PRG

## Build Instructions

### Linux Binaries

```bash
cargo build --release
stat target/release/rc64
```

### Windows Binaries (cross-built from Linux using `cross`)

```bash
cargo install cross
sudo setfacl -m u:$(id -u):rwx /var/run/docker.sock
cross build --release --target 86_64-pc-windows-gnu
stat target/x86_64-pc-windows-gnu/release/rc64.exe
```

## Usage

Boot to C64 prompt

```
target/release/rc64
```

Start & autoload a PRG file

```
target/release/rc64 path/to/game.prg
```

Run test suite

```
cargo test --release
```

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