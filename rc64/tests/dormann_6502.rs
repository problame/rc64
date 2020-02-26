// only run in release mode
#[cfg(not(debug_assertions))]
#[test]
fn dormann_6502_functional_test_suite_no_bcd() {
    let reference_output = r#"
writing RAM length=0x10000
self-jump reached, stopping emulator
cpu stopped, exiting emulator
emulator ran 57267961 cycles, executed 26764109 instrs 
    "#
    .trim();

    assert_cli::Assert::main_binary()
        .with_args(&[
            "--autostart-file-type",
            "bin-0x0400",
            "../dormann_6502_functional_test_suite/disable_decimal=1/6502_functional_test.bin.padded",
            "--disable-clock-freq-limit",
            "--stop-at-self-jmp",
            "336d",
            "--non-interactive",
            "--no-gui",
            "--exit-after-cycles",
            "57267962",
        ])
        .stderr()
        .is(reference_output)
        .unwrap();
}
