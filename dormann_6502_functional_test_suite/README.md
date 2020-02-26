This directory contains pre-built versions of Klaus Dormann's 6502 functional test suite,
with different settings for the `disable_decimal` configuration flag (BCD mode).

## How to build these files yourself

* Get a **Windows** machine (a 32-bit Linux machine might also do the trick, untested though)
* Untar the tarball in this dir, unzip the as65_142.zip
* Assemble:
    ```
    as65.exe -l -m -w -h0 6502_functional_test.a65
    ```
* => the resulting file is `6502_functional_test.bin`
* Pad the bin-file ([see GitHub issue](https://github.com/Klaus2m5/6502_65C02_functional_tests/issues/11)):
  ```
  dd if=6502_functional_test.bin of=6502_functional_test.bin.padded bs=1 seek=10
  ```

