# Resources

- Kernal and BASIC ROM disassembly that matches what we have in this tree: http://www.unusedino.de/ec64/technical/misc/c64/romlisting.html#A408
- Memory Map
  -  https://sta.c64.org/cbm64mem.html
- Instruction Encoding
  - https://www.c64-wiki.com/wiki/Opcode
- VICE wrapper that adds useful memory viz, etc: https://sourceforge.net/projects/c64-debugger/
- vic-2.txt

# Random Vice Emulator Knowledge

## Run vice with breakpoint right when it starts to jump into a loaded PRG (SYS command)
```
../vice-3.3/install/bin/x64 -kernal rc64/rsrc/kernal.img -basic rc64/rsrc/basic_rom.img  -remotemonitor  -native-monitor -initbreak 0xe144 PRG_PATH
```

## Extract PRGs From A  Disk Image File

```
cd extracttothisdir
../vice-3.3/install/bin/c1541 PRG_FILE
> extract
```
