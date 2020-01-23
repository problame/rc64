; https://digitalerr0r.net/2011/04/30/commodore-64-programming-6-raster-interrupts/


* = $0801                               ; BASIC starts at #2049
!byte $0d,$08,$dc,$07,$9e,$20,$34,$39   ; BASIC to load $c000
!byte $31,$35,$32,$00,$00,$00           ; inserts BASIC line: 2012 SYS 49152
* = $c000     				            ; start address for 6502 code


loop
    lda $d012
    cmp #$ff
    bne    loop
    inc $d021
    jmp loop