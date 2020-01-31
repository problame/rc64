RASTER = $20

;*** Startadresse 
*=$0801
;** BASIC-Zeile: 2018 SYS 2064:NEW
 !word main-2, 2018 
 !byte $9e
 !text " 2064:"
 !byte $a2,$00,$00,$00

main
 sei                                ;IRQs sperren

 lda #<myIRQ                        ;Adresse unserer Routine in
 sta $0314                          ;den RAM-Vektor
 lda #>myIRQ
 sta $0315

 lda #%00000001                     ;Raster-IRQs vom VIC-II aktivieren
 sta $d01a

 lda #RASTER                        ;Hier soll unsere Linie erscheinen
 sta $d012                      

 lda $d011                          ;Zur Sicherheit höchstes BIT
 and #%01111111                     ;für die Rasterzeile löschen
 sta $d011

 lda #%01111111                     ;Timer-IRQs abschalten
 sta $dc0d
 lda $dc0d                          ;zur Sicherheit bestätigen

 lda #%00000001                     ;Sicherheitshalber auch den
 sta $d019                          ;Raster-IRQ bestätigen

 cli                                ;Interrupts erlauben

 rts                                ;zurück zum BASIC



;*** Platz für ein kleines BASIC-Programm lassen: 
;*** z. B. 10 PRINT "HALLO ";:GOTO 10 
!align 1023,0
myIRQ

myIRQMain 
 lda #$01                           ;weiß in den Akku 
 sta $d020                          ;Rahmenfarbe setzen 
 ldx #$0a                           ;            2TZ
loop2 
 dex                                ;10 * 2TZ = 20TZ 
 bne loop2                          ; 9 * 3TZ = 27TZ 
                                    ; 1 * 2TZ =  2TZ 
 nop                                ;            2TZ 
 nop                                ;            2TZ 
 nop                                ;            2TZ 
 lda #$00                           ;            2TZ ;schwarz in den Akku 
 sta $d020                          ;            4TZ ;Rahmenfarbe setzen 
                                    ;=============== 
                                    ;           63TZ 
 lda #%00000001                     ;IRQ bestätigen 
 sta $d019 
 jmp $ea31                          ;zum Schluß zum 'Timer-Interrupt' springen