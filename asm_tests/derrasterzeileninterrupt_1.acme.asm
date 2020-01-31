RASTER          = $2f               ;Hier beginnen die Linien

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

 lda #RASTER                        ;Hier soll unser Interrupt auftreten
 sta $d012                      

 lda $d011                          ;Zur Sicherheit höchstes BIT
 and #%01111111                     ;für die Rasterzeile löschen

 ;*** TEST TEST TEST ***
 ;and #%11101111                     ;BS-Ausgabe abschalten
 ;*** TEST TEST TEST ***

 sta $D011

 lda #%0111111                      ;Timer-IRQs abschalten
 sta $dc0d
 lda $dc0d                          ;zur Sicherheit bestätigen

 lda #%00000001                     ;Sicherheitshalber auch den
 sta $d019                          ;Raster-IRQ bestätigen

 cli                                ;Interrupts erlauben

 rts                                ;zurück zum BASIC



!align 1023,0
myIRQ
;*** Wenn wir hier landen, sind bereits 38-45 Taktzyklen in der
;*** aktuellen Rasterzeile (ab jetzt als STARTROW bezeichnet) vergangen!

;*** zweiten IRQ einrichten
;*** da die Zeit bei aktivierten ROM nicht reicht, können wir den
;*** 2. Raster-IRQ erst in der übernächsten Zeile (STARTROW+2) bekommen.
 lda #<doubleIRQ                    ;(2 TZ) 2. Raster-IRQ einrichten
 sta $0314                          ;(4 TZ)
 lda #>doubleIRQ                    ;(2 TZ)
 sta $0315                          ;(4 TZ)
 tsx                                ;(2 TZ) Stackpointer im X-Reg. retten
 stx doubleIRQ+1                    ;(4 TZ) und fürs zurückholen sichern!
 nop                                ;(2 TZ)
 nop                                ;(2 TZ)
 nop                                ;(2 TZ)
 lda #%00000001                     ;(2 TZ) 1. Raster-IRQ später bestätigen
                                    ;------
                                    ;26 TZ
;*** Jetzt sind 64-71 Taktzyklen vergangen und wir sind
;*** auf jeden Fall in nächsten Rasterzeile (STARTROW+1)!
;*** Verbraucht wurden dort 1-8 TZ
 inc $D012                          ;(6 TZ) 2. IRQ in der übernächsten Zeile STARTROW+2
                                    ;       $D012 wurde bereits automatisch erhöht
 sta $D019                          ;(4 TZ) IRQ bestätigen
 cli                                ;(2 TZ) Interrupts für den 2. Raster-IRQ
                                    ;       wieder freigeben

;*** Wir befinden uns immer noch in Rasterzeile STARTROW+1 und 
;*** haben bisher 13-20 Zyklen verbraucht

;*** etwas Zeit verschwenden...
 ldx #$08                           ;            2 TZ
 dex                                ;8 * 2 TZ = 16 TZ
 bne *-1                            ;7 * 3 TZ = 21 TZ
                                    ;1 * 2 TZ =  2 TZ
                                    ;          ------
                                    ;           41 TZ

;*** Bis hier sind 54-61 Taktzyklen vergangen, jetzt auf den IRQ warten...
;*** Der nächste Rasterinterrupt wird während dieser NOPs auftreten!
 nop                                ;2 TZ (56)
 nop                                ;2 TZ (58)
 nop                                ;2 TZ (60)
 nop                                ;2 TZ (62)
 nop                                ;2 TZ (64)
 nop                                ;2 TZ (66)

doubleIRQ
;*** Wir sind nun in Rasterzeile STARTROW+2 und
;*** haben bisher genau 38 oder 39 Taktzyklen benötigt!!
;*** Wir können so sicher sein, da der IRQ während der NOPs auftrat.

;*** Jetzt exakt soviele Taktzyklen 'verschwenden', wie in 
;*** dieser Zeile noch zu verarbeiten sind (also 24 oder 25).
 ldx #$00                           ;(2 TZ) Platzhalter für 1. Stackpointer
 txs                                ;(2 TZ) Stackpointer vom 1. IRQ wiederherstellen
 nop                                ;(2 TZ)
 nop                                ;(2 TZ)
 nop                                ;(2 TZ)
 bit $01                            ;(3 TZ)
 lda #$0c                           ;(2 TZ) grau als 'Hauptfarbe' in den Akku
 ldy #$00                           ;(2 TZ) 'Zeilenfarbe' schwarz ins Y-Reg. laden
 ldx $d012                          ;(4 TZ)
 cpx $d012                          ;(4 TZ) sind wir noch in Rasterzeile STARTROW+2?
                                    ;======
                                    ;25 TZ = 63 oder 64 TZ!!!

 beq myIRQMain                      ;(3 TZ) wenn JA einen letzten Takt 'verschwenden'
                                    ;(2 TZ) sonst einfach weiterlaufen...

;*** Wir beginnen also immer exakt nach 3 TZ in der dritten Rasterzeile (STARTROW+3)
;*** nach dem 1. Raster-IRQ (den hatten wir ja für Zeile STARTROW festgelegt

myIRQMain
                                    ; 3TZ (s. oben)
 sty $D020                          ; 4TZ schwarz
 iny                                ; 2TZ 
 sty $D020                          ; 4TZ weiß
 dey                                ; 2TZ
 sty $D020                          ; 4TZ s
 iny                                ; 2TZ
 sty $D020                          ; 4TZ w
 dey                                ; 2TZ
 sty $D020                          ; 4TZ s
 iny                                ; 2TZ
 sty $D020                          ; 4TZ w
 dey                                ; 2TZ
 sty $D020                          ; 4TZ s
 iny                                ; 2TZ
 sty $D020                          ; 4TZ w
 dey                                ; 2TZ
 sty $D020                          ; 4TZ s
 iny                                ; 2TZ 
 sty $D020                          ; 4TZ w
                                    ;====
                                    ;61TZ
 
 sta $D020                          ; 4TZ grau

 lda #<myIRQ                        ;Original IRQ-Vektor setzen
 sta $0314
 lda #>myIRQ
 sta $0315

 lda #RASTER                        ;ursprüngliche Rasterzeile zurücksetzen
 sta $d012

 lda #%00000001                     ;IRQ bestätigen
 sta $d019

 jmp $ea31                          ;zum Schluß zum 'Timer-Interrupt' springen