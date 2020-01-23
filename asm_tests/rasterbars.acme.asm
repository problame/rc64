RASTER          = 47                ;Hier den 1. Raster-IRQ auslösen

;*** Startadresse 
*=$0801
;*** BASIC-Zeile: 2018 SYS 2062
 !word main-2, 2018 
 !byte $9e
 !text " 2062"
 !byte $00,$00,$00

main
 jsr $e544                          ;Bildschirm löschen
 lda #0                             ;schwarz
 sta $d020                          ;für Rahmen
 sta $d021                          ;und Hintergrund
 
 sei                                ;IRQs sperren
 lda #<myIRQ                        ;Adresse unserer Routine in
 sta $0314                          ;den RAM-Vektor
 lda #>myIRQ
 sta $0315
 lda #%00000001                     ;Raster-IRQs vom VIC-II aktivieren
 sta $d01a
 lda #RASTER                        ;Hier soll unsere Linie erscheinen
 sta $d012                      
 lda $d011                          ;Zur Sicherheit höchste BIT
 and #%01111111                     ;für die Rasterzeile löschen
 sta $d011
 lda #%01111111                     ;Timer-IRQs abschalten
 sta $dc0d
 lda $dc0d
 lda #%0000001                      ;evtl. aktiven Raster-IRQ bestätigen
 sta $d019
 cli                                ;Interrupts erlauben 
                          
 jmp *                              ;Endlosschleife



;*** an Pagegrenze ausrichten, damit die Sprünge passen
!align 255,0

myIRQ
;*** Wenn wir hier landen, sind bereits 38-45 Taktzyklen
;*** in der aktuellen Rasterzeile (RASTER) vergangen!

;*** Zweiten IRQ einrichten
;*** Da die Zeit bei aktivierten ROM nicht reicht,
;*** können wir den 2. Raster-IRQ erst in der übernächsten Zeile bekommen
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
;*** aufjedenfall in nächsten Rasterzeile (RASTER+1)!
;*** Verbraucht wurden 1-8 TZ
 inc $d012                          ;(6 TZ) 2. IRQ in der übernächsten Zeile (RASTER+2)!!!
                                    ;       $d012 wurde bereits automatisch erhöht
 sta $d019                          ;(4 TZ) IRQ bestätigen
 cli                                ;(2 TZ) Interrupts für den 2. Raster-IRQ
                                    ;       wieder freigeben

;*** Wir befinden uns in Rasterzeile RASTER+1 und 
;*** haben bisher 13-20 Zyklen verbraucht

;*** etwas Zeit verschwenden...
 ldx #$08                           ;            2 TZ
 dex                                ;8 * 2 TZ = 16 TZ
 bne *-1                            ;7 * 3 TZ = 21 TZ
                                    ;1 * 2 TZ =  2 TZ
                                    ;          ------
                                    ;           41 TZ

;*** Bis hier sind 54-61 Taktzyklen vergannen, jetzt auf den IRQ warten...
;*** Der nächste Rasterinterrupt wird während dieser NOPs auftreten!
 nop                                ;2 TZ (55)
 nop                                ;2 TZ (57)
 nop                                ;2 TZ (59)
 nop                                ;2 TZ (51)
 nop                                ;2 TZ (63)
 nop                                ;2 TZ (65)

doubleIRQ
;*** Wir sind nun in Rasterzeile RASTER+2 und
;*** haben bisher genau 38 oder 39 Taktzyklen benötigt!!
;*** Wir können so sicher sein, da der IRQ während der NOPs auftrat.

;*** Jetzt exakt soviele Taktzyklen 'verschwenden', wie in 
;*** dieser Zeile noch zu verarbeiten sind (also 24 oder 25).
 ldx #$00                           ;(2 TZ) Platzhalter für 1. Stackpointer
 txs                                ;(2 TZ) Stackpointer vom 1. IRQ wiederherstellen
 nop                                ;(2 TZ)
 nop                                ;(2 TZ)
 nop                                ;(2 TZ)
 nop                                ;(2 TZ)
 bit $01                            ;(3 TZ)
 ldx $D012                          ;(4 TZ)
 lda #$01                           ;(2 TZ) weiß schonmal in den Akku
 cpx $D012                          ;(4 TZ) sind wir noch in Rasterzeile 22?
                                    ;------
                                    ;25 TZ = 63 oder 64 TZ!!!

 beq myIRQMain                      ;(3 TZ) wenn JA einen letzten Takt 'verschwenden'
                                    ;(2 TZ) sonst einfach weiterlaufen...

;*** Wir beginnen also immer exakt nach 3 TZ in der dritten Rasterzeile (RASTER+3)
;*** nach dem 1. Raster-IRQ (den hatten wir ja in für Zeile RASTER festgelegt)
myIRQMain
 ldx #$ff                           ;X mit -1 initialisieren, da gleich INX folgt!
nextColor
 inx                                ;Schleifenzähler erhöhen
 ldy delaytable,X                   ;Wartezeit holen
 dey                                ;verringern
 bne *-1                            ;solange größer 0 zurück zum DEY
 lda rowcolortable,X                ;Farbe holen
 sta $d021                          ;und ins Register für die Hintergrundfarbe
 nop                                ;ahhhh einfach mal nichts 'tun'
 bpl nextColor                      ;solange die Farbe positiv ist -> @loop
 
 lda #<myIRQ                        ;Original IRQ-Vektor setzen
 sta $0314
 lda #>myIRQ
 sta $0315

 lda #RASTER
 sta $d012

 lda #%00000001                     ;IRQ bestätigen
 sta $d019

 jmp $ea81                          ;zum Ende des 'Timer-Interrupts' springen



!align 255,0
rowcolortable
 !byte 6, 11, 11, 12, 12, 15, 1, 15, 12, 12, 11, 11, 6
 !byte 0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0,  0, 0
 !byte 6, 11, 11, 12, 12, 15, 1, 15, 12, 12, 11, 11, 6
 !byte 0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0,  0, 0
 !byte 6, 11, 11, 12, 12, 15, 1, 15, 12, 12, 11, 11, 6
 !byte 0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0,  0, 0
 !byte 6, 11, 11, 12, 12, 15, 1, 15, 12, 12, 11, 11, 6
 !byte 0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0,  0, 0
 !byte 6, 11, 11, 12, 12, 15, 1, 15, 12, 12, 11, 11, 6
 !byte 0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0,  0, 0
 !byte 6, 11, 11, 12, 12, 15, 1, 15, 12, 12, 11, 11, 6
 !byte 0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0,  0, 0
 !byte 6, 11, 11, 12, 12, 15, 1, 15, 12, 12, 11, 11, 6
 !byte 0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0,  0, 0
 !byte 6, 11, 11, 12, 12, 15, 1, 15, 12, 12, 11, 11, 6
 !byte 0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0,  0, 0
 !byte $f0


!align 255,0
delaytable
 !byte 9                            ;letzte Zeile vor der Anzeige        
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ; 1. Textzeile
 !byte 2, 8, 8, 9, 9, 9, 9, 10      ; 2. 
 !byte 2, 8, 8, 9, 9, 9, 9, 10      ; 3. 
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ; 4. 
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ; 5. 

 !byte 2, 8, 8, 9, 9, 9, 9, 9       ; 6. 
 !byte 2, 8, 8, 9, 9, 9, 9, 10      ; 7. 
 !byte 2, 8, 8, 9, 9, 9, 9, 10      ; 8. 
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ; 9. 
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;10. Textzeile

 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;11.
 !byte 2, 8, 8, 9, 9, 9, 9, 10      ;12.
 !byte 2, 8, 8, 9, 9, 9, 9, 10      ;13.
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;14.
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;15.

 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;16.
 !byte 2, 8, 8, 9, 9, 9, 9, 10      ;17.
 !byte 2, 8, 8, 9, 9, 9, 9, 10      ;18.
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;19.
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;20. Textzeile

 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;21.
 !byte 2, 8, 8, 9, 9, 9, 9, 10      ;22.
 !byte 2, 8, 8, 9, 9, 9, 9, 10      ;23.
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;24.
 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;25. Textzeile

 !byte 2, 8, 8, 9, 9, 9, 9, 9       ;26. 'Sicherheitszeile'