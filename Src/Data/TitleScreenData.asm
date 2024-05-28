;----------------------
; Title Graphics:
;----------------------

TitleScreenHeader:
.WORD TitleScreenData, $0000

TitleScreenData:	;Title Screen Data
; Palette
.BYTE $3F,$00,4		;At $3F00: Title Logo Palette (BG Palette 0)
.BYTE $0F,$30,$27,$2A	;Black (BG), White, Orange, Lime Green
.BYTE $3F,$18,4		;At $3F18: Title Balloon Palette (Sprite Palette 2)
.BYTE $0F,$16,$30,$21	;Black (BG), Red, White, Light Blue
; BALLOON (TM)
.BYTE $20,$7C,33	;Row 1 & TM Header
.BYTE $F0,$F1,$24,$24,$24,$24	; TM & Spacing
.BYTE $E0,$E1,$E1,$E2,$E0,$E1,$E1,$E2,$E0,$E2,$24,$E0,$E2,$24,$E0,$E1,$E1,$E2,$E0,$E1,$E1,$E2,$E0,$EC,$24,$E0,$E2	;Row 1
.BYTE $20,$A2,27	;Row 2 Header
.BYTE $E3,$E3,$E3,$E5,$E3,$E3,$E3,$E5,$E3,$E5,$24,$E3,$E5,$24,$E3,$E3,$E3,$E5,$E3,$E3,$E3,$E5,$E3,$E3,$F3,$E3,$E5	;Row 2
.BYTE $20,$C2,27	;Row 3 Header
.BYTE $E3,$E4,$E3,$E7,$E3,$E4,$E3,$E5,$E3,$E5,$24,$E3,$E5,$24,$E3,$E4,$E3,$E5,$E3,$E4,$E3,$E5,$E3,$E3,$E3,$E3,$E5	;Row 3
.BYTE $20,$E2,27	;Row 4 Header
.BYTE $E3,$E3,$E3,$E2,$E3,$E3,$E3,$E5,$E3,$E5,$24,$E3,$E5,$24,$E3,$E3,$E3,$E5,$E3,$E3,$E3,$E5,$E3,$E3,$E3,$E3,$E5	;Row 4
.BYTE $21,$02,27	;Row 5 Header
.BYTE $E3,$E4,$E3,$E5,$E3,$F2,$E3,$E5,$E3,$E3,$E2,$E3,$E3,$E2,$E3,$E3,$E3,$E5,$E3,$E3,$E3,$E5,$E3,$F2,$E3,$E3,$E5	;Row 5
.BYTE $21,$22,27	;Row 6 Header
.BYTE $E6,$E3,$E3,$E7,$EB,$24,$E6,$E7,$E6,$E3,$E7,$E6,$E3,$E7,$E6,$E3,$E3,$E7,$E6,$E3,$E3,$E7,$EB,$24,$E6,$E3,$E7	;Row 6
; FIGHT
.BYTE $21,$4C,18,$E0,$E1,$E1,$E2,$E0,$E2,$E0,$E1,$E1,$E2,$E8,$24,$E0,$E2,$E0,$E1,$E1,$E2	;Row 1
.BYTE $21,$6C,18,$E3,$E3,$E3,$E7,$E3,$E5,$E3,$F5,$F6,$E7,$E3,$F3,$E3,$E5,$E6,$E3,$E3,$E7	;Row 2
.BYTE $21,$8C,18,$E3,$E3,$EF,$24,$E3,$E5,$E3,$24,$24,$24,$E3,$E3,$E3,$E5,$24,$E3,$E5,$24	;Row 3
.BYTE $21,$AC,18,$E3,$E3,$E1,$EA,$E3,$E5,$E3,$E9,$E3,$E2,$E3,$E3,$E3,$E5,$24,$E3,$E5,$24	;Row 4
.BYTE $21,$CC,18,$E3,$E3,$EF,$24,$E3,$E5,$E3,$F3,$E3,$E5,$E3,$F2,$E3,$E5,$24,$E3,$E5,$24	;Row 5
.BYTE $21,$EC,18,$E6,$E7,$24,$24,$E6,$E7,$E6,$E7,$E6,$E7,$EB,$24,$E6,$E7,$24,$E6,$E7,$24	;Row 6
; Game Modes
.BYTE $22,$48,16,$0A,$24,$24,$01,$25,$19,$15,$0A,$22,$0E,$1B,$24,$10,$0A,$16,$0E 	; A  1-PLAYER GAME
.BYTE $22,$88,16,$0B,$24,$24,$02,$25,$19,$15,$0A,$22,$0E,$1B,$24,$10,$0A,$16,$0E 	; B  2-PLAYER GAME
.BYTE $22,$C8,16,$0C,$24,$24,$0B,$0A,$15,$15,$18,$18,$17,$24,$24,$1D,$1B,$12,$19 	; C  BALLOON  TRIP
; Copyright
.BYTE $23,$49,14,$F4,$01,$09,$08,$04,$24,$17,$12,$17,$1D,$0E,$17,$0D,$18,$00		; ©1984 NINTENDO
