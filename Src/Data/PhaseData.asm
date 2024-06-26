.linecont +	;Use CA65's line continuation to make the defines a little more readable

; Pointers to the phases' headers, in the order they appear in game
.define PhaseHeaderPointers \
	Phase1Header, \
	Phase2Header, \
	Phase3Header, \
	BonusPhaseHeader, \
	Phase4Header, \
	Phase5Header, \
	Phase6Header, \
	BonusPhaseHeader, \
	Phase7Header, \
	Phase8Header, \
	Phase9Header, \
	BonusPhaseHeader, \
	Phase10Header, \
	Phase11Header, \
	Phase12Header, \
	BonusPhaseHeader

; Phase Header pointers, but split into the low and high bytes for easier copying
PhasePointersLow:
	.LOBYTES PhaseHeaderPointers
PhasePointersHigh:
	.HIBYTES PhaseHeaderPointers

;----------------------
; Phase Headers:
;----------------------

Phase1Header:
	.WORD IslandsLayout, BigCenterPlatform, $0000
	.BYTE $10,$06,$ff	; Cloud (1)
	.BYTE $ff			; Propellers (None)
	.WORD Phase1Enemies
	.BYTE 2				; Collision Boxes (3)
	.WORD Phase1Collision

Phase2Header:
	.WORD IslandsLayout, BigCenterPlatform, Phase2Platforms, $0000
	.BYTE $18,$0c
	.BYTE $04,$0e,$ff	; Clouds (2)
	.BYTE $ff			; Propellers (None)
	.WORD Phase2Enemies
	.BYTE 4				; Collision Boxes (5)
	.WORD Phase2Collision

BonusPhaseHeader:
	.WORD BonusPhaseGround, $0000
	.BYTE $ff			; Clouds (None)
	.BYTE $ff			; Propellers (None)
	.WORD NoEnemies
	.BYTE 0				; Collision Box (1)
	.WORD BonusPhaseCollision

Phase4Header:
	.WORD IslandsLayout, ScatterPlatforms, $0000
	.BYTE $08,$06
	.BYTE $18,$0a,$ff	; Clouds (2)
	.BYTE $ff			; Propellers (None)
	.WORD Phase4Enemies
	.BYTE 6				; Collision Boxes (7)
	.WORD Phase4Collision

Phase5Header:
	.WORD IslandsLayout, Phase5Platforms, $0000
	.BYTE $04,$06
	.BYTE $12,$08,$ff	; Clouds (2)
	.BYTE $ff			; Propellers (None)
	.WORD Phase5Enemies
	.BYTE 7				; Collision Boxes (8)
	.WORD Phase5Collision

Phase7Header:
	.WORD IslandsLayout, Phase7Platforms, $0000
	.BYTE $06,$06
	.BYTE $14,$10,$ff		; Clouds (2)
	.BYTE $0f,$0d,$01,$ff	; Propeller (1)
	.WORD Phase7Enemies
	.BYTE 9					; Collision Boxes (10)
	.WORD Phase7Collision

Phase10Header:
	.WORD IslandsLayout, Phase10Platforms, $0000
	.BYTE $04,$06
	.BYTE $10,$0e,$ff		; Clouds (2)
	.BYTE $08,$0e,$03
	.BYTE $0d,$09,$03
	.BYTE $12,$08,$03
	.BYTE $17,$0d,$03,$ff	; Propellers (4)
	.WORD Phase10Enemies
	.BYTE 3					; Collision Boxes (4)
	.WORD Phase10Collision

Phase9Header:
	.WORD IslandsLayout, ScatterPlatforms, $0000
	.BYTE $10,$06
	.BYTE $1a,$0c,$ff		; Clouds (2)
	.BYTE $08,$08,$01
	.BYTE $18,$04,$01,$ff	; Propellers (2)
	.WORD Phase4Enemies
	.BYTE 6					; Collision Boxes (7)
	.WORD Phase4Collision

Phase6Header:	; Phases 6 and 12 use the same header
Phase12Header:
	.WORD IslandsLayout, Phase6Platforms, $0000
	.BYTE $0e,$06
	.BYTE $0c,$14,$ff	; Clouds (2)
	.BYTE $ff			; Propellers (None)
	.WORD Phase6Enemies
	.BYTE 6				; Collision Boxes (7)
	.WORD Phase6Collision

Phase3Header:
	.WORD IslandsLayout, Phase3Platforms, $0000
	.BYTE $04,$08
	.BYTE $16,$10,$ff	; Clouds (2)
	.BYTE $ff			; Propellers (None)
	.WORD Phase3Enemies
	.BYTE 9				; Collision Boxes (10)
	.WORD Phase3Collision

Phase8Header:
	.WORD IslandsLayout, Phase8Platforms, $0000
	.BYTE $04,$10
	.BYTE $18,$10,$ff		; Clouds (2)
	.BYTE $0e,$06,$01,$ff	; Propeller (1)
	.WORD Phase8Enemies
	.BYTE 7					; Collision Boxes (8)
	.WORD Phase8Collision

Phase11Header:
	.WORD IslandsLayout, Phase11Platforms, $0000
	.BYTE $04,$08
	.BYTE $0e,$10,$ff		; Clouds (2)
	.BYTE $10,$07,$01,$ff	; Propeller (1)
	.WORD Phase11Enemies
	.BYTE 7					; Collision Boxes (8)
	.WORD Phase11Collision

;----------------------
; Phase Graphics:
;----------------------

IslandsLayout:
	.BYTE $23,$40,136
	.BYTE $39,$38,$39,$38,$39,$38,$39,$33,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$30,$38,$39,$38,$39,$38,$39,$38	; Top of islands + blank space between
	.BYTE $3c,$3b,$3c,$3b,$3c,$3b,$3c,$3d,$58,$59,$5a,$5b,$58,$59,$5a,$5b,$58,$59,$5a,$5b,$58,$59,$5a,$5b,$3a,$3b,$3c,$3b,$3c,$3b,$3c,$3b	; Middle of islands + top of water between
	.BYTE $60,$61,$62,$63,$60,$61,$62,$63,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$60,$61,$62,$63,$60,$61,$62,$63	; Bottoms of islands + water between
	.BYTE $5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f	; Water
BGAttributes: 
	.BYTE $40,$50,$50,$50,$50,$90,$a0,$a0
	.BYTE $23,$f0,16,$00,$00,$a0,$a0,$a0,$a0,$00,$00,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$00

BonusPhaseGround:
	.BYTE $a3,$04,4,$93,$94,$94,$94
	.BYTE $a3,$05,4,$95,$96,$96,$96
	.BYTE $a2,$ea,5,$93,$94,$94,$94,$94
	.BYTE $a2,$eb,5,$95,$96,$96,$96,$96
	.BYTE $a3,$34,3,$93,$94,$94
	.BYTE $a3,$35,3,$95,$96,$96
	.BYTE $a3,$1a,4,$93,$94,$94,$94
	.BYTE $a3,$1b,4,$95,$96,$96,$96
	.BYTE $63,$80,32,$97
	.BYTE $23,$a0,32
	.BYTE $98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99
	.BYTE $98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99
	.BYTE $23,$c0,8,$40,$50,$50,$50,$50,$90,$a0,$a0
	.BYTE $63,$e8,16,$ff,$00

BigCenterPlatform:
	.BYTE $22,$49,14,$30,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$33,$00

Phase2Platforms:
	.BYTE $21,$57,6,$30,$31,$32,$31,$32,$33
	.BYTE $21,$65,6,$30,$31,$32,$31,$32,$33,$00

ScatterPlatforms:
	.BYTE $21,$90,3,$30,$31,$33
	.BYTE $22,$26,3,$30,$31,$33
	.BYTE $22,$57,3,$30,$31,$33
	.BYTE $22,$6c,3,$30,$31,$33
	.BYTE $22,$f2,3,$30,$31,$33,$00

Phase5Platforms:
	.BYTE $20,$cb,3,$30,$31,$33
	.BYTE $a1,$6d,3,$3e,$3f,$40
	.BYTE $a1,$59,4,$3e,$3f,$3f,$40
	.BYTE $a1,$a5,3,$3e,$3f,$40
	.BYTE $22,$aa,3,$30,$31,$33
	.BYTE $22,$b3,3,$30,$31,$33,$00

Phase7Platforms:
	.BYTE $20,$e2,2,$30,$33
	.BYTE $20,$fb,2,$30,$33
	.BYTE $21,$57,2,$30,$33
	.BYTE $21,$93,2,$30,$33
	.BYTE $22,$0b,2,$30,$33
	.BYTE $22,$47,2,$30,$33
	.BYTE $22,$83,2,$30,$33
	.BYTE $22,$cf,4,$30,$31,$32,$33,$00

Phase10Platforms:
	.BYTE $22,$ca,3,$30,$31,$33
	.BYTE $22,$d2,3,$30,$31,$33,$00

Phase6Platforms:
	.BYTE $21,$08,4,$30,$31,$32,$33
	.BYTE $21,$14,4,$30,$31,$32,$33
	.BYTE $a1,$a5,4,$3e,$3f,$3f,$40
	.BYTE $a1,$ba,4,$3e,$3f,$3f,$40
	.BYTE $22,$6c,6,$30,$31,$32,$31,$32,$33,$00

Phase3Platforms:
	.BYTE $22,$ee,4,$30,$31,$32,$33
	.BYTE $20,$f9,3,$30,$31,$33
	.BYTE $a1,$1a,3,$3f,$3f,$40
	.BYTE $21,$90,3,$30,$31,$33
	.BYTE $a1,$b1,3,$3f,$3f,$40
	.BYTE $22,$28,3,$30,$31,$33
	.BYTE $a2,$49,3,$3f,$3f,$40
	.BYTE $20,$ea,2,$30,$33,$00

Phase8Platforms:
	.BYTE $a2,$6c,3,$3e,$3f,$40
	.BYTE $a2,$73,3,$3e,$3f,$40
	.BYTE $20,$e4,4,$30,$31,$32,$33
	.BYTE $20,$f8,4,$30,$31,$32,$33
	.BYTE $21,$a8,4,$30,$31,$32,$33
	.BYTE $21,$b5,4,$30,$31,$32,$33,$00

Phase11Platforms:
	.BYTE $22,$64,2,$30,$33
	.BYTE $22,$08,2,$30,$33
	.BYTE $21,$ac,2,$30,$33
	.BYTE $21,$b4,2,$30,$33
	.BYTE $22,$18,2,$30,$33
	.BYTE $22,$7c,2,$30,$33,$00

;----------------------
; Phase Collision:
;----------------------

Phase1Collision:
	.BYTE $10,$c8,$48
	.BYTE $38,$ff,$b8
	.BYTE $cf,$cf,$8f
	.BYTE $e0,$e0,$98

Phase2Collision:
	.BYTE $10,$c4,$48,$b8,$28
	.BYTE $3c,$ff,$b8,$e8,$58
	.BYTE $cf,$cf,$8f,$4f,$57
	.BYTE $e0,$e0,$98,$58,$60

BonusPhaseCollision:
	.BYTE $10
	.BYTE $ff
	.BYTE $df
	.BYTE $ec

Phase4Collision:
	.BYTE $10,$c8,$80,$30,$b8,$60,$90
	.BYTE $38,$ff,$98,$48,$d0,$78,$a8
	.BYTE $cf,$cf,$5f,$87,$8f,$97,$b7
	.BYTE $e0,$e0,$68,$90,$98,$a0,$c0

Phase5Collision:
	.BYTE $10,$c8,$58,$68,$c8,$28,$50,$98
	.BYTE $38,$ff,$70,$78,$d0,$30,$68,$b0
	.BYTE $cf,$cf,$2f,$57,$4f,$67,$a7,$a7
	.BYTE $e0,$e0,$38,$6c,$6c,$7c,$b0,$b0

Phase7Collision:
	.BYTE $10,$c8,$12,$da,$ba,$9a,$5a,$3a,$1a,$7a
	.BYTE $38,$ff,$1e,$e6,$c6,$a6,$66,$46,$26,$96
	.BYTE $cf,$cf,$37,$37,$4f,$5f,$7f,$8f,$9f,$af
	.BYTE $e0,$e0,$40,$40,$58,$68,$88,$98,$a8,$b8

Phase10Collision:
	.BYTE $10,$c8,$52,$92
	.BYTE $38,$ff,$66,$a6
	.BYTE $cf,$cf,$af,$af
	.BYTE $e0,$e0,$b8,$b8

Phase6Collision:
	.BYTE $10,$c8,$40,$a0,$28,$d0,$60
	.BYTE $38,$ff,$60,$c0,$30,$d8,$90
	.BYTE $cf,$cf,$3f,$3f,$67,$67,$97
	.BYTE $e0,$e0,$48,$48,$88,$88,$a0

Phase3Collision:
	.BYTE $10,$c8,$50,$c8,$88,$40,$70,$d0,$88,$48
	.BYTE $38,$ff,$60,$e0,$98,$58,$90,$d8,$90,$50
	.BYTE $cf,$cf,$37,$37,$5f,$87,$b7,$3c,$64,$8c
	.BYTE $e0,$e0,$40,$40,$68,$90,$c0,$60,$80,$a8

Phase8Collision:
	.BYTE $10,$c8,$20,$c0,$40,$a8,$60,$98
	.BYTE $38,$ff,$40,$e0,$60,$c8,$68,$a0
	.BYTE $cf,$cf,$37,$37,$67,$67,$97,$97
	.BYTE $e0,$e0,$40,$40,$70,$70,$b0,$b0

Phase11Collision:
	.BYTE $10,$c8,$20,$40,$60,$a0,$c0,$e0
	.BYTE $38,$ff,$30,$50,$70,$b0,$d0,$f0
	.BYTE $cf,$cf,$97,$7f,$67,$67,$7f,$97
	.BYTE $e0,$e0,$a0,$88,$70,$70,$88,$a0

;----------------------
; Enemy Data:
;----------------------

Phase1Enemies:
	.BYTE 3	; 3 Enemies
	.BYTE 88,	120,	4	; Pink bird at X: 88, Y: 120
	.BYTE 120,	120,	4	; Pink bird at X: 120, Y: 120
	.BYTE 152,	120,	4	; Pink bird at X: 152, Y: 120

Phase2Enemies:
	.BYTE 5	; 5 Enemies
	.BYTE 200,	56,		5	; Green bird at X: 200, Y: 56
	.BYTE 56,	64,		5	; Green bird at X: 56, Y: 64
	.BYTE 88,	120,	4	; Pink bird at X: 88, Y: 120
	.BYTE 120,	120,	4	; Pink bird at X: 120, Y: 120
	.BYTE 152,	120,	4	; Pink bird at X: 152, Y: 120

NoEnemies:	; Used in Bonus Phases
	.BYTE 0	; 0 Enemies

Phase4Enemies:
	.BYTE 5	; 5 Enemies
	.BYTE 148,	160,	6	; Yellow bird at X: 148, Y: 160
	.BYTE 132,	72,		5	; Green bird at X: 132, Y: 72
	.BYTE 52,	112,	4	; Pink bird at X: 52, Y: 112
	.BYTE 100,	128,	4	; Pink bird at X: 100, Y: 128
	.BYTE 188,	120,	4	; Pink bird at X: 188, Y: 120

Phase5Enemies:
	.BYTE 6	; 6 Enemies
	.BYTE 92,	24,		6	; Yellow bird at X: 92, Y: 24
	.BYTE 36,	80,		5	; Green bird at X: 36, Y: 80
	.BYTE 100,	64,		5	; Green bird at X: 100, Y: 64
	.BYTE 196,	56,		5	; Green bird at X: 196, Y: 56
	.BYTE 84,	144,	4	; Pink bird at X: 84, Y: 144
	.BYTE 156,	144,	4	; Pink bird at X: 156, Y: 144

Phase7Enemies:
	.BYTE 6	; 6 Enemies
	.BYTE 16,	32,		6	; Yellow bird at X: 16, Y: 32
	.BYTE 216,	32,		6	; Yellow bird at X: 216, Y: 32
	.BYTE 184,	56,		5	; Green bird at X: 184, Y: 56
	.BYTE 152,	72,		5	; Green bird at X: 152, Y: 72
	.BYTE 88,	104,	5	; Green bird at X: 88, Y: 104
	.BYTE 56,	120,	4	; Pink bird at X: 56, Y: 120

Phase10Enemies:
	.BYTE 5	; 5 Enemies
	.BYTE 84,	152,	5	; Green bird at X: 84, Y: 152
	.BYTE 148,	152,	5	; Green bird at X: 148, Y: 152
	.BYTE 108,	57,		6	; Yellow bird at X: 108, Y: 57
	.BYTE 148,	49,		6	; Yellow bird at X: 148, Y: 49
	.BYTE 188,	89,		6	; Yellow bird at X: 188, Y: 89

Phase6Enemies:	; Also Phase 12's Enemies
	.BYTE 5	; 5 Enemies
	.BYTE 80,	40,		6	; Yellow bird at X: 80, Y: 40
	.BYTE 168,	40,		6	; Yellow bird at X: 168, Y: 40
	.BYTE 36,	80,		6	; Yellow bird at X: 36, Y: 80
	.BYTE 204,	80,		6	; Yellow bird at X: 204, Y: 80
	.BYTE 112,	128,	4	; Pink bird at X: 112, Y: 128

Phase3Enemies:
	.BYTE 5	; 5 Enemies
	.BYTE 80,	32,		6	; Yellow bird at X: 80, Y: 32
	.BYTE 200,	32,		6	; Yellow bird at X: 200, Y: 32
	.BYTE 132,	72,		5	; Green bird at X: 132, Y: 72
	.BYTE 68,	112,	5	; Green bird at X: 68, Y: 112
	.BYTE 120,	160,	4	; Pink bird at X: 120, Y: 160

Phase8Enemies:
	.BYTE 6	; 6 Enemies
	.BYTE 40,	32,		6	; Yellow bird at X: 40, Y: 32
	.BYTE 200,	32,		6	; Yellow bird at X: 200, Y: 32
	.BYTE 72,	80,		5	; Green bird at X: 72, Y: 80
	.BYTE 176,	80,		5	; Green bird at X: 176, Y: 80
	.BYTE 92,	128,	4	; Pink bird at X: 92, Y: 128
	.BYTE 148,	128,	4	; Pink bird at X: 148, Y: 128

Phase11Enemies:
	.BYTE 6	; 6 Enemies
	.BYTE 32,	128,	4	; Pink bird at X: 32, Y: 128
	.BYTE 64,	104,	5	; Green bird at X: 64, Y: 104
	.BYTE 96,	80,		6	; Yellow bird at X: 96, Y: 80
	.BYTE 160,	80,		6	; Yellow bird at X: 160, Y:80
	.BYTE 192,	104,	5	; Green bird at X: 192, Y: 104
	.BYTE 224,	128,	4	; Pink bird at X: 224, Y: 128