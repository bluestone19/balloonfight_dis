.define PhaseHeaderPointers Phase1Header, Phase2Header, Phase3Header, BonusPhaseHeader, Phase4Header, Phase5Header, Phase6Header, BonusPhaseHeader, Phase7Header, Phase8Header, Phase9Header, BonusPhaseHeader, Phase10Header, Phase11Header, Phase12Header, BonusPhaseHeader

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
.BYTE $18,$0a,$ff
.BYTE $ff
.WORD Phase4Enemies
.BYTE 6
.WORD Phase4Collision

Phase5Header:
.WORD IslandsLayout, Phase5Platforms, $0000
.BYTE $04,$06
.BYTE $12,$08,$ff
.BYTE $ff
.WORD Phase5Enemies
.BYTE 7
.WORD Phase5Collision

Phase7Header:
.WORD IslandsLayout, Phase7Platforms, $0000
.BYTE $06,$06
.BYTE $14,$10,$ff
.BYTE $0f,$0d,$01,$ff
.WORD Phase7Enemies
.BYTE 9
.WORD Phase7Collision

Phase10Header:
.WORD IslandsLayout, Phase10Platforms, $0000
.BYTE $04,$06
.BYTE $10,$0e,$ff
.BYTE $08,$0e,$03
.BYTE $0d,$09,$03
.BYTE $12,$08,$03
.BYTE $17,$0d,$03,$ff
.WORD Phase10Enemies
.BYTE 3
.WORD Phase10Collision

Phase9Header:
.WORD IslandsLayout, ScatterPlatforms, $0000
.BYTE $10,$06
.BYTE $1a,$0c,$ff
.BYTE $08,$08,$01
.BYTE $18,$04,$01,$ff
.WORD Phase4Enemies
.BYTE 6
.WORD Phase4Collision

Phase6Header:
Phase12Header:
.WORD IslandsLayout, Phase6Platforms, $0000
.BYTE $0e,$06
.BYTE $0c,$14,$ff
.BYTE $ff
.WORD Phase6Enemies
.BYTE 6
.WORD Phase6Collision

Phase3Header:
.WORD IslandsLayout, Phase3Platforms, $0000
.BYTE $04,$08
.BYTE $16,$10,$ff
.BYTE $ff
.WORD Phase3Enemies
.BYTE 9
.WORD Phase3Collision

Phase8Header:
.WORD IslandsLayout, Phase8Platforms, $0000
.BYTE $04,$10
.BYTE $18,$10,$ff
.BYTE $0e,$06,$01,$ff
.WORD Phase8Enemies
.BYTE 7
.WORD Phase8Collision

Phase11Header:
.WORD IslandsLayout, Phase11Platforms, $0000
.BYTE $04,$08
.BYTE $0e,$10,$ff
.BYTE $10,$07,$01,$ff
.WORD Phase11Enemies
.BYTE 7
.WORD Phase11Collision

;----------------------
; Phase Graphics:
;----------------------

IslandsLayout:
.BYTE $23,$40,136
.BYTE $39,$38,$39,$38,$39,$38,$39,$33,$24,$24,$24,$24,$24,$24,$24,$24
.BYTE $24,$24,$24,$24,$24,$24,$24,$24,$30,$38,$39,$38,$39,$38,$39,$38
.BYTE $3c,$3b,$3c,$3b,$3c,$3b,$3c,$3d,$58,$59,$5a,$5b,$58,$59,$5a,$5b
.BYTE $58,$59,$5a,$5b,$58,$59,$5a,$5b,$3a,$3b,$3c,$3b,$3c,$3b,$3c,$3b
.BYTE $60,$61,$62,$63,$60,$61,$62,$63,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f
.BYTE $5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$60,$61,$62,$63,$60,$61,$62,$63
.BYTE $5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f
.BYTE $5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f
BGAttributes: .BYTE $40,$50,$50,$50,$50,$90,$a0,$a0
.BYTE $23,$f0,$10,$00,$00,$a0,$a0,$a0,$a0,$00,$00,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$00

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
.BYTE $22,$49,$0e,$30,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$33,$00

Phase2Platforms:
.BYTE $21,$57,$06,$30,$31,$32,$31,$32,$33
.BYTE $21,$65,$06,$30,$31,$32,$31,$32,$33,$00

ScatterPlatforms:
.BYTE $21,$90,$03,$30,$31,$33
.BYTE $22,$26,$03,$30,$31,$33
.BYTE $22,$57,$03,$30,$31,$33
.BYTE $22,$6c,$03,$30,$31,$33
.BYTE $22,$f2,$03,$30,$31,$33,$00

Phase5Platforms:
.BYTE $20,$cb,$03,$30,$31,$33
.BYTE $a1,$6d,$03,$3e,$3f,$40
.BYTE $a1,$59,$04,$3e,$3f,$3f,$40
.BYTE $a1,$a5,$03,$3e,$3f,$40
.BYTE $22,$aa,$03,$30,$31,$33
.BYTE $22,$b3,$03,$30,$31,$33,$00

Phase7Platforms:
.BYTE $20,$e2,$02,$30,$33
.BYTE $20,$fb,$02,$30,$33
.BYTE $21,$57,$02,$30,$33
.BYTE $21,$93,$02,$30,$33
.BYTE $22,$0b,$02,$30,$33
.BYTE $22,$47,$02,$30,$33
.BYTE $22,$83,$02,$30,$33
.BYTE $22,$cf,$04,$30,$31,$32,$33,$00

Phase10Platforms:
.BYTE $22,$ca,$03,$30,$31,$33
.BYTE $22,$d2,$03,$30,$31,$33,$00

Phase6Platforms:
.BYTE $21,$08,$04,$30,$31,$32,$33
.BYTE $21,$14,$04,$30,$31,$32,$33
.BYTE $a1,$a5,$04,$3e,$3f,$3f,$40
.BYTE $a1,$ba,$04,$3e,$3f,$3f,$40
.BYTE $22,$6c,$06,$30,$31,$32,$31,$32,$33,$00

Phase3Platforms:
.BYTE $22,$ee,$04,$30,$31,$32,$33
.BYTE $20,$f9,$03,$30,$31,$33
.BYTE $a1,$1a,$03,$3f,$3f,$40
.BYTE $21,$90,$03,$30,$31,$33
.BYTE $a1,$b1,$03,$3f,$3f,$40
.BYTE $22,$28,$03,$30,$31,$33
.BYTE $a2,$49,$03,$3f,$3f,$40
.BYTE $20,$ea,$02,$30,$33,$00

Phase8Platforms:
.BYTE $a2,$6c,$03,$3e,$3f,$40
.BYTE $a2,$73,$03,$3e,$3f,$40
.BYTE $20,$e4,$04,$30,$31,$32,$33
.BYTE $20,$f8,$04,$30,$31,$32,$33
.BYTE $21,$a8,$04,$30,$31,$32,$33
.BYTE $21,$b5,$04,$30,$31,$32,$33,$00

Phase11Platforms:
.BYTE $22,$64,$02,$30,$33
.BYTE $22,$08,$02,$30,$33
.BYTE $21,$ac,$02,$30,$33
.BYTE $21,$b4,$02,$30,$33
.BYTE $22,$18,$02,$30,$33
.BYTE $22,$7c,$02,$30,$33,$00

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
.BYTE $03
.BYTE $58,$78,$04
.BYTE $78,$78,$04
.BYTE $98,$78,$04

Phase2Enemies:
.BYTE $05
.BYTE $c8,$38,$05
.BYTE $38,$40,$05
.BYTE $58,$78,$04
.BYTE $78,$78,$04
.BYTE $98,$78,$04

NoEnemies:
.BYTE $00

Phase4Enemies:
.BYTE $05
.BYTE $94,$a0,$06
.BYTE $84,$48,$05
.BYTE $34,$70,$04
.BYTE $64,$80,$04
.BYTE $bc,$78,$04

Phase5Enemies:
.BYTE $06
.BYTE $5c,$18,$06
.BYTE $24,$50,$05
.BYTE $64,$40,$05
.BYTE $c4,$38,$05
.BYTE $54,$90,$04
.BYTE $9c,$90,$04

Phase7Enemies:
.BYTE $06
.BYTE $10,$20,$06
.BYTE $d8,$20,$06
.BYTE $b8,$38,$05
.BYTE $98,$48,$05
.BYTE $58,$68,$05
.BYTE $38,$78,$04

Phase10Enemies:
.BYTE $05
.BYTE $54,$98,$05
.BYTE $94,$98,$05
.BYTE $6c,$39,$06
.BYTE $94,$31,$06
.BYTE $bc,$59,$06

Phase6Enemies:
.BYTE $05
.BYTE $50,$28,$06
.BYTE $a8,$28,$06
.BYTE $24,$50,$06
.BYTE $cc,$50,$06
.BYTE $70,$80,$04

Phase3Enemies:
.BYTE $05
.BYTE $50,$20,$06
.BYTE $c8,$20,$06
.BYTE $84,$48,$05
.BYTE $44,$70,$05
.BYTE $78,$a0,$04

Phase8Enemies:
.BYTE $06
.BYTE $28,$20,$06
.BYTE $c8,$20,$06
.BYTE $48,$50,$05
.BYTE $b0,$50,$05
.BYTE $5c,$80,$04
.BYTE $94,$80,$04

Phase11Enemies:
.BYTE $06
.BYTE $20,$80,$04
.BYTE $40,$68,$05
.BYTE $60,$50,$06
.BYTE $a0,$50,$06
.BYTE $c0,$68,$05
.BYTE $e0,$80,$04