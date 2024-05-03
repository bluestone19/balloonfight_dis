le03d:
.BYTE $08,$08,$f0,$08,$08,$00
le043:
.BYTE $00,$00,$00,$08,$08,$08,$00,$00,$01,$08
.BYTE $08,$09,$00,$00,$ff,$08,$08,$07,$fe,$00,$00,$06,$08,$08,$02,$00
.BYTE $00,$0a,$08,$08,$02,$00,$01,$0a,$08,$09,$04,$00,$00,$0c,$08,$08
.BYTE $00,$00,$fe,$08,$08,$06,$fd,$fd,$fd,$05,$05,$05
le079:
.BYTE $08,$08,$08,$00
.BYTE $00,$00,$08,$08,$07,$00,$00,$ff,$08,$08,$09,$00,$00,$01,$0a,$08
.BYTE $08,$02,$00,$00,$06,$08,$08,$fe,$00,$00,$06,$08,$07,$fe,$00,$ff
.BYTE $04,$08,$08,$fc,$00,$00,$08,$08,$0a,$00,$00,$02,$0b,$0b,$0b,$03,$03,$03
le0af:
.BYTE $00,$00,$01,$02,$03,$04,$05
le0b6:
.BYTE $00,$00,$01,$02,$03,$06,$07
le0bd:
.BYTE $00,$00,$01,$02,$03,$06,$08
le0c4:
.BYTE $00,$09,$0a,$02,$0b,$0c,$05
le0cb:
.BYTE $00,$00,$01,$02,$03,$04,$05
le0d2:
.BYTE $00,$00,$01,$02,$0d,$0e,$05
le0d9:
.BYTE $00,$1a,$1b,$1c,$1d,$1e,$1f
le0e0:
.BYTE $01,$1a,$1b,$20,$1d,$1e,$fc
le0e7:
.BYTE $00,$1a,$21,$22,$1d,$23,$24
le0ee:
.BYTE $00,$00,$38,$35,$0d,$39,$37
le0f5:
.BYTE $00,$00,$34,$35,$03,$36,$37
le0fc:
.BYTE $00,$09,$3a,$35,$0b,$3b,$37
PlayerFlash2B:
.BYTE $00,$ce,$cf,$d0,$d1,$d2,$d3
le10a:
.BYTE $00,$25,$26,$27,$28,$29,$2a
le111:
.BYTE $01,$28,$29,$2c,$25,$26,$2b
le118:
.BYTE $00,$4c,$cc,$02,$4d,$cd,$05
le11f:
.BYTE $00,$4c,$2f,$22,$4d,$30,$24
le126:
.BYTE $07,$4c,$2f,$35,$4d,$30,$37
le12d:
.BYTE $02,$4c,$2f,$27,$4d,$30,$2a
le134:
.BYTE $07,$4c,$2f,$2b,$4d,$30,$2c
le13b:
.BYTE $00,$0f,$10,$02,$11,$12,$05
le142:
.BYTE $00,$0f,$10,$02,$11,$19,$07
le149:
.BYTE $00,$0f,$10,$02,$11,$19,$08
le150:
.BYTE $00,$13,$14,$02,$15,$16,$05
le157:
.BYTE $00,$0f,$10,$02,$11,$12,$05
le15e:
.BYTE $03,$13,$17,$02,$15,$18,$05
le165:
.BYTE $04,$13,$2d,$1c,$15,$2e,$1f
le16c:
.BYTE $05,$13,$2d,$20,$15,$2e,$fc
le173:
.BYTE $04,$13,$2f,$22,$15,$30,$24
le17a:
.BYTE $00,$13,$3c,$35,$15,$3d,$37
le181:
.BYTE $00,$0f,$40,$35,$11,$41,$37
le188:
.BYTE $03,$13,$3e,$35,$15,$3f,$37
PlayerFlash1B:
.BYTE $00,$d4,$d5,$d0,$d6,$d7,$d3
le196:
.BYTE $00,$25,$31,$27,$32,$33,$2a
le19d:
.BYTE $02,$25,$31,$27,$32,$33,$2a
le1a4:
.BYTE $00,$fc,$48,$42,$fc,$49,$43
le1ab:
.BYTE $00,$fc,$48,$44,$fc,$49,$45
le1b2:
.BYTE $00,$fc,$4a,$46,$fc,$4b,$47
le1b9:
.BYTE $00,$fc,$a4,$a5,$fc,$a6,$a7
le1c0:
.BYTE $08,$fc,$71,$fc,$fc,$72,$73
le1c7:
.BYTE $08,$fc,$74,$fc,$fc,$75,$76
le1ce:
.BYTE $08,$fc,$71,$77,$fc,$72,$73
le1d5:
.BYTE $08,$fc,$74,$77,$fc,$75,$76
le1dc:
.BYTE $08,$fc,$71,$78,$fc,$72,$73
le1e3:
.BYTE $08,$fc,$74,$78,$fc,$75,$76
le1ea:
.BYTE $08,$fc,$71,$79,$fc,$72,$73
le1f1:
.BYTE $08,$fc,$74,$79,$fc,$75,$76
le1f8:
.BYTE $00,$4e,$4f,$50,$51,$52,$53
le1ff:
.BYTE $00,$4e,$4f,$50,$51,$5e,$58
le206:
.BYTE $00,$4e,$4f,$50,$51,$5e,$5d
le20d:
.BYTE $00,$54,$55,$50,$56,$57,$53
le214:
.BYTE $00,$4e,$4f,$50,$51,$52,$53
le21b:
.BYTE $00,$59,$5a,$50,$5b,$5c,$53
le222:
.BYTE $00,$fc,$5f,$60,$fc,$61,$62
le229:
.BYTE $06,$63,$64,$60,$fc,$65,$62
le230:
.BYTE $00,$66,$67,$60,$68,$69,$62
le237:
.BYTE $00,$6a,$67,$60,$6b,$69,$62
le23e:
.BYTE $00,$fc,$6c,$6d,$fc,$6e,$6f
le245:
.BYTE $00,$fc,$6c,$6d,$fc,$6e,$70

.define PlayerAnim00 le0af, le0b6, le0bd, le0b6
.define PlayerAnim01 le0c4, le0cb, le0d2, le0cb
.define PlayerAnim02 le0d9, le0e0, le0e7, le0e0
.define PlayerAnim03 le0ee, le0f5, le0fc, le0f5
.define PlayerAnim04 le10a, le10a, le10a, le10a
.define PlayerAnim05 le111, le111, le111, le111
.define PlayerAnim06 le118, le118, le118, le118
.define PlayerAnim07 le11f, le11f, le11f, le11f
.define PlayerAnim08 le126, le126, le126, le126
.define PlayerAnim09 le12d, le12d, le12d, le12d
.define PlayerAnim0A le134, le134, le134, le134
.define PlayerAnim0B le13b, le142, le149, le142
.define PlayerAnim0C le150, le157, le15e, le157
.define PlayerAnim0D le165, le16c, le173, le16c
.define PlayerAnim0E le17a, le181, le188, le181
.define PlayerAnim0F le196, le196, le196, le196
.define PlayerAnim10 le19d, le19d, le19d, le19d
.define PlayerAnim11 le1a4, le1ab, le1b2, le1ab
.define PlayerAnim12 le1b9, le1a4, le1b9, le1a4
.define PlayerAnims PlayerAnim00, PlayerAnim01, PlayerAnim02, PlayerAnim03, PlayerAnim04, PlayerAnim05, PlayerAnim06, PlayerAnim07, PlayerAnim08, PlayerAnim09, PlayerAnim0A, PlayerAnim0B, PlayerAnim0C, PlayerAnim0D, PlayerAnim0E, PlayerAnim0F, PlayerAnim10, PlayerAnim11, PlayerAnim12
PlayerAnimLower:
.LOBYTES PlayerAnims
PlayerAnimUpper:
.HIBYTES PlayerAnims

.define PlayerFlashAnim1B le181, le181, PlayerFlash1B, le181
.define PlayerFlashAnim2B le0f5, le0f5, PlayerFlash2B, le0f5
.define PlayerFlashAnims PlayerFlashAnim1B, PlayerFlashAnim2B
PlayerFlashAnimLower:
.LOBYTES PlayerFlashAnims
PlayerFlashAnimUpper:
.HIBYTES PlayerFlashAnims

.define EnemyAnim1 le1f8, le1ff, le206, le1ff
.define EnemyAnim2 le20d, le214, le21b, le214
.define EnemyAnim3 le23e, le23e, le222, le229
.define EnemyAnim4 le230, le230, le230, le230
.define EnemyAnim5 le1c0, le1c0, le1c0, le1c0
.define EnemyAnim6 le1c0, le1c7, le1c0, le1c7
.define EnemyAnim7 le1ce, le1d5, le1ce, le1d5
.define EnemyAnim8 le1dc, le1e3, le1dc, le1e3
.define EnemyAnim9 le1ea, le1f1, le1ea, le1f1
.define EnemyAnimA le23e, le245, le23e, le245
.define EnemyAnimB le237, le237, le237, le237
.define EnemyAnims EnemyAnim1, EnemyAnim2, EnemyAnim3, EnemyAnim4, EnemyAnim5, EnemyAnim6, EnemyAnim7, EnemyAnim8, EnemyAnim9, EnemyAnimA, EnemyAnimB
EnemyAnimLower:
.LOBYTES EnemyAnims
EnemyAnimUpper:
.HIBYTES EnemyAnims

le34c:
	.BYTE $44,$2c,$00
	.BYTE $00,$00,$04
le352:
	.BYTE $24,$08,$00
FishSpr0:
	.BYTE $00,$fc,$fc,$fe,$fc
FishSpr1:
	.BYTE $00,$7b,$fc,$fc,$7c,$fc,$fc
FishSpr2:
	.BYTE $00,$7d,$7e,$fc,$7f,$80,$fc
FishSpr3:
	.BYTE $00,$81,$82,$83,$84,$85,$86
FishSpr4:
	.BYTE $00,$87,$88,$fc,$89,$8a,$fc
FishSpr5:
	.BYTE $00,$8b,$8c,$fc,$8d,$8e,$fc
FishSpr6:
	.BYTE $00,$8f,$90,$fc,$fc,$fc,$fc

.define FishSprPointers FishSpr0, FishSpr1, FishSpr2, FishSpr3, FishSpr4, FishSpr5, FishSpr6
FishSprPointersLower:
.LOBYTES FishSprPointers
FishSprPointersUpper:
.HIBYTES FishSprPointers

OAMObjectOrder1:
.BYTE $20,$38,$50,$68,$80,$98,$b0,$c8,$08
OAMObjectOrder2:
.BYTE $20,$38,$c8,$b0,$98,$80,$68,$50,$08