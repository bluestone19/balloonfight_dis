MusicTrackInitData: ;Music Track Init Data
	; Offsets to each track's init data
		.BYTE PhaseClearInitData - MusicTrackInitData		; Sequence 0: Phase Clear
		.BYTE GameOverInitData - MusicTrackInitData			; Sequence 1: Game Over
		.BYTE PauseInitData - MusicTrackInitData			; Sequence 2: Pause
		.BYTE NewStartInitData - MusicTrackInitData			; Sequence 3: New Start
		.BYTE EnemyDownInitData - MusicTrackInitData		; Sequence 4: Enemy Down
		.BYTE SuperBonusInitData - MusicTrackInitData		; Sequence 5: Super Bonus
		.BYTE BonusTripInitData - MusicTrackInitData		; Sequence 6: Bonus Phase / Balloon Trip
		.BYTE EatenByFishInitData - MusicTrackInitData		; Sequence 7: Eaten By Fish
		.BYTE ParachuteInitData - MusicTrackInitData		; Sequence 8: Parachuting
		.BYTE RespawnInitData - MusicTrackInitData			; Sequence 9: Respawn
		.BYTE BubbleCollectInitData - MusicTrackInitData	; Sequence 10: Bubble Collect 

	PhaseClearInitData: ;Phase Clear Music Init Data
		.BYTE 12
		.WORD PhaseClearSq1,PhaseClearSq2,PhaseClearTri,PhaseClearNoise
	GameOverInitData: ;Game Over Music Init Data
		.BYTE 21
		.WORD GameOverSq1,GameOverSq2,GameOverTri,GameOverNoise
	PauseInitData: ;Pause Music Init Data
		.BYTE 12
		.WORD PauseSq1,$0000,PauseTri,$0000
	NewStartInitData: ;New Start Music Init Data
		.BYTE 21
		.WORD NewStartSq1,NewStartSq2,NewStartTri,NewStartNoise
	EnemyDownInitData: ;Enemy Down Music Init Data
		.BYTE 0
		.WORD $0000,EnemyDownSq2,EnemyDownTri,$0000
	SuperBonusInitData: ;Super Bonus Music Init Data
		.BYTE 0
		.WORD SuperBonusSq1,SuperBonusSq2,SuperBonusTri,SuperBonusNoise
	BonusTripInitData: ;Bonus Trip Music Init Data
		.BYTE 21
		.WORD BonusTripSq1,BonusTripSq2,BonusTripTri,BonusTripNoise
	EatenByFishInitData: ;Eaten By Fish Music Init Data
		.BYTE 21
		.WORD EatenByFishSq1,$0000,EatenByFishTri,$0000
	ParachuteInitData: ;Parachute Music Init Data
		.BYTE 21
		.WORD $0000,ParachuteSq2,ParachuteTri,$0000
	RespawnInitData: ;Respawn Music Init Data
		.BYTE 12
		.WORD RespawnSq1,RespawnSq2,RespawnTri,$0000
	BubbleCollectInitData: ;Bubble Collect Music Init Data
		.BYTE 0
		.WORD $0000,BubbleCollectSq2,BubbleCollectTri,$0000

BubbleCollectSq2:
	.BYTE $82,$02
	.BYTE $8b,$02
	.BYTE $80,$08
		.BYTE $02
		.BYTE $10
		.BYTE $02
		.BYTE $16
		.BYTE $02
		.BYTE $52
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $1a
	.BYTE $00
BubbleCollectTri:
	.BYTE $82,$02
	.BYTE $80,$10
		.BYTE $02
		.BYTE $16
		.BYTE $02
		.BYTE $52
		.BYTE $02
		.BYTE $5a
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $56
	.BYTE $81,$02

RespawnSq1:
	.BYTE $80,$12
		.BYTE $02
		.BYTE $0c
		.BYTE $02
		.BYTE $04
		.BYTE $02
		.BYTE $0c
		.BYTE $02
		.BYTE $04
		.BYTE $02
		.BYTE $2a
		.BYTE $02
	.BYTE $81,$04
		.BYTE $02
	.BYTE $80,$04
		.BYTE $02
	.BYTE $81,$04
	.BYTE $88,$02
		.BYTE $02
	.BYTE $00
RespawnSq2:
	.BYTE $88,$02
		.BYTE $02
	.BYTE $80,$04
		.BYTE $02
		.BYTE $2a
		.BYTE $02
		.BYTE $24
		.BYTE $02
		.BYTE $2a
		.BYTE $02
		.BYTE $24
		.BYTE $02
		.BYTE $1c
		.BYTE $02
	.BYTE $81,$22
		.BYTE $02
	.BYTE $80,$22
		.BYTE $02
	.BYTE $81,$24
	.BYTE $88,$02
RespawnTri:
	.BYTE $88,$02
	.BYTE $80,$56
		.BYTE $02
		.BYTE $4e
		.BYTE $02
		.BYTE $12
		.BYTE $02
		.BYTE $4e
		.BYTE $02
		.BYTE $12
		.BYTE $02
		.BYTE $0c
		.BYTE $02
	.BYTE $81,$10
		.BYTE $02
	.BYTE $80,$10
		.BYTE $02
	.BYTE $81,$12
	.BYTE $88,$02

BonusTripSq1:
	.BYTE $c3	; Repeat 3 times
		.BYTE $81,$02
			.BYTE $02
			.BYTE $1c	; C 4
			.BYTE $02
			.BYTE $02
			.BYTE $02
			.BYTE $1c	; C 4
			.BYTE $1c	; C 4
		.BYTE $ff
	.BYTE $c6	; Repeat 6 times
		.BYTE $88,$1c	; C 4
		.BYTE $ff
	.BYTE $c7	; Repeat 7 times
		.BYTE $82,$4c	; A 2
			.BYTE $4c	; A 2
			.BYTE $2a	; G 4
			.BYTE $4c	; A 2
		.BYTE $ff
	.BYTE $c6	; Repeat 6 times
		.BYTE $88,$1c	; C 4
		.BYTE $ff
	.BYTE $c4	; Repeat 4 times
		.BYTE $81,$46	; A 3
			.BYTE $02
			.BYTE $46	; A 3
			.BYTE $02
			.BYTE $32	; B 4
			.BYTE $02
			.BYTE $46	; A 3
		.BYTE $80,$2e	; A 4
			.BYTE $2e	; A 4
		.BYTE $ff
	.BYTE $c3	; Repeat 3 times
		.BYTE $82,$46	; A 3
			.BYTE $46	; A 3
		.BYTE $81,$32	; B 4
			.BYTE $32	; B 4
			.BYTE $46	; A 3
			.BYTE $2e	; A 4
		.BYTE $ff
	.BYTE $80,$0c	; E 5
		.BYTE $0c	; E 5
	.BYTE $81,$46	; A 3
		.BYTE $46	; A 3
		.BYTE $46	; A 3
	.BYTE $80,$04	; C 5
		.BYTE $04	; C 5
	.BYTE $81,$46	; A 3
		.BYTE $46	; A 3
		.BYTE $02
	.BYTE $c8	; Repeat 8 times
		.BYTE $82,$4c	; A 2
			.BYTE $4c	; A 2
			.BYTE $2a	; G 4
			.BYTE $4c	; A 2
		.BYTE $ff
	.BYTE $c2	; Repeat 2 times
		.BYTE $81,$46	; A 3
		.BYTE $80,$32	; B 4
			.BYTE $32	; B 4
		.BYTE $82,$46	; A 3
			.BYTE $04	; C 5
		.BYTE $81,$46	; A 3
			.BYTE $2a	; G 4
		.BYTE $ff
	.BYTE $c2	; Repeat 2 times
		.BYTE $81,$0c	; E 5
			.BYTE $0c	; E 5
		.BYTE $80,$04	; C 5
			.BYTE $04	; C 5
		.BYTE $81,$04	; C 5
		.BYTE $80,$2e	; A 4
			.BYTE $2e	; A 4
		.BYTE $81,$2e	; A 4
		.BYTE $82,$24	; E 4
		.BYTE $ff
	.BYTE $00
BonusTripSq2:
	.BYTE $81,$32	; B 4
		.BYTE $02
		.BYTE $02
		.BYTE $06	; C#5
		.BYTE $0c	; E 5
		.BYTE $32	; B 4
		.BYTE $02
		.BYTE $02
	.BYTE $8a,$2e	; A 4
	.BYTE $8b,$02
	.BYTE $8a,$2e	; A 4
	.BYTE $8b,$02
	.BYTE $8a,$2e	; A 4
	.BYTE $8b,$02
	.BYTE $88,$2e	; A 4
		.BYTE $32	; B 4
		.BYTE $2e	; A 4
	.BYTE $d0	; Repeat 16 times
		.BYTE $8c,$2c	; G#4
			.BYTE $24	; E 4
		.BYTE $ff
	.BYTE $d0	; Repeat 16 times
		.BYTE $2e	; A 4
		.BYTE $20	; D 4
		.BYTE $ff
	.BYTE $c3	; Repeat 3 times
		.BYTE $80,$28	; F#4
			.BYTE $02
		.BYTE $82,$02
		.BYTE $80,$2c	; G#4
			.BYTE $02
			.BYTE $32	; B 4
			.BYTE $02
			.BYTE $24	; E 4
			.BYTE $02
		.BYTE $82,$02
		.BYTE $81,$02
		.BYTE $80,$28	; F#4
			.BYTE $02
			.BYTE $06	; C#5
			.BYTE $02
			.BYTE $28	; F#4
			.BYTE $02
		.BYTE $81,$02
		.BYTE $80,$24	; E 4
			.BYTE $02
			.BYTE $32	; B 4
			.BYTE $02
			.BYTE $24	; E 4
			.BYTE $02
		.BYTE $ff
	.BYTE $80,$28	; F#4
		.BYTE $02
	.BYTE $82,$02
	.BYTE $80,$2c	; G#4
		.BYTE $02
		.BYTE $32	; B 4
		.BYTE $02
		.BYTE $24	; E 4
		.BYTE $02
	.BYTE $82,$02
	.BYTE $89,$0c
		.BYTE $0a
		.BYTE $08
		.BYTE $06
		.BYTE $32	; B 4
		.BYTE $30
		.BYTE $2e
		.BYTE $2c	; G#4
		.BYTE $2a
		.BYTE $28	; F#4
		.BYTE $26
		.BYTE $24	; E 4
		.BYTE $02
		.BYTE $02
		.BYTE $02
	.BYTE $86,$02
	.BYTE $c7	; Repeat 7 times
		.BYTE $84,$02
		.BYTE $ff
	.BYTE $c4	; Repeat 4 times
		.BYTE $80,$28	; F#4
			.BYTE $02
		.BYTE $82,$02
		.BYTE $80,$2c	; G#4
			.BYTE $02
			.BYTE $32	; B 4
			.BYTE $02
			.BYTE $24	; E 4
			.BYTE $02
		.BYTE $82,$02
		.BYTE $81,$02
		.BYTE $80,$28	; F#4
			.BYTE $02
			.BYTE $06	; C#5
			.BYTE $02
			.BYTE $28	; F#4
			.BYTE $02
		.BYTE $81,$02
		.BYTE $80,$24	; E 4
			.BYTE $02
			.BYTE $32	; B 4
			.BYTE $02
			.BYTE $24	; E 4
			.BYTE $02
		.BYTE $ff
	.BYTE $c8	; Repeat 8 times
		.BYTE $84,$02
		.BYTE $ff
BonusTripTri:
	.BYTE $81,$14
		.BYTE $02
		.BYTE $02
		.BYTE $14
		.BYTE $1a
		.BYTE $14
		.BYTE $02
		.BYTE $02
	.BYTE $88,$10
		.BYTE $10
		.BYTE $10
		.BYTE $10
		.BYTE $14
		.BYTE $10
	.BYTE $85,$3c
	.BYTE $81,$44
	.BYTE $85,$4a
	.BYTE $81,$44
	.BYTE $88,$28
		.BYTE $24
		.BYTE $20
		.BYTE $46
		.BYTE $42
		.BYTE $40
	.BYTE $c6	; Repeat 6 times
		.BYTE $81,$3c
			.BYTE $02
			.BYTE $02
			.BYTE $44
			.BYTE $02
			.BYTE $02
			.BYTE $02
			.BYTE $4a
			.BYTE $02
			.BYTE $46
			.BYTE $36
			.BYTE $36
			.BYTE $38
			.BYTE $38
			.BYTE $02
			.BYTE $3a
			.BYTE $02
		.BYTE $80,$3c
			.BYTE $3c
		.BYTE $81,$02
			.BYTE $24
			.BYTE $02
			.BYTE $02
			.BYTE $2c	; G#4
			.BYTE $24
		.BYTE $88,$24
			.BYTE $1e
			.BYTE $46
			.BYTE $36
			.BYTE $38
			.BYTE $3a
		.BYTE $ff
	.BYTE $c4	; Repeat 4 times
		.BYTE $84,$02
		.BYTE $ff
BonusTripNoise:
	.BYTE $d8	; Repeat 24 times
		.BYTE $81,$06
		.BYTE $ff
	.BYTE $c6	; Repeat 6 times
		.BYTE $88,$06
		.BYTE $ff
	.BYTE $c7	; Repeat 7 times
		.BYTE $81,$06
			.BYTE $06
		.BYTE $80,$06
			.BYTE $06
		.BYTE $81,$06
			.BYTE $06
		.BYTE $80,$06
			.BYTE $06
		.BYTE $81,$06
			.BYTE $06
		.BYTE $ff
	.BYTE $c6	; Repeat 6 times
		.BYTE $88,$06
		.BYTE $ff
	.BYTE $e0	; Repeat 32 times
		.BYTE $81,$06
			.BYTE $06
		.BYTE $ff
	.BYTE $82,$0f
	.BYTE $81,$06
		.BYTE $06
	.BYTE $ea	; Repeat 42 times
		.BYTE $06,$06
			.BYTE $06
			.BYTE $06
		.BYTE $ff
		
PauseSq1:
	.BYTE $c5	; Repeat 5 times
		.BYTE $80,$0e	; F 5
			.BYTE $58	; F 6
		.BYTE $ff
	.BYTE $00
PauseTri:
	.BYTE $c5	; Repeat 5 times
		.BYTE $80,$0e	; F 4
			.BYTE $58	; F 5
		.BYTE $ff

GameOverSq1:
	.BYTE $82,$1c	; C 4
		.BYTE $1c	; C 4
	.BYTE $c3	; Repeat 3 times
		.BYTE $82,$1c	; C 4
			.BYTE $1c	; C 4
		.BYTE $81,$1c	; C 4
			.BYTE $1c	; C 4
			.BYTE $1c	; C 4
			.BYTE $02
		.BYTE $ff
	.BYTE $c7	; Repeat 7 times
		.BYTE $88,$1c	; C 4
		.BYTE $ff
	.BYTE $00
GameOverSq2:
	.BYTE $83,$02
	.BYTE $80,$0e	; F 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $4e	; C 6
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $4e	; C 6
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $4e	; C 6
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
	.BYTE $88,$4e	; C 6	\
		.BYTE $18	; A#5	| 3
		.BYTE $16	; A 5	/
		.BYTE $12	; G 5	\
		.BYTE $0e	; F 5	| 3
		.BYTE $0c	; E 5	/
		.BYTE $0e	; F 5
GameOverTri:
	.BYTE $83,$02
	.BYTE $81,$3e
		.BYTE $3e
	.BYTE $82,$46
		.BYTE $1c
		.BYTE $46
	.BYTE $81,$02
		.BYTE $38
		.BYTE $3e
		.BYTE $02
	.BYTE $82,$46
		.BYTE $1c
	.BYTE $82,$48
		.BYTE $48
	.BYTE $81,$3e
		.BYTE $3e
	.BYTE $82,$38
	.BYTE $88,$24
		.BYTE $20
		.BYTE $1c
		.BYTE $48
		.BYTE $46
		.BYTE $42
		.BYTE $3e
GameOverNoise:
	.BYTE $82,$09
		.BYTE $09
	.BYTE $c6
		.BYTE $82,$03
			.BYTE $0c
		.BYTE $ff
	.BYTE $c6
		.BYTE $88,$06
		.BYTE $ff

ParachuteSq2:
	.BYTE $ed
		.BYTE $89,$2a
			.BYTE $02
			.BYTE $04
			.BYTE $0c
			.BYTE $02
			.BYTE $04
			.BYTE $08
			.BYTE $02
			.BYTE $30
			.BYTE $26
			.BYTE $02
			.BYTE $30
		.BYTE $ff
ParachuteTri:
	.BYTE $80,$02
	.BYTE $ed
		.BYTE $89,$0c
			.BYTE $02
			.BYTE $12
			.BYTE $4e
			.BYTE $02
			.BYTE $12
			.BYTE $18
			.BYTE $02
			.BYTE $0e
			.BYTE $08
			.BYTE $02
			.BYTE $0e
		.BYTE $ff

EatenByFishSq1:
	.BYTE $80,$42
		.BYTE $02
		.BYTE $48
		.BYTE $02
		.BYTE $1e
		.BYTE $02
		.BYTE $24
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $2a
		.BYTE $02
	.BYTE $c6
		.BYTE $8c,$30
			.BYTE $2a
		.BYTE $ff
	.BYTE $00
EatenByFishTri:
	.BYTE $80,$24
		.BYTE $02
		.BYTE $2a
		.BYTE $02
		.BYTE $30
		.BYTE $02
		.BYTE $06
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $0c
		.BYTE $02
	.BYTE $c6
		.BYTE $8c,$12
			.BYTE $18
		.BYTE $ff

EnemyDownSq2:
	.BYTE $80,$56
		.BYTE $54
		.BYTE $52
		.BYTE $50
	.BYTE $81,$02
	.BYTE $80,$5e
		.BYTE $5a
		.BYTE $54
		.BYTE $50
		.BYTE $18
		.BYTE $14
		.BYTE $10
		.BYTE $0a
		.BYTE $06
		.BYTE $30
		.BYTE $2c
		.BYTE $28
		.BYTE $02
	.BYTE $00
EnemyDownTri:
	.BYTE $80,$1a
		.BYTE $18
		.BYTE $16
		.BYTE $14
	.BYTE $81,$02
	.BYTE $80,$02
		.BYTE $5e
		.BYTE $5a
		.BYTE $54
		.BYTE $50
		.BYTE $18
		.BYTE $14
		.BYTE $10
		.BYTE $0a
		.BYTE $06
		.BYTE $30
		.BYTE $2c
		.BYTE $28

PhaseClearSq1:
	.BYTE $82,$1c
		.BYTE $02
		.BYTE $1c
		.BYTE $02
		.BYTE $02
		.BYTE $1c
		.BYTE $1c
	.BYTE $00
PhaseClearSq2:
	.BYTE $81,$10
		.BYTE $0a
		.BYTE $32
		.BYTE $28
	.BYTE $80,$32
		.BYTE $02
		.BYTE $32
		.BYTE $02
	.BYTE $82,$32
	.BYTE $81,$06
		.BYTE $02
		.BYTE $06
		.BYTE $02
	.BYTE $82,$32
PhaseClearTri:
	.BYTE $81,$54
		.BYTE $1a
		.BYTE $10
		.BYTE $0a
	.BYTE $80,$10
		.BYTE $02
		.BYTE $10
		.BYTE $02
	.BYTE $82,$10
	.BYTE $81,$16
		.BYTE $02
		.BYTE $16
		.BYTE $02
	.BYTE $82,$0a
PhaseClearNoise:
	.BYTE $83,$03
		.BYTE $0c
	.BYTE $82,$03
		.BYTE $0c
		.BYTE $0c

NewStartSq1:
	.BYTE $c2
		.BYTE $88,$1c
			.BYTE $1c
			.BYTE $1c
			.BYTE $1c
			.BYTE $1c
			.BYTE $1c
		.BYTE $83,$1c
		.BYTE $80,$04
			.BYTE $04
			.BYTE $2a
			.BYTE $02
		.BYTE $82,$1c
		.BYTE $ff
	.BYTE $81,$4c
		.BYTE $02
		.BYTE $4c
		.BYTE $02
		.BYTE $2a
		.BYTE $02
		.BYTE $4c
		.BYTE $1c
	.BYTE $81,$4c
		.BYTE $02
		.BYTE $4c
		.BYTE $02
		.BYTE $4c
	.BYTE $00
NewStartSq2:
	.BYTE $88,$2e
		.BYTE $2e
		.BYTE $2e
		.BYTE $30
		.BYTE $04
		.BYTE $30
	.BYTE $c4
		.BYTE $80,$2e
			.BYTE $04
		.BYTE $ff
	.BYTE $83,$02
	.BYTE $88,$2e
		.BYTE $2e
		.BYTE $2e
		.BYTE $30
		.BYTE $04
		.BYTE $30
	.BYTE $c4
		.BYTE $80,$2e
			.BYTE $04
		.BYTE $ff
	.BYTE $83,$02
	.BYTE $84,$02
		.BYTE $02
NewStartTri:
	.BYTE $c2
		.BYTE $88,$3e
			.BYTE $3e
			.BYTE $3e
			.BYTE $42
			.BYTE $46
			.BYTE $42
		.BYTE $84,$3e
		.BYTE $ff
	.BYTE $85,$3e
	.BYTE $81,$3e
	.BYTE $88,$1c
		.BYTE $46
		.BYTE $1c
	.BYTE $81,$02
		.BYTE $3e
		.BYTE $3e
		.BYTE $3e
	.BYTE $82,$34
		.BYTE $02
NewStartNoise:
	.BYTE $c2
		.BYTE $88,$06
			.BYTE $06
			.BYTE $06
			.BYTE $06
			.BYTE $06
			.BYTE $06
		.BYTE $82,$06
			.BYTE $06
			.BYTE $06
			.BYTE $06
		.BYTE $ff
	.BYTE $c2
		.BYTE $81,$06
			.BYTE $06
		.BYTE $80,$06
			.BYTE $06
		.BYTE $81,$06
			.BYTE $06
			.BYTE $06
			.BYTE $06
		.BYTE $80,$06
			.BYTE $06
		.BYTE $ff
	.BYTE $09

SuperBonusSq1:
	.BYTE $80,$10
		.BYTE $02
		.BYTE $10
		.BYTE $02
		.BYTE $10
		.BYTE $02
		.BYTE $0c
		.BYTE $0c
		.BYTE $0c
		.BYTE $02
		.BYTE $0c
		.BYTE $02
		.BYTE $14
		.BYTE $14
		.BYTE $14
		.BYTE $02
		.BYTE $14
		.BYTE $02
	.BYTE $85,$10
		.BYTE $00
SuperBonusSq2:
	.BYTE $80,$32
		.BYTE $02
		.BYTE $32
		.BYTE $02
		.BYTE $32
		.BYTE $02
	.BYTE $c2
		.BYTE $32
		.BYTE $32
		.BYTE $32
		.BYTE $02
		.BYTE $32
		.BYTE $02
		.BYTE $ff
	.BYTE $85,$32
SuperBonusTri:
	.BYTE $80,$54
		.BYTE $02
		.BYTE $54
		.BYTE $02
		.BYTE $54
		.BYTE $02
		.BYTE $50
		.BYTE $50
		.BYTE $50
		.BYTE $02
		.BYTE $50
		.BYTE $02
		.BYTE $56
		.BYTE $56
		.BYTE $56
		.BYTE $02
		.BYTE $56
		.BYTE $02
	.BYTE $85,$54
SuperBonusNoise:
	.BYTE $c4
		.BYTE $85,$0c
		.BYTE $ff