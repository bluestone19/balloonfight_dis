.linecont +	;Use CA65's line continuation to make the defines a little more readable

; This table is used to update the Y position for each of the objects within the character sprites.
; The list is actually read cumulatively backwards, so it starts from the 0 at the end.
; For example, the first object is 0px off from the object's position, the next is 8px down from that, and the next again, before jumping 16px back up to start the next column.
SpriteYOffsets:
	.BYTE $08,$08,$f0,$08,$08,$00

; These tables are used to define the X Offsets of each Object within a game object sprite.
; First three are first column, last three are second column. 1st is left when facing left, right when facing right. Opposite for 2nd.
; Sprites go from top to bottom, so first is top object in 1st column, second is middle in 1st, etc.
; The left is basically the same as right, just with the columns swapping positions and the offsets reversed
SpriteLeftXOffsets:
	.BYTE 	0,	0,	0,	8,	8,	8	; 0: Normal
	.BYTE 	0,	0,	1,	8,	8,	9	; 1: Bottom row shift 1px right
	.BYTE 	0,	0,<-1,	8,	8,	7	; 2: Bottom row shift 1px left
	.BYTE <-2,	0,	0,	6,	8,	8	; 3: Top row shift 2px left
	.BYTE 	2,	0,	0,	10,	8,	8	; 4: Top row shift 2px right
	.BYTE 	2,	0,	1,	10,	8,	9	; 5: Top row shift 2px right, Bottom row shift 1px right
	.BYTE 	4,	0,	0,	12,	8,	8	; 6: Top row shift 4px right
	.BYTE 	0,	0,<-2,	8,	8,	6	; 7: Bottom row shift 2px left
	.BYTE <-3,<-3,<-3,	5,	5,	5	; 8: Shift everthing 3px left
SpriteRightXOffsets:
	.BYTE 	8,	8,	8,	0,	0,	0	; 0: Normal
	.BYTE 	8,	8,	7,	0,	0,<-1	; 1: Bottom row shift 1px left
	.BYTE 	8,	8,	9,	0,	0,	1	; 2: Bottom row shift 1px right
	.BYTE 	10,	8,	8,	2,	0,	0	; 3: Top row shift 2px right
	.BYTE 	6,	8,	8,<-2,	0,	0	; 4: Top row shift 2px left
	.BYTE 	6,	8,	7,<-2,	0,<-1	; 5: Top row shift 2px left, Bottom row shift 1px left
	.BYTE 	4,	8,	8,<-4,	0,	0	; 6: Top row shift 4px left
	.BYTE 	8,	8,	10,	0,	0,	2	; 7: Bottom row shift 2px right
	.BYTE 	11,	11,	11,	3,	3,	3	; 8: Shift everything 3px right

; Frame data is 7 bytes each for players & enemies. 1st byte determines offsets for rows, next 6 are tile IDs
; Player Frames

	Fr_PlayerFlap0_2B:
		.BYTE $00,$00,$01,$02,$03,$04,$05
	Fr_PlayerFlap1_2B:
		.BYTE $00,$00,$01,$02,$03,$06,$07
	Fr_PlayerFlap2_2B:
		.BYTE $00,$00,$01,$02,$03,$06,$08

	Fr_PlayerAIdle0_2B:
		.BYTE $00,$09,$0a,$02,$0b,$0c,$05
	Fr_PlayerAIdle1_2B:
		.BYTE $00,$00,$01,$02,$03,$04,$05
	Fr_PlayerAIdle2_2B:
		.BYTE $00,$00,$01,$02,$0d,$0e,$05

	Fr_PlayerRun0_2B:
		.BYTE $00,$1a,$1b,$1c,$1d,$1e,$1f
	Fr_PlayerRun1_2B:
		.BYTE $01,$1a,$1b,$20,$1d,$1e,$fc
	Fr_PlayerRun2_2B:
		.BYTE $00,$1a,$21,$22,$1d,$23,$24

	Fr_PlayerGIdle0_2B:
		.BYTE $00,$00,$38,$35,$0d,$39,$37
	Fr_PlayerGIdle1_2B:
		.BYTE $00,$00,$34,$35,$03,$36,$37
	Fr_PlayerGIdle2_2B:
		.BYTE $00,$09,$3a,$35,$0b,$3b,$37
	Fr_PlayerFlash_2B:
		.BYTE $00,$ce,$cf,$d0,$d1,$d2,$d3

	Fr_PlayerFSkid_2B:
		.BYTE $00,$25,$26,$27,$28,$29,$2a
	Fr_PlayerTSkid_2B:
		.BYTE $01,$28,$29,$2c,$25,$26,$2b

	Fr_PlayerAPop_2B:
		.BYTE $00,$4c,$cc,$02,$4d,$cd,$05
	Fr_PlayerRunPop_2B:
		.BYTE $00,$4c,$2f,$22,$4d,$30,$24
	Fr_PlayerGIdlePop_2B:
		.BYTE $07,$4c,$2f,$35,$4d,$30,$37
	Fr_PlayerFSkidPop_2B:
		.BYTE $02,$4c,$2f,$27,$4d,$30,$2a
	Fr_PlayerTSkidPop_2B:
		.BYTE $07,$4c,$2f,$2b,$4d,$30,$2c

	Fr_PlayerFlap0_1B:
		.BYTE $00,$0f,$10,$02,$11,$12,$05
	Fr_PlayerFlap1_1B:
		.BYTE $00,$0f,$10,$02,$11,$19,$07
	Fr_PlayerFlap2_1B:
		.BYTE $00,$0f,$10,$02,$11,$19,$08

	Fr_PlayerAIdle0_1B:
		.BYTE $00,$13,$14,$02,$15,$16,$05
	Fr_PlayerAIdle1_1B:
		.BYTE $00,$0f,$10,$02,$11,$12,$05
	Fr_PlayerAIdle2_1B:
		.BYTE $03,$13,$17,$02,$15,$18,$05

	Fr_PlayerRun0_1B:
		.BYTE $04,$13,$2d,$1c,$15,$2e,$1f
	Fr_PlayerRun1_1B:
		.BYTE $05,$13,$2d,$20,$15,$2e,$fc
	Fr_PlayerRun2_1B:
		.BYTE $04,$13,$2f,$22,$15,$30,$24

	Fr_PlayerGIdle0_1B:
		.BYTE $00,$13,$3c,$35,$15,$3d,$37
	Fr_PlayerGIdle1_1B:
		.BYTE $00,$0f,$40,$35,$11,$41,$37
	Fr_PlayerGIdle2_1B:
		.BYTE $03,$13,$3e,$35,$15,$3f,$37
	Fr_PlayerFlash_1B:
		.BYTE $00,$d4,$d5,$d0,$d6,$d7,$d3

	Fr_PlayerFSkid_1B:
		.BYTE $00,$25,$31,$27,$32,$33,$2a
	Fr_PlayerTSkid_1B:
		.BYTE $02,$25,$31,$27,$32,$33,$2a

	Fr_PlayerFall0:
		.BYTE $00,$fc,$48,$42,$fc,$49,$43
	Fr_PlayerFall1:
		.BYTE $00,$fc,$48,$44,$fc,$49,$45
	Fr_PlayerFall2:
		.BYTE $00,$fc,$4a,$46,$fc,$4b,$47
	Fr_PlayerZap:
		.BYTE $00,$fc,$a4,$a5,$fc,$a6,$a7

; Enemy Frames
	Fr_EnemyPumpDown_B0:
		.BYTE $08,$fc,$71,$fc,$fc,$72,$73
	Fr_EnemyPumpUp_B0:
		.BYTE $08,$fc,$74,$fc,$fc,$75,$76
	Fr_EnemyPumpDown_B1:
		.BYTE $08,$fc,$71,$77,$fc,$72,$73
	Fr_EnemyPumpUp_B1:
		.BYTE $08,$fc,$74,$77,$fc,$75,$76
	Fr_EnemyPumpDown_B2:
		.BYTE $08,$fc,$71,$78,$fc,$72,$73
	Fr_EnemyPumpUp_B2:
		.BYTE $08,$fc,$74,$78,$fc,$75,$76
	Fr_EnemyPumpDown_B3:
		.BYTE $08,$fc,$71,$79,$fc,$72,$73
	Fr_EnemyPumpUp_B3:
		.BYTE $08,$fc,$74,$79,$fc,$75,$76

	Fr_EnemyFly0:
		.BYTE $00,$4e,$4f,$50,$51,$52,$53
	Fr_EnemyFly1:
		.BYTE $00,$4e,$4f,$50,$51,$5e,$58
	Fr_EnemyFly2:
		.BYTE $00,$4e,$4f,$50,$51,$5e,$5d

	Fr_EnemyAIdle_0:
		.BYTE $00,$54,$55,$50,$56,$57,$53
	Fr_EnemyAIdle_1:
		.BYTE $00,$4e,$4f,$50,$51,$52,$53
	Fr_EnemyAIdle_2:
		.BYTE $00,$59,$5a,$50,$5b,$5c,$53

	Fr_EnemyDeployPara0:
		.BYTE $00,$fc,$5f,$60,$fc,$61,$62
	Fr_EnemyDeployPara1:
		.BYTE $06,$63,$64,$60,$fc,$65,$62
	Fr_EnemyParachuting:
		.BYTE $00,$66,$67,$60,$68,$69,$62
	Fr_EnemyTornParachute:
		.BYTE $00,$6a,$67,$60,$6b,$69,$62
	Fr_EnemyFall0:
		.BYTE $00,$fc,$6c,$6d,$fc,$6e,$6f
	Fr_EnemyFall1:
		.BYTE $00,$fc,$6c,$6d,$fc,$6e,$70

; Player Animations
	; Air w/ 2 Balloons
		.define PlayerFlapAnim2B \
			Fr_PlayerFlap0_2B, \
			Fr_PlayerFlap1_2B, \
			Fr_PlayerFlap2_2B, \
			Fr_PlayerFlap1_2B
		.define PlayerAIdleAnim2B \
			Fr_PlayerAIdle0_2B, \
			Fr_PlayerAIdle1_2B, \
			Fr_PlayerAIdle2_2B, \
			Fr_PlayerAIdle1_2B
	; Ground w/ 2 Balloons
		.define PlayerRunAnim2B \
			Fr_PlayerRun0_2B, \
			Fr_PlayerRun1_2B, \
			Fr_PlayerRun2_2B, \
			Fr_PlayerRun1_2B
		.define PlayerGIdleAnim2B \
			Fr_PlayerGIdle0_2B, \
			Fr_PlayerGIdle1_2B, \
			Fr_PlayerGIdle2_2B, \
			Fr_PlayerGIdle1_2B
		.define PlayerFSkidAnim2B \
			Fr_PlayerFSkid_2B, \
			Fr_PlayerFSkid_2B, \
			Fr_PlayerFSkid_2B, \
			Fr_PlayerFSkid_2B
		.define PlayerTSkidAnim2B \
			Fr_PlayerTSkid_2B, \
			Fr_PlayerTSkid_2B, \
			Fr_PlayerTSkid_2B, \
			Fr_PlayerTSkid_2B
	; Pop Animations
		.define PlayerAPopAnim2B \
			Fr_PlayerAPop_2B, \
			Fr_PlayerAPop_2B, \
			Fr_PlayerAPop_2B, \
			Fr_PlayerAPop_2B
		.define PlayerRunPopAnim2B \
			Fr_PlayerRunPop_2B, \
			Fr_PlayerRunPop_2B, \
			Fr_PlayerRunPop_2B, \
			Fr_PlayerRunPop_2B
		.define PlayerGIdlePopAnim2B \
			Fr_PlayerGIdlePop_2B, \
			Fr_PlayerGIdlePop_2B, \
			Fr_PlayerGIdlePop_2B, \
			Fr_PlayerGIdlePop_2B
		.define PlayerFSkidPopAnim2B \
			Fr_PlayerFSkidPop_2B, \
			Fr_PlayerFSkidPop_2B, \
			Fr_PlayerFSkidPop_2B, \
			Fr_PlayerFSkidPop_2B
		.define PlayerTSkidPopAnim2B \
			Fr_PlayerTSkidPop_2B, \
			Fr_PlayerTSkidPop_2B, \
			Fr_PlayerTSkidPop_2B, \
			Fr_PlayerTSkidPop_2B
	; Air w/ 1 Balloon
		.define PlayerFlapAnim1B \
			Fr_PlayerFlap0_1B, \
			Fr_PlayerFlap1_1B, \
			Fr_PlayerFlap2_1B, \
			Fr_PlayerFlap1_1B
		.define PlayerAIdleAnim1B \
			Fr_PlayerAIdle0_1B, \
			Fr_PlayerAIdle1_1B, \
			Fr_PlayerAIdle2_1B, \
			Fr_PlayerAIdle1_1B
	; Ground w/ 1 Balloon
		.define PlayerRunAnim1B \
			Fr_PlayerRun0_1B, \
			Fr_PlayerRun1_1B, \
			Fr_PlayerRun2_1B, \
			Fr_PlayerRun1_1B
		.define PlayerGIdleAnim1B \
			Fr_PlayerGIdle0_1B, \
			Fr_PlayerGIdle1_1B, \
			Fr_PlayerGIdle2_1B, \
			Fr_PlayerGIdle1_1B
		.define PlayerFSkidAnim1B \
			Fr_PlayerFSkid_1B, \
			Fr_PlayerFSkid_1B, \
			Fr_PlayerFSkid_1B, \
			Fr_PlayerFSkid_1B
		.define PlayerTSkidAnim1B \
			Fr_PlayerTSkid_1B, \
			Fr_PlayerTSkid_1B, \
			Fr_PlayerTSkid_1B, \
			Fr_PlayerTSkid_1B
	; 0 Balloons
		.define PlayerFallAnim \
			Fr_PlayerFall0, \
			Fr_PlayerFall1, \
			Fr_PlayerFall2, \
			Fr_PlayerFall1
		.define PlayerZapAnim \
			Fr_PlayerZap, \
			Fr_PlayerFall0, \
			Fr_PlayerZap, \
			Fr_PlayerFall0
	; Full pointer table
		.define PlayerAnims \
			PlayerFlapAnim2B, \
			PlayerAIdleAnim2B, \
			PlayerRunAnim2B, \
			PlayerGIdleAnim2B, \
			PlayerFSkidAnim2B, \
			PlayerTSkidAnim2B, \
			PlayerAPopAnim2B, \
			PlayerRunPopAnim2B, \
			PlayerGIdlePopAnim2B, \
			PlayerFSkidPopAnim2B, \
			PlayerTSkidPopAnim2B, \
			PlayerFlapAnim1B, \
			PlayerAIdleAnim1B, \
			PlayerRunAnim1B, \
			PlayerGIdleAnim1B, \
			PlayerFSkidAnim1B, \
			PlayerTSkidAnim1B, \
			PlayerFallAnim, \
			PlayerZapAnim
	; Split tables
		PlayerAnimLower:
			.LOBYTES PlayerAnims
		PlayerAnimUpper:
			.HIBYTES PlayerAnims

	; Player Flash Animations
		.define PlayerFlashAnim1B \
			Fr_PlayerGIdle1_1B, \
			Fr_PlayerGIdle1_1B, \
			Fr_PlayerFlash_1B, \
			Fr_PlayerGIdle1_1B
		.define PlayerFlashAnim2B \
			Fr_PlayerGIdle1_2B, \
			Fr_PlayerGIdle1_2B, \
			Fr_PlayerFlash_2B, \
			Fr_PlayerGIdle1_2B
	; Full pointer table
		.define PlayerFlashAnims \
			PlayerFlashAnim1B, \
			PlayerFlashAnim2B
	; Split tables
		PlayerFlashAnimLower:
			.LOBYTES PlayerFlashAnims
		PlayerFlashAnimUpper:
			.HIBYTES PlayerFlashAnims

; Enemy Animations
	; Air w/ Balloon
		.define EnemyFlyAnim \
			Fr_EnemyFly0, \
			Fr_EnemyFly1, \
			Fr_EnemyFly2, \
			Fr_EnemyFly1
		.define EnemyAIdleAnim \
			Fr_EnemyAIdle_0, \
			Fr_EnemyAIdle_1, \
			Fr_EnemyAIdle_2, \
			Fr_EnemyAIdle_1
	; Parachuting
		.define EnemyDeployParaAnim \
			Fr_EnemyFall0, \
			Fr_EnemyFall0, \
			Fr_EnemyDeployPara0, \
			Fr_EnemyDeployPara1
		.define EnemyParachuteAnim \
			Fr_EnemyParachuting, \
			Fr_EnemyParachuting, \
			Fr_EnemyParachuting, \
			Fr_EnemyParachuting
	; Ground & Pumping
		.define EnemyGIdleAnim \
			Fr_EnemyPumpDown_B0, \
			Fr_EnemyPumpDown_B0, \
			Fr_EnemyPumpDown_B0, \
			Fr_EnemyPumpDown_B0
		.define EnemyPumpAnimB0 \
			Fr_EnemyPumpDown_B0, \
			Fr_EnemyPumpUp_B0, \
			Fr_EnemyPumpDown_B0, \
			Fr_EnemyPumpUp_B0
		.define EnemyPumpAnimB1 \
			Fr_EnemyPumpDown_B1, \
			Fr_EnemyPumpUp_B1, \
			Fr_EnemyPumpDown_B1, \
			Fr_EnemyPumpUp_B1
		.define EnemyPumpAnimB2 \
			Fr_EnemyPumpDown_B2, \
			Fr_EnemyPumpUp_B2, \
			Fr_EnemyPumpDown_B2, \
			Fr_EnemyPumpUp_B2
		.define EnemyPumpAnimB3 \
			Fr_EnemyPumpDown_B3, \
			Fr_EnemyPumpUp_B3, \
			Fr_EnemyPumpDown_B3, \
			Fr_EnemyPumpUp_B3
	; Falling/Defeat
		.define EnemyFallingAnim \
			Fr_EnemyFall0, \
			Fr_EnemyFall1, \
			Fr_EnemyFall0, \
			Fr_EnemyFall1
		.define EnemyTornParaAnim \
			Fr_EnemyTornParachute, \
			Fr_EnemyTornParachute, \
			Fr_EnemyTornParachute, \
			Fr_EnemyTornParachute
	; Full pointer table
		.define EnemyAnims \
			EnemyFlyAnim, \
			EnemyAIdleAnim, \
			EnemyDeployParaAnim, \
			EnemyParachuteAnim, \
			EnemyGIdleAnim, \
			EnemyPumpAnimB0, \
			EnemyPumpAnimB1, \
			EnemyPumpAnimB2, \
			EnemyPumpAnimB3, \
			EnemyFallingAnim, \
			EnemyTornParaAnim
	; Split tables
		EnemyAnimLower:
			.LOBYTES EnemyAnims
		EnemyAnimUpper:
			.HIBYTES EnemyAnims

PlayerAnimBalloonOffset:
	.BYTE 68, 44, 0
PlayerFlashAnimBalloonOffset:
	.BYTE 0, 0, 4
EnemyAnimBalloonOffset:
	.BYTE 36, 8, 0

; Fish Sprites
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
	; Pointer table
		.define FishSprPointers \
			FishSpr0, \
			FishSpr1, \
			FishSpr2, \
			FishSpr3, \
			FishSpr4, \
			FishSpr5, \
			FishSpr6
	; Split pointer tables
		FishSprPointersLower:
			.LOBYTES FishSprPointers
		FishSprPointersUpper:
			.HIBYTES FishSprPointers

; These tables determine offset in OAM where the game object starts.
; Every frame the game swaps the table it uses to combat sprites blocking eachother and becoming invisible. (This causes that signature NES sprite flicker instead)
OAMObjectOrder1:
	.BYTE $20,$38,$50,$68,$80,$98,$b0,$c8,$08
OAMObjectOrder2:
	.BYTE $20,$38,$c8,$b0,$98,$80,$68,$50,$08