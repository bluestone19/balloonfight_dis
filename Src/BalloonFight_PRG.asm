;Balloon Fight (JP/USA/EU) Disassembly - PRG ROM
;-----------------------
;Starting disassembly with nesrevplus v0.3b (2018-09-18 16:39, 5ms)
;Manual disassembly and commenting by LuigiBlood
;CA65 Conversion and multi-region support by Bluestone19

REGION	.set 1	;0 = JP, 1 = US, 2 = EU

.p02

.include "Define/Macros.asm"
.include "Define/MemoryDefines.asm"

.SEGMENT "CODE"
Reset:
	lda #0		; \
	sta PPUCTRL	; | Initialize PPU registers
	sta PPUMASK	; /

	.IF REGION >= 1
	@WaitLoop1:
		lda PPUSTATUS	; \
		bpl @WaitLoop1	; |
	@WaitLoop2:
		lda PPUSTATUS	; | Get to next V-Blank
		bmi @WaitLoop2	; |
	.ENDIF
	@WaitLoop3:
		lda PPUSTATUS	; |
		bpl @WaitLoop3	; /

	sei			; Disable Interrupts
	cld			; Clear Decimal Mode (Useless on NES/Famicom)
	ldx #$ff	; \ Initialize Stack Pointer
	txs			; / to $01FF

	ldx #$12				; \
	lda #0					; | Initialize part of RAM
	@ZPGClearLoop:
		sta $00,x			; | $0012 - $00FF
		inx					; |
		bne @ZPGClearLoop	; /

	ldx #2					; \
	@HALCheckLoop:
		lda HALStringMem,x	; | Check if system was reset
		cmp HALString,x		; | by checking if $07FA has "HAL" string
		bne FullRAMInit		; |
		dex					; | If found, then skip to the end
		bpl @HALCheckLoop	; | Else, proceed with initialization code
	bmi FinishReset			; / (BUG: In US/EU it gets rewritten during gameplay so it never skips.)

	FullRAMInit:
		ldx #0					; \
		txa						; | Initialize parts of RAM
		@RAMClearLoop:
			sta $00,x			; | $0000 - $00FF: Main RAM
			sta $0700,x			; | $0700 - $07FF: Balloon Trip RAM
			inx					; |
			bne @RAMClearLoop	; /

		lda #50						; \
		sta Temp15					; | - Initialize Balloon Trip Ranking Scores -
		@BTRankScoreInit:
			lda #50					; | Add +50 to Score
			jsr AddScore			; | and update
			lda #0					; |
			sta StatusUpdateFlag	; | Clear Status Bar Update Flag
			jsr RankScoreUpdate		; | Update Balloon Trip Rank 01 to 50 Scores
			dec Temp15				; | with multiples of 50
			bne @BTRankScoreInit	; /

		ldx #14						; \
		@TopScoreLoop:
			lda DefaultTopScores,x	; | Write default High Scores
			sta GameATopScore,x		; | for each game mode
			dex						; |
			bpl @TopScoreLoop		; /

		ldx #4					; \
		@P1ScoreInit:
			lda #0				; | Initialize Player 1 Score
			sta P1Score,x		; |
			dex					; |
			bpl @P1ScoreInit	; /

		lda #0			; \
		jsr AddScore	; / Update Score

		ldx #2					; \
		@HALWriteLoop:
			lda HALString,x		; | Write "HAL" to $07FA
			sta HALStringMem,x	; | for future Reset checking
			dex					; |
			bpl @HALWriteLoop	; /
	FinishReset:
		lda #%00011110		; \ PPUMASK Shadow
		sta PPUMASKShadow	; / Enable Background and Sprites
		lda #%10010000		; \ PPUCTRL Shadow
		sta PPUCTRLShadow	; / Enable NMI at V-Blank, BG Pattern Table at $1000
		jmp StartGame	; Start

HALString:
	.BYTE "HAL"
DefaultTopScores:
	.BYTE 0,0,0,1,0	;1 Player Mode
	.BYTE 0,0,0,1,0	;2 Player Mode
	.BYTE 0,0,5,2,0	;Balloon Trip Mode

;----------------------
; NMI/PPU Management code

	NMI:
		pha			; \
		phx			; | Push A, X, Y
		phy			; /

		lda #0		; \
		sta OAMADDR	; | Upload OAM Buffer
		lda #2		; | $0200 - $02FF to OAM (via DMA)
		sta OAMDMA	; /

		lda PPUBufferPosition	; \ Check for PPU Buffer Upload
		cmp PPUBufferSize		; |
		beq NMISkipUpload		; | If Position in buffer != Buffer Size
		jsr UploadPPUAndMaskBuffer		; / Then Upload PPU Buffer
		NMISkipUpload:

		jsr UpdateStarBG	; Update Star Animation
		jsr UpdateStatusBar	; Update Status Bar
		inc FrameCounter	; Increment Frame Counter
		stppuaddr $2000	; Nametable 0 -> PPUADDR
		lda #0			; \
		sta PPUSCROLL	; | PPUSCROLL = X:0, Y:0
		sta PPUSCROLL	; /
		jsr GotoAudioMain	; Manage Audio
		lda #1					; \ Set Video Frame Done Flag
		sta FrameProcessFlag	; /

		lda GameMode		; \ If Game Mode is Balloon Fight mode
		beq EndInterrupt	; / then end NMI

		; Balloon Trip Scrolling
		@WaitLoop:
			lda PPUSTATUS	; \ Wait for V-Blank End
			bmi @WaitLoop	; /
		.IF REGION <= 1		; NTSC Timing for BT Scroll Shear (JP/US)
			ldx #4
			ldy #198
		.ELSEIF REGION = 2	; PAL Timing for BT Scroll Shear (EU)
			ldx #8
			ldy #16
		.ENDIF
		BTScrollShearLoop:
			dey						; \ Wait (X*256)+Y loops
			bne BTScrollShearLoop	; | for updating the scrolling
			dex						; | mid frame (under scoreboard)
			bne BTScrollShearLoop	; /

		lda PPUCTRLShadowBT	; \
		ora PPUCTRLShadow	; | Combine & apply both PPUCTRL Shadows
		sta PPUCTRL			; /
		lda BTXScroll	; \
		sta PPUSCROLL	; | Input X scroll value
		lda #0			; | PPUSCROLL = X:[$17], Y:0
		sta PPUSCROLL	; /
			
		EndInterrupt:
			ply			; \
			plx			; | Pull A, X, Y
			pla			; /
		rti

	BRKLoop:
		jmp BRKLoop	; Loop

	DisableNMI:
		lda PPUCTRLShadow	; \
		and #%01111111		; / Disable NMI
	WritePPUCTRL:
		sta PPUCTRL			; \
		sta PPUCTRLShadow	; | Update PPUCTRL
		rts					; /

	EnableNMI:
		lda PPUCTRLShadow	; \
		ora #%10000000		; | Enable NMI
		bne WritePPUCTRL	; /

	ClearPPUMask:
		lda #%00000000	; Clear PPUMASK
	WritePPUMask:
		pha
		jsr FinishFrame
		pla
		sta PPUMASK	; Update PPUMASK
		rts

	UploadPPUAndMask:
		lda PPUMASKShadow	; Write PPUMASK Shadow to PPUMASK
		bne WritePPUMask
	UploadPPU:
		jsr NewPPUBlock
		ldy #0					; \
		@Loop:
			lda PPUTempBlock,y	; | Put PPU Data
			sta PPUBuffer,x		; | to upload to Nametable 1
			inx					; |
			iny					; |
			cpy UploadBlockSize	; |
			bne @Loop			; /
		stx PPUBufferSize		; Update PPU Buffer Size
		rts

	CopyPPUTempBlock:
		lday PPUTempBlock
	CopyPPUBlock:
		sta ScorePointerLo	; \ Update Pointer
		sty ScorePointerHi	; / [$21] = $YYAA
		phx
		ldy #2					; \
		lda (ScorePointer),y	; | Get Data Size + 3
		clc						; | to include Address and Size info
		adc #3					; |
		sta Temp12				; /
		ldx PPUBufferSize	; Get PPU Buffer Size
		ldy #0						; \
		@Loop:
			lda (ScorePointer),y	; | Copy PPU Upload Block
			sta PPUBuffer,x			; |
			inx						; |
			iny						; |
			cpy Temp12				; |
			bne @Loop				; /
		stx PPUBufferSize		; Update PPU Buffer Size
		plx
		rts

	NewPPUBlock:
		ldx PPUBufferSize		; X = PPU Buffer Size
		lda #0		; \
		sta Temp12	; |
		lda $55		; |
		aslr 4		; | PPUADDR_H = 001000XX
		rol Temp12	; | PPUADDR_L = DDD00000 | [$54]
		asl			; |
		rol Temp12	; |
		ora $54		; |
		pha			; /
		lda Temp12		; \
		ora #$20		; | Put PPUADDR High Byte
		sta PPUBuffer,x	; / (From Nametable 1)
		inx				; \
		pla				; | Put PPUADDR Low Byte
		sta PPUBuffer,x	; /
		inx					; \
		lda UploadBlockSize	; | Put Upload Size
		sta PPUBuffer,x		; /
		inx	; \ Return:
		rts	; / X = Current PPU Buffer Address

	UploadPPUAndMaskBuffer:
		phy	; \ Push Y & X
		phx	; /
		jsr ContinueBufferUpload 
		plx	; \ Pull X & Y
		ply	; /
		rts
		ContinueBufferUpload:
			ldx PPUBufferPosition	; Get Current Position in PPU Upload Buffer
			lda PPUBuffer,x		; \
			inx					; |
			sta PPUAddressHi	; |
			sta PPUADDR			; | Get PPU Address
			lda PPUBuffer,x		; | And Set PPUADDR
			inx					; |
			sta PPUADDR			; /
			ldy PPUBuffer,x	; Get Upload Size to Y
			inx
			@Loop:
				lda PPUBuffer,x	; \
				inx				; |
				sta PPUDATA		; | Upload Y bytes to PPU
				dey				; |
				bne @Loop		; /
			lda PPUAddressHi	; \
			cmp #$3f			; | If Upload Address != $3FXX (Palette Data)
			bne @Skip			; / Then Skip this section
			stppuaddr $3F00	; PPUADDR = $3F00
			sta PPUADDR	; Write $00 to PPUADDR twice
			sta PPUADDR
			@Skip:
			stx PPUBufferPosition		; \
			cpx PPUBufferSize			; | If PPU Buffer Position != PPU Buffer Size
			bne ContinueBufferUpload	; / Then Upload more data
			rts

.include "BalloonTrip.asm"

.include "FishManage.asm"
.include "SparkManage.asm"
.include "PropellerManage.asm"
.include "BalloonManage.asm"

;----------------------
; Bonus Phase code
;----------------------

InitializeBonusPhase:
	lda #$20		; \ Play Bonus Phase music
	sta MusicReq	; /
	jsr SetBonusPhase
	jsr InitBalloons
	ldx TwoPlayerFlag		; \
	@PlayerInitLoop:
		lda P1Lives,x		; |
		bmi @SkipInit		; | Init players that still have lives
		jsr InitPlayerType	; |
		@SkipInit:			; /
		dex
		bpl @PlayerInitLoop
	ldx #0					; \
	stx PlayerInvincible	; | Clear Player Invincibility
	stx PlayerInvincible+1	; /
	lda #20					; \ 20 Balloons per Bonus Phase
	sta BonusBalloonStock	; /
BonusPhaseGameLoop:
		jsr Pause	; Check for pausing
		inc StarUpdateFlag	; Update stars each frame
		jsr ManageScorePopup
		jsr ObjectManage
		lda BonusBalloonStock		; \
		beq @OutOfBalloons			; | Manage spawning balloons if there are some left
		jsr ManageBonusBalloonSpawn	; /
		@OutOfBalloons:
		jsr ManageBonusBalloons
		lda BonusBalloonStock
		bne BonusPhaseGameLoop
		ldx #9
		@BalloonCheckLoop:
			lda BalloonStatus,x
			bpl BonusPhaseGameLoop
			dex
			bpl @BalloonCheckLoop
		lda FrameCounter
		bne BonusPhaseGameLoop
	jsr ClearPPU
	ldx #2
	stx StatusUpdateFlag
	jsr WaitXFrames
	lday BonusEndScreenPalette
	jsr CopyPPUBlock
	lday SuperBonusTextAttribute
	jsr CopyPPUBlock
	lday BonusStatusAttribute
	jsr CopyPPUBlock
	ldx TwoPlayerFlag
	@BonusEndPlayerLoop:
		lda #32					; \ Move Players into place for Bonus Phase end screen
		sta ObjectXPosInt,x		; | Set X to 32px
		lda PlayerBonusYPos,x	; |
		sta ObjectYPosInt,x		; / Set Y to 80px or 112px
		lda #3				; \ Set player status to 3 (Ground Idle)
		sta ObjectStatus,x	; /
		lda #1					; \ Set players facing right
		sta ObjectDirection,x	; /
		jsr InitPlayerType
		jsr DrawObjectX
		dex
		bpl @BonusEndPlayerLoop
	lda #68					; \
	sta BalloonXPosInt		; | Set bonus end screen Balloon X positions to 68px
	sta BalloonXPosInt+1	; /
	lda #84					; \
	sta BalloonYPosInt		; | Set bonus end screen Balloon Y positions to 84px and 116px
	lda #116				; |
	sta BalloonYPosInt+1	; /
	lda #1				; \
	sta BalloonStatus	; | Set bonus end screen Balloon statuses to 1
	sta BalloonStatus+1	; /
	ldx TwoPlayerFlag				; \
	@EndBalloonLoop:
		jsr ManageBalloonXSprite	; | Draw only the balloons necessary for the number of players
		dex							; /
		bpl @EndBalloonLoop
	jsr Wait20Frames
	lda #$2b			; \
	sta PPUTempBlock	; |
	lda #$24			; | "x  "
	sta PPUTempBlock+1	; |
	sta PPUTempBlock+2	; /
	lda #12
	sta $54
	lda #11
	sta $55
	lda #5
	sta UploadBlockSize
	lda P1BonusBalloons
	jsr ld1c9
	lda TwoPlayerFlag
	beq @SkipP2
	lda #15
	sta $55
	lda P2BonusBalloons
	jsr ld1c9
	@SkipP2:
	jsr Wait20Frames
	lda BalloonPts
	sta $57
	lda #0
	sta $58
	sta $59
	lda #8
	sta $54
	lda #11
	sta $55
	lda #3
	sta $56
	lda BalloonPts
	jsr UploadPPU
	lda TwoPlayerFlag
	beq @SkipP22
	lda #15
	sta $55
	jsr UploadPPU
	@SkipP22:
	lda #<-1			; \
	sta BalloonStatus	; | Make both balloons pop
	sta BalloonStatus+1	; /
	ldx TwoPlayerFlag
	@EndBalloonLoop2:
		jsr ManageBalloonXSprite
		dex
		bpl @EndBalloonLoop2
	lda #$02	; \ Play Pop SFX
	sta SFX1Req	; /
	ldx #2
	jsr WaitXFrames
	ldx TwoPlayerFlag
	@EndBalloonLoop3:
		jsr ManageBalloonXSprite
		dex
		bpl @EndBalloonLoop3
	jsr ld1a0
	jsr Wait20Frames
	lda #$01	; \ End SFX
	sta SFX1Req	; /
	jsr CheckTotalBonusBalloons
	bne @SkipSuperBonus
	lday PerfectText
	jsr CopyPPUBlock
	jsr FinishFrame
	ldx #26
	@SBLoadLoop:
		lda SuperBonusText,x	; \
		sta PPUTempBlock,x		; | Load Super Bonus Text to PPUTempBlock
		dex						; /
		bpl @SBLoadLoop
	lda SuperBonusPtsUpper
	sta PPUTempBlock+17
	lda SuperBonusPtsLower
	sta PPUTempBlock+18
	jsr CopyPPUTempBlock
	lda #$10		; \ Play super bonus jingle
	sta MusicReq	; /
	@SkipSuperBonus:
	ldx #120
	jsr WaitXFrames
	jsr ld1a0
	ld070:
		lda #0
		sta TargetUpdateScore
		ldx #4
		jsr ld213
		jsr CopyPPUTempBlock
		lda TwoPlayerFlag
		beq ld08e
		inc TargetUpdateScore
		ldx #$12
		jsr ld213
		lda #$65
		ldy #0
		jsr CopyPPUBlock
		ld08e:
		lda #$01
		sta SFX2Req
		ldx #2
		jsr WaitXFrames
		lda $5d
		cmp #$24
		bne ld070
		lda TwoPlayerFlag
		beq @FinishMainBonusPts
		lda a:PPUTempBlock+20	;Absolute addressing on ZP location?
		cmp #$24
		bne ld070
	@FinishMainBonusPts:
	ldx #10			; \ Wait 10 frames
	jsr WaitXFrames	; /
	jsr CheckTotalBonusBalloons
	bne @SkipSuperBonus
	lda SuperBonusPtsUpper	; \
	sta ScoreDigit4			; | Put Super Bonus points into upper ScoreDigits for awarding
	lda SuperBonusPtsLower	; |
	sta ScoreDigit5			; /
	lda TwoPlayerFlag
	sta TargetUpdateScore
	@SBPlayerLoop:
		jsr UpdateScore
		dec TargetUpdateScore
		bpl @SBPlayerLoop
	lda #$01	; \ Play point count SFX
	sta SFX2Req	; /
	jsr Wait20Frames
	@SkipSuperBonus:
	lda #0			; \
	sta ScoreDigit4	; | Clear upper two ScoreDigits
	sta ScoreDigit5	; /
	ldx #1
	@LifeCheckLoop:
		lda P1Lives,x
		bpl @PlayerXAlive
		sta ObjectBalloons,x
		@PlayerXAlive:
		dex
		bpl @LifeCheckLoop
	jmp PhaseCleared

SetBonusPhase:
	ldx BonusPhaseIntensity			; \ Set up Bonus Phase
	lda BalloonPtsData,x			; | according to Intensity (max 4)
	sta BalloonPts					; | 
	lda BalloonRiseSpeedData,x		; | Set points per balloon
	sta BalloonRiseSpeed			; | Set rising speed
	lda SuperBonusPtsUpperData,x	; | Set super bonus points
	sta SuperBonusPtsUpper			; |
	lda SuperBonusPtsLowerData,x	; |
	sta SuperBonusPtsLower			; /
	cpx #4					; \ Increment Bonus Phase Intensity
	bcs @SkipIncrease		; | until maximum (4)
	inc BonusPhaseIntensity	; /
	@SkipIncrease:
	lda #0				; \
	sta P1BonusBalloons	; | Initialize Balloon Counters
	sta P2BonusBalloons	; /
	rts

BalloonPtsData:	; Points per balloon
	.BYTE 3,5,7,7,7
BalloonRiseSpeedData:	; Rising Speed
	.BYTE $80,$90,$98,$a0,$a8
SuperBonusPtsUpperData:	; Super Bonus x0000 Points
	.BYTE 1,1,2,2,3
SuperBonusPtsLowerData:	; Super Bonus 0x000 Points
	.BYTE 0,5,0,5,0

CheckTotalBonusBalloons:
	lda P1BonusBalloons
	cadc P2BonusBalloons
	cmp #20
	rts

BonusEndScreenPalette:
	.BYTE $3f,$00,$10	;$3F00, 16 bytes
	.BYTE $0f,$30,$30,$30	;White, 	White, 		White
	.BYTE $0f,$30,$27,$15	;White, 	Orange, 	Magenta
	.BYTE $0f,$30,$02,$21	;White,		Blue,		Cyan
	.BYTE $0f,$16,$16,$16	;Red,		Red,		Red
BonusTotalText:
	;P1 Bonus Total Text
	.BYTE $21,$73,11	; 11 Bytes to 0x2173
	.BYTE $29,$00,$00,$00,$00,$00,$24,$19,$1d,$1c,$26	; "=00000 PTS."
	;P2 Bonus Total Text
	.BYTE $21,$f3,11	; 11 Bytes to 0x21F3
	.BYTE $29,$00,$00,$00,$00,$00,$24,$19,$1d,$1c,$26	; "=00000 PTS."
SuperBonusTextAttribute:
	.BYTE $23,$e8,8		; 8 Bytes to 0x23E8
	.BYTE $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	; Set attributes for the SUPER BONUS text
BonusStatusAttribute:
	.BYTE $23,$c0,8		; 8 Bytes to 0x23C0
	.BYTE $40,$50,$50,$50,$50,$90,$a0,$a0
PerfectText:
	.BYTE $22,$88,17	; 17 Bytes to 0x2288
	.BYTE $19,$24,$0e,$24,$1b,$24,$0f,$24,$0e,$24,$0c,$24,$1d,$24,$2c,$2c,$2c ; "P E R F E C T !!!"
SuperBonusText:
	.BYTE $22,$c6,23	; 23 Bytes to 0x22C6
	.BYTE $1c,$1e,$19,$0e,$1b,$24,$0b,$18,$17,$1e,$1c,$24,$24,$24,$01,$00,$00,$00,$00,$19,$1d,$1c,$2c ; "SUPER BONUS   10000PTS."
PlayerBonusYPos:
	.BYTE 80,112

ld1a0:
	ldx #28
	@Loop:
		lda BonusTotalText,x
		sta PPUTempBlock,x
		dex
		bpl @Loop
	ldx #4
	ldy P1BonusBalloons
	jsr ld1dc
	ldx #18
	ldy P2BonusBalloons
	jsr ld1dc
	jsr CopyPPUTempBlock
	lda TwoPlayerFlag
	bne ld1c2
	rts

ld1c2:
	lda #101
	ldy #0
	jmp CopyPPUBlock
ld1c9:
	ldy #0
	ld1cb:
	cmp #10
	bcc ld1d5
	iny
	sbc #10
	jmp ld1cb
	ld1d5:
	sty $5a
	sta $5b
	jmp UploadPPU

ld1dc:
	dey				; \ If Y == 0, erase zeroes
	bmi EraseZeroes	; /
	lda BalloonPts
	cadcx PPUTempBlock+2
	cmp #10
	bcc ld1ed
	sbc #10
	inc PPUTempBlock+1,x
	ld1ed:
	sta PPUTempBlock+2,x
	lda PPUTempBlock+1,x
	cmp #10
	bcc @SkipMod10
	sbc #10
	inc PPUTempBlock,x
	sta PPUTempBlock+1,x
	@SkipMod10:
	jmp ld1dc

EraseZeroes:
	ldy #0
	@Loop:
		lda PPUTempBlock,x	; Get tile X in PPUTempBlock
		beq @Continue	; If this tile is 0, continue
		cmp #$24	; \ Return if not empty tile
		bne :+		; /
		@Continue:
		lda #$24			; \ Set tile to blank
		sta PPUTempBlock,x	; /
		inx	; Go to next tile
		iny		; \ Only loop up to 4 times
		cpy #4	; /
		bne @Loop
	:rts

ld213:
	lda $59,x
	cmp #$24
	beq ld243
	tay
	bne ld238
	lda $58,x
	cmp #$24
	beq ld243
	lda $58,x
	bne ld232
	lda PPUTempBlock,x
	cmp #$24
	beq ld243
	lda #10
	sta $58,x
	dec PPUTempBlock,x
	ld232:
	lda #10
	sta $59,x
	dec $58,x
	ld238:
	dec $59,x
	phx
	lda #10
	jsr AddScore
	plx
	ld243:
	jmp EraseZeroes

ClearPPU:	;Clear PPU?
	jsr ClearPPUMask
	jsr DisableNMI
	stppuaddr $2000	; Nametable 0 -> PPUADDR
	jsr ClearNametable	; \ Clear Nametable 0
	jsr ClearNametable	; / Clear Nametable 1
	jsr EnableNMI
	jsr UploadPPUAndMask
	ldx #63
	ldy #0
	sty StarUpdateFlag
	@ClearLoop:
		lda #$f0		; \
		sta OAM,y		; |
		inyr 4			; | Hide all sprites
		dex				; |
		bpl @ClearLoop	; /
	rts

ClearNametable:
	ldx #$F0			; \
	lda #$24			; |
	@ClearLoop:
		sta PPUDATA		; |
		sta PPUDATA		; | Fill PPU Nametable with empty tiles
		sta PPUDATA		; | $3C0 bytes ($F0 * 4)
		sta PPUDATA		; |
		dex				; |
		bne @ClearLoop	; /

	ldx #$40			; \
	lda #0				; |
	@ClearLoop2:
		sta PPUDATA		; | Fill attributes with 0
		dex				; | $40 bytes
		bne @ClearLoop2	; /
	rts			; Total: $400 bytes

InitGameModeAB:
	jsr ClearPPUMask
	jsr DisableNMI
	lda GameMode		; Check Game Mode for Initialization
	beq @Skip
	jmp InitBalloonTrip
	@Skip:	; Initialize Balloon Fight Game Mode
	ldy CurrentPhaseHeader	; \
	lda PhasePointersLow,y	; |
	sta LoadPointerLo		; | Load Phase Graphics
	lda PhasePointersHigh,y	; |
	sta LoadPointerHi		; |
	jsr UploadBackground	; /
	ldx #0							; \
	@CloudLoop:
		jsr GetByteFromLoadPointer	; |
		cmp #$ff					; | Load Clouds (XX YY)
		beq @FinishClouds			; | until one has $FF as
		sta PPUBlockAddrLo			; | X coordinate
		jsr GetByteFromLoadPointer	; |
		sta PPUBlockAddrHi			; /
		ldy #3							; \
		@Loop1:
			jsr ld4fb_setppuaddr_render	; |
			lda #4						; |
			sta Temp12					; | Render Cloud
			lda CloudTiles,y			; | to the screen
			@Loop2:
				sta PPUDATA				; |
				cadc #4					; |
				dec Temp12				; |
				bne @Loop2				; |
			inc PPUBlockAddrHi			; |
			dey							; |
			bpl @Loop1					; /
		lda PPUBlockAddrHi
		ssbc #4
		sta PPUBlockAddrHi
		jsr GetAttributeAddr
		sta CloudAttrAddrLo0,x
		incr 2, PPUBlockAddrLo
		jsr GetAttributeAddr
		sta CloudAttrAddrLo1,x
		incr 2, PPUBlockAddrHi
		jsr GetAttributeAddr
		sta CloudAttrAddrLo3,x
		decr 2, PPUBlockAddrLo
		jsr GetAttributeAddr
		sta CloudAttrAddrLo2,x
		stx SelectedCloud
		lda #3
		jsr UpdateCloudAttribute
		jsr UploadPPUAndMaskBuffer
		ldx SelectedCloud
		lda PPUBlockAddrLo
		aslr 3
		cadc #16
		sta CloudXPos,x
		lda PPUBlockAddrHi
		aslr 3
		sta CloudYPos,x
		inx
		jmp @CloudLoop	; Load another cloud data
	@FinishClouds:
	dex				; \ Write amount of clouds to RAM
	stx CloudCount	; /
	ldx #0							; \
	@PropellerLoop:
		jsr GetByteFromLoadPointer	; |
		cmp #$ff					; | Load Propellers (XX YY TT)
		beq @FinishPropellers		; | until one has $FF as
		sta PPUBlockAddrLo			; | X coordinate
		jsr GetByteFromLoadPointer	; |
		sta PPUBlockAddrHi			; |
		jsr GetByteFromLoadPointer	; |
		sta PropellerAngle,x		; /
		lda PPUBlockAddrLo	; \
		aslr 3				; | X Pos = (Tile X Pos * 8) + 12
		adc #12				; |
		sta PropellerXPos,x	; /
		lda PPUBlockAddrHi	; \
		aslr 3				; | Y Pos = (Tile Y Pos * 8) + 12
		adc #12				; |
		sta PropellerYPos,x	; /
		lda #0					; \ Set propeller state to 0 (not spinning)
		sta PropellerState,x	; /
		jsr ld4fb_setppuaddr_render
		sta PropellerAddrLo,x
		lda Temp13
		sta PropellerAddrHi,x
		jsr DrawUploadPropellerX
		jsr SetPropellerAttribute	; \ Set all four propeller attributes
		incr 2, PPUBlockAddrLo		; |
		jsr SetPropellerAttribute	; |
		incr 2, PPUBlockAddrHi		; |
		jsr SetPropellerAttribute	; |
		decr 2, PPUBlockAddrLo		; |
		jsr SetPropellerAttribute	; /
		inx
		jmp @PropellerLoop	; Load another propeller data
	@FinishPropellers:
	dex					; \ Write amount of propellers to RAM
	stx PropellerCount	; /
	jsr GetByteFromLoadPointer	; \
	sta DataPointerLo			; | Load Enemy Data Pointer
	jsr GetByteFromLoadPointer	; |
	sta DataPointerHi			; /
	ldy #0				; \
	lda (DataPointer),y	; | Load Enemy Amount
	tax					; |
	dex					; |
	bpl @LoadEnemy		; /
	inc PhaseType		; If No Enemies then it's a Bonus Phase Type
	jmp @LoadCollision	; Skip Enemy Loading
	@LoadEnemy:
		iny
		@LoadLoop:
			lda (DataPointer),y		; \ Load Enemy X Position
			iny						; |
			sta ObjectXPosInt+2,x	; /
			lda (DataPointer),y		; \ Load Enemy Y Position
			iny						; |
			sta ObjectYPosInt+2,x	; /
			lda (DataPointer),y	; \ Load Enemy Type
			iny					; |
			sta ObjectType+2,x	; /
			lda #2					; \ Initialize Enemy Status
			sta ObjectStatus+2,x	; / (02 = Sitting)
			lda #1					; \ Initialize Enemy Balloons
			sta ObjectBalloons+2,x	; / (01 = Sitting/Parachute)
			lda EnemyStartDelay		; \ Initialize Enemy Anim Timer
			sta ObjectAnimTimer+2,x	; /
			dex
			bpl @LoadLoop	; Load another enemy data
	@LoadCollision:
		jsr GetByteFromLoadPointer	; \ Load Amount of Platforms
		sta PlatformCount			; /
		jsr GetByteFromLoadPointer	; \
		sta LeftPointerLo			; | Load Platform Collision Pointer
		jsr GetByteFromLoadPointer	; | Left Side
		tay							; |
		sta LeftPointerHi			; |
		lda LeftPointerLo			; /
		jsr NextPlatformPointer	; \
		sta RightPointerLo		; | Load Right Side Platform Collision Pointer
		sty RightPointerHi		; /
		jsr NextPlatformPointer	; \
		sta TopPointerLo		; | Load Top Side Platform Collision Pointer
		sty TopPointerHi		; /
		jsr NextPlatformPointer	; \
		sta BottomPointerLo		; | Load Bottom Side Platform Collision Pointer
		sty BottomPointerHi		; /
FinishGameLoad:
	jsr InitializeStarBG
	jsr LoadPalette
	jsr EnableNMI
	jmp UploadPPUAndMask	; Then return

LoadPalette:
	ldx #34					; \
	@PaletteLoop:
		lda BasePalette,x	; | Copy Base Palette
		sta PPUTempBlock,x	; | to PPU Temp Block
		dex					; |
		bpl @PaletteLoop	; /
	lda PhaseType			; \ Check Phase Type...
	bne @LoadBonusPalette	; /
	lda CurrentPhaseHeader	; \ ...If Normal Phase
	and #%1100				; | Select Palette based
	ora #%0011				; | on current level header
	tay						; /
	ldx #3						; \
	@GrassLoop:
		lda GrassPalettes,y		; | Copy Single Palette Data
		sta PPUTempBlock+3,x	; | to Background Palette 1
		dey						; | to PPU Temp Block
		dex						; |
		bpl @GrassLoop			; /
	@Next:
	jmp CopyPPUTempBlock
	@LoadBonusPalette:
		ldx BonusPhaseIntensity	; ...If Bonus Phase
		lda BalloonPalPointerLower,x	; \
		sta LoadPointerLo				; | Select Balloon Palette
		lda BalloonPalPointerUpper,x	; | based on Intensity Level
		sta LoadPointerHi				; /
		ldx #3						; \
		ldy #7						; |
		@CopyLoop1:
			lda (LoadPointer),y		; | Copy Second Palette Data
			sta PPUTempBlock+27,x	; | to Sprite Palette 2
			dey						; | to PPU Temp Block
			dex						; |
			bpl @CopyLoop1			; /
		lda GameMode	; \ If Balloon Trip mode
		bne @Next		; / then stop and copy PPU Temp Block as is
		@CopyLoop2:
			lda (LoadPointer),y		; \ Copy First Palette Data
			sta PPUTempBlock+3,y	; | to Background Palette 1
			dey						; | to PPU Temp Block
			bpl @CopyLoop2			; /
		bmi @Next

BasePalette:
	.BYTE $3f,$00,$20	; 32 bytes, put at $3F00 (Palettes)
	.BYTE $0f,$2a,$09,$07	; Black (BG), Lime Green, 	Dark Green, Brown
	.BYTE $0f,$30,$27,$15	; Black (BG), White,		Orange, 	Magenta
	.BYTE $0f,$30,$02,$21	; Black (BG), White,		Blue,		Cyan
	.BYTE $0f,$30,$00,$10	; Black (BG), White,		Dark Gray,	Light Gray
	.BYTE $0f,$16,$12,$37	; Black (BG), Red,			Blue,		Beige
	.BYTE $0f,$12,$16,$37	; Black (BG), Blue,			Red,		Beige
	.BYTE $0f,$17,$11,$35	; Black (BG), Dark Orange,	Blue,		Pink
	.BYTE $0f,$17,$11,$2b	; Black (BG), Dark Orange,	Blue,		Light Green
GrassPalettes:
	.BYTE $0f,$2a,$09,$07	; Black (BG), Lime Green,	Dark Green,	Brown
	.BYTE $0f,$26,$06,$07	; Black (BG), Light Orange,	Dark Red,	Brown
	.BYTE $0f,$1b,$0c,$07	; Black (BG), Blue Green,	Dark Blue,	Brown
	.BYTE $0f,$2c,$01,$06	; Black (BG), Cyan,			Dark Blue,	Dark Red

.define BonusBalloonPalettes GreenBalloonPalette, OrangeBalloonPalette, RedBalloonPalette, RedBalloonPalette, RedBalloonPalette
BalloonPalPointerLower:
	.LOBYTES BonusBalloonPalettes
BalloonPalPointerUpper:
	.HIBYTES BonusBalloonPalettes

GreenBalloonPalette:
	.BYTE $0f,$02,$08,$06	; Black (BG), Dark Blue,	Dark Green,	Dark Red
	.BYTE $0f,$2b,$30,$12	; Black (BG), Light Green,	White,		Blue
OrangeBalloonPalette:
	.BYTE $0f,$07,$0a,$19	; Black (BG), Brown,		Dark Green,	Green
	.BYTE $0f,$26,$30,$2b	; Black (BG), Light Orange,	White,		Light Green
RedBalloonPalette:
	.BYTE $0f,$07,$0c,$1c	; Black (BG), Brown,		Dark Blue,	Dark Cyan
	.BYTE $0f,$15,$30,$26	; Black (BG), Red,			White,		Light Orange

NextPlatformPointer:
	sec
	adc PlatformCount
	bcc :+
	iny
	:rts

CloudTiles:
	.BYTE $7f,$7e,$7d,$7c

UploadBackground:	;Argument: $001D = Pointer to pointers to screen data
	jsr GetByteFromLoadPointer
	sta DataPointerLo
	jsr GetByteFromLoadPointer
	sta DataPointerHi
	tax
	beq :+
ld4a4:
	jsr GetByteFromDataPointer
	tax
	beq UploadBackground
	and #%01111111
	sta PPUADDR
	jsr GetByteFromDataPointer
	sta PPUADDR
	jsr GetByteFromDataPointer
	sta Temp12
	txa
	and #%10000000
	lsrr 5
	ora PPUCTRLShadow
	sta PPUCTRL
	txa
	and #%01000000
	bne ld4d8
ld4cc:
	jsr GetByteFromDataPointer
	sta PPUDATA
	dec Temp12
	bne ld4cc
	beq ld4a4
ld4d8:
	jsr GetByteFromDataPointer
ld4db:
	sta PPUDATA
	dec Temp12
	bne ld4db
	beq ld4a4
	:rts

GetByteFromLoadPointer:
	ldy #0				; \ (No offset)
	lda (LoadPointer),y	; / Load Byte from LoadPointer into A
	inc LoadPointerLo	; \
	bne :+				; | Increment LoadPointer
	inc LoadPointerHi	; /
	:rts

GetByteFromDataPointer:
	ldy #0				; \ (No offset)
	lda (DataPointer),y	; / Load Byte from DataPointer into A
	inc DataPointerLo	; \
	bne :+				; | Increment DataPointer
	inc DataPointerHi	; /
	:rts

ld4fb_setppuaddr_render:
	lda $55
	sta Temp12
	lda #0
	aslr 4, Temp12
	rol
	asl Temp12
	rol
	ora #%00100000
	sta PPUADDR
	sta Temp13
	lda Temp12
	ora $54
	sta PPUADDR
	rts

GetAttributeAddr:
	lda UploadTileY	; \
	and #%11111100	; | Temp12 = (TileY && -4) * 2
	asl				; | Attribute increases by 8 for every 4 tiles down
	sta Temp12		; /
	lda UploadTileX	; \
	lsrr 2			; | Attribute increases by 1 for every 4 tiles right
	ora Temp12		; / A = (TileX / 4) || Temp12
	ora #$C0	; Attribute bytes are from 23C0-23FF
	pha	; Push attribute byte address to stack
	lda UploadTileY	; \
	and #2			; | Temp12 = TileY && 2
	sta Temp12		; / Every other 2 rows, add 2 to attribute quadrant index
	lda UploadTileX	; \
	and #2			; | A = (TileX && 2) / 2
	lsr				; / Every other 2 columns, add 1 to attribute quadrant index
	ora Temp12	; A = A || Temp12 (The quadrant of the attribute this tile is in)
	tay	; Y = quadrant of attribute
	pla	; A = attribute byte address
	rts

SetPropellerAttribute:
	lda #$23	; \ Nametable 0's Attributes all have
	sta PPUADDR	; / Addresses like $23XX
	jsr GetAttributeAddr	; \ Get lower byte of attribute
	sta PPUADDR				; /
	lda PPUDATA				; \
	lda PPUDATA				; | Get & modify attribute byte around Propeller
	and PropellerAttrAnd,y	; | Set target attribute metatile to use palette 1 (White/Orange/Red)
	ora PropellerAttrOr,y	; /
	pha	; Push new attribute byte to stack
	lda #$23				; \
	sta PPUADDR				; | Set PPUADDR back to attribute byte
	jsr GetAttributeAddr	; |
	sta PPUADDR				; /
	pla			; \ Store new attribute byte
	sta PPUDATA	; /
	rts

PropellerAttrAnd:
	.BYTE %11111100
	.BYTE %11110011
	.BYTE %11001111
	.BYTE %00111111
PropellerAttrOr:
	.BYTE %00000001
	.BYTE %00000100
	.BYTE %00010000
	.BYTE %01000000

DrawUploadPropellerX:
	jsr DrawPropellerX
	jmp UploadPPUAndMaskBuffer

InitBalloonTrip:	; Initialize Balloon Trip Game Mode
	lday $23C0				; \ Set Attribute for upper-left screen
	jsr SetBTBGAttribute	; /
	lday $27C0				; \ Set Attribute for upper-right screen
	jsr SetBTBGAttribute	; / (Redundant due to horizontal mirroring?)
	ldya $2360		; \ Draw water in nametable 0
	jsr DrawBTWater	; /
	ldya $2760		; \ Draw water in nametable 1
	jsr DrawBTWater	; /
	inc PhaseType	; Set phase type to 1 (Bonus/Balloon Trip)
	jmp FinishGameLoad	; Finish setting up game and return

SetBTBGAttribute:
	styappuaddr	;Start from PPU Address in Y and A (Y is upper, A is lower)
	ldx #0	; Loop from X = 0 to 7
	@StatusLoop:
		lda BGAttributes,x	; \ Load status bar attribute data into PPUDATA
		sta PPUDATA			; /
		inx		; \ Only first 8 bytes (First 4 rows of tiles)
		cpx #8	; /
		bne @StatusLoop
	lda #%00000000	; Attr block of all 0 (Grass)
	ldx #40	; Next 40 attribute bytes (Next 20 rows of tiles)
	jsr @AttrSetLoop
	lda #%10101010	; Attr block of all 2 (Water)
	ldx #16	; Last 16 attribute bytes (Last 6 rows of tiles, 2 rows are cut off)
	@AttrSetLoop:
		sta PPUDATA	; \ Write attribute byte A to PPUDATA
		dex			; / X times
		bne @AttrSetLoop
	rts

DrawBTWater:
	styappuaddr	;Start from PPU Address in Y and A (Y is upper, A is lower)
	ldx #32		; \ Draw the top of the water
	lda #$58	; | 32 tiles (one row) of tile $58-$5B repeating
	jsr @Draw	; /
	ldx #64		; \ Draw the rest of the water
	lda #$5c	; / 64 tiles (two rows) of tile $5C-$5F repeating
	@Draw:
		sta Temp12	; Temp12 = Starting tile offset
		@Loop:
			txa			; \
			and #3		; | A = (4 - X) % 4 + Temp12 (Effectively, but technically only works for Temp12 % 4 == 0, which works for water tiles)
			eor #3		; | Loops between Temp12 and (Temp12 + 3). But starts at (Temp12 + 3)
			ora Temp12	; /
			sta PPUDATA	; Draw A as next tile
			dex	; Loop until X == 0
			bne @Loop
		rts

InitializeStarBG:
	ldx #0
	@Loop:
		jsr LoadStarXAddr	; \ Create each star at their position
		jsr CreateStar		; /
		lda StarAddressHi	; \
		ora #4				; | Create another star at PPUAddress || $400
		sta StarAddressHi	; | (Redundant due to horizontal mirroring)
		jsr CreateStar		; /
		inxr 2		; \ Increase X by 2
		cpx #128	; / Until X == 128
		bne @Loop
	rts

CreateStar:
	lda PPUAddressLo	; \
	sta PPUADDR			; | Set PPU Address
	lda StarAddressLo	; |
	sta PPUADDR			; /
	lda PPUDATA	; \
	lda PPUDATA	; / Get Star Tile at position
	cmp #$24	; \ Return if tile was not blank
	bne :+		; /
	txa		; \ Y = X && 3
	and #3	; | Initial frame for star = (index % 4) + 1
	tay		; /
	jmp SetStarTile
	:rts

UpdateStarBG:
	lda StarUpdateFlag	; \ If Star Update Flag == 0
	beq :+				; / Then Do Nothing
	dec StarUpdateFlag
	lda StarAnimID	; \ Update and Get Current Star ID
	cadc #2			; | StarAnimID += 2
	and #63			; |	StarAnimID %= 64
	sta StarAnimID	; |	
	tax				; / X = StarAnimID
	jsr LoadStarXAddr	; \
	lda StarAddressHi	; |
	sta PPUADDR			; | Set PPU Address for Star Tile X
	lda StarAddressLo	; |
	sta PPUADDR			; /
	lda PPUDATA	; \ Get current tile of star
	lda PPUDATA	; /
	ldy #3					; \
	@Loop:
		cmp StarAnimTiles,y	; | Get index of current star tile
		beq SetStarTile		; | Update it to the next
		dey					; |
		bpl @Loop			; |
	:rts					; / If not a star or empty tile, don't modify

SetStarTile:
	lda StarAddressHi		; \
	sta PPUADDR				; |
	lda StarAddressLo		; | Set PPU Address
	sta PPUADDR				; /
	lda StarAnimTiles+1,y	; \ Write star tile Y + 1
	sta PPUDATA				; /
	rts

StarAnimTiles:	;Star Tile Animation Frames
	.BYTE $24,$ed,$ee,$ef,$24	;Empty, Low, middle, high, empty

LoadStarXAddr:
	lda StarPositions,x
	sta StarAddressLo
	lda StarPositions+1,x
	sta StarAddressHi
	rts

StarPositions:	;PPU Addresses of each BG star
	.WORD $2163,$21A5,$20CB,$20B7,$217D,$229B,$20F2,$2249
	.WORD $216D,$220B,$2292,$2195,$211C,$2148,$20E0,$230B
	.WORD $20CE,$21D0,$2106,$2119,$2230,$228A,$2288,$20A4
	.WORD $2242,$2168,$223C,$2136,$21CA,$20BC,$2196,$214C
	; The following stars do not animate
	.WORD $2235,$20EF,$2268,$20A6,$21BB,$217A,$20EA,$21F1
	.WORD $20C2,$2177,$2154,$20BA,$22C5,$20BE,$20FA,$21AE
	.WORD $2146,$219A,$20D2,$213D,$222B,$20B0,$21B6,$20AC
	.WORD $20B3,$20DB,$20F6,$212C,$20E7,$2162,$21E4,$214E

UpdateScore:
	lda #0	; Only Update Score
AddScore:
	sta ScoreDigit1		; Score to Add

	; Check for cases where score should not be added
	lda DemoFlag	; \ If not Demo Play
	beq @Continue	; | then continue
	:rts			; / Else return (Don't update score in demo mode)
	@Continue:
	ldx TargetUpdateScore	; \ If [TargetUpdateScore] >= 2
	cpx #2					; | Then return
	bcs :-					; /
	lda P1Lives,x	; \ If Player X has no lives
	bmi :-			; / Then return

	ldy #100			; \ Process Score to add up
	jsr DivideByY		; | Score to add / 100
	cadc ScoreDigit5	; |
	sta ScoreDigit3		; |
	ldy #10				; |
	jsr DivideByY		; | Modulo Result / 10
	sta ScoreDigit2		; /

	ldx MainMenuCursor		; \ Selected Game Mode?
	lda TopScoreAddrLo,x	; |
	sta ScorePointerLo		; | Setup Pointer to Default Top Score
	lda #>GameATopScore		; | [$21] = 06XX
	sta ScorePointerHi		; /

	lda TargetUpdateScore	; \
	aslr 2					; | X = [TargetUpdateScore] * 5
	ora TargetUpdateScore	; |
	tax						; /

	clc
	lda P1Score0,x	; \ Add Score 0000X
	adc ScoreDigit1	; | Lock Score between 0 and 9
	jsr ScoreModulo	; | First Digit
	sta P1Score0,x	; /

	lda P1Score1,x	; \ Add Score 000X0
	adc ScoreDigit2	; | Lock Score between 0 and 9
	jsr ScoreModulo	; | Second Digit
	sta P1Score1,x	; /
	
	lda P1Score2,x	; \ Add Score 00X00
	adc ScoreDigit3	; | Lock Score between 0 and 9
	jsr ScoreModulo	; | Third Digit
	sta P1Score2,x	; /
	
	lda P1Score3,x	; \ Add Score 0X000
	adc ScoreDigit4	; | Lock Score between 0 and 9
	jsr ScoreModulo	; | Fourth Digit
	sta P1Score3,x	; /
	
	lda P1Score4,x	; \ Add Score X0000
	adc #0			; | Lock Score between 0 and 9
	jsr ScoreModulo	; | Fifth Digit
	sta P1Score4,x	; /

	inxr 4
	ldy #4						; \ From highest digit
	@HiCheckLoop:
		lda P1Score,x			; | If this score digit is
		cmp (ScorePointer),y	; | under Highest Top Score Digit
		bcc @SkipHiCheck		; | then Top Score was not beaten
		bne @HiScoreBeaten		; | so go to @SkipHiCheck (stop checking)
		dex						; | if not equal then Top score is beaten
		dey						; | if equal then check the lower digit
		bpl @HiCheckLoop		; / until the last.
	@HiScoreBeaten:
	ldy #0
	lda TargetUpdateScore	; \
	aslr 2					; | X = [TargetUpdateScore] * 5
	ora TargetUpdateScore	; |
	tax						; /
	@CopyHiLoop:
		lda P1Score,x			; \
		sta (ScorePointer),y	; | Copy Current Score
		inx						; | to Highest Top Score
		iny						; |
		cpy #5					; |
		bne @CopyHiLoop			; /
	@SkipHiCheck:
	ldy #4						; \
	@CopyLoop:
		lda (ScorePointer),y	; | Copy Highest Top Score
		sta TopScore,y			; | back to Current Top Score
		dey						; | 
		bpl @CopyLoop			; /
	inc StatusUpdateFlag	; Set flag to update status bar
	lda GameMode		; \
	beq :+				; | If Balloon Trip Mode then
	jsr UpdateBTRank	; / Ranking Update
	:rts

TopScoreAddrLo:	;Lower byte for each of the static top score memory locations
	.BYTE <GameATopScore,<GameBTopScore,<GameCTopScore

DivideByY:	; Divide DivisionRemainder by Y
	sty Temp12
	ldx #<-1	; Set X to -1
	lda DivisionRemainder
	@Loop:
		ssbc Temp12	; \ Subtract Y 
		inx			; | X + 1
		bcs @Loop	; / If A >= 0 still then continue
	cadc Temp12	; Add Y value again to cancel overflow
	sta DivisionRemainder	; DivisionRemainder = Remainder
	txa			; A and X = Result
	rts

ScoreModulo:
	cmp #10			; \ Check if Score Digit >= 10
	bcs @Subtract	; | Then ...
	rts				; / Else return
	@Subtract:
		ssbc #10	; Then subtract 10 from digit
		rts

UpdateStatusBar:
	ldy StatusUpdateFlag	; \ Check status bar update flag
	dey						; | (Do operation to update registers)
	beq DrawStatusBar		; | If flag was 1, redraw status bar
	bpl UpdateStatusLives	; | If flag was >1, also update lives
	rts						; / If flag was empty, do nothing

DrawStatusBar:
	stppuaddr $2043	; PPUADDR = $2043
	lda #$8e	; \ Upload I- to PPU
	sta PPUDATA	; /

	ldx #4				; \
	@Loop1:
		lda P1Score,x	; | Upload Player 1 Score to PPU
		sta PPUDATA		; |
		dex				; |
		bpl @Loop1		; |
	lda #0				; |
	sta PPUDATA			; /

	lda #$24	; \
	sta PPUDATA	; | Upload 2 empty spaces to PPU
	sta PPUDATA	; /
	ldx #$8c	; \
	stx PPUDATA	; | Upload TOP- to PPU
	inx			; |
	stx PPUDATA	; /

	ldx #4				; \
	@LoopTop:
		lda TopScore,x	; | Upload Top Score to PPU
		sta PPUDATA		; |
		dex				; |
		bpl @LoopTop	; |
	lda #0				; |
	sta PPUDATA			; /

	lda #$24	; \
	sta PPUDATA	; | Upload 2 empty spaces to PPU
	sta PPUDATA	; /

	lda GameMode		; \ If Game Mode is Balloon Trip Mode
	bne DrawRankText	; / then render RANK 
	lda TwoPlayerFlag	; \ If Single Player
	beq @Finish			; / then don't render Player 2 Score

	lda #$8f	; \ Upload II- to PPU
	sta PPUDATA	; /

	ldx #4				; \
	@Loop2:
		lda P2Score,x	; | Upload Player 2 Score to PPU
		sta PPUDATA		; |
		dex				; |
		bpl @Loop2		; |
	lda #0				; |
	sta PPUDATA			; /
	@Finish:
	dec StatusUpdateFlag	;Reset status update flag when done
	rts

UpdateStatusLives:
	dec StatusUpdateFlag
	stppuaddr $2062	; PPUADDR = $2062 GAME OVER Player 1 Status Bar
	lda P1Lives		; \ Draw P1 life count or game over text
	jsr @DrawLives	; / 
	lda TwoPlayerFlag	; \ If Single Player
	beq :+				; / then return
	stppuaddr $2075
	lda P2Lives			; Draw P2 life count or game over text, then return
	@DrawLives:
		bmi DrawGameOverB	; If lives are negative, then upload GAME OVER
		ReturnFromDGOB:
		sta PPUAddressHi		; \
		ldx #6					; | Draw up to 7 life icons
		@Loop:
			lda #$24			; | Upload amount of lives to PPU
			cpx PPUAddressHi	; |
			bcs @Skip			; |
			lda #$2a			; | 0x2A = Life icon
			@Skip:
			sta PPUDATA			; |
			dex					; |
			bpl @Loop			; /
		:rts

DrawGameOverB:	;Draw "GAME OVER" text for individual player if in game B
	lda TwoPlayerFlag	; \ If Single Player
	beq ReturnFromDGOB	; / then go back
	ldx #8					; \
	@Loop:
		lda GameOverText,x	; | Upload GAME OVER to PPU
		sta PPUDATA			; |
		dex					; |
		bpl @Loop			; /
	rts

GameOverText:	;GAME OVER (Reversed)
	.BYTE $1b,$0e,$1f,$18,$24,$0e,$16,$0a,$10

DrawRankText:
	ldy #4				; \
	@Loop:
		lda RankText,y	; | Upload RANK- to PPU
		sta PPUDATA		; |
		dey				; |
		bpl @Loop		; /
	lda $4a		; \
	sta PPUDATA	; | Upload Rank Number to PPU
	lda $49		; |
	sta PPUDATA	; /
	dec StatusUpdateFlag
	rts

RankText:	;RANK-
	.BYTE $fb,$fa,$f9,$f8,$f7

CreateScorePopup:
	sta Temp12	; \
	stx Temp13	; | Preserve A, X, and Y
	sty Temp14	; /
	ldx #1
	@Loop:
		lda PopupState,x	; \ If slot X is empty,
		bmi @SelectPopup	; / perfect! Use this one
		dex	;If that slot was full, try the next
		bpl @Loop
	ldx #1	; If both were full,
	lda PopupCountdown+1	; \ Compare their countdowns.
	cmp PopupCountdown		; /
	bcc @SelectPopup	; If Popup[1].countdown < Popup[0].Countdown, select slot 1
	dex	; Otherwise, select slot 0. (Select the one that's closer to disappearing to overwrite, or slot 0 if both are equal)
	@SelectPopup:	;X is set to the index of the Score Popup Slot to fill
	lda #100				; \ Set new popup's countdown to 100 frames
	sta PopupCountdown,x	; /
	lda Temp12			; \ The value of A when the function was called represents the score value.
	sta PopupState,x	; | Store this into the Popup State
	tay					; / then also put it in Y for indexing
	txa		; \ Multiply X by 8
	aslr 3	; | Because 2 objects per popup * 4 bytes per object in OAM
	tax		; /
	lda ScorePopupLeft,y	; \
	sta OAM+$f1,x			; | Set the graphics of each object in the popup sprite
	lda ScorePopupRight,y	; |
	sta OAM+$f5,x			; /
	ldy Temp13			; \ Original X was the index of object that got hit
	lda ObjectYPosInt,y	; | Get the Y position of that object
	ssbc #8				; | Put popup 8 pixels above it
	sta OAM+$f0,x		; |
	sta OAM+$f4,x		; /
	lda ObjectXPosInt,y	; \ Get the X position of the object
	sta OAM+$f3,x		; | Set the popup's X position to match
	cadc #8				; | Offset the right side of popup by 8 px
	sta OAM+$f7,x		; /
	lda TargetUpdateScore	; \
	sta OAM+$f2,x			; | Set popup sprite attributes
	sta OAM+$f6,x			; /
	ldy Temp14	; \
	ldx Temp13	; | Restore A, X, and Y
	lda Temp12	; /
	rts

; Score popup options: 300, 500, 750, 1000, 1500, 2000
ScorePopupLeft:
	; Spr:  3,  5,  7, 10, 15, 20
	.BYTE $f4,$f5,$f6,$f7,$f8,$f9
ScorePopupRight:
	; Spr: 00, 00, 50, 00, 00, 00
	.BYTE $fb,$fb,$fa,$fb,$fb,$fb

ManageScorePopup:
	ldx #1
	@Loop:
		lda $061a,x
		bmi @Next
		dec $0618,x
		bne @Next
		lda #$ff
		sta $061a,x
		txa
		aslr 3
		tay
		lda #$f0
		sta $02f0,y
		sta $02f4,y
		@Next:
		dex
		bpl @Loop
	rts

ClearScorePopups:
	ldx #1
	@Loop:
		lda #0					; \ Set both countdowns to 0
		sta PopupCountdown,x	; /
		lda #<-1			; \ Set both Popup States to -1
		sta PopupState,x	; /
		dex
		bpl @Loop
	rts

DisplayTitleScreen:
	jsr ClearPPU
	jsr ClearPPUMask
	jsr FinishFrame
	jsr DisableNMI
	tpa TitleScreenHeader, LoadPointer
	jsr UploadBackground
	jsr EnableNMI
	jmp UploadPPUAndMask

.include "Data/TitleScreenData.asm"

GotoTitleScreen:	; Manage Title Screen
	jsr EnableNMI
	jsr DisplayTitleScreen
	lda #0				; \ Reset Frame Counter
	sta FrameCounter	; /
	TitleScreenLoop:
		jsr FinishFrame
		lda FrameCounter	; \ Start demo if Frame Counter overflows
		beq StartDemo		; / 
		jsr ManageMenu	; Set Modes & Cursor
		jsr PollController0
		tax	; Copy controller state into X
		and #StartBtn	; \ If Start button is pressed
		bne :+			; / then exit Title Screen loop
		txa	; Reload controller state from X
		and #SelectBtn	; \ If Select button is NOT pressed
		beq @Next		; / then loop again
		lda #0				; \ Reset Frame Counter
		sta FrameCounter	; /
		ldx MainMenuCursor	; \
		lda TSNextOption,x	; | Select Next Mode
		sta MainMenuCursor	; /
		@Next:
		jmp TitleScreenLoop	; Loop
	:rts

StartDemo:
	inc DemoFlag		; Set Demo Flag
	inc TwoPlayerFlag		; Set to 2 Players
	lda #0		; \ Disable All Sound Channels
	sta SND_CHN	; /
	sta GameMode		; Set Game Mode to 00 (Balloon Fight)
	jsr StartDemoGame
	lda #0
	sta DemoFlag
	beq GotoTitleScreen

TSNextOption:	;Title Screen choices
	.BYTE 1,2,0

ManageMenu:
	lda MainMenuCursor	; \
	lsr					; | Set Game Mode
	sta GameMode		; / depending on selected mode
	lda MainMenuCursor	; \
	tax					; | Set Amount of players
	and #1				; | depending on selected mode
	sta TwoPlayerFlag	; /
	lda MenuCursorYOptions,x	; \ Set Y position of menu cursor balloon
	sta BalloonYPosInt			; /
	lda #44				; \ Set X position of menu cursor balloon
	sta BalloonXPosInt	; /
	ldx #0				; \ Set graphics of menu cursor balloon
	stx BalloonStatus	; /
	jmp ManageBalloonXSprite	; Update menu balloon sprite

MenuCursorYOptions:
	.BYTE 140,156,172

.include "Data/PhaseData.asm"

.include "Data/SpriteAnimData.asm"

DrawObjectX:
	lda OAMObjectOrder1,x	; \ Set Pointer from the first table
	sta OAMPointerLo		; /
	lda FrameCounter		; \
	lsr						; | Every 2 frames
	bcc @SelectOrder		; | Set Pointer from the second table
	lda OAMObjectOrder2,x	; | (Shuffles order in OAM to prevent sprites from being totally blocked)
	sta OAMPointerLo		; /
	@SelectOrder:
	lda #>OAM			; \ Set Pointer to $02xx
	sta OAMPointerHi	; / (OAM)
	lda ObjectBalloons,x	; \ If Object X Balloons >= 0
	bpl @StandardSprite		; /
	cmp #<-1			; \ If Object X Balloons == -1
	beq @ClearSprite	; /
	jmp DrawBubbleSprite
	@ClearSprite:
		ldy #20					; \
		@ClearLoop:
			lda #$f0			; | Set all 6 of this sprite's Object's
			sta (OAMPointer),y	; | Y positions to $F0
			deyr 4				; | Decrease Y by 4, 5 times
			bpl @ClearLoop		; /
		rts
	@StandardSprite:
	cpx #8				; \ If Object is Fish
	beq @AnimateFish	; /
	lda ObjectStatus,x		; \
	aslr 2					; | (Object Status * 4) + Animation Frame
	adc ObjectAnimFrame,x	; /
	cpx #2				; \ If Object is a enemy
	bcs @AnimateEnemy	; / Otherwise, it's a player
	ldy ObjectBalloons,x			; \ 
	adc PlayerAnimBalloonOffset,y	; | Y = (Object Status * 4) + Animation Frame
	tay								; / + [PlayerAnimBalloonOffset + Balloons]
	lda PlayerAnimLower,y	; \
	sta LoadPointerLo		; | Set pointer
	lda PlayerAnimUpper,y	; |
	sta LoadPointerHi		; /
	lda PlayerInvincible,x	; \ If Player X is invincible
	beq le429				; /
	ldy ObjectBalloons,x	; Y = Player X Balloons
	lda PlayerFlashAnimBalloonOffset,y	; \
	adc ObjectAnimFrame,x				; | Y = [PlayerFlashAnimBalloonOffset+Balloons]+Frame
	tay									; /
	lda PlayerFlashAnimLower,y	; \
	sta LoadPointerLo			; | Set pointer
	lda PlayerFlashAnimUpper,y	; |
	sta LoadPointerHi			; /
	jmp le429
	@AnimateEnemy:
	ldy ObjectBalloons,x
	cadcy EnemyAnimBalloonOffset
	tay
	lda EnemyAnimLower,y
	sta LoadPointerLo
	lda EnemyAnimUpper,y
	sta LoadPointerHi
	bne le429
	@AnimateFish:
	ldy ObjectStatus,x
	bmi @ClearSprite
	lda FishSprPointersLower,y
	sta LoadPointerLo
	lda FishSprPointersUpper,y
	sta LoadPointerHi
	le429:
	lda ObjectXPosInt,x
	sta Temp15
	lda ObjectYPosInt,x
	sta Temp12
	txa
	beq le444
	cpx #1
	bne le43c
	lda #1
	bne le444
	le43c:
	lda ObjectType,x
	cadc #2
	and #3
	le444:
	ldy ObjectDirection,x
	beq le44b
	ora #$40
	le44b:
	ldy ObjectBalloons,x
	cpy #2
	bne le459
	ldy ObjectStatus,x
	cpy #5
	bne le459
	eor #$40
	le459:
	ldy ObjectYPosInt,x
	cpy #$c9
	bcs le463
	cpx #9
	bne le465
	le463:
	ora #$20
	le465:
	sta Temp14
	tpa SpriteLeftXOffsets, ScorePointer
	lda ObjectDirection,x
	beq le47c
	tpa SpriteRightXOffsets, ScorePointer
	le47c:
	ldy #0
	lda (LoadPointer),y
	inc LoadPointerLo
	bne @le486
	inc LoadPointerHi
	@le486:
	asl
	sta Temp13
	asl
	adc Temp13
	adc ScorePointerLo
	sta ScorePointerLo
	bcc @le494
	inc ScorePointerHi
	@le494:
		phx	; Preserve X
		ldx #5
		ldy #0
		@Loop:
			lda Temp12	; Object's Y Position
			cadcx SpriteYOffsets
			sta (OAMPointer),y
			sta Temp12	; Store Y Position with offset for next piece of sprite
			iny
			sty Temp13
			ldy #0
			lda (LoadPointer),y
			inc LoadPointerLo
			bne @le4b1
			inc LoadPointerHi
			@le4b1:
			ldy Temp13
			sta (OAMPointer),y
			iny
			lda Temp14
			sta (OAMPointer),y
			iny
			sty Temp13
			ldy #0
			lda Temp15
			cadcy (ScorePointer)
			inc ScorePointerLo
			bne @le4ca
			inc ScorePointerHi
			@le4ca:
			ldy Temp13
			sta (OAMPointer),y
			iny
			dex
			bpl @Loop
		plx	; Restore X
	rts

DrawBubbleSprite:
	phx	; Preserve X register
	ldy OAMPointerLo	; Y = position of this enemy slot in OAM
	lda ObjectYPosInt,x	; \
	sta OAM+0,y			; | First two objects in sprite use Object Y Position
	sta OAM+4,y			; /
	cadc #8			; \
	sta OAM+8,y		; | Next two offset down by 8 px
	sta OAM+12,y	; /
	lda #$f0		; \
	sta OAM+16,y	; | Last two are unused, put offscreen
	sta OAM+20,y	; /
	lda ObjectXPosInt,x	; \
	sta OAM+3,y			; | first and third use Object X Position
	sta OAM+11,y		; /
	cadc #8			; \
	sta OAM+7,y		; | second and fourth are offset right by 8px
	sta OAM+15,y	; /
	lda ObjectYPosInt,x	; \ Compare Object Y Pos to 208px
	cmp #208			; / (208 is technically negative, so the apparent rules are reversed)
	lda #%00000011		; \ If Y <= 208px, render above the BG
	bcc @SetBGPriority	; / (When above the water)
	lda #%00100011	; If bubble is below the water, render behind the BG
	@SetBGPriority:
	sta OAM+2,y	; Set sprite attributes. Palette is always 3 (Green)
	lda ObjectStatus,x	; \ Check if the bubble is currently getting popped
	bne @IsPopping		; /
	lda OAM+2,y		; \
	sta OAM+6,y		; | Set attributes of the other 3 objects to match
	sta OAM+10,y	; |
	sta OAM+14,y	; /
	lda #$da		; \
	sta OAM+1,y		; | Set tile index for all four objects to
	lda #$db		; | 0xDA-0xDD
	sta OAM+5,y		; |
	lda #$dc		; | Could potentially be made shorter by replacing LDAs with INCs
	sta OAM+9,y		; |
	lda #$dd		; |
	sta OAM+13,y	; /
	ldx OAMPointerLo	; X = position of this enemy slot in OAM
	lda FrameCounter	; \ Every 32 frames, change shape of bubble
	and #%00100000		; |
	beq @Finish			; / Half the time, keep normal shape
	lda FrameCounter	; \
	and #%01000000		; | Every 64 frames, switch between squishing vertically and horizontally
	bne @SquishVertical	; /
	inc OAM+0,x	; \ Move top two sprites down by 1px
	inc OAM+4,x	; |
	bne @Finish	; / then finish up
	@SquishVertical:
		inc OAM+3,x		; \ Move the left two sprite right by 1px
		inc OAM+11,x	; /
	@Finish:
		plx	; Restore X
		rts
	@IsPopping:
		lda OAM+2,y	; Get attribute from top left corner
		ora #$40	; \ Set horizontal flip bit
		sta OAM+6,y	; / for top right object
		ora #$80		; \ Set vertical flip bit too
		sta OAM+14,y	; / for bottom right object
		and #%10111111	; \ Clear horizontal flip bit
		sta OAM+10,y	; / for bottom left object
		lda #$de		; \
		sta OAM+1,y		; | Set tile index for all 4 objects to 0xDE
		sta OAM+5,y		; | (Bubble popped sprite)
		sta OAM+9,y		; |
		sta OAM+13,y	; /
		dec ObjectCountdown,x	; Decrement countdown until disappearing
		bpl @Finish2	; If countdown still > 0, return
		lda #$ff				; \ Set balloons to -1
		sta ObjectBalloons,x	; /
		lda #$f0			; \ Set Y Position offscreen
		sta ObjectYPosInt,x	; /
		lda #$04	; \ Play bubble collect jingle
		sta SFX2Req	; /
		@Finish2:
		plx	; Restore X
		rts

SplashManage:
	ldx SplashAnim	; \ X = Splash animation frame
	bmi :+			; / Return if negative
	lda SplashAnimLo,x	; \
	sta LoadPointerLo	; | LoadPointer = Pointer to Splash Animation X Data
	lda SplashAnimHi,x	; |
	sta LoadPointerHi	; /
	ldy #0
	ldx #0
	@LoadLoop:
		lda (LoadPointer),y	; \ Load Data into OAM
		sta OAM+$e0,x		; /
		iny	; \ Go on to next byte in data & OAM
		inx	; /
		cmp #$f0	; \
		bne @Skip	; | If this object is offscreen, move on to next object in OAM
		inxr 3		; /
		@Skip:
		cpx #16			; \ Loop and continue to load unless finished
		bne @LoadLoop	; /
	ldy #15
	@Loop2:
		lda OAM+$e0,y		; \
		cadc SplashXOffset	; | Offset the splash sprites by the Splash X Offset
		sta OAM+$e0,y		; /
		deyr 4	; Go to next X Position
		bpl @Loop2
	lda FrameCounter	; \
	and #3				; | Only update anim frame every 4th frame
	bne :+				; /
	dec SplashAnim	; Go to next water plonk animation frame
	:rts

.define SplashPointers Splash5, Splash4, Splash3, Splash2, Splash1
SplashAnimLo:
	.LOBYTES SplashPointers
SplashAnimHi:
	.HIBYTES SplashPointers

; The splash sprite data is stored in this funny little format
; The four sprites that make up the splash are all defined by one line
; A single $f0 means that sprite is not used in this frame, otherwise it is copied to OAM
; All the sprites use palette 3. X is offset by splash position.
Splash1:
	.BYTE $d0,$ae,$03,$04
	.BYTE $f0	; Sprite 1: Empty
	.BYTE $f0	; Sprite 2: Empty
	.BYTE $f0	; Sprite 3: Empty
Splash2:
	.BYTE $c8,$af,$03,$04
	.BYTE $d0,$b0,$03,$04
	.BYTE $f0	; Sprite 2: Empty
	.BYTE $f0	; Sprite 3: Empty
Splash3:
	.BYTE $c8,$b1,$03,$fc
	.BYTE $c8,$b2,$03,$04
	.BYTE $d0,$b3,$03,$04
	.BYTE $f0	; Sprite 3: Empty
Splash4:
	.BYTE $c8,$b4,$03,$00
	.BYTE $c8,$b4,$43,$08	; Horizontal flip
	.BYTE $d0,$b5,$03,$00
	.BYTE $d0,$b5,$43,$08	; Horizontal flip
Splash5:
	.BYTE $f0	; Sprite 0: Empty
	.BYTE $f0	; Sprite 1: Empty
	.BYTE $f0	; Sprite 2: Empty
	.BYTE $f0	; Sprite 3: Empty

;These are all physical constant data for the 12 object types
ObjectGravityData:		;Fractional
	.BYTE $04,$04,$05,$06,$03,$03,$03,$06,$0a,$0a,$0a,$0a
ObjectFlapAccelData:	;Fractional
	.BYTE $28,$32,$46,$78,$00,$00,$00,$64,$00,$00,$00,$00

ObjectAirXAccelData:	;Fractional
	.BYTE $0a,$1e,$32,$70,$00,$00,$00,$70,$00,$00,$00,$00
ObjectGroundXAccelData:	;Fractional
	.BYTE $14,$3c,$64,$a0,$00,$00,$00,$a0,$00,$00,$00,$00

ObjectMaxXVelDataFrac:
	.BYTE $70,$b0,$e0,$40,$80,$80,$80,$40,$00,$00,$00,$00
ObjectMaxXVelDataInt:
	.BYTE $00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$00

ObjectMinXVelDataFrac:
	.BYTE $90,$50,$20,$c0,$80,$80,$80,$c0,$00,$00,$00,$00
ObjectMinXVelDataInt:
	.BYTE $ff,$ff,$ff,$fe,$ff,$ff,$ff,$fe,$00,$00,$00,$00

ObjectMaxYVelDataFrac:
	.BYTE $50,$90,$c0,$40,$40,$40,$40,$40,$00,$00,$00,$00
ObjectMaxYVelDataInt:
	.BYTE $00,$00,$00,$01,$00,$00,$00,$01,$02,$02,$02,$02

ObjectMinYVelDataFrac:
	.BYTE $b0,$70,$40,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
ObjectMinYVelDataInt:
	.BYTE $ff,$ff,$ff,$fe,$ff,$ff,$ff,$fe,$fe,$01,$fe,$fe

ObjectManage:
	jsr CheckObjectPairCollision
	ldx #7
	@Loop:
		lda ObjectBalloons,x	; \ Check all Object's Balloons
		bpl @IsAlive			; | If >= 0 then proceed
		cmp #<-1				; | else if == -1 then go to next object
		beq @Next				; |
		jsr ManageBubbleX		; | Otherwise it's a bubble
		jmp @Next				; /
		@IsAlive:
		cpx #2			; \ Skip if Object is Player
		bcc @SkipJingle	; /
		cmp #1			; \ Skip if doesn't have exactly 1 balloon
		bne @SkipJingle	; /
		lda ObjectStatus,x	; \ Skip if Object Status >= 2
		cmp #2				; |
		bcs @SkipJingle		; /
		lda SFX2Req	; \ If Object X is enemy, balloon == 1, and status < 2
		ora #$20	; | Play Enemy Parachute Jingle
		sta SFX2Req	; /
		@SkipJingle:
		dec ObjectAnimTimer,x	; \ Object's Anim Timer != 0
		bne @EveryFrameUpd		; /
		lda #3					; \ Object's Anim Timer = 3
		sta ObjectAnimTimer,x	; /
		cpx #2				; \ Object is not Player
		bcs @SkipTimerDec	; /
		dec PlayerInvTimer,x	; \ Player Invincibility Timer Handle
		bne @SkipTimerDec		; | Decrease Time until 0
		lda #0					; | Then disable invincibility
		sta PlayerInvincible,x	; /
		@SkipTimerDec:
		jsr ObjectUpdateAnim
		stx TargetUpdateScore
		jsr ObjectFlapPumpManage
		jsr ManageObjectState
		@EveryFrameUpd:
		jsr ManageObjectVelocity
		jsr ObjectXPlatformCollision
		jsr ManageObjectLanding
		@Next:
		jsr DrawObjectX
		dex
		bpl @Loop	; Loop back
	rts

ObjectUpdateAction:
	cpx #2			; \ If Enemy then rely on RNG
	bcs @AutoInput	; / If Player then rely on joypad
	lda FrameCounter	; \
	and #15				; | Enemy only reacts every 16 frames
	bne @GetPlayerInput	; /
	jsr UpdateRNG		; \ Update Enemy Action
	sta ObjectAction,x	; /
	@GetPlayerInput:
		lda DemoFlag	; \ If Demo Play then
		bne @AutoInput	; / do automatic inputs
		jsr PollControllerX
		lda Joy1Press,x		; \ Read Pressed Buttons
		sta ObjectAction,x	; / into Player Action
		:rts
	@AutoInput:	; Demo Players & Enemies
		lda ObjectYPosInt,x	; \ If Player Y > #160 (Low on screen)
		cmp #160			; | Then
		bcc @YCheckSkip		; /
			lda ObjectAction,x	; \
			ora #BBtn			; | Do rapid fire
			sta ObjectAction,x	; / (B Button)
			rts
		@YCheckSkip:
		dec ObjectCountdown,x	; \
		bne :-					; / then return
		jsr UpdateRNG	; Get a random number for countdown to next decision
		ldy ObjectType,x		; \ Depending on object type, determine delay with RNG
		and AutoInWaitRange,y	; | Reduce RNG to a certain range,
		adc AutoInWaitBase,y	; / then add a base value
		sta ObjectCountdown,x	; Store input cooldown
		stx Temp12	; Temp12 = X (Object Index)
		lda FrameCounter	; \
		rolr 2				; | Select player to target
		eor Temp12			; | Y = 0 or 1
		and #1				; | Target swaps every 4 frames, and reversed for every other enemy
		tay					; /
		lda ObjectBalloons,y	; \ If selected player doesn't exist,
		bmi @ForceB				; |	Or if Player Y is invincible
		lda PlayerInvincible,y	; | Then ascend
		bne @ForceB				; /
		lda #0				; \ Clear Object X's action
		sta ObjectAction,x	; /
		lda ObjectYPosInt,y		; \
		ssbc #4					; | If Player[Y].YPos - 4 >= Object[X].YPos
		cmp ObjectYPosInt,x		; | Don't hold B
		bcs @ChooseDirection	; /
		@ForceB:
			lda #BBtn			; \ Make character hold B (ascend)
			sta ObjectAction,x	; /
		@ChooseDirection:
			lda ObjectXPosInt,x	; \
			cmp ObjectXPosInt,y	; | If Object[X].XPos >= Player[Y].XPos then go left
			bcs @GoLeft			; /
			lda ObjectAction,x	; \
			ora #RightDPad		; | Otherwise go right
			sta ObjectAction,x	; | then return
			rts					; /
			@GoLeft:
			lda ObjectAction,x	; \
			ora #LeftDPad		; | Go left then return
			sta ObjectAction,x	; |
			rts					; /

AutoInWaitRange:
	.BYTE 31,15,7
AutoInWaitBase:
	.BYTE 32,16,8


;----------------------
; Joypad Code
;----------------------

PollController0:
	ldx #0	; Read Controller 0
PollControllerX:
	lda #1		; \
	sta JOY1	; | Output Strobe to both controllers
	lda #0		; |
	sta JOY1	; /
	ldy #7				; \
	@ReadLoop:
		lda JOY1,x		; |
		sta Temp12		; |
		lsr				; | Poll Controller X
		ora Temp12		; | to Joy1Press + X
		lsr				; |
		rol Joy1Press,x	; | Bits are rotated in from right to left
		dey				; |
		bpl @ReadLoop	; /
	ldy Joy1Hold,x	; \
	lda Joy1Press,x	; |
	sta Joy1Hold,x	; | Check for pressed buttons
	tya				; |
	eor Joy1Press,x	; |
	and Joy1Press,x	; /
	rts			; Returns pressed buttons in A

ManageObjectState:
	lda ObjectBalloons,x	; \ If object has balloons
	bne @Continue			; / then continue
	@ClearXVel:
		lda #0					; \ If no balloons:
		sta ObjectXVelFrac,x	; | X Velocity = 0
		sta ObjectXVelInt,x		; /
		rts	; Return
	@Continue:
	cmp #2						; \ If 2 balloons
	beq ManageActiveObjectState	; /
	cpx #2						; \ If object is a player
	bcc ManageActiveObjectState	; /
	lda ObjectStatus,x	; \ If Object Status >= 2
	cmp #2				; | then zero X velocity
	bcs @ClearXVel		; /
ManageDriftVel:
	lda ObjectXVelFrac,x	; \
	sta TempDriftVelLo		; | TempDriftVel = Object[X].XVel
	lda ObjectXVelInt,x		; |
	sta TempDriftVelHi		; /
	jsr DivideDriftVel	; Divide TempDriftVel (Object's old X Velocity)
	lda ObjectDriftXVelFrac,x	; \
	cadc TempDriftVelLo			; |
	sta ObjectDriftXVelFrac,x	; | Object[X].DriftXVel += TempDriftVel
	sta TempDriftVelLo			; | TempDriftVel = Object[X].DriftXVel
	lda ObjectDriftXVelInt,x	; |
	adc TempDriftVelHi			; |
	sta ObjectDriftXVelInt,x	; |
	sta TempDriftVelHi			; /
	jsr DivideDriftVel	; Divide TempDriftVel (Object's new Drift velocity)
	lda ObjectXVelFrac,x	; \
	ssbc TempDriftVelLo		; |
	sta ObjectXVelFrac,x	; | Object[X].XVel -= TempDriftVel
	lda ObjectXVelInt,x		; |
	sbc TempDriftVelHi		; |
	sta ObjectXVelInt,x		; /
	rts

ManageActiveObjectState:
	lda ObjectStatus,x	; \
	cmp #6				; | Return if Object X's Status >= 6
	bcc @CheckFToTSkid	; |
	rts					; /
	@CheckFToTSkid:
		lda ObjectStatus,x		; \
		cmp #4					; | If State == 4 (Forward skid), check if a switch to turning skid state is needed
		bne @CheckWalkToFSkid	; / Else skip to next check
		lda ObjectAction,x		; \
		and #LeftDPad			; | Check Left Button input
		beq @CheckRightFtoTSkid	; / If no left input, check right
		lda ObjectDirection,x	; \  
		beq @CheckWalkToFSkid	; | If pressing left and facing left, skip
		bne @SetTurnSkid		; / If pressing left and facing right, start skidding
		@CheckRightFtoTSkid:
			lda ObjectAction,x		; \
			and #RightDPad			; | Check Right Button input
			beq @CheckWalkToFSkid	; / If no left input, skip to next check
			lda ObjectDirection,x	; \ If pressing right and facing left, start skidding
			bne @CheckWalkToFSkid	; / If pressing right facing right, skip to next check
		@SetTurnSkid:
			lda #5				; \ Set state to 5 (Turning skid)
			sta ObjectStatus,x	; /
	@CheckWalkToFSkid:
		lda ObjectStatus,x		; \
		cmp #2					; | If State == 2, (Walking)
		bne @CheckSkidToWalk	; / Else skip to next check
		lda ObjectAction,x	; \ Check left input
		and #LeftDPad		; /
		beq @CheckRightWalktoFSkid	; If no left input, try checking right
		lda #0					; \ If left input, compare against direction
		beq @CheckWalkDirMatch	; / 0 = left
		@CheckRightWalktoFSkid:
			lda ObjectAction,x	; \ Check right input
			and #RightDPad		; /
			beq @SetFSkid	; If neither direction was pressed, start skidding
			lda #1	; 1 = right, compare against direction
		@CheckWalkDirMatch:
			cmp ObjectDirection,x	; \ If pressed direction == object direction,
			beq @CheckSkidToWalk	; / Don't start skidding
		@SetFSkid:
			lda #4				; \ Set state to skidding
			sta ObjectStatus,x	; /
	@CheckSkidToWalk:
		lda ObjectStatus,x		; \
		cmp #4					; | If State >= 4 (Skidding)
		bcc @CheckStandToWalk	; / Else Skip to next check
		lda ObjectAction,x	; \ Check left input
		and #LeftDPad		; /
		beq @CheckRightSkidToWalk	; If no left input, check right
		lda ObjectDirection,x	; \
		bne @CheckStandToWalk	; | If left button pressed and direction is right, skip to next check
		beq @SetWalking			; / If left button pressed and direction is left, set state to walking
		@CheckRightSkidToWalk:
			lda ObjectAction,x	; \ Check right input
			and #RightDPad		; /
			beq @CheckStandToWalk	; If no right input, skip to next check
			lda ObjectDirection,x	; \ If pressing right and facing left, skip to next check
			beq @CheckStandToWalk	; / If pressing right and facing right, set state to walking
		@SetWalking:
			lda #2				; \ Set state to 2 (Walking)
			sta ObjectStatus,x	; /
	@CheckStandToWalk:
		lda ObjectStatus,x		; \
		cmp #3					; | If object state == 3 (Standing)
		bne @ObjectSetDirection	; / Else Skip to next check
		lda ObjectAction,x			; \ Check for horizontal input
		and #LeftDPad | RightDPad	; /
		beq @ObjectSetDirection	; If nothing, stay standing
		lda #2				; \ Else set state to 2 (walking)
		sta ObjectStatus,x	; /
	@ObjectSetDirection:
		lda ObjectStatus,x		; \
		cmp #4					; | If object state < 4 (Not skidding)
		bcs @ApplySkidXAccel	; / Else skip to next check
		lda ObjectAction,x	; \ Check left button
		and #LeftDPad		; /
		beq @CheckRightSetDirection
		lda #0				; \ Set direction to left
		beq @SetDirection	; /
		@CheckRightSetDirection:
			lda ObjectAction,x	; \ Check right button
			and #RightDPad		; /
			beq @ApplySkidXAccel
			lda #1	; Set direction to right
		@SetDirection:
			sta ObjectDirection,x	; Set direction to match input
	@ApplySkidXAccel:
		lda ObjectStatus,x		; \
		cmp #4					; | If object state >= 4 (Skidding)
		bcc @CheckXAccelMode	; / Else skip to next check
		lda ObjectAnimFrame,x	; \
		cmp #1					; | Only apply acceleration when animation frame == 1
		bne @CheckXAccelMode	; /
		ldy ObjectType,x	; Y = Object Type
		lda ObjectDirection,x	; \ Check facing direction
		beq @ApplySkidRight		; /
		lda ObjectXVelFrac,x			; \
		ssbcy ObjectGroundXAccelData	; |
		sta ObjectXVelFrac,x			; | Object[X].XVel -= ObjectGroundXAccel
		lda ObjectXVelInt,x				; |
		sbc #0							; |
		jmp @SetXVel1					; /
		@ApplySkidRight:
			lda ObjectXVelFrac,x			; \
			cadcy ObjectGroundXAccelData	; |
			sta ObjectXVelFrac,x			; | Object[X].XVel += ObjectGroundXAccel
			lda ObjectXVelInt,x				; |
			adc #0							; |
			jmp @SetXVel1					; /
	@CheckXAccelMode:
		lda ObjectStatus,x	; \ If state == 0 (Flying)
		beq @ApplyFlyXAccel	; /
		cmp #2					; \ If state == 2 (Walking)
		beq @ApplyWalkXAccel	; /
		cmp #3				; \ If state == 3 (Ground idle)
		beq @ApplyFlyXAccel	; /
		jmp @SkidEndCheck	; Else move on to SkidEndCheck (State is air idle or skidding)
	@ApplyFlyXAccel:
		lda ObjectAnimFrame,x		; \
		cmp #1						; | Only apply acceleration when animation frame == 1
		beq @ContinueApplyFlyXAccel	; /
		jmp @SkidEndCheck	; Skip if not on frame
		@ContinueApplyFlyXAccel:
		ldy ObjectType,x	; Y = Object[X].Type
		lda ObjectAction,x	; \
		and #LeftDPad		; | If Left isn't pressed, check Right.
		beq @CheckFlyRight	; / Else apply leftward acceleration
		lda ObjectXVelFrac,x		; \
		ssbcy ObjectAirXAccelData	; |
		sta ObjectXVelFrac,x		; | Object[X].XVel -= ObjectAirXAccel
		lda ObjectXVelInt,x			; |
		sbc #0						; |
		jmp @SetXVel1				; /
		@CheckFlyRight:
			lda ObjectAction,x	; \
			and #RightDPad		; | If Right isn't pressed either, finish check.
			beq @SkidEndCheck	; / Else apply rightward acceleration
			lda ObjectXVelFrac,x		; \
			cadcy ObjectAirXAccelData	; |
			sta ObjectXVelFrac,x		; | Object[X].XVel += ObjectAirXAccel
			lda ObjectXVelInt,x			; |
			adc #0						; /
		@SetXVel1:
			sta ObjectXVelInt,x	; Store Int portion of XVel
			jmp @SkidEndCheck	; Finish and go to next check
	@ApplyWalkXAccel:
		lda ObjectAnimFrame,x	; \
		cmp #1					; | Only apply acceleration when animation frame == 1
		bne @SkidEndCheck		; /
		ldy ObjectType,x	; Y = Object[X].Type
		lda ObjectAction,x	; \
		and #LeftDPad		; | If Left isn't pressed, check Right
		beq @CheckWalkRight	; / Else apply leftward acceleration
		lda ObjectXVelFrac,x			; \
		ssbcy ObjectGroundXAccelData	; |
		sta ObjectXVelFrac,x			; | Object[X].XVel -= ObjectAirXAccel
		lda ObjectXVelInt,x				; |
		sbc #0							; |
		jmp @SetXVel2					; /
		@CheckWalkRight:
			lda ObjectAction,x	; \
			and #RightDPad		; | If Right isn't pressed either, finish check
			beq @SkidEndCheck	; / Else apply rightward acceleration
			lda ObjectXVelFrac,x			; \
			cadcy ObjectGroundXAccelData	; |
			sta ObjectXVelFrac,x			; |	Object[X].XVel += ObjectAirXAccel
			lda ObjectXVelInt,x				; |
			adc #0							; /
		@SetXVel2:
			sta ObjectXVelInt,x	; Store Int portion of XVel
		lda ObjectAction,x			; \
		and #LeftDPad | RightDPad	; | If no directional input, don't play footstep SFX
		beq @SkidEndCheck			; /
		cpx #2				; \ Don't play footstep SFX for enemies
		bcs @SkidEndCheck	; /
		lda SFX1Req	; \
		ora #$08	; | Play footstep SFX
		sta SFX1Req	; /
	@SkidEndCheck:
		lda ObjectStatus,x	; \ If player X is skidding (Status >= 4)
		cmp #4				; /
		bcc :+	; Return if not
		lda ObjectDirection,x	; \ Go to separate check if direction is 1 (Right)
		bne @RightSkid			; /
		lda ObjectXVelInt,x	; \ If facing left and X Vel >= 0, then finish skidding
		bmi :+				; | Otherwise if not stopped, return
		bpl @EndSkid		; /
		@RightSkid:
			lda ObjectXVelInt,x	; \ If facing right and X Vel < 0, then finish skidding
			bpl :+				; / Otherwise if not stopped, return
		@EndSkid:
			lda ObjectStatus,x		; \
			cmp #5					; | Flip direction if Status == 5 (Turning skid)
			bne @SkipDirectionFlip	; /
			lda ObjectDirection,x	; \
			eor #1					; | Reverse direction
			sta ObjectDirection,x	; /
			@SkipDirectionFlip:
			lda #3				; \ Set status to 3 (Ground idle)
			sta ObjectStatus,x	; /
			lda #0					; \
			sta ObjectXVelFrac,x	; | Set X Velocity to 0
			sta ObjectXVelInt,x		; /
			:rts

ManageObjectLanding:
	lda LandingFlag
	bne le9b6
	lda PlayerInvincible,x
	beq le99a
	lda BTPlatformX
	beq le99a
	ssbcx ObjectXPosInt
	jsr GetAbsoluteValue
	cmp #5
	bcc le9b6
	le99a:
	cpx #2
	bcc le9a4
	lda ObjectBalloons,x
	cmp #2
	bne :+
	le9a4:
	lda ObjectStatus,x
	cmp #2
	bcc :+
	cmp #6
	bcs :+
	lda #1				; \ Object state = 1 (Air Idle)
	sta ObjectStatus,x	; /
	sta ObjectCountdown,x
	rts
	le9b6:
	lda #0					; \
	sta ObjectYVelFrac,x	; | Object[X].YVel = 0
	sta ObjectYVelInt,x		; | Object[X].YPosFrac = 0 (Align to top of pixel)
	sta ObjectYPosFrac,x	; /
	sta LandingFlag	; Clear LandingFlag
	cpx #2
	bcc le9fd
	lda ObjectBalloons,x
	cmp #2
	beq le9f3
	cmp #1
	bne :+
	lda ObjectStatus,x
	cmp #2
	bcs :+
	lda #2
	sta ObjectStatus,x
	lda EnemyStartDelay
	sta ObjectAnimTimer,x
	lda #0					; \
	sta ObjectXVelFrac,x	; | Object[X].XVel = 0
	sta ObjectXVelInt,x		; /
	sta ObjectDriftXVelFrac,x	; \ Object[X].DriftXVel = 0
	sta ObjectDriftXVelInt,x	; /
	lda #$40	; \ Play Enemy Landing SFX
	sta SFX2Req	; /
	:rts
	le9f3:
		lda #0				; \ Object status = 0 (Flying/Falling)
		sta ObjectStatus,x	; /
		lda #1
		sta ObjectCountdown,x
		rts
	le9fd:
		lda ObjectStatus,x
		cmp #1
		bne :+
		cmp #6
		bcs :+
		lda ObjectXVelFrac,x
		ora ObjectXVelInt,x
		bne lea13
		lda #3
		bne lea15
		lea13:
		lda #2
		lea15:
		sta ObjectStatus,x
		:rts


;----------------------
; Object Code
;----------------------

ObjectUpdateAnim:
	cpx #2			; \ Object is not Player
	bcs @NotPlayer	; /
	lda PlayerInvincible,x	; \ If Player X Invincible
	bne @NextFrame			; /
	lda ObjectStatus,x	; \ If Player X Status == 1
	cmp #1				; | Then update animation every 8th frame
	beq @SlowAnim		; /
	cmp #3			; \ If Player X Status != 3
	bne @NextFrame	; | Then update animation
	beq @SlowAnim	; / Else update animation every 8th frame
	@NotPlayer:
	lda ObjectStatus,x	; \ If Enemy Status == 1
	cmp #1				; | Then update animation every 8th frame
	beq @SlowAnim		; /
	cmp #3			; \ If Enemy Status < 1
	bcc @NextFrame	; / Then update animation
	lda FrameCounter	; \
	and #3				; | Update Animation Frame
	bne @ClampFrame		; | every 4 frames
	beq @NextFrame		; /
	@SlowAnim:
		lda FrameCounter	; \ Update Animation Frame
		and #7				; | every 8 frames
		bne @ClampFrame		; /
	@NextFrame:
		inc ObjectAnimFrame,x	; Increment Animation Frame
	@ClampFrame:
		lda ObjectAnimFrame,x	; \
		and #3					; | Stay within Frame 0 to 3
		sta ObjectAnimFrame,x	; /
	bne :+ 
	lda ObjectStatus,x	; \
	bne :+				; | Increment Status if not 0
	inc ObjectStatus,x	; /
	:rts

ManageObjectVelocity:
	lda ObjectHitCooldown,x	; \
	beq @SkipDec			; | Countdown object I-frames
	dec ObjectHitCooldown,x	; /
	@SkipDec:
	cpx #2			; \ If Object X is not player
	bcs @ManageYVel	; /
	lda PlayerFreeze,x	; \ If Player X is not frozen
	beq @ManageYVel		; /
	; Manage frozen player
		lda FrameCounter	; \
		lsr					; | Return every other frame
		bcc :+				; /
		inc ObjectAnimFrame,x	; \
		lda ObjectAnimFrame,x	; | Progress Anim Frame
		and #3					; |
		sta ObjectAnimFrame,x	; /
		lda #1				; \ Keep status at 1
		sta ObjectStatus,x	; /
		dec ObjectCountdown,x	; Progress countdown
		bne :+	; When countdown finishes:
		lda #0				; \
		sta PlayerFreeze,x	; | Remove freeze
		sta ObjectStatus,x	; / Set status to 0
		lda #$20	; \ Play Falling SFX
		sta SFX1Req	; /
		:rts
	@ManageYVel:
		lda ObjectYVelFrac,x	; \
		clc						; |
		ldy ObjectType,x		; | Y = Current object's Type
		adc ObjectGravityData,y	; | Accelerate downward with gravity (Determined by type)
		sta ObjectYVelFrac,x	; /
		bcc @GravNoCarry		; \
		inc ObjectYVelInt,x		; / If necessary, carry over into Int portion
		@GravNoCarry:			
		lda ObjectYVelInt,x
		bmi @CheckYVelUp	; If Y Velocity is negative (upward) then impose lower limit
		cmp ObjectMaxYVelDataInt,y	; \
		bcc @ApplyYVel				; | If Y Velocity is too high, limit it, otherwise go ahead and apply Y Velocity
		bne @LimitYVelDown			; |
		lda ObjectYVelFrac,x		; | If YVelInt == Int portion of upper limit, then also check YVelFrac
		cmp ObjectMaxYVelDataFrac,y	; |
		bcc @ApplyYVel				; /
		@LimitYVelDown:
			lda ObjectMaxYVelDataFrac,y	; \
			sta ObjectYVelFrac,x		; | If Y Velocity was too high, set it back down to the maximum
			lda ObjectMaxYVelDataInt,y	; |
			sta ObjectYVelInt,x			; /
			jmp @ApplyYVel
		@CheckYVelUp:
			cmp ObjectMinYVelDataInt,y	; \
			bcc @LimitYVelUp			; | If Y Velocity is too negative, limit it, otherwise go ahead and apply Y Velocity
			bne @ApplyYVel				; |
			lda ObjectYVelFrac,x		; | If YVelInt == Int portion of lower limit, then also check YVelFrac
			cmp ObjectMinYVelDataFrac,y	; |
			bcs @ApplyYVel				; /
			@LimitYVelUp:
				lda ObjectMinYVelDataFrac,y	; \
				sta ObjectYVelFrac,x		; | If Y Velocity was too low, set it back up to the minimum
				lda ObjectMinYVelDataInt,y	; |
				sta ObjectYVelInt,x			; /
		@ApplyYVel:
			jsr ObjectApplyYVelocity
			cmp #$f8		; \ If object hasn't fallen into the water completely
			bcs @ManageXVel	; | Then go on to managing X Velocity
			cmp #$e8		; |
			bcc @ManageXVel	; / Otherwise, kill them
			lda #<-1				; \ Balloons = -1
			sta ObjectBalloons,x	; /
			lda #4			; \ Do Water Plonk
			sta SplashAnim	; / Animation
			lda ObjectXPosInt,x	; \ Move Splash to object's position
			sta SplashXOffset	; /
			cpx #2			; \ If Object X is Player
			bcc @PlayerSink	; /
			lda #$80				; \ Set balloons to 0x80 (Bubble)
			sta ObjectBalloons,x	; /
			lda #0				; \ Status = 0
			sta ObjectStatus,x	; /
			lda #$01	; \ Play Bubble Rise SFX
			sta SFX3Req	; /
			bne @ManageXVel	; Always taken. Finish Managing Y Vel
			@PlayerSink:
				lda PhaseType	; \ Don't do splash SFX if bonus phase
				bne @ManageXVel	; /
				lda #$40	; \ Play Splash SFX
				sta SFX1Req	; /
	@ManageXVel:
		lda ObjectXVelInt,x
		bmi @CheckXVelLeft	; If X Velocity is negative (Left) then impose lower limit
		cmp ObjectMaxXVelDataInt,y	; \
		bcc @ApplyXVel				; | If X Velocity is too high, limit it, otherwise go ahead and apply X Velocity
		bne @LimitXVelRight			; |
		lda ObjectXVelFrac,x		; | If XVelInt == Int portion of upper limit, then also check XVelFrac
		cmp ObjectMaxXVelDataFrac,y	; |
		bcc @ApplyXVel				; /
		@LimitXVelRight:
			lda ObjectMaxXVelDataFrac,y	; \
			sta ObjectXVelFrac,x		; | If X Velocity was too high, set it back down to the maximum
			lda ObjectMaxXVelDataInt,y	; |
			sta ObjectXVelInt,x			; /
			jmp @ApplyXVel
		@CheckXVelLeft:
			cmp ObjectMinXVelDataInt,y	; \
			bcc @LimitXVelLeft			; | If X Velocity is too negative, limit it, otherwise go ahead and apply X Velocity
			bne @ApplyXVel				; |
			lda ObjectXVelFrac,x		; | If XVelInt == Int portion of lower limit, then also check XVelFrac
			cmp ObjectMinXVelDataFrac,y	; |
			bcs @ApplyXVel				; /
			@LimitXVelLeft:
				lda ObjectMinXVelDataFrac,y	; \
				sta ObjectXVelFrac,x		; | If X Velocity was too low, set it back up to the minimum
				lda ObjectMinXVelDataInt,y	; |
				sta ObjectXVelInt,x			; /
		@ApplyXVel:
			jsr ObjectApplyXVelocity
			lda GameMode		; \ If game mode A or B, don't limit X position
			beq @SkipBTXLimit	; / In Balloon Trip, prevent wraparound by limiting X position
			lda ObjectXPosInt,x
			cmp #16			; \
			bcs @SkipXMin	; | If X Pos < 16, set to 16
			lda #16			; /
			@SkipXMin:
			cmp #224		; \
			bcc @SkipXMax	; | If X Pos >= 224, set to 224
			lda #224		; /
			@SkipXMax:
			sta ObjectXPosInt,x	; Store changes to X Position
			@SkipBTXLimit:
			lda PhaseType	; \ Return if it's a normal phase
			beq :+			; /
			lda ObjectBalloons,x	; \ Return if Object is still alive
			bne :+					; /
			lda ObjectYPosInt,x	; \ For falling players in bonus phases
			cmp #200			; | If Y Pos < 200px return
			bcc :+				; /
			lda #199			; \ In bonus phases, players bounce on floor.
			sta ObjectYPosInt,x	; / Set Y Pos back to 199px if they fell below 200px
			lda ObjectType,x			; \
			cmp #11						; | If type != 11 then restore them
			bne @RestoreFallenPlayer	; /
			dec ObjectType,x	; \ Decrement type,
			jsr ObjectBounceY	; | Bounce the player back up,
			jmp ReduceYVelocity	; / reduce their bounce velocity & return
			@RestoreFallenPlayer:
			lda #2					; \ Set Balloons to 2
			sta ObjectBalloons,x	; /
			lda #3				; \ Set Type to 3
			sta ObjectType,x	; /
			:rts

ObjectApplyXVelocity:
	lda ObjectXPosFrac,x	; \ Apply Velocity to
	cadcx ObjectXVelFrac	; | X Position (Frac)
	sta ObjectXPosFrac,x	; /
	lda ObjectXPosInt,x	; \ Apply Velocity to
	adc ObjectXVelInt,x	; | X Position (Int)
	sta ObjectXPosInt,x	; /
	rts

ObjectApplyYVelocity:
	lda ObjectYPosFrac,x	; \ Apply Velocity to
	cadcx ObjectYVelFrac	; | Y Position (Frac)
	sta ObjectYPosFrac,x	; /
	lda ObjectYPosInt,x	; \ Apply Velocity to
	adc ObjectYVelInt,x	; | Y Position (Int)
	sta ObjectYPosInt,x	; /
	rts

ObjectYApplyXVelocity:
	jsr SwapXY	; Swap X & Y registers
	jsr ObjectApplyXVelocity
	jmp SwapXY	; Swap X & Y back before returning

ObjectYApplyYVelocity:
	jsr SwapXY	; Swap X & Y registers
	jsr ObjectApplyYVelocity
	jmp SwapXY	; Swap X & Y back before returning

ObjectFlapPumpManage:
	cpx #2			; \ If not player
	bcs @Enemies	; /
	lda ObjectBalloons,x	; \ If player still has balloons
	bne @Players			; /
	lda ObjectAnimFrame,x	; \ If player animation frame != 0
	bne @Players			; /
	lda #0				; \ Then Player Status = 0 (Dead)
	sta ObjectStatus,x	; / and return
	rts
	@Players:	; Player
		lda ObjectStatus,x	; \ If Player Status < 6
		cmp #6				; | Then check input
		bcc @CheckInput		; /
		lda #1				; \ Else Status = 1
		sta ObjectStatus,x	; /
		dec ObjectBalloons,x	; Decrease one balloon
		rts
	@Enemies:	; Enemy
		lda ObjectBalloons,x	; \ If Enemy Balloons == 2
		cmp #2					; | Then check input
		beq @CheckInput			; /
		lda ObjectAnimFrame,x	; \ If enemy animation frames != 0
		bne :+					; / Then
		lda ObjectBalloons,x	; \ If Enemy Status != 0
		bne @EnemyReinflate		; / Then
		lda #0				; \ Enemy Status = 0 (Dead)
		sta ObjectStatus,x	; /
		rts
		@EnemyReinflate:
			lda ObjectStatus,x	; \ If Enemy Status != 0
			bne @EnemyCont		; / Then
			inc ObjectStatus,x	; Increase Enemy Status
			:rts
			@EnemyCont:
			cmp #2	; \ If Player
			bcc :-	; / then return
			dec ObjectCountdown,x	; \ Decrement countdown before doing further processing
			bne :+					; / Return until next time if not done counting down
			lda EnemyInflateSpeed	; \ Set next countdown to EnemyInflateSpeed
			sta ObjectCountdown,x	; /
			inc ObjectStatus,x	; \
			lda ObjectStatus,x	; | Increment status, and return unless state = 7
			cmp #7				; | State 7 means enemy finished inflating balloon
			bcc :+				; /
			lda #2					; \ Set enemy balloons to 2
			sta ObjectBalloons,x	; /
			lda #0				; \ Set state to 0 (Flying)
			sta ObjectStatus,x	; /
			ldy ObjectType,x			; \
			lda EnemyUpgradeNextType,y	; / A = next type to upgrade into
			ldy ObjectUpgradeFlag,x	; \
			bne @Upgrade			; | If upgrade flag is set, change type to next
			dec ObjectUpgradeFlag,x	; | Otherwise, maintain color but just fly again?
			lda ObjectType,x		; |
			and #3					; /
			@Upgrade:
			sta ObjectType,x	; Update Enemy Type
			lda #<-2			; \ Object[x].YVelInt = -2
			sta ObjectYVelInt,x	; /
			:rts
	@CheckInput:
		jsr ObjectUpdateAction
		lda ObjectAction,x	; Limit action to valid inputs
		and #ABtn | BBtn | LeftDPad | RightDPad
		beq @ApplyObjectAction	; Skip if no valid input
		cpx #2					; \ If is player making input
		bcs @ApplyObjectAction	; | Disable invincibility
		lda #0					; | Else skip to rest of action management
		sta PlayerInvincible,x	; /
		@ApplyObjectAction:
			lda ObjectAction,x	; \
			and #BBtn			; | Check B button
			bne @BPressed		; /
			lda ObjectAction,x	; \
			and #ABtn			; | Check A button
			bne @APressed		; /
			lda #0				; \ If A+B not pressed then
			sta ABtnCooldown,x	; | Immediately clear A button cooldown flag
			beq :+				; / Return
		@APressed:
			lda ABtnCooldown,x	; \ Return if the A Button was already being pressed
			bne :+				; /
		@BPressed:
			lda ObjectStatus,x	; \ 
			cmp #2				; | If object was airborne (Flying or Air Idle)
			bcc @CheckFlap		; / Check if the object can flap currently
			dec ObjectYPosInt,x	; \ If object was grounded
			dec ObjectYPosInt,x	; / Object[X].YPos -= 2
			lda #0					; \
			sta ObjectYVelFrac,x	; | Object[X].YVel = 0
			sta ObjectYVelInt,x		; /
			beq @CanFlap	; Can always flap from ground
			@CheckFlap:
				cmp #1			; \ If was previously idling in air,
				beq @CanFlap	; / Object can flap immediately
				lda ObjectAnimFrame,x	; \ If in flying state and frame is 0, it can flap
				bne :+					; / Else return
			@CanFlap:
				lda #0				; \ State = 0 (Flying)
				sta ObjectStatus,x	; /
				lda #1					; \ Set ObjectAnimFrame = 1
				sta ObjectAnimFrame,x	; / (This frame is when X Acceleration is applied elsewhere)
				lda #1				; \ Set A Button Cooldown Flag
				sta ABtnCooldown,x	; /
				ldy #0	; Y = 0 (Write to SFX1/Play Player Flap)
				cpx #2					; \ If Object slot < 2 (Is player)
				bcc @DoPlayerFlapSFX	; / Play Player Flap SFX
				iny	; Y = 1 (Write to SFX2/Play Enemy Flap)
				@DoPlayerFlapSFX:
				lda SFX1Req,y	; \ Play Flap SFX
				ora #$10		; | If Y == 0, Player Flap
				sta SFX1Req,y	; / If Y == 1, Enemy Flap (Bird Tweet)
				lda ObjectYVelFrac,x		; \
				sec							; |
				ldy ObjectType,x			; | Object[x].YVel -= ObjectFlapAccel
				sbc ObjectFlapAccelData,y	; | ObjectFlapAccel is based on Type
				sta ObjectYVelFrac,x		; |
				bcs :+						; | If carry clear, also borrow from int portion
				dec ObjectYVelInt,x			; /
				:rts

EnemyUpgradeNextType:
	.BYTE $01,$02,$02,$03
	.BYTE $01,$02,$02,$03
	.BYTE $01,$02,$02,$03

ManageBubbleX:
	lda ObjectStatus,x	; \ If Object(x).Status != 0
	bne :+				; / then don't do anything
	jsr ManageDriftVel
	jsr ObjectApplyXVelocity
	lda ObjectYPosFrac,x	; \
	ssbc #96				; |
	sta ObjectYPosFrac,x	; | Y Pos -= 0.375px (0x0.60)
	lda ObjectYPosInt,x		; |
	sbc #0					; |
	sta ObjectYPosInt,x		; /
	cmp #241			; \ If Y Pos < 241px then despawn
	bcc @SkipDespawn	; /
	lda #<-1				; \ Set Balloons to -1 (Despawn bubble)
	sta ObjectBalloons,x	; /
	@SkipDespawn:
	phx	; Preserve X
	ldy #1
	@Loop:
		lda ObjectBalloons,y	; \
		beq @Next				; | Skip if Player[Y].Balloons <= 0
		bmi @Next				; /
		lda ObjectYPosInt,x		; \
		ssbcy ObjectYPosInt		; |
		jsr GetAbsoluteValue	; | Skip if |Object[X].YPos - Player[Y].YPos| >= 24
		cmp #24					; |
		bcs @Next				; /
		lda ObjectXPosInt,x		; \
		ssbcy ObjectXPosInt		; |
		jsr GetAbsoluteValue	; | Skip if |Object[X].XPos - Player[Y].XPos| >= 16
		cmp #16					; |
		bcs @Next				; /
		lda #<-1			; \ Object[X].Status = -1
		sta ObjectStatus,x	; /
		lda #3					; \ Object[X].Countdown = 3
		sta ObjectCountdown,x	; /
		lda #120			; \ Set Scroll Lock Timer to 120
		sta ScrollLockTimer	; /
		lda #$02	; \ Play Pop SFX
		sta SFX1Req	; /
		lda #50					; \
		sty TargetUpdateScore	; | Give 500 points to Player Y
		jsr AddScore			; /
		lda #1					; \ Update score bar
		ldx TargetUpdateScore	; /
		jsr CreateScorePopup	; Make score popup
		plx	; \ Restore X and finish
		rts	; /
		@Next:
		dey
		bpl @Loop
	plx	; Restore X
	:rts

ObjectXPlatformCollision:
	ldy ObjectBalloons,x	; \
	dey						; | If Object[X].Balloons > 0, continue with check
	bpl @ObjectValid		; |
	:rts					; / Return if object has no balloons
	@ObjectValid:
	lda ObjectYPosInt,x	; \
	cmp #<-7			; | If object isn't hitting ceiling, continue with collision check
	bcc @Continue		; |
	lda ObjectYVelInt,x	; | If object is hitting ceiling but moving down, return
	bpl :-				; / If Object is hitting ceiling while moving upward, bounce
	lda #0				; \ Clear Collision Flags
	sta CollisionFlags	; | Then bounce off ceiling
	jmp lede1			; /
	@Continue:
	ldy PlatformCount
	bmi :--
	@PlatformLoop:
		lda #0				; \ Clear Collision Flags
		sta CollisionFlags	; /
		; Check Top
			lda (TopPointer),y	; \
			ssbc #24			; | If Platform[Y].TopY - 24 >= Object[X].YPos then skip
			cmp ObjectYPosInt,x	; |
			bcs @Next			; /
			adc #3
			cmp ObjectYPosInt,x
			bcc @CheckBottom
			lda #1
			bne @CheckLeft
		@CheckBottom:
			lda (BottomPointer),y
			cmp ObjectYPosInt,x
			bcc @Next
			sbc #3
			cmp ObjectYPosInt,x
			bcs @CheckLeft2
			lda #2
		@CheckLeft:
			sta CollisionFlags
			lda (LeftPointer),y
			cmp #16
			beq @CheckRight
			ssbc #12
			cmp ObjectXPosInt,x
			bcs @ClearFlags
		@CheckRight:
			lda (RightPointer),y
			cmp #<-1
			beq @CheckLeft2
			ssbc #4
			cmp ObjectXPosInt,x
			bcs @CheckLeft2
		@ClearFlags:
			lda #0				; \ Clear Collision Flags
			sta CollisionFlags	; /
		@CheckLeft2:
			lda (LeftPointer),y
			ssbc #16
			beq @CheckRight2
			cmp ObjectXPosInt,x
			bcs @Next
			adc #4
			cmp ObjectXPosInt,x
			bcc @CheckRight2
			lda CollisionFlags
			ora #4
			bne @SetFlagLeft
		@CheckRight2:
			lda (RightPointer),y
			cmp #<-1
			beq @Next
			cmp ObjectXPosInt,x
			bcc @Next
			sbc #4
			cmp ObjectXPosInt,x
			bcs @Next
			lda CollisionFlags
			ora #8
		@SetFlagLeft:
			sta CollisionFlags
		@Next:
			lda CollisionFlags
			bne @CheckCollisionFlags
			dey
			bmi :+
			jmp @PlatformLoop
	:rts

	@CheckCollisionFlags:
		lsr CollisionFlags	; \
		bcc @NoLanding		; | If CollisionFlags.bit7 is set and Object[X].YVel >= 0, then they're landing
		lda ObjectYVelInt,x	; | Else skip landing
		bmi @NoLanding		; /
		lda (TopPointer),y	; \
		sbc #24				; | Object[X].YPosInt = Platform[Y].TopY - 23
		sta ObjectYPosInt,x	; | Align object to top of platform
		inc ObjectYPosInt,x	; /
		lda #1			; \ Set landing flag
		sta LandingFlag	; /
		@NoLanding:
		lsr CollisionFlags
		bcc ledf4
		lda ObjectYVelInt,x
		bpl ledf4
		lda (BottomPointer),y
	lede1:
		sta ObjectYPosInt,x
		jsr ObjectBounceY
		jsr ReduceYVelocity
		cpx #2
		bcs ledf0
		jsr PlayBumpSFX
		ledf0:
		lda LandingFlag
		bne :+
		ledf4:
		lsr CollisionFlags
		bcc ledff
		lda ObjectXVelInt,x
		bmi ledff
		bpl lee08
		ledff:
		lsr CollisionFlags
		bcc :+
		lda ObjectXVelInt,x
		bpl :+
		lee08:
		jsr ObjectBounceX
		jsr ReduceXVelocity
		lda ObjectXVelInt,x
		ora ObjectXVelFrac,x
		beq :+
		lda ObjectDirection,x
		eor #1
		sta ObjectDirection,x
		lda SFX2Req	; \
		ora #$02	; | Play Bump SFX
		sta SFX2Req	; /
		:rts

CheckObjectPairCollision:
	ldx #7
	@LoopX:
		stx Temp12	; \ For each X, start Y at the value just before
		ldy Temp12	; | That way, duplicate pairs are not considered
		dey			; /
		bpl @LoopY	;If Y goes negative (because X was 0) then finish
		@NextYLocal:	;A local, branchable way to jump to the bottom of the Y loop from the top
			jmp @NextY
		@LoopY:
			lda ObjectBalloons,x	; \
			bmieq @NextYLocal		; | Skip this pair if either object 
			lda ObjectBalloons,y	; | has 0 or less Balloons
			bmieq @NextYLocal		; /
			lda #0				; \
			sta CollisionFlags	; / Clear collision flags

			lda ObjectYPosInt,y		; \
			ssbcx ObjectYPosInt		; | If abs(Object(y).Y - Object(x).Y)
			jsr GetAbsoluteValue	; | <= 24
			cmp #24					; | then
			bcs @leec0				; /
			lda ObjectYPosInt,x		; \ If
			cadc #24				; | abs((Object(y).Y + 7)
			sta Temp12				; |   - (Object(x).Y + 24))
			lda ObjectYPosInt,y		; | >= 4 then
			cadc #7					; |
			ssbc Temp12				; |
			jsr GetAbsoluteValue	; |
			cmp #4					; |
			bcs @lee6a				; /
			lda #1
			bne @lee7c
			@lee6a:
			lda ObjectYPosInt,y		; \ If abs(Object(y).Y + 17 - Object(x).Y)
			cadc #17				; | >= 4 then
			ssbcx ObjectYPosInt		; |
			jsr GetAbsoluteValue	; |
			cmp #4					; |
			bcs @lee8f				; /
			lda #2
			@lee7c:
			sta CollisionFlags
			lda ObjectXPosInt,y		; \ If abs(Object(y).X - Object(x).X)
			ssbcx ObjectXPosInt		; | < 16 then
			jsr GetAbsoluteValue	; |
			cmp #16					; |
			bcc @lee8f				; /
			lda #0
			sta CollisionFlags
			@lee8f:
			lda ObjectXPosInt,x		; \
			cadc #16				; | If abs((Object(y).X + 7)
			sta Temp12				; |		 - (Object(x).X + 16))
			lda ObjectXPosInt,y		; | >= 4 then
			cadc #7					; |
			ssbc Temp12				; |
			jsr GetAbsoluteValue	; |
			cmp #4					; |
			bcs @leeaa				; /
			lda #4
			bne @leebc
			@leeaa:
			lda ObjectXPosInt,y		; \ If abs(Object(y).X + 9 - Object(x).X)
			cadc #9					; | >= 4 then
			ssbcx ObjectXPosInt		; |
			jsr GetAbsoluteValue	; |
			cmp #4					; |
			bcs @leec0				; /
			lda #8
			@leebc:
			ora CollisionFlags
			sta CollisionFlags
			@leec0:
			lda #0					; \
			sta CollisionUnknown	; /
			lsr CollisionFlags	; \ CollisionFlags.bit0 = Velocity Y related
			bcc @leecd			; |
			jsr ObjectYVelSbcYX	; | 
			bmi @DoYBounce		; /
			@leecd:
			lsr CollisionFlags	; \ CollisionFlags.bit1 = Velocity Y related
			bcc @leef1			; |
			jsr ObjectYVelSbcYX	; |
			bmi @leef1			; /
			@DoYBounce:
				jsr CheckGroundedBird	; \ Don't bounce off of a grounded bird
				bcs @SkipYBounce		; /
				jsr ObjectBounceY	; \
				jsr ReduceYVelocity	; | Bounce both objects and reduce velocity vertically
				jsr SwapXY			; |
				jsr ObjectBounceY	; |
				jsr ReduceYVelocity	; |
				jsr SwapXY			; /
			@SkipYBounce:
			lda #1
			sta CollisionUnknown
			@leef1:
			lsr CollisionFlags	; \ CollisionFlags.bit2 = Velocity X related
			bcc @leefa			; |
			jsr ObjectXVelSbcYX	; |
			bmi @DoXBounce		; /
			@leefa:
			lsr CollisionFlags	; \ CollisionFlags.bit3 = Velocity X related
			bcc @lef1e			; |
			jsr ObjectXVelSbcYX	; |
			bmi @lef1e			; /
			@DoXBounce:
				jsr CheckGroundedBird	; \ Don't bounce off of a grounded bird
				bcs @SkipXBounce		; /
				jsr ObjectBounceX	; \
				jsr ReduceXVelocity	; | Bounce both objects and reduce velocity horizontally
				jsr SwapXY			; |
				jsr ObjectBounceX	; |
				jsr ReduceXVelocity	; |
				jsr SwapXY			; /
			@SkipXBounce:
			lda #1
			sta CollisionUnknown
			@lef1e:
			jsr ManageCollisionDamage
			jsr SwapXY
			jsr ManageCollisionDamage
			jsr SwapXY
			@NextY:
			dey			; \
			bmi @NextX	; | Loop Y Objects
			jmp @LoopY	; /
		@NextX:
		dex			; \
		bmi :+		; | Loop X Objects
		jmp @LoopX	; /
	:rts

ManageCollisionDamage:
	cpx #2				; \ Is Object X a player?
	bcc @IncludesPlayer	; |
	cpy #2				; | Is Object Y a player?
	bcc @IncludesPlayer	; /
	jmp @Skip	; Skip if both enemies
	@IncludesPlayer:
	lda #0				; \ Reset Collision Score Offset
	sta ColScoreOffset	; /
	lda ObjectHitCooldown,x	; \
	beq @ColReady			; | If the object has a hit cooldown active, skip
	jmp @Skip				; /
	@ColReady:
	lda CollisionUnknown	; \
	bne @FlagSet			; | Skip if CollisionUnknown isn't set
	jmp @Skip				; /
	@FlagSet:
	cpx #2
	bcs @XIsEnemy
	lda PlayerInvincible,x
	beq @lef72
	jmp @Skip	; Skip
	@XIsEnemy:
	lda ObjectBalloons,x
	cmp #1
	bne @lef72
	lda ObjectStatus,x
	cmp #2
	bcs @lef7f
	lda #1
	sta ColScoreOffset
	@lef72:
	lda ObjectYPosInt,y
	cadc #4
	cmp ObjectYPosInt,x
	bcc @lef7f
	jmp @Skip	; Skip
	@lef7f:
	lda #20					; \ Give 20 frames before next hit
	sta ObjectHitCooldown,x	; /
	lda #0					; \ Reset Animation frame
	sta ObjectAnimFrame,x	; /
	cpy #2
	bcc @lef97
	lda ObjectBalloons,y
	cmp #2
	beq @lef97
	jmp @Skip	; Skip
	@lef97:
	lda SFX1Req	; \
	ora #$02	; | Play Pop SFX
	sta SFX1Req	; /
	lda ObjectBalloons,x	; \
	cmp #2					; |
	bne @lefc0				; | If Balloons != 2 or is player
	cpx #2					; |
	bcs @lefc0				; / If Balloons == 2 and not player
	sty Temp12
	ldy ObjectStatus,x
	lda lf053,y
	ldy Temp12
	pha
	pla
	bne @lefb7
	jmp @Skip	; Skip
	@lefb7:
	sta ObjectStatus,x
	lda #0
	sta ObjectAnimFrame,x
	beq @lefea
	@lefc0:
	dec ObjectBalloons,x
	bne @lefce
	lda #<-1				; \
	sta ObjectYVelInt,x		; | Set Object X's Y Velocity to -1
	lda #0					; |
	sta ObjectYVelFrac,x	; /
	@lefce:
	lda #0					; \
	sta ObjectStatus,x		; | Object[X].Status = 0
	sta ObjectXVelFrac,x	; | Object[X].XVelocity = 0
	sta ObjectXVelInt,x		; /
	lda ObjectXPosInt,x				; \
	bmi @SetDriftVelPositive		; | If Object X is on left side,
	lda #$ff						; | Then set Drifting X Velocity to -0.5
	bne @SetDriftVel				; |
	@SetDriftVelPositive:			; | If Object X is on right side,
		lda #0						; | Then set Drifting X Velocity to 0.5
	@SetDriftVel:					; |
		sta ObjectDriftXVelInt,x	; |
		lda #$80					; |
		sta ObjectDriftXVelFrac,x	; /
	@lefea:
	sty Temp12
	ldy ObjectType,x
	lda lf05e,y
	sta ObjectType,x
	lda #1
	sta ObjectUpgradeFlag,x
	ldy Temp12
	cpy #2
	bcs @Skip	; Skip
	lda ObjectType,x
	cmp #7
	beq @lf011
	cmp #8
	bcc @lf011
	lda SFX2Req
	ora #$80
	sta SFX2Req
	@lf011:
	ldy ObjectType,x
	lda lf06a,y
	sta Temp13
	lda ColScoreOffset
	beq @lf023
	lda lf076,y
	sta Temp13
	@lf023:
	lda lf082,y
	cadc ColScoreOffset
	sta Temp14
	lda Temp12
	sta TargetUpdateScore
	pha
	phx
	lda Temp13
	pha
	lda Temp14
	jsr CreateScorePopup
	pla
	jsr AddScore
	plx
	ply
	@Skip:
	lda ObjectType,x	; \ If Object X is not dead
	cmp #11				; | then don't play any SFX
	bne :+				; /
	lda PhaseType	; \ If it's Bonus Phase
	bne :+			; / then don't play any SFX
	lda #$20	; \ Play Falling SFX
	sta SFX1Req	; /
	:rts

lf053:
	.BYTE 6,6,7,8,9,10
	.BYTE 0,0,0,0,0
lf05e:
	.BYTE 4,5,6,7
	.BYTE 8,9,10,11
	.BYTE 8,9,10,11
lf06a:
	.BYTE 0,0,0,0
	.BYTE 50,75,100,100
	.BYTE 75,100,150,100
lf076:
	.BYTE 0,0,0,0
	.BYTE 50,75,100,100
	.BYTE 100,150,200,100
lf082:
	.BYTE 0,0,0,0
	.BYTE 1,2,3,3
	.BYTE 2,3,4,3

GetAbsoluteValue:
	pha			; \
	pla			; |
	bpl :+		; | Get Absolute Value of A
	eor #$ff	; |
	clc			; |
	adc #1		; |
	:rts		; /

ObjectXVelSbcYX:
	lda ObjectXVelFrac,y	; \ Object(y).XVelocityFrac - Object(x).XVelocityFrac
	sec						; |
	sbc ObjectXVelFrac,x	; /
	lda ObjectXVelInt,y	; \ Object(y).XVelocity - Object(x).XVelocity
	sbc ObjectXVelInt,x	; /
	rts

ObjectYVelSbcYX:
	lda ObjectYVelFrac,y	; \ Object(y).YVelocityFrac - Object(x).YVelocityFrac
	sec						; |
	sbc ObjectYVelFrac,x	; /
	lda ObjectYVelInt,y	; \ Object(y).YVelocity - Object(x).YVelocity
	sbc ObjectYVelInt,x	; /
	rts

SwapXY:
	stx Temp12
	sty Temp13
	ldx Temp13
	ldy Temp12
	rts

CheckGroundedBird:	; Return with Carry set if object pair contains a grounded Balloon Bird
	cpx #2	; \ If Object X is player,
	bcc :+	; / Return with Carry clear
	lda ObjectStatus,x	; \ If Object(x).Status < 2
	cmp #2				; | Return with Carry clear
	bcc :+				; /
	lda #1					; \ 
	cmp ObjectBalloons,x	; | If Object(x).Balloons <= 1
	bcs :+					; / Return with Carry set

	cpy #2	; \ If Object Y is player,
	bcc :+	; / Return with Carry clear
	lda ObjectStatus,y	; \ If Object(y).Status < 2
	cmp #2				; |
	bcc :+				; /
	lda #1					; \ If 1 - Object(y).Balloons
	cmp ObjectBalloons,y	; / Return result in Carry
	:rts

ObjectBounceX:
	lda #0					; \
	sec						; |
	sbc ObjectXVelFrac,x	; | Reverse X Velocity of Object X
	sta ObjectXVelFrac,x	; | (Bounce Horizontally)
	lda #0					; |
	sbc ObjectXVelInt,x		; |
	sta ObjectXVelInt,x		; /
	lda #0						; \
	ssbcx ObjectDriftXVelFrac	; |
	sta ObjectDriftXVelFrac,x	; | Also reverse the drifting velocity
	lda #0						; | (Used for parachuting birds)
	sbc ObjectDriftXVelInt,x	; |
	sta ObjectDriftXVelInt,x	; /
	lda ObjectAction,x	; \
	and #BBtn			; | Clear player's held left & right inputs
	sta ObjectAction,x	; /
	rts

ObjectBounceY:
	lda #0					; \
	sec						; |
	sbc ObjectYVelFrac,x	; | Reverse Y Velocity of Object X
	sta ObjectYVelFrac,x	; | (Bounce Vertically)
	lda #0					; |
	sbc ObjectYVelInt,x		; |
	sta ObjectYVelInt,x		; |
	rts						; /

ReduceTempVel:
	sta VelMult	; A = multiplier for Velocity. (Always #$CD in game)
	lda TempVelInt		; \ If Velocity Int >= 0
	bpl DivideVelocity	; / then goto DivideVelocity
	lda #0				; \
	ssbc TempVelFrac	; | Get absolute value of Velocity Frac
	sta TempVelFrac		; /
	lda #0			; \
	sbc TempVelInt	; | Get absolute value of Velocity Int
	sta TempVelInt	; /
	jsr DivideVelocity
	lda #0				; \
	ssbc PreciseVelSub	; | Invert PreciseVel
	sta PreciseVelSub	; |
	lda #0				; |
	sbc PreciseVelFrac	; |
	sta PreciseVelFrac	; |
	lda #0				; |
	sbc PreciseVelInt	; |
	sta PreciseVelInt	; /
	rts

DivideVelocity:	; Take in VelMult and TempVel, return PreciseVel = (TempVel / 256 * VelMult)
	phx
	lda #0				; \
	sta PreciseVelSub	; | Init
	sta PreciseVelFrac	; |
	sta PreciseVelInt	; /
	ldx #8	; Loop 8 times
	@Loop:
		asl PreciseVelSub	; \
		rol PreciseVelFrac	; | Shift PreciseVel 1 bit left
		rol PreciseVelInt	; /
		asl VelMult	; \ If the next bit in VelMult is 0,
		bcc @Next	; / Go to next
		clc					; \
		lda TempVelFrac		; | If the current bit of VelMult was 1, then
		adc PreciseVelSub	; | add the TempVel one byte lower into the Fraction and Subfraction of PreciseVel.
		sta PreciseVelSub	; | The way this loop works is each go around PreciseVel is shifted left (doubled)
		lda TempVelInt		; | So if VelMult only had the uppermost bit set, The result would be TempVel / 256 * 128
		adc PreciseVelFrac	; |
		sta PreciseVelFrac	; |
		lda #0				; |
		adc PreciseVelInt	; |
		sta PreciseVelInt	; /
		@Next:
		dex
		bne @Loop
	plx
	rts

ReduceXVelocity:
	lda ObjectXVelFrac,x	; \
	sta TempWordLo			; | TempVel = Object(x).XVel
	lda ObjectXVelInt,x		; |
	sta TempWordHi			; /
	lda #$cd			; \ Reduce velocity
	jsr ReduceTempVel	; /
	lda PreciseVelFrac		; \
	sta ObjectXVelFrac,x	; | Update X Velocity
	lda PreciseVelInt		; |
	sta ObjectXVelInt,x		; /
	rts

ReduceYVelocity:
	lda ObjectYVelFrac,x	; \
	sta TempVelFrac			; |	TempVel = Object(x).YVel
	lda ObjectYVelInt,x		; |
	sta TempVelInt			; /
	lda #$cd			; \ Reduce velocity
	jsr ReduceTempVel	; /
	lda PreciseVelFrac		; \
	sta ObjectYVelFrac,x	; | Update Y Velocity
	lda PreciseVelInt		; |
	sta ObjectYVelInt,x		; /
	rts

DivideDriftVel:
	ldy #4	; Divide by 16 (Shift right 4 times)
	@Loop:
		lda TempDriftVelHi	; \
		asl					; | Preserve sign
		ror TempDriftVelHi	; | Divide by 2
		ror TempDriftVelLo	; /
		dey
		bne @Loop
	rts

UpdateRNG:
	phx	;Preserve X
	ldx #11				; \ Loop 11 times
	@Loop:
		asl RNGLower	; |
		rol RNGUpper	; |
		rolr 2			; | Do Pseudo Random
		eor RNGLower	; | Number Generator stuff?
		rol				; |
		eor RNGLower	; |
		lsrr 2			; |
		eor #$ff		; |
		and #1			; |
		ora RNGLower	; |
		sta RNGLower	; |
		dex				; |
		bne @Loop		; /
	plx
	lda RNGOutput	; Return A = [$1B]
	rts

StartGame:
	jsr GotoTitleScreen
	ldx #9				; \
	@Loop:
		lda #0			; | Player 1 Score to 000000
		sta P1Score,x	; |
		dex				; |
		bpl @Loop		; /
	sta TargetUpdateScore		; Update Player 1 Score
	inc P1Lives		; +1 Life to Player 1
	jsr AddScore	; Update Player Score
	lda #%1111	; \ Enable Sound Channels
	sta SND_CHN	; /
	lda #1		; \ Stop All Sounds
	sta SFX1Req	; /
	lda #2
StartDemoGame:
	sta P1Lives	; Set Player 1 Lives to 2
	ldy TwoPlayerFlag	; \ If it's 2 players
	bne @GiveP2Lives	; | Then give lives to Player 2
	lda #$ff			; / Else no lives
	@GiveP2Lives:
	sta P2Lives		; Set Player 2 Lives to -1 or 2
	ldx #0
	stx BTPlatformX	; Put Balloon Trip Platform at 0px	
	stx CurrentPhaseHeader	; Current Level Header = 0
	stx CurrentPhaseNum	; Current Phase = 0
	stx BonusPhaseIntensity	; Bonus Phase Level = 0
	dex
	stx ObjectBalloons+1	; Set Player 2 Balloons to -1
	ldx TwoPlayerFlag		; \
	@Loop:
		jsr InitPlayerType	; | Set up both player types
		dex					; |
		bpl @Loop			; /
LoadPhase:
	lda #0			; \ Set to Regular Phase
	sta PhaseType	; /
	lda CurrentPhaseNum		; \
	lsrr 2					; | Phase difficulty = Current Phase / 4
	cmp #8					; | 
	bcc @SetPhaseDifficulty	; | Can't be higher than 8
	lda #8					; /
	@SetPhaseDifficulty:
	tax							; \
	lda EnemyStartDelayData,x	; | Assign the phase's difficulty
	sta EnemyStartDelay			; | based on EnemyStartDelayData and EnemyInflateSpeedData
	lda EnemyInflateSpeedData,x	; |
	sta EnemyInflateSpeed		; /
	lda CurrentPhaseNum			; \
	cmp #2						; | If Current Phase >= 2
	bcs @FinishSetDifficulty	; / then finish
	lda #3					; \ For Phases 1 & 2:
	sta EnemyStartDelay		; | Set enemy start delay to 3 (Very short! They get going almost immediately)
	sta EnemyInflateSpeed	; / Set inflate speed to 3 (Moderate)
	@FinishSetDifficulty:
	ldx #7							; \
	@ClearLoop:
		lda #0						; | Initialize variables for each object (except Fish?)
		sta ObjectDirection,x		; | - Direction (0 = Left, 1 = Right)
		sta ObjectHitCooldown,x		; |
		sta ObjectUpgradeFlag,x		; |
		sta ObjectXVelFrac,x		; | - X Velocity (Frac)
		sta ObjectXVelInt,x			; | - X Velocity (Int)
		sta ObjectYVelFrac,x		; | - Y Velocity (Frac)
		sta ObjectYVelInt,x			; | - Y Velocity (Int)
		sta ObjectDriftXVelFrac,x	; |
		sta ObjectDriftXVelInt,x	; |
		sta ObjectXPosFrac,x		; | - X Positions (Frac)
		sta ObjectYPosFrac,x		; | - Y Positions (Frac)
		lda #1						; |
		sta ObjectAnimTimer,x		; |
		sta ObjectCountdown,x		; |
		lda #3						; |
		sta ObjectAnimFrame,x		; | - Animation Frame
		dex							; |
		bpl @ClearLoop				; /
	ldx #5						; \
	@EnemyClearLoop:
		lda #<-1				; | Initialize Enemies
		sta ObjectBalloons+2,x	; |
		dex						; |
		bpl @EnemyClearLoop		; /
	ldx TwoPlayerFlag			; \
	@PlayerInitLoop:
		jsr InitializePlayerX	; | Initialize Players
		dex						; |
		bpl @PlayerInitLoop		; /
	jsr ClearPPU
	jsr InitGameModeAB
	lda EnemyStartDelay	; \
	cmp #16				; | If Enemy Start Delay < 16,
	bcs @SkipDelaySet	; | 
	lda #88				; | Set Enemy Start Delay to 88
	sta EnemyStartDelay	; /
	@SkipDelaySet:
	jsr InitializeFish
	jsr ClearScorePopups
	lda GameMode
	beq @BalloonFightMode	; Balloon Fight Game Mode
	jmp BalloonTripInit	; Balloon Trip Game Mode
	@BalloonFightMode:
	lda PhaseType
	beq @NormalPhase	; Normal Phase Type
	jmp InitializeBonusPhase	; Bonus Phase Type
	@NormalPhase:
	jsr InitializeSparks
	lda CurrentPhaseHeader	; \
	and #3					; | Don't do start jingle if phase header index not divisible by 4
	bne @SkipStartJingle	; /
	lda #8			; \
	sta MusicReq	; / Play Stage Start jingle
	ldx DemoFlag				; \
	bne BalloonFightGameLoop	; / Demo Flag
	@SkipStartJingle:
	lda #$ff				; \ Show Phase Number for
	sta PhaseDisplayTimer	; / 255 frames
	inc CurrentPhaseNum		; Increment Current Phase Number
BalloonFightGameLoop:	; Balloon Fight Game Loop
	jsr Pause
	lda PhaseDisplayTimer	; \
	beq @SkipPhaseText		; | Display Phase Number
	dec PhaseDisplayTimer	; | if the time is not 0
	jsr DrawPhaseText		; /
	@SkipPhaseText:
	jsr UpdateRNG
	jsr ObjectManage
	jsr FishManage
	jsr ManageCloudBolt
	jsr ManageCloudBlink
	jsr ManageSparks
	jsr ManageScorePopup
	jsr SplashManage
	jsr PropellerManage
	inc StarUpdateFlag
	ldx TwoPlayerFlag	; X = 2 Player Flag
	@PlayerRespawnLoop:
		lda ObjectBalloons,x	; \ If Player X has balloons
		bpl @SkipRespawn		; / then skip respawn code
		lda DemoFlag	; \ If Demo Play
		bne :+			; / then return
		lda P1Lives,x		; \ If Player X Lives < 0
		bmi @SkipRespawn	; / then skip respawn code
		dec PlayerSpawnDelay,x	; \ Decrease Player X Respawn Delay
		bne @PhaseClearCheck	; / If not 0 then ?
		phx	; Preserve X
		jsr InitializeSparkDifficulty
		plx	; Restore X
		ldy #2
		dec P1Lives,x	; Decrement Player X Lives
		sty StatusUpdateFlag	; Update Status Bar
		bmi @SkipRespawn	; If Player X has no more lives then don't respawn
		jsr InitializePlayerX	; \ Spawn in the player again
		jsr InitPlayerType		; /
		lda #$80		; \ Play Respawn Jingle
		sta MusicReq	; /
		@SkipRespawn:
		dex						; \ Loop with Player 1
		bpl @PlayerRespawnLoop	; /
	lda P1Lives			; \ If Player 1 has lives
	bpl @SkipGameOver	; / continue
	lda P2Lives			; \ If Player 1 & 2 have 0 lives
	bmi EndGameToTitle	; / then game over
	@SkipGameOver:
	lda DemoFlag			; \ If not Demo Play
	beq @PhaseClearCheck	; / then check if phase is cleared
	jsr PollController0
	lda Joy1Press				; \
	and #SelectBtn | StartBtn	; | If START or SELECT is pressed
	beq BalloonFightGameLoop	; / then loop
	:rts
	@PhaseClearCheck:
		ldx #5	; Enemy Check
		@EnemyCheckLoop:
			lda ObjectBalloons+2,x		; \ If Enemy Balloons
			beq @NextEnemy				; | == 0 then ?
			bpl BalloonFightGameLoop	; /  > 0 then loop
			@NextEnemy:
			dex					; \ Check next enemy
			bpl @EnemyCheckLoop	; /
		lda SplashAnim	; Loop if water plonk effect
		bpl BalloonFightGameLoop	; is not finished yet.
		ldx TwoPlayerFlag	; Only check existing players
		@PlayerCheckLoop:
			ldy ObjectBalloons,x	; \ If Player X has balloons
			dey						; | then 
			bpl @NextPlayer			; /
			lda P1Lives,x	; \ If Player X has no lives
			bmi @NextPlayer	; / then skip
			lda #<-1				; \ Set Player X balloons
			sta ObjectBalloons,x	; / to none
			lda #1					; \ Set Player X Respawn Delay
			sta PlayerSpawnDelay,x	; / to 1 frame
			jmp BalloonFightGameLoop	; loop
			@NextPlayer:
			dex						; \ Loop player checks until
			bpl @PlayerCheckLoop	; / we can assume phase is cleared.
		lda #$02		; \ Play Stage Clear jingle
		sta MusicReq	; /
PhaseCleared:
	ldx #150		; \ Wait 150 frames
	jsr WaitXFrames	; /
	ldx CurrentPhaseHeader	; \
	inx						; | Get to next level
	cpx #16					; | if past level ID #16
	bne @SetHeader			; |
	ldx #4					; | then loop back to level ID 4
	@SetHeader:
	stx CurrentPhaseHeader	; /
	jmp LoadPhase	; Load Next Level

EndGameToTitle:	; Manage Game Over
	lda #$01		; \ Play Game Over jingle
	sta MusicReq	; /
EndGameNoJingle:
	lda #0
	sta BTXScroll		; Reset PPUSCROLL Shadow
	sta PPUCTRLShadowBT	; Reset PPUCTRL Shadow
	sta Temp15	; Use Temp15 as delay timer
	jsr DrawGameOverText
	@WaitLoop:
		jsr FinishFrame
		jsr PollController0			; \ Press START or SELECT
		and #SelectBtn | StartBtn	; | to come back to Title Screen
		bne @BackToTitle			; /
		dec Temp15		; \ Wait for 256 frames
		bne @WaitLoop	; / to come back to Title Screen
	@BackToTitle:
	jmp StartGame	; Back to Title Screen

InitializePlayerX:
	lda P1Lives,x	; \ If Player X has negative lives
	bmi :+			; / Then don't do anything
	lda PlayerStartingX,x	; \ Set up X coordinate for Player X
	sta ObjectXPosInt,x		; /
	lda #184			; \ Set up Y coordinate for Player X
	sta ObjectYPosInt,x	; /
	sta PlayerInvincible,x	; Set up invincibility for Player X
	lda #200				; \ Set up invincibility time
	sta PlayerInvTimer,x	; / for Player X
	lda #90					; \
	ldy P1Lives,x			; | If Player X has lives
	bpl @SetDelay			; | Then set respawn delay to #$5A
	lda #1					; | Else set respawn delay to #$01
	@SetDelay:
	sta PlayerSpawnDelay,x	; /
	lda #0
	sta PlayerFreeze,x	; Clear Player X Freeze Flag
	sta ObjectXVelInt,x		; \ Set up Player X's X Velocity to $00
	sta ObjectXVelFrac,x	; /
	:rts

PlayerStartingX:
	.BYTE $20,$d0

InitPlayerType:
	lda #3				; \ Set Player X type to 03 (2 Balloons)
	sta ObjectType,x	; /
	lda #2					; \ Set Player X Balloons to 02
	sta ObjectBalloons,x	; /
	rts

EnemyStartDelayData:
	.BYTE $58,$50,$58,$50,$50,$40,$38,$30,$28
EnemyInflateSpeedData:
	.BYTE $04,$04,$03,$03,$02,$02,$02,$02,$02

DrawPhaseText:
	lda PhaseDisplayTimer	; \ Toggle between "PHASE-??"
	and #$20				; | and empty
	beq @ClearText			; / every #$20 frames?
	ldx #10					; \
	@LoadLoop:
		lda PhaseText,x		; | Copy "PHASE-  " PPU Block
		sta PPUTempBlock,x	; |
		dex					; |
		bpl @LoadLoop		; /
	ldy #10					; \
	lda CurrentPhaseNum		; | Add 1st digit of
	sta DivisionRemainder	; | Phase Number
	jsr DivideByY			; | (Divide by 10)
	sta PPUTempBlock+9		; /
	lda DivisionRemainder	; \ Add 2nd digit of
	sta PPUTempBlock+10		; / Phase Number
	jmp CopyPPUTempBlock
	@ClearText:
		lday PhaseTextEmpty	; \ Copy Empty PPU Block
		jmp CopyPPUBlock	; /

PhaseText:	
	.BYTE $20,$6c,$08	; $206C - 8 tiles
	.BYTE $19,$11,$0a,$1c,$0e,$25,$00,$00	; "PHASE-  "
PhaseTextEmpty:	
	.BYTE $20,$6c,$08	; $206C - 8 tiles
	.BYTE $24,$24,$24,$24,$24,$24,$24,$24	; "        "

DrawGameOverText:
	jsr FinishFrame
	ldx #1							; \
	@Loop:
		lda GOPPUBlockPointersLo,x	; | Prepare Game Over
		ldy GOPPUBlockPointersHi,x	; | PPU blocks
		jsr CopyPPUBlock			; | to upload
		dex							; |
		bpl @Loop					; /
	ldx #15				; \
	@EmptyLoop:
		lda #$24		; | Prepare 16 empty tiles
		sta $5a,x		; | to upload
		dex				; |
		bpl @EmptyLoop	; /
	lda #16				; \ Size: 16 bytes
	sta PPUTempBlock+2	; /
	lda #$21			; \ PPUADDR = $21xx
	sta PPUTempBlock	; /
	ldx #2						; \
	@AddrLoop:
		lda GOClearLoAddr,x		; | Prepare uploading
		sta PPUTempBlock+1		; | empty tiles to nametable
		jsr CopyPPUTempBlock	; | ($2188, $21A8, $21E8)
		dex						; | to PPU Buffer
		bpl @AddrLoop			; /
	rts

.define GOPPUBlockPointers GameOverMainText, GameOverMainAttr
GOPPUBlockPointersLo:	; Pointers to PPU Blocks
	.LOBYTES GOPPUBlockPointers
GOPPUBlockPointersHi:
	.HIBYTES GOPPUBlockPointers

GOClearLoAddr:	; Empty tiles lower PPUADDR
	.BYTE $88,$a8,$e8
GameOverMainText:	; "   GAME  OVER   "
	.BYTE $21,$c8,16	;16 bytes to $21C8
	.BYTE $24,$24,$24,$10,$0a,$16,$0e,$24,$24,$18,$1f,$0e,$1b,$24,$24,$24	; "   GAME  OVER   "
GameOverMainAttr:	; Tile Attributes
	.BYTE $23,$da,4
	.BYTE %10101010,%10101010,%10101010,%10101010

Wait20Frames:
	ldx #20
WaitXFrames:
	@Loop:
		jsr FinishFrame
		dex
		bne @Loop
	rts

FinishFrame:
	lda #0
	sta FrameProcessFlag
	FrameWaitLoop:
		lda FrameProcessFlag
		beq FrameWaitLoop
	dec FrameProcessFlag
	:rts

Pause:
	jsr FrameWaitLoop
	lda DemoFlag	; \ If Demo Flag Set
	bne :-			; / then don't do anything
	jsr PollController0	; \
	and #StartBtn		; | If START is not pressed
	beq :-				; / then don't do anything
	lda #4			; \ Play Pause jingle
	sta MusicReq	; /
	lda PPUMASKShadow	; \
	and #%11101111		; | Hide Sprites
	sta PPUMASK			; /
	@PauseLoop:
		jsr FinishFrame		; \
		jsr PollController0	; |
		and #StartBtn		; | If START is not pressed
		beq @PauseLoop		; / then loop
	lda PPUMASKShadow	; \
	sta PPUMASK			; / Show Sprites
	ldy #$04		; \
	lda PhaseType	; | Play Pause jingle if
	ora GameMode	; | it's a Normal Phase in Balloon Fight Game Mode
	beq @PauseSnd	; | Else play Balloon Trip / Bonus Phase music
	ldy #$20		; |
	@PauseSnd:
	sty MusicReq	; /
	rts

InitializeFish:
	lda #1
	sta FishUnused1	; \ Set Unused Variables?
	sta FishUnused2	; /
	lda #$FF		; \ Reset Water Plonk Animation
	sta SplashAnim	; /
	sta ObjectStatus+8	; Fish Status = #$FF
	sta FishTargetEaten	; Fish Target Eaten Flag = #$FF
	ldx #1
	stx ObjectType+8	; Fish Type = #$01
	stx ObjectBalloons+8	; Fish Balloons = #$01
	inx						; \ Update Status Bar
	stx StatusUpdateFlag	; /
	lda #64				; \ Set Fish X position
	sta ObjectXPosInt+8	; / to 64px
	rts

.include "Sound.asm"

.SEGMENT "VECTORS"
	.WORD NMI		;NMI
	.WORD Reset		;RESET
	.WORD BRKLoop	;IRQ/BRK