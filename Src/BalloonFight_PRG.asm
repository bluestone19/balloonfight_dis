;Balloon Fight (JP/USA/EU) Disassembly - PRG ROM
;-----------------------
;Starting disassembly with nesrevplus v0.3b (2018-09-18 16:39, 5ms)
;Manual disassembly and commenting by LuigiBlood
;CA65 Conversion and multi-region support by Bluestone19

REGION	.set 1	;0 = JP, 1 = US, 2 = EU

.p02

.include "Macros.asm"
.include "MemoryDefines.asm"

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

;----------------------
; NMI/PPU Management code
;----------------------

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
		cpy TempBlockSize	; |
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
	lda TempBlockSize	; | Put Upload Size
	sta PPUBuffer,x		; /
	inx			; \ Return:
	rts			; / X = Current PPU Buffer Address

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

;----------------------
; Balloon Trip Game Mode code
;----------------------

BalloonTripInit:
	lda #$20		; \ Play Balloon Trip Music
	sta MusicReq	; /
	jsr BTSetBalloonPoints
	jsr UpdateBTRank
	lda #$ff			; \ No platforms
	sta PlatformCount	; /
	tpa BTStartLayout, LeftPointer
	lda #128			; \ Set Player 1 X Position
	sta ObjectXPosInt	; / to 128 (0x80, middle of screen)
	sta BTPlatformX		; Set Balloon Trip Starting Platform X Position to #$80
	lda #112			; \ Set Player 1 Y Position
	sta ObjectYPosInt	; / to 112px (0x70, middle of screen)
	jsr InitBalloons
	lda #0
	sta P1Lives		; 0 Lives to Player 1
	sta TileScrollCount		; Init Tile Scroll Counter
	sta ScreenScrollCount	; Init Screen Scroll Counter
	sta SparkIntensity		; Init SparkIntensity
	sta ScrollLockTimer	; Init Scrolling Lock Time
	sta PhaseType		; Phase Type = 0
	jsr InitializeFish
	ldx #19
	@SparkResetLoop:
		lda #$ff			; \ Reset All 20 Sparks
		sta SparkAnim,x		; | Animation Frame = -1
		lda #240			; |
		sta SparkYPosInt,x	; | Y Position = 240 (Offscreen)
		dex					; |
		bpl @SparkResetLoop	; /
BalloonTripGameLoop:
	jsr Pause
	jsr ObjectManage
	lda ScrollLockTimer	; If Screen is locked
	bne @SkipFish		; then don't manage Fish
	jsr FishManage
	@SkipFish:
	lda FrameCounter			; \ Manage Screen Scrolling and stuff
	lsr							; | every 2 frames...
	bcs @ContinueScrollUpdate	; /
	jmp BTPhysics
	@ContinueScrollUpdate:
	lda ScrollLockTimer	; \ ...unless the scrolling
	beq @SkipLockTimer	; | is locked
	dec ScrollLockTimer	; /
	jmp BTPhysics
	@SkipLockTimer:
	lda BTXScroll		; \ If the scrolling X position
	bne @SkipPPUCTRL	; | is 0 then
	lda PPUCTRLShadowBT	; | Toggle between
	eor #1				; | nametable 0 and 1
	sta PPUCTRLShadowBT	; /
	@SkipPPUCTRL:
	dec BTXScroll	; Scroll 1 pixel from the left
	lda BTPlatformX		; \ Skip if starting platform
	beq @SkipPlatform	; / does not exist
	inc BTPlatformX	; Scroll starting platform 1px to the right
	lda BTPlatformX		; \
	cmp #240			; | If Starting Platform reaches
	bcc @PlatformExists	; | X position #$F0
	lda #0				; | then disappear
	sta BTPlatformX		; /
	@PlatformExists:
		lda PlayerInvincible	; \ If Player is invincible
		beq @SkipPlatform		; | (has not yet moved)
		inc ObjectXPosInt		; / then scroll 1px to the right
	@SkipPlatform:
	ldx #7	; Max of 8 balloons on screen
	@BalloonLoop:	; Scroll all the balloons
		lda BalloonStatus,x	; \ If balloon doesn't exist
		bmi @SkipBalloon	; / skip to the next one
		inc BalloonXPos,x	; Scroll balloon 1px to the right
		lda BalloonXPos,x	; \
		cmp #248			; | If balloon's X position
		bne @SkipBalloon	; | reaches #$F8
		lda #$ff			; | then make it disappear
		sta BalloonStatus,x	; |
		lda #240			; |
		sta BalloonYPos,x	; | And reset the balloon counter
		lda #0				; |
		sta P1TripBalloons	; /
		@SkipBalloon:
		dex					; \ Check next balloon
		bpl @BalloonLoop	; /
	ldx #19	; Max of 20 sparks on screen
	@SparkLoop:	;Scroll all the sparks
		lda SparkAnim,x	; \ If Lightning Bolt doesn't exist
		bmi @SkipSpark	; / then skip to next one
		inc SparkXPosInt,x	; Scroll Spark 1 pixel to the right
		lda SparkXPosInt,x	; \
		cmp #248			; | If bolt's X position
		bcc @SkipSpark		; | reaches #$F8
		lda #240			; | then make it disappear
		sta SparkYPosInt,x	; |
		sta SparkAnim,x		; /
		@SkipSpark:
		dex				; \ Check next bolt
		bpl @SparkLoop	; /
	lda BTXScroll	; \ Every 8 pixel scrolled
	and #7			; |
	bne BTPhysics	; /
	ldx ObjectBalloons	; \ If Player still has balloons
	dex					; |
	bmi BTPhysics		; /
	lda #0					; \
	sta TargetUpdateScore	; | Add 10 to Player 1 Score
	lda #1					; |
	jsr AddScore			; /
	inc TileScrollCount	; Increment Tile Scroll Counter
	lda TileScrollCount		; \
	and #31					; | If 32 tiles have been scrolled
	bne @GenerateObstacles	; /
	inc ScreenScrollCount	; Increment Screen Scroll Counter
	lda ScreenScrollCount	; \
	cmp #10					; | If 10 screens have been scrolled
	bne @GenerateObstacles	; /
	lda #2					; \ Then reset to Screen #$02
	sta ScreenScrollCount	; /
	ldy SparkIntensity	; \
	iny					; | Increment
	tya					; | Lightning Bolt Intensity Level
	and #3				; |
	sta SparkIntensity	; /
	@GenerateObstacles:
		ldx ScreenScrollCount		; \
		lda ScreenLayoutOrder,x		; |
		asl							; |
		tay							; | Manage Screen Layout
		lda BTScreenSubroutines,y	; | Jump to subroutines
		sta RightPointerLo			; | dedicated to each screen layout
		lda BTScreenSubroutines+1,y	; |
		sta RightPointerHi			; |
		jsr DoBTScreenRoutine		; /
BTPhysics:
	ldx #7
	@BalloonLoop:
		lda BalloonStatus,x	; \ If Balloon X does not exist
		bmi @SkipBalloon	; / then skip collision check
		jsr lcece_ballooncollision
		lda P1BonusBalloons	; \
		beq @SkipBalloon	; | Every balloon touched
		dec P1BonusBalloons	; | counts towards the
		inc P1TripBalloons	; / main counter
		phx
		lda BalloonPts	; \ Add Score
		jsr AddScore	; /
		plx
		@SkipBalloon:
		jsr lce2f_balloonxspritemanage
		dex					; \ Check next balloon
		bpl @BalloonLoop	; /
	ldx #19
	@SparkLoop:
		lda SparkAnim,x	; \ If Lightning Bolt exists?
		bmi @SparkEmpty	; /
		lda ScrollLockTimer	; \ If Scrolling is locked
		bne @SkipSparkMove	; /
		jsr UpdateSparkPos	; Update Lightning Bolt Position
		lda SparkYPosInt,x	; \
		cmp #2				; | If Y pos < #$02
		bcs @NoBounce		; | then
		jsr SparkBounceYSFX	; / Bounce Lightning Bolt Vertically
		@NoBounce:
		cmp #216			; \ If Y pos >= #$D8
		bcc @SkipSparkMove	; | then
		jsr SparkBounceYSFX	; / Bounce Lightning Bolt Vertically
		@SkipSparkMove:
		jsr SparkPlayerCollision
		@SparkEmpty:
		lda FrameCounter	; \
		and #7				; | Get Lightning Bolt
		lsr					; | Animation Frame Tile
		tay					; |
		lda lc9dd,y			; | (Unused Note: Animation is 8 frames
		pha					; / but only half of them are used.)
		lda FrameCounter	; \
		lsr					; | Every 2 frames...
		txa					; |
		bcc @SparkAddressX	; /
		sta Temp12	; \
		lda #19		; | ...count from the end
		sbc Temp12	; /
		@SparkAddressX:
		aslr 2	; \ Get OAM Sprite Address
		tay		; /
		pla				; \ Update Lightning Bolt Sprite
		sta OAM+$b1,y	; / Tile ID
		lda SparkYPosInt,x	; \
		sta OAM+$b0,y		; / Y position
		lda SparkXPosInt,x	; \
		sta OAM+$b3,y		; / X position
		lda #0			; \
		sta OAM+$b2,y	; / Use Sprite Palette 0
		dex				; \ Loop to next bolt
		bpl @SparkLoop	; /
	lda P1TripBalloons	; \ If you touched
	cmp #20				; | 20 balloons in a row...
	bcc @DrawBTPlatform	; /
	inc ScoreDigit4		; \ Add 10000
	lda #0				; | to score
	jsr AddScore		; /
	dec ScoreDigit4		; Reset score to add
	lda #$10		; \ Play Bonus Phase Perfect jingle
	sta MusicReq	; /
	inc PhaseType	; Set Bonus Phase Type
	jsr LoadPalette	; Update Balloon Palette
	jsr BTSetBalloonPoints
	dec PhaseType	; Reset to Normal Phase
	ldx #100				; \ Wait for 100 frames
	jsr WaitYFrames	; /
	lda #$20		; \ Play Balloon Trip Music
	sta MusicReq	; /
	@DrawBTPlatform:
		ldx #$f0			; \ If Balloon Trip Starting Platform
		lda BTPlatformX		; | X position is 0
		beq @HideBTPlatform	; / then don't make it appear on screen
		ldx #$88	; \ At Y position #$88:
		@HideBTPlatform:
		stx OAM+0	; | Display Left and Right
		stx OAM+4	; / sides of Platform
		sta OAM+3	; \
		clc			; | Display Left and Right
		adc #8		; | sides at current X position
		sta OAM+7	; /
		lda FrameCounter	; \
		and #3				; | Switch between palettes
		sta OAM+2			; | on platform
		sta OAM+6			; /
		ldx #$e3	; \
		stx OAM+1	; | Display Tile #$E3 and #$E4
		inx			; |
		stx OAM+5	; /
	lda ObjectBalloons		; \ If Player is dead (no balloons)
	bmi BalloonTripGameOver	; / then game over
	jmp BalloonTripGameLoop	; else game loop

BalloonTripGameOver:
	jsr RankScoreUpdate
	lda #$01	; \ Play SFX
	sta SFX1Req	; /
	jsr FinishFrame
	lda #$02		; \ Play Stage Clear jingle
	sta MusicReq	; /
	jmp lf36a	; Put Game Over on Screen

DoBTScreenRoutine:
	jmp (RightPointer)

BTScreenSubroutines:	; Screen Layout Subroutines
	.WORD BTLoadLayout		;0: Place sparks and balloons in predefined layout
	.WORD BTRandScreen		;1: Randomly place balloons and moving sparks
	.WORD BTRandWBubble		;2: Same as 1 but with chance of a Bubble appearing
	.WORD BTOncomingSparks	;3: Sparks come flying at player
	.WORD BTRTS				;4: Empty screen

ScreenLayoutOrder:	; The pattern for each screen
	.BYTE 0,0		;First two screens, use predefined layout. Not repeated
	.BYTE 2,2,2,2,2	;Five screens, standard randomly moving sparks, randomly placed balloons, chance for a bubble
	.BYTE 4,3,1		;Empty screen, Fast sparks flying to right, normal screen but without bubble

BTLoadLayout:
	ldy #0				; \
	lda (LeftPointer),y	; | Read Layout Data Byte
	inc LeftPointerLo	; | at LeftPointer and Increment
	bne @ParseByte		; | Bit format: BS0YYYYY
	inc LeftPointerHi	; / B = Balloon, S = Spark, Y = Tile Y Position
	@ParseByte:
	tax		; Store loaded Layout Byte in X
	beq :+	; If Layout Byte = 00 then return
	aslr 3		; \ Set Y position
	sta Temp15	; /
	lda #0		; \ Clear Temp14
	sta Temp14	; /
	txa		;Load Layout Byte to A again
	and #%11000000		; \ Cut out Y bits
	cmp #%10000000		; | If bit B is set
	bne @CheckSparkBit	; | then
	jsr BTSpawnBalloon	; / Spawn Balloon
	jmp BTLoadLayout	; Repeat
	@CheckSparkBit:
	cmp #0				; \ If bit S is set
	bne :+				; | then
	jsr BTSpawnSpark	; / Spawn Spark
	jmp BTLoadLayout	; Repeat
	:rts

BTRandScreen:
	jsr UpdateRNG		; Get random number in A
	and #$7f				; \
	cmp #4					; |
	bcc BTRandomizeSpark	; | If RNG value is between
	cmp #24					; | 4 and 23
	bcs BTRandomizeSpark	; |
	aslr 3					; | then spawn Balloon
	sta Temp15				; | at Tile Y position value
	jsr BTSpawnBalloon		; /
BTRandomizeSpark:
	jsr UpdateRNG			; \
	and #$3f				; |
	cmp #2					; | If RNG value is between
	bcc :+					; | 2 and 23
	cmp #24					; | then spawn Lightning Bolt
	bcs :+					; | at Tile Y position value
	aslr 3					; | and set Y velocity value
	sta Temp15				; | using BTSparkVelOptions value
	jsr UpdateRNG			; | (depending on full loop)
	and #$3f				; | + RNG value up to 63
	ldx SparkIntensity		; |
	adc BTSparkVelOptions,x	; |
	sta Temp14				; |
	jsr BTSpawnSpark		; /
	jsr UpdateRNG			; \
	lsr						; | Make Y velocity value negative
	bcc BTRandomizeSpark	; | 50% of the time
	jsr SparkBounceYSFX		; |
	jmp BTRandomizeSpark	; /
	:rts

BTSparkVelOptions:
	.BYTE $20,$30,$40,$60

BTRandWBubble:
	jsr UpdateRNG		; \ Get a random number
	and #%11001111		; | Discard two bits
	bne BTRandScreen	; / If not 0, continue
	ldy ObjectBalloons+1	; \ 
	iny						; | 
	bne BTRandScreen		; / If Bubble exists, continue
	lda #230			; \ Otherwise, make a new Bubble
	sta ObjectYPosInt+1	; / First set Bubble Y to 230 (0xE6)
	lda RNGOutput		; \
	and #127			; | Bubble X Position is random
	adc #64				; | between 64 and 191
	sta ObjectXPosInt+1	; /
	lda #$80				; \ Set balloons to 0x80
	sta ObjectBalloons+1	; / Which means it's a Bubble
	lda #0				; \ Bubble Status = 00
	sta ObjectStatus+1	; /
BTRTS:
	rts

BTOncomingSparks:
	jsr BTRandomizeSpark	; Randomly Spawn Spark
	jsr UpdateRNG		; \
	and #$7f			; | Set X Velocity (Frac)
	sta SparkXVelFrac,x	; / RNG up to 127
	rts

BTSpawnBalloon:
	ldx #7						; \
	@SlotLoop:
		lda BalloonStatus,x		; | Find Balloon that
		bmi @BalloonSlotFound	; | hasn't spawned yet
		dex						; |
		bpl @SlotLoop			; /
	rts
	@BalloonSlotFound:
		lda #1				; \ Set Balloon Type/GFX? to 01
		sta BalloonStatus,x	; /
		lda #0				; \ Set Balloon X position to 00
		sta BalloonXPos,x	; /
		lda Temp15			; \ Set Balloon Y position to [$15]
		sta BalloonYPos,x	; /
	rts

BTSpawnSpark:
	ldx #19					; \
	@SlotLoop:
		lda $0530,x			; | Find open slot
		bmi SparkSlotFound	; | for a Spark
		dex					; |
		bpl @SlotLoop		; /
	rts
	SparkSlotFound:
		lda #0
		sta $0530,x	; Set Animation Frame to 00
		sta $0490,x	; Set X position to 00
		sta $04f4,x	; Set Y velocity to 00
		sta $0508,x	; \ Set X velocity to 00 (Int and Frac)
		sta $04e0,x	; /
		lda Temp14		; \ Set Y velocity (Frac) to [$14]
		sta $051c,x		; /
		lda Temp15		; \ Set Y position to [$15]
		sta $04a4,x		; /
	rts

BTStartLayout:	; Screen Premade Layout Data
	.BYTE $00
	.BYTE $00
	.BYTE $09,$00
	.BYTE $08,$8c,$00
	.BYTE $07,$18,$00
	.BYTE $18,$00
	.BYTE $19,$00
	.BYTE $1a,$00
	.BYTE $84,$94,$1a,$00
	.BYTE $1a,$00
	.BYTE $1a,$00
	.BYTE $0b,$12,$00
	.BYTE $0c,$13,$00
	.BYTE $0d,$14,$00
	.BYTE $14,$00
	.BYTE $00
	.BYTE $90,$00
	.BYTE $07,$00
	.BYTE $07,$8c,$96,$00
	.BYTE $08,$00
	.BYTE $09,$00	; Not loaded
	.BYTE $00
	.BYTE $18,$00	;\
	.BYTE $17,$00	;| Not loaded
	.BYTE $16,$00	;/
	.BYTE $00
	.BYTE $00
	.BYTE $00
	.BYTE $00
	.BYTE $00
	.BYTE $8a,$90,$00
	.BYTE $00
	.BYTE $00
	.BYTE $08,$00
	.BYTE $09,$98,$00
	.BYTE $0a,$00
	.BYTE $00
	.BYTE $00
	.BYTE $86,$8a,$15,$00	;Balloons might not load
	.BYTE $14,$00
	.BYTE $8e,$13,$00
	.BYTE $00
	.BYTE $03,$0d,$00
	.BYTE $0d,$0e,$00
	.BYTE $0c,$0d,$00
	.BYTE $0d,$19,$00
	.BYTE $86,$92,$00	;Bottom Balloon can be blocked
	.BYTE $00
	.BYTE $98,$00
	.BYTE $00
	.BYTE $0a,$12,$00
	.BYTE $09,$13,$00
	.BYTE $08,$14,$00
	.BYTE $07,$15,$00
	.BYTE $07,$16,$00	;\ Not loaded
	.BYTE $07,$00		;/ 
	.BYTE $00
	.BYTE $00

BTSetBalloonPoints:
	jsr SetBonusPhase	; Set up Balloon Points
	asl BalloonPts	; \
	lda BalloonPts	; | Multiply Balloon Points
	aslr 2			; | by 10
	adc BalloonPts	; |
	sta BalloonPts	; /
	rts

UpdateBTRank:
	lda #0		; \ Set Balloon Trip Rank 01 (00 + 1)
	sta Temp12	; /
	@RankLoop:
		lda Temp12			; \
		aslr 2				; | Setup Pointer to
		adc Temp12			; | 0700 + (Rank)*5
		sta LoadPointerLo	; |
		lda #7				; |
		sta LoadPointerHi	; /
		ldy #4					; \
		@ReadDigitLoop:
			lda (LoadPointer),y	; | Check each digit
			cmp P1Score,y		; | If P1 Score Digit < Rank Score
			bcc @SetRank		; | then stop
			bne @Continue		; | If >= then check next Rank Score
			dey					; |
			bpl @ReadDigitLoop	; / Else check next digit
		bmi @SetRank	; When done, update current Rank
		@Continue:
		inc Temp12		; \
		lda Temp12		; | If (Rank+1) != 50 (!)
		cmp #50			; | then check the next rank
		bne @RankLoop	; | else update current rank
	dec Temp12			; /
	@SetRank:
		inc Temp12		; \
		lda Temp12		; |
		pha				; | Update Current Rank variable
		sta ScoreDigits	; |
		ldy #10			; |
		jsr DivideByY	; | (Rank+1) / 10
		sta BTRankHi	; | Write second digit
		lda ScoreDigits	; |
		sta BTRankLo	; | Write first digit (modulo)
		pla				; |
		sta Temp12		; /
	rts

RankScoreUpdate:
	jsr UpdateBTRank	; Update Balloon Trip Rank
	dec Temp12	; \
	lda #49		; | A = (Rank - 49)
	sec			; |
	sbc Temp12	; /
	sta Temp13	; \
	aslr 2		; | Y = A * 5
	adc Temp13	; |
	tay			; /
	lda Temp12			; \
	aslr 2				; | [$1D] = Pointer to Score Rank
	adc Temp12			; |
	sta LoadPointerLo	; |
	clc					; |
	adc #5				; |
	sta DataPointerLo	; | [$1F] = Pointer to Score Rank+1
	lda #7				; |
	sta LoadPointerHi	; |
	sta DataPointerHi	; /
	tya			; \ If Rank == 49 then
	beq @Skip	; / only update one rank score.
	dey						; \
	@CopyLoop1:
		lda (LoadPointer),y	; | Shift Balloon Trip
		sta (DataPointer),y	; | Score Ranking
		dey					; | by one rank above
		bne @CopyLoop1		; |
	lda (LoadPointer),y		; |
	sta (DataPointer),y		; /
	@Skip:
	ldy #4					; \
	@CopyLoop2:
		lda P1Score,y		; | Copy current score
		sta (LoadPointer),y	; | to current Score Rank
		dey					; |
		bpl @CopyLoop2		; /
	rts

;----------------------
; Fish code
;----------------------

lc5b7:	; Fish Animation 0
	.BYTE $01,$02,$03,$03
lc5bb:	; Fish Animation 1
	.BYTE $02,$01,$ff,$03,$04,$05,$06,$ff

lc5c3:
	lda FishFrameTime	; \
	lsrr 3				; | X = FishFrameTime / 8
	tax					; /
	lda FishAnimation	; \
	bne lc5d5			; | If Fish Animation? == 0
	lda lc5b7,x			; / set Fish Status
	jmp lc5d8
	lc5d5:
	lda lc5bb,x	; If Fish Animation? != 0
	lc5d8:
	sta ObjectStatus+8	; Update Fish Status
	ldx #8
	jsr le3a4
	lda FishTargetEaten	; \ If Fish Target Eaten Flag
	beq :+				; / is set
	ldx FishTargetID	; X = Fish Target
	lda FishFrameTime		; \
	cmp #32					; |
	bne lc5f4				; | If Fish Frame Time == $20
	lda #$ff				; | then target is eaten
	sta ObjectBalloons,x	; | (Balloons = -1)
	bmi lc610				; /
	lc5f4:
	bcs :+	; If Fish Frame Time < $20
	lda ObjectDirection+8	; \ Depending on Fish Direction
	bne @MoveLeft			; /
	lda ObjectXPosInt+8	; \
	clc					; | Move Fish 4 pixels to the right
	adc #4				; /
	bne @FinishMove
	@MoveLeft:
	lda ObjectXPosInt+8	; \ or
	sec					; | Move Fish 4 pixels to the left
	sbc #4				; /
	@FinishMove:
	sta ObjectXPosInt,x
	lda ObjectYPosInt+8	; \
	sec					; | Fish Target's Y position =
	sbc #10				; | (Fish Y - $0A)
	sta ObjectYPosInt,x	; /
	lc610:
	jsr le3a4
	:rts

FishSearchTarget:
	lda #$ff			; \ Reset Target
	sta FishTargetID	; / to none
	ldx #7						; \
	@Loop:
		lda ObjectBalloons,x	; | Check each object
		bmi @NextTarget			; | if it exists,
		lda ObjectYPosInt,x		; | if Y pos >= #$9A
		cmp #$b4				; | if X pos == Fish X pos
		bcc @NextTarget			; | then the first one
		lda ObjectXPosInt,x		; | that meets these conditions
		cmp ObjectXPosInt+8		; | is the target
		beq FishSetTarget		; |
		@NextTarget:
		dex						; | else check next object
		bpl @Loop				; /
	rts
	FishSetTarget:
		stx FishTargetID	; Update Target
		lda ObjectDirection,x	; \ Update Fish Direction
		sta ObjectDirection+8	; / with Target Object's Direction
		lda #0
		sta FishAnimation	; Reset Fish Animation?
		sta FishFrameTime	; Reset Fish Frame Time
		sta FishTargetEaten	; Reset Fish Target Eaten Flag
		sta FishYDirection	; Reset Fish Y Direction to Up
		lda #$dc			; \ Set Fish Y position to #$DC
		sta ObjectYPosInt+8	; /
		rts

FishMove:
	inc ObjectXPosInt+8	; Move Fish +1 pixel to the right
	lda ObjectXPosInt+8	; \
	cmp #177			; | If Fish X position >= 177px (0xB1)
	bcc :+				; | then go back to X pos = 64 (0x40)
	lda #64				; |
	sta ObjectXPosInt+8	; /
	:rts

lc658:
	lda FishYDirection	; \ If Fish Y Direction == Up
	bne @FishDescend	; /
	dec ObjectYPosInt+8	; Fish Y goes up by 1 pixel
	lda ObjectYPosInt+8	; \
	cmp #$c4			; | If Fish Y Position is about
	bcs @FinishYMove	; | to go above $C4
	inc ObjectYPosInt+8	; | then
	inc FishAnimation	; | Set Fish Animation? to 1
	inc FishYDirection	; | and Fish Y Direction to down
	bne @FinishYMove	; /
	@FishDescend:
		inc ObjectYPosInt+8	; Fish Y goes down by 1 pixel
	@FinishYMove:
	inc FishFrameTime	; Increase Fish Frame Time
	lda FishFrameTime	; \
	cmp #24				; | If Fish Frame Time == 24
	bne lc6a3			; /
	ldx FishTargetID		; \
	lda ObjectBalloons,x	; | If Target exists...
	bmi lc6a3				; / (has balloons)
	lda ObjectYPosInt,x	; \
	clc					; | If the target is above
	adc #16				; | the fish by 16 pixels
	cmp ObjectYPosInt+8	; |
	bcc lc6a3			; /
	ldy ObjectType,x	; \
	lda lc6b8,y			; | Change Target Object Type
	sta ObjectType,x	; /
	lda #0					; \ Insta Kill
	sta ObjectStatus,x		; | Target Object Status = 0
	sta ObjectBalloons,x	; / Target Object Balloons = 0
	lda MusicReq	; \
	ora #$40		; | Play Fish Jingle
	sta MusicReq	; /
	inc FishTargetEaten	; Set Fish Target Eaten Flag
	lc6a3:
	lda FishAnimation	; \ Fish Animation? != 0
	beq :+	; /
	lda FishFrameTime	; \
	cmp #$28			; | If Fish Frame Time? == $28
	beq lc6b3			; / OR
	cmp #$30	; \ If Fish Frame Time? == $30
	bne :+		; /
	lc6b3:
	lda #$cc			; \ then
	sta ObjectYPosInt+8	; / Fish Y Position = $CC
	:rts

lc6b8:
    ;12 bytes
.BYTE $08,$09,$0a,$0b,$08,$09,$0a,$0b,$08,$09,$0a,$0b

lc6c4:
	lda FishYDirection	; \ If Fish Direction is Up
	bne :+		; /
	ldx $048b	;
	lda $88,x	; \ Do Object X exist?
	bmi lc6e0	; /
	lda ObjectYPosInt,x	; \
	cmp #$b4	; | Is Object X >= Y pos #$B4?
	bcc lc6e0	; /
	lda ObjectXPosInt,x	; \
	cmp #$40	; | Is Object X between
	bcc lc6e0	; | X positions #$40 and #$B1?
	cmp #$b1	; | If so, teleport fish
	bcc lc6ee	; /
lc6e0:
	lda #$30	; \ Else
	sec			; | Fish Frame Time? = $30 - itself
	sbc $048d	; |
	sta $048d	; /
	inc FishYDirection	; Set Fish Direction to Down
	bne :+
lc6ee:
	lda ObjectXPosInt,x	; \ Teleport Fish
	sta $99		; / to Object's X position
	lda ObjectDirection,x	; \ Change Fish Direction
	sta $0450	; / to Object's Direction
	:rts

FishManage:
	lda $87		; \ If Fish Status >= 0
	bpl lc70d	; / then handle eating
	jsr FishMove	
	jsr FishSearchTarget
	lda $048b	; \ If Target found
	bpl lc709	; / then handle Fish attack
	rts
lc709:
	lda #$40	; \ Play Fish Eating SFX
	sta $f3		; /
lc70d:
	jsr lc6c4	; Handle Fish Teleport to Target
	jsr lc658	; Handle Fish Target Eating
	jmp lc5c3	; Handle Fish Target Eating Movement


;----------------------
; Spark Code

	InitializeSparks:
		ldx #1						; \
		@InitLoop:
			lda #$ff				; | Reset 2 Sparks
			sta SparkAnim,x			; |
			sta SparkLightning,x	; |
			dex						; |
			bpl @InitLoop			; /
		jsr lc77a_cloudboltselect	; Select Cloud that sends the bolt?
	lc726:
		ldx CurrentPhaseNum	; \
		cpx #24				; | There are only 25 (#$18) levels of spark intensity
		bcc @Continue		; | X = Current Phase OR X = 24
		ldx #24				; /
		@Continue:
		lda SparkIntensityData,x	; \ Change Spark Intensity
		sta SparkIntensity			; /
		lda SparkCountdownData,x	; \ Change Spark Countdown
		sta SparkCountdown			; / depending on current phase
		lda #$f0	; \
		sta OAM+$e0	; | Hide last 3 sprites
		sta OAM+$e4	; |
		sta OAM+$e8	; /
		lda #3
		jmp lc856	; Blink selected cloud

	SparkIntensityData:
		.BYTE 0,0,0,0,0
		.BYTE 0,0,0,0,0
		.BYTE 1,1,1,1,1
		.BYTE 1,2,1,1,1
		.BYTE 1,1,1,1,1
	SparkCountdownData:
		.BYTE 15,15,12,12,12
		.BYTE 12,10,10,10,10
		.BYTE 12,12,10,10,10
		.BYTE 08,10,10,08,08
		.BYTE 08,08,08,08,05

	lc77a_cloudboltselect:	; Randomly select a cloud to send bolts?
		lda CloudCount	; \ If there are clouds then select one
		bpl lc781		; / else don't do anything
	lc77e:
		sta $a4		; Select Cloud
		rts
	lc781:
		jsr UpdateRNG
	lc784:
		cmp CloudCount		; \ If RNG value <= amount of Clouds
		bcc lc77e	; | then select cloud based on value
		beq lc77e	; /
		clc				; \ Subtract to the RNG
		sbc CloudCount	; | the amount of clouds
		jmp lc784		; / until the condition is right

	ManageCloudBolt:
		lda FrameCounter	; \
		and #$7f			; | Every 128 frames...
		beq lc797			; /
		:rts
	lc797:
		dec SparkCountdown	; \ Do Lightning Bolt Countdown
		bne :-	; / ...once it reaches zero...
		ldx #0
		lda SparkAnim,x
		bmi lc7ad
		inx
		lda SparkAnim,x
		bmi lc7ad
		lda #1
		sta SparkCountdown
		rts
	lc7ad:
		ldy $a4
		sty $a5
		bpl lc7b4
		rts
	lc7b4:
		lda #$80
		sta SparkXPosFrac,x
		sta SparkYPosFrac,x
		lda #0
		sta SparkAnim,x
		lda $00b2,y
		sta SparkXPosInt,x
		lda $00b5,y
		sta SparkYPosInt,x
		ldy SparkIntensity
		jsr UpdateRNG
		and #$1f
		adc SparkXVelFracData,y
		sta SparkXVelFrac,x
		lda SparkYVelFracData,y
		sta SparkYVelFrac,x
		lda SparkXVelIntData,y
		sta SparkXVelInt,x
		lda SparkYVelIntData,y
		sta SparkYVelInt,x
		jsr UpdateRNG			; \
		and #3					; |	Randomize lightning strike direction?
		sta SparkLightning,x	; /
		tay
		lda lc897,y
		clc
		adc SparkXPosInt,x
		sta SparkXPosInt,x
		lda lc89b,y
		clc
		adc SparkYPosInt,x
		sta SparkYPosInt,x
		lda lc88f,y
		beq lc811
		jsr SparkBounceX
	lc811:
		lda lc893,y
		beq lc819
		jsr SparkBounceY
	lc819:
		lda SparkIntensity
		cmp #5
		bcs lc821
		inc $ba
	lc821:
		lda #6
		sec
		sbc SparkIntensity
		sta $b8
		lda SFX1Req
		ora #$04
		sta SFX1Req
		jmp lc77a_cloudboltselect

	ManageCloudBlink:
		lda $b8		; \ If Lightning Bolt Countdown != 1
		cmp #$01	; | then return
		bne :+		; /
		lda $0530	; \ If Lightning Bolt 0 doesn't exist
		bmi lc846	; / then prepare for one
		lda $0531	; \ If Lightning Bolt 1 doesn't exist
		bmi lc846	; / then prepare for one
		lda #$02	; \ Else up the countdown to 2
		sta $b8		; /
		rts
	lc846:
		lda FrameCounter	; \
		and #$7f			; | If Frame Counter < 64
		cmp #$40			; | then don't do anything
		bcc :+				; | If not equal to 64
		bne lc856			; / then don't play SFX
		lda $f1		; \
		ora #$08	; | Play Sound Effect
		sta $f1		; /
	lc856:
		and #$03
		tax
		lda lc88b,x
		sta $5a
		ldx $a4	; \ Blink the selected cloud
		bmi :+	; /
		lda #$23	; \
		sta $57		; | Set Tile Attribute Palette
		lda $a6,x	; | at PPUADDR[$23xx], Size = 1
		sta $58		; | 
		lda #$01	; |
		sta $59		; /
		jsr lc883	; Set 16x16 Tile Attribute 1
		lda $a9,x
		sta $58
		jsr lc883	; Set 16x16 Tile Attribute 2
		lda $ac,x
		sta $58
		jsr lc883	; Set 16x16 Tile Attribute 3
		lda $af,x
		sta $58
	lc883:			; Set 16x16 Tile Attribute 4
		lda #$57			; \
		ldy #0				; | Copy Temp PPU Block
		jmp CopyPPUBlock	; / [$0057]
		:rts

	lc88b:
		.BYTE $55,$ff,$00,$ff
	lc88f:
		.BYTE $00,$00,$ff,$ff
	lc893:
		.BYTE $ff,$00,$00,$ff
	lc897:
		.BYTE $10,$10,$f0,$f0
	lc89b:
		.BYTE $de,$22,$22,$de

	SparkXVelFracData:
		.BYTE $60,$70,$80,$90,$a0,$b0
	SparkXVelIntData:
		.BYTE $00,$00,$00,$00,$00,$00
	SparkYVelFracData:
		.BYTE $c0,$f0,$20,$50,$80,$b0
	SparkYVelIntData:
		.BYTE $00,$00,$01,$01,$01,$01

	ManageSparks:	; Only for Games A & B. Balloon Trip not included
		ldx #1
		@Loop:
			lda SparkAnim,x
			bpl @Continue
			jmp @ManageNextSpark
			@Continue:
			lda SparkLightning,x
			bmi @LeftScreenBounce
			tay
			phx
			ldx $a5
			lda $b2,x
			adc LightningStrikeXOffsets,y
			sta OAM+$e3	;Lightning Strike Spr 0 X
			sta OAM+$e7	;Lightning Strike Spr 1 X
			sta OAM+$eb	;Lightning Strike Spr 2 X
			lda $b5,x
			adc LightningStrikeYOffsets,y
			sta OAM+$e0	;Lightning Strike Spr 0 Y
			adc LightningStrikeYDirectionOffsets,y
			sta OAM+$e4	;Lightning Strike Spr 1 Y
			adc LightningStrikeYDirectionOffsets,y
			sta OAM+$e8	;Lightning Strike Spr 2 Y
			tya		; \
			and #3	; |	X = Y AND 3
			tax		; /
			tya
			lsrr 2
			tay
			lda FrameCounter
			lsrr 2
			bcs @SkipYInc
			tya		; \
			adc #5	; | Add 5 to Y
			tay		; /
			@SkipYInc:
			lda lca15,y
			sta OAM+$e1
			lda lca1f,y
			sta OAM+$e5
			lda lca29,y
			sta OAM+$e9
			lda lca33,x
			sta OAM+$e2
			sta OAM+$e6
			sta OAM+$ea
			plx
			lda FrameCounter
			and #7
			bne @SkipSettingSparkLightning
			lda SparkLightning,x
			clc
			adc #4
			sta SparkLightning,x
			cmp #$14
			bcc @SkipSettingSparkLightning
			lda #$ff
			sta SparkLightning,x
			@SkipSettingSparkLightning:
			lda SparkLightning,x
			cmp #16
			bcs @LeftScreenBounce
			jmp @ManageNextSpark
			@LeftScreenBounce:
				jsr UpdateSparkPos
				lda SparkXPosInt,x
				cmp #2
				bcs @RightScreenBounce
				jsr SparkBounceXSFX
			@RightScreenBounce:
				lda SparkXPosInt,x
				cmp #$f7
				bcc @TopScreenBounce
				jsr SparkBounceXSFX
			@TopScreenBounce:
				lda SparkYPosInt,x
				cmp #2
				bcs @BottomScreenCheck
				jsr SparkBounceYSFX
			@BottomScreenCheck:
				lda SparkYPosInt,x
				cmp #$e0
				bcc @SparkCollision
				lda #$ff
				sta SparkAnim,x
				lda #$f0
				sta SparkYPosInt,x
				jmp @ManageNextSpark
			@SparkCollision:
				jsr SparkPlatformCollision
				jsr SparkPlayerCollision
			ldy SparkAnim,x
			iny
			tya
			and #7
			sta SparkAnim,x
			ldy SparkAnim,x
			lda lc9dd,y
			sta Temp12
			txa
			aslr 2
			clc
			tay
			lda SparkYPosInt,x
			cmp #$d0
			sta OAM,y
			lda SparkXPosInt,x
			sta OAM+3,y
			lda Temp12
			sta OAM+1,y
			lda #0	; Set priority to 0 (In front of background)
			bcc @SetPriority
			lda #$20	; Set priority to 1 (Behind of background)
			@SetPriority:
			sta OAM+2,y
			@ManageNextSpark:
				dex
				bmi :+
				jmp @Loop
		:rts

	UpdateSparkPos:
		lda SparkXVelFrac,x	; \
		clc					; | Update X Position (Frac)
		adc SparkXPosFrac,x	; |	X = X + XVel
		sta SparkXPosFrac,x	; /
		lda SparkXVelInt,x	; \
		adc SparkXPosInt,x	; | Update X Position (Int)
		sta SparkXPosInt,x	; /
		lda SparkYVelFrac,x	; \
		clc					; | Update Y Position (Frac)
		adc SparkYPosFrac,x	; | Y = Y + YVel
		sta SparkYPosFrac,x	; /
		lda SparkYVelInt,x	; \
		adc SparkYPosInt,x	; | Update Y Position (Int)
		sta SparkYPosInt,x	; /
		rts

	lc9dd:
		.BYTE $9d,$9e,$9f,$9e,$9d,$a0,$a1,$a0
	LightningStrikeXOffsets:
		.BYTE $08,$08,$f0,$f0
		.BYTE $08,$08,$f0,$f0
		.BYTE $08,$08,$f0,$f0
		.BYTE $08,$08,$f0,$f0
	LightningStrikeYOffsets:
		.BYTE $ee,$0a,$0a,$ee
		.BYTE $ee,$0a,$0a,$ee
		.BYTE $ee,$0a,$0a,$ee
		.BYTE $ee,$0a,$0a,$ee
	LightningStrikeYDirectionOffsets:
		.BYTE $f8,$08,$08,$f8
		.BYTE $f8,$08,$08,$f8
		.BYTE $f8,$08,$08,$f8
		.BYTE $f8,$08,$08,$f8
	lca15:
		.BYTE $91,$93,$97,$97,$fc
		.BYTE $92,$95,$9a,$9a,$fc
	lca1f:
		.BYTE $fc,$94,$98,$98,$fc
		.BYTE $fc,$96,$9b,$9b,$fc
	lca29:
		.BYTE $fc,$fc,$99,$99,$fc
		.BYTE $fc,$fc,$9c,$9c,$fc
	lca33:
		.BYTE $c0,$40,$00,$80

	SparkBounceXSFX:
		lda $f3		; \
		ora #$80	; | Play SFX
		sta $f3		; /
	SparkBounceX:
		lda #0				; \
		sec					; |
		sbc SparkXVelFrac,x	; | Lightning Bolt
		sta SparkXVelFrac,x	; | Reverse X Velocity
		lda #0				; |
		sbc SparkXVelInt,x	; |
		sta SparkXVelInt,x	; /
		rts

	SparkBounceYSFX:
		lda $f3		; \
		ora #$80	; | Play SFX
		sta $f3		; /
	SparkBounceY:
		lda #0				; \
		sec					; |
		sbc SparkYVelFrac,x	; | Lightning Bolt
		sta SparkYVelFrac,x	; | Reverse Y Velocity
		lda #0				; |
		sbc SparkYVelInt,x	; |
		sta SparkYVelInt,x	; /
		rts

	SparkPlatformCollision:
		ldy PlatformCount
		@Loop:
			lda #0				; \
			sta CollisionFlags	; / Reset Collision Flags
			lda (TopPointer),y	; Get upper bound of platform Y
			sec		; \
			sbc #8	; / Offset upward by 8px for sprite size
			cmp SparkYPosInt,x	; Compare Platform(y).TopY against Spark(x).Y
			bcs @NoCollision	; Platform(y).TopY-8 >= Spark(x).Y (Spark too far above)
			adc #3
			cmp SparkYPosInt,x
			bcc @CheckBottom	; Platform(y).TopY-5 < Spark(x).Y
			lda #1					; If Spark X's Y Pos is 5-7 px above top of Platform Y
			bne @SetVerticalFlag	; Set bit 1 of the Collision Flags
			@CheckBottom:
				lda (BottomPointer),y	; Lower bound of Platform Y
				cmp SparkYPosInt,x	; Compare Platform(y).BottomY against Spark(x).Y
				bcc @NoCollision	; Platform(y).BottomY < Spark(x).Y (Spark too far below)
				sbc #3
				cmp SparkYPosInt,x
				bcs @CheckLeft	; Platform(y).BottomY-3 >= Spark(x).Y
				lda #2		; If Spark X's Y Pos is 0-2px above bottom of Platform Y, set bit 2 of Collision Flags
			@SetVerticalFlag:
				sta CollisionFlags
			lda (LeftPointer),y	; Get left bound of platform Y
			cmp #16
			beq @LeftEdgeValid	; If the left edge is 16 (Up against edge of screen)
			sec
			sbc #4
			cmp SparkXPosInt,x
			bcs @ResetFlags	; Platform(y).LeftX-4 >= Spark(x).X
			@LeftEdgeValid:
			lda (RightPointer),y	; Get right bound of platform Y
			cmp SparkXPosInt,x
			bcs @CheckLeft	; Platform(y).RightX >= Spark(x).X
			@ResetFlags:
				lda #0				; \
				sta CollisionFlags	; / Reset Collision Flags
			@CheckLeft:
			lda (LeftPointer),y	; Get left bound of platform Y
			cmp #16
			beq @LeftEdgeValid2	; If the left edge is 16 (Up against edge of screen)
			sec
			sbc #8
			cmp SparkXPosInt,x
			bcs @NoCollision
			adc #3
			cmp SparkXPosInt,x
			bcc @LeftEdgeValid2
			lda CollisionFlags
			ora #4
			bne @SetHorizontalFlag
			@LeftEdgeValid2:
			lda (RightPointer),y	; Get right bound of platform Y
			cmp #$FF
			beq @NoCollision
			cmp SparkXPosInt,x
			bcc @NoCollision
			sbc #3
			bcs @NoCollision
			lda CollisionFlags
			ora #8
			@SetHorizontalFlag:
			sta CollisionFlags
			@NoCollision:
			lda CollisionFlags
			bne @CheckFlags
			@CheckNext:
			dey
			bmi :+
			jmp @Loop
		:rts
		@CheckFlags:
			lsr CollisionFlags
			bcc @SkipTopBounce
			lda SparkYVelInt,x
			bmi @SkipTopBounce
			jsr SparkBounceYSFX
		@SkipTopBounce:
			lsr CollisionFlags
			bcc @SkipBottomBounce
			lda SparkYVelInt,x
			bpl @SkipBottomBounce
			jsr SparkBounceYSFX
		@SkipBottomBounce:
			lsr CollisionFlags
			bcc @SkipLeftBounce
			lda SparkXVelInt,x
			bmi @SkipLeftBounce
			jsr SparkBounceXSFX
		@SkipLeftBounce:
			lsr CollisionFlags
			bcc @SkipRightBounce
			lda SparkXVelInt,x
			bpl @SkipRightBounce
			jsr SparkBounceXSFX
		@SkipRightBounce:
		jmp @CheckNext
		rts

	SparkPlayerCollision:		; Lightning Bolt Player Collision
		ldy #1
		@Loop:
			lda ObjectBalloons,y	; \
			bmi @Skip				; | If Player Y has balloons...
			beq @Skip				; /
			lda PlayerInvincible,y	; \ and if Player Y is not invincible...
			bne @Skip				; /
			lda SparkXPosInt,x		; \
			sec						; | If Player Y's X position
			sbc ObjectXPosInt,y		; | is within the X position
			jsr GetAbsoluteValue	; | of Lightning Bolt X
			cmp #8					; | (size 8 pixels)
			bcs @Skip				; /
			lda SparkYPosInt,x		; \
			sec						; |
			sbc ObjectYPosInt,y		; | If Player Y's Y position
			sec						; | is within the Y position
			sbc #8					; | of Lightning Bolt X
			jsr GetAbsoluteValue	; | (size 12 pixels high
			cmp #12					; | to take balloons into account)
			bcs @Skip				; /
			lda #0
			sta ObjectBalloons,y	; Player Y's balloons = 00
			lda #1
			sta ObjectStatus,y	; Player Y's status = 01 
			sta PlayerFreeze,y	; Player Y's freeze flag = 01
			lda #11
			sta ObjectType,y	; Player Y's type = 0B
			lda #32
			sta ObjectCountdown,y	; Player Y's ? = 32
			lda SFX1Req	; \
			ora #$80	; | Play SFX
			sta SFX1Req	; /
			lda #$f0			; \
			sta SparkYPosInt,x	; | Lightning Bolt X
			lda #$ff			; | disappears
			sta SparkAnim,x		; /
			@Skip:
			dey			; \ Check next player
			bpl @Loop	; /
		rts

;----------------------
; Propeller code
;----------------------

PropellerManage:
	ldx PropellerCount
	bmi :+
	@Loop:
		jsr lcba8
		lda $0604,x
		beq @Next
		txa
		eor FrameCounter
		and #1
		bne @Next
		ldy PropellerType,x
		iny
		tya
		and #3
		sta PropellerType,x
		jsr lcccb
		lda PropellerType,x
		cmp #1
		bne @Next
		dec $060e,x
		bne @Next
		dec $0604,x
	@Next:
		dex
		bpl @Loop
	:rts

lcba8:
	ldy #7
	lda $0604,x
	bne lcbb2
	jmp lcc3a
lcbb2:
	lda ObjectBalloons,y
	bmi lcc2f
	beq lcc2f
	cpy #2
	bcc lcbc1
	cmp #1
	beq lcc2f
lcbc1:
	lda ObjectXPosInt,y
	cadc #8
	sec
	sbc PropellerXPos,x
	sta Temp12
	jsr GetAbsoluteValue
	cmp #18
	bcs lcc2f
	lda ObjectYPosInt,y
	cadc #12
	sec
	sbc PropellerYPos,x
	sta Temp13
	jsr GetAbsoluteValue
	cmp #18
	bcs lcc2f
	lda Temp12
	bmi lcbfc
	cmp #3
	bcc lcc0b
	lda #2
	sta ObjectYVelInt,y
	jsr PlayBumpSFX
	jsr ObjectYApplyYVelocity
	bne lcc0b
lcbfc:
	cmp #$fd
	bcs lcc0b
	lda #$fe
	sta ObjectYVelInt,y
	jsr ObjectYApplyYVelocity
	jsr PlayBumpSFX
lcc0b:
	lda Temp13
	bmi lcc20
	cmp #3
	bcc lcc2f
	lda #2
	sta ObjectXVelInt,y
	jsr ObjectYApplyXVelocity
	jsr PlayBumpSFX
	bne lcc2f
lcc20:
	cmp #$fd
	bcs lcc2f
	lda #$fe
	sta ObjectXVelInt,y
	jsr ObjectYApplyXVelocity
	jsr PlayBumpSFX
lcc2f:
	dey
	bpl lcbb2
	rts

PlayBumpSFX:
	lda SFX2Req
	ora #2
	sta SFX2Req
	rts

lcc3a:
	lda ObjectBalloons,y
	bmi lccb8
	beq lccb8
	cpy #2
	bcc lcc73
	lda PropellerType,x
	cmp #3
	bne lcc73
	lda PropellerXPos,x
	sec
	sbc #10
	cmp ObjectXPosInt,y
	bcs lcc73
	adc #4
	cmp ObjectXPosInt,y
	bcc lcc73
	lda PropellerYPos,x
	sec
	sbc #$1c
	cmp ObjectYPosInt,y
	bcs lcc73
	adc #4
	cmp ObjectYPosInt,y
	bcc lcc73
	jsr lccbf
	lcc73:
	lda ObjectXPosInt,y
	cadc #8
	sec
	sbc $05d2,x
	jsr GetAbsoluteValue
	sta Temp12
	lda ObjectYPosInt,y
	cadc #12
	sec
	sbc $05dc,x
	jsr GetAbsoluteValue
	sta Temp13
	lda $05fa,x
	cmp #3
	beq lcca2
	lda Temp12
	pha
	lda Temp13
	sta Temp12
	pla
	sta Temp13
	lcca2:
	lda Temp12
	cmp #20
	bcs lccb8
	lda Temp13
	cmp #$0b
	bcs lccb8
	lda #1
	sta $0604,x
	lda #$32
	sta $060e,x
	lccb8:
	dey
	bmi :+
	jmp lcc3a
	:rts

lccbf:
	phx
	tya
	tax
	inc $cb
	jsr le983
	plx
	rts

lcccb:
	lda $05f0,x
	sta $57
	lda $05e6,x
	sta $58
	lda #3
	sta $59
	ldy $05fa,x
	lda PropellerTileUL,y
	sta $5a
	lda PropellerTileUM,y
	sta $5b
	lda PropellerTileUR,y
	sta $5c
	jsr lcd0f
	lda PropellerTileML,y
	sta $5a
	lda PropellerTileMM,y
	sta $5b
	lda PropellerTileMR,y
	sta $5c
	jsr lcd0f
	lda PropellerTileLL,y
	sta $5a
	lda PropellerTileLM,y
	sta $5b
	lda PropellerTileLR,y
	sta $5c
lcd0f:
	phy
	lda #$57
	ldy #0
	jsr CopyPPUBlock
	ply
	lda $58
	cadc #$20
	sta $58
	bcc :+
	inc $57
	:rts

PropellerTileUL:
	.BYTE $a1,$24,$24,$24
PropellerTileUM:
	.BYTE $a2,$9e,$ab,$24
PropellerTileUR:
	.BYTE $24,$24,$ac,$24
PropellerTileML:
	.BYTE $a3,$24,$ad,$a8
PropellerTileMM:
	.BYTE $a4,$9f,$ae,$a9
PropellerTileMR:
	.BYTE $a5,$24,$af,$aa
PropellerTileLL:
	.BYTE $24,$24,$b0,$24
PropellerTileLM:
	.BYTE $a6,$a0,$b1,$24
PropellerTileLR:
	.BYTE $a7,$24,$24,$24


;----------------------
; Balloon code
;----------------------

InitBalloons:
	ldx #9				; \ Reset all 10 balloons
	@ClearLoop:
		lda #$ff		; | GFX = #$FF
		sta $055d,x		; |
		lda #$f0		; | Y Positions = #$F0
		sta $057b,x		; |
		dex				; |
		bpl @ClearLoop	; /
	rts

lcd5a:
	dec $05cc
	beq lcd60
	rts
lcd60:
	lda RNGOutput
	and #$3f
	adc #$28
	sta $05cc
	ldx #9
lcd6b:
	lda $055d,x
	bmi lcd74
	dex
	bpl lcd6b
	rts
lcd74:
	lda #0
	sta $055d,x
	sta $0599,x
	sta $058f,x
	lda #$80
	sta $0571,x
	sta $0585,x
	lda #$d0
	sta $057b,x
	jsr UpdateRNG
	and #3
	tay
	lda lceae,y
	sta $0567,x
	ldy #0
	lda RNGOutput
	sta $05b7,x
	bpl lcda2
	dey
lcda2:
	tya
	sta $05c1,x
	dec $05cb
	rts

lcdaa:
	ldx #9
lcdac:
	lda $055d,x
	bmi lce22
	beq lcdfc
	lda $0599,x
	sta Temp12
	lda $058f,x
	sta Temp13
	jsr lf1a6
	lda $05b7,x
	cadc Temp12
	sta $05b7,x
	sta Temp12
	lda $05c1,x
	adc Temp13
	sta $05c1,x
	sta Temp13
	jsr lf1a6
	lda $0599,x
	sec
	sbc Temp12
	sta $0599,x
	lda $058f,x
	sbc Temp13
	sta $058f,x
	lda $0571,x
	cadcx $0599
	sta $0571,x
	lda $0567,x
	adc $058f,x
	sta $0567,x
lcdfc:
	lda $0585,x
	sec
	sbc BalloonRiseSpeed
	sta $0585,x
	bcs lce0b
	dec $057b,x
lce0b:
	lda $057b,x
	cmp #$f0
	beq lce1d
	cmp #$a8
	bcs lce22
	lda #1
	sta $055d,x
	bne lce22
lce1d:
	lda #$ff
	sta $055d,x
lce22:
	jsr lce2f_balloonxspritemanage
	jsr lcece_ballooncollision
	dex
	bmi :+
	jmp lcdac
	:rts

lce2f_balloonxspritemanage:
	ldy $055d,x
	iny
	lda lceb2,y
	sta Temp13
	txa
	sta Temp12
	asl
	adc Temp12
	aslr 2
	tay
	lda $057b,x
	sta $0250,y
	sta $0254,y
	cadc #8
	sta $0258,y
	lda $0567,x
	sta $0253,y
	cadc #4
	sta $025b,y
	cadc #4
	sta $0257,y
	lda Temp13
	sta $0252,y
	sta $0256,y
	sta $025a,y
	lda $055d,x
	bmi lce99
	lda #$a8
	sta $0251,y
	lda #$a9
	sta $0255,y
	lda FrameCounter
	lsrr 4
	and #7
	stx Temp13
	tax
	lda lceb2+3,x
	sta $0259,y
	lda $025a,y
	eor lcebd,x
	sta $025a,y
	ldx Temp13
	rts

lce99:
	lda #$f0
	sta $057b,x
	lda #$ac
	sta $0251,y
	lda #$ad
	sta $0255,y
	lda #$fc
	sta $0259,y
	rts

lceae:
.BYTE $20,$50,$a0,$d0
lceb2:
    ;11 bytes
.BYTE $02,$22,$02,$aa,$ab,$ab,$aa,$aa,$ab,$ab,$aa
lcebd:
    ;17 bytes
.BYTE $00,$00,$40,$40,$40,$40,$00,$00,$fc,$fc,$df,$fc,$fc,$e0,$e2
.BYTE $e1,$fc

lcece_ballooncollision:
	ldy #$01
lced0:
	lda $0088,y
	bmi lcf0f
	beq lcf0f
	lda $055d,x
	bmi :+
	lda $009a,y
	cmp #$c0
	bcs lcf0f
	sec
	sbc $057b,x
	jsr GetAbsoluteValue
	cmp #$18
	bcs lcf0f
	lda $0091,y
	sec
	sbc $0567,x
	jsr GetAbsoluteValue
	cmp #$10
	bcs lcf0f
	lda #$ff
	sta $055d,x
	lda P1BonusBalloons,y
	cadc #$01
	sta P1BonusBalloons,y
	lda #$02
	sta SFX1Req
	rts
lcf0f:
	dey
	bpl lced0
	:rts


;----------------------
; Bonus Phase code
;----------------------

lcf13:
	lda #$20
	sta MusicReq
	jsr SetBonusPhase
	jsr InitBalloons
	ldx TwoPlayerFlag
lcf1f:
	lda P1Lives,x
	bmi lcf26
	jsr InitPlayerType
lcf26:
	dex
	bpl lcf1f
	ldx #0
	stx $bd
	stx $be
	lda #$14
	sta $05cb
lcf34:
	jsr Pause
	inc StarUpdateFlag
	jsr ManageScorePopup
	jsr ObjectManage
	lda $05cb
	beq lcf47
	jsr lcd5a
lcf47:
	jsr lcdaa
	lda $05cb
	bne lcf34
	ldx #9
lcf51:
	lda $055d,x
	bpl lcf34
	dex
	bpl lcf51
	lda FrameCounter
	bne lcf34
	jsr ClearPPU
	ldx #2
	stx $46
	jsr WaitYFrames
	lday BonusEndScreenPalette
	jsr CopyPPUBlock
	lday ld15a
	jsr CopyPPUBlock
	lday ld165
	jsr CopyPPUBlock
	ldx TwoPlayerFlag
lcf7e:
	lda #$20
	sta ObjectXPosInt,x
	lda ld19e,x
	sta ObjectYPosInt,x
	lda #3
	sta ObjectStatus,x
	lda #1
	sta ObjectDirection,x
	jsr InitPlayerType
	jsr le3a4
	dex
	bpl lcf7e
	lda #$44
	sta $0567
	sta $0568
	lda #$54
	sta $057b
	lda #$74
	sta $057c
	lda #1
	sta $055d
	sta $055e
	ldx TwoPlayerFlag
lcfb5:
	jsr lce2f_balloonxspritemanage
	dex
	bpl lcfb5
	jsr Wait20Frames
	lda #$2b
	sta $57
	lda #$24
	sta $58
	sta $59
	lda #$0c
	sta $54
	lda #$0b
	sta $55
	lda #5
	sta $56
	lda P1BonusBalloons
	jsr ld1c9
	lda TwoPlayerFlag
	beq lcfe8
	lda #$0f
	sta $55
	lda P2BonusBalloons
	jsr ld1c9
lcfe8:
	jsr Wait20Frames
	lda $0559
	sta $57
	lda #0
	sta $58
	sta $59
	lda #8
	sta $54
	lda #$0b
	sta $55
	lda #3
	sta $56
	lda $0559
	jsr UploadPPU
	lda TwoPlayerFlag
	beq ld013
	lda #$0f
	sta $55
	jsr UploadPPU
ld013:
	lda #$ff
	sta $055d
	sta $055e
	ldx TwoPlayerFlag
ld01d:
	jsr lce2f_balloonxspritemanage
	dex
	bpl ld01d
	lda #2
	sta SFX1Req
	ldx #2
	jsr WaitYFrames
	ldx TwoPlayerFlag
ld02e:
	jsr lce2f_balloonxspritemanage
	dex
	bpl ld02e
	jsr ld1a0
	jsr Wait20Frames
	lda #1
	sta SFX1Req
	jsr CheckTotalBonusBalloons
	bne ld068
	lday ld170
	jsr CopyPPUBlock
	jsr FinishFrame
	ldx #$1a
ld04f:
	lda ld184,x
	sta $57,x
	dex
	bpl ld04f
	lda SuperBonusPtsUpper
	sta $68
	lda SuperBonusPtsLower
	sta $69
	jsr CopyPPUTempBlock
	lda #$10
	sta MusicReq
ld068:
	ldx #$78
	jsr WaitYFrames
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
	lda #1
	sta $f1
	ldx #2
	jsr WaitYFrames
	lda $5d
	cmp #$24
	bne ld070
	lda TwoPlayerFlag
	beq ld0a8
	lda a:$006b	;Absolute addressing on ZP location?
	cmp #$24
	bne ld070
ld0a8:
	ldx #$0a
	jsr WaitYFrames
	jsr CheckTotalBonusBalloons
	bne ld0ce
	lda SuperBonusPtsUpper
	sta ScoreDigit4
	lda SuperBonusPtsLower
	sta ScoreDigit5
	lda TwoPlayerFlag
	sta TargetUpdateScore
ld0c0:
	jsr UpdateScore
	dec TargetUpdateScore
	bpl ld0c0
	lda #1
	sta $f1
	jsr Wait20Frames
ld0ce:
	lda #0
	sta ScoreDigit4
	sta ScoreDigit5
	ldx #1
ld0d6:
	lda P1Lives,x
	bpl ld0dc
	sta $88,x
ld0dc:
	dex
	bpl ld0d6
	jmp lf353

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

ld13e:
.BYTE $21,$73,$0b,$29,$00,$00,$00,$00,$00,$24,$19,$1d,$1c,$26
ld14c:
.BYTE $21,$f3,$0b,$29,$00,$00,$00,$00,$00,$24,$19,$1d,$1c,$26
ld15a:
.BYTE $23,$e8,$08,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
ld165:
.BYTE $23,$c0,$08,$40,$50,$50,$50,$50,$90,$a0,$a0
ld170:
.BYTE $22,$88,$11,$19,$24,$0e,$24,$1b,$24,$0f,$24,$0e,$24,$0c,$24,$1d,$24,$2c,$2c,$2c
ld184:
.BYTE $22,$c6,$17,$1c,$1e,$19,$0e,$1b,$24,$0b,$18,$17,$1e,$1c,$24
.BYTE $24,$24,$01,$00,$00,$00,$00,$19,$1d,$1c,$2c
ld19e:
.BYTE $50,$70

ld1a0:
	ldx #28
ld1a2:
	lda ld13e,x
	sta $57,x
	dex
	bpl ld1a2
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
	dey
	bmi ld1fe
	lda $0559
	cadcx $59
	cmp #10
	bcc ld1ed
	sbc #10
	inc $58,x
ld1ed:
	sta $59,x
	lda $58,x
	cmp #10
	bcc ld1fb
	sbc #10
	inc $57,x
	sta $58,x
ld1fb:
	jmp ld1dc
ld1fe:
	ldy #0
ld200:
	lda $57,x
	beq ld208
	cmp #36
	bne :+
ld208:
	lda #36
	sta $57,x
	inx
	iny
	cpy #4
	bne ld200
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
	lda $57,x
	cmp #$24
	beq ld243
	lda #10
	sta $58,x
	dec $57,x
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
	jmp ld1fe

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

InitGameMode:
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
				clc						; |
				adc #4					; |
				dec Temp12				; |
				bne @Loop2				; |
			inc PPUBlockAddrHi			; |
			dey							; |
			bpl @Loop1					; /
		lda PPUBlockAddrHi
		sec
		sbc #4
		sta PPUBlockAddrHi
		jsr ld51c
		sta $a6,x
		incr 2, PPUBlockAddrLo
		jsr ld51c
		sta $a9,x
		incr 2, PPUBlockAddrHi
		jsr ld51c
		sta $af,x
		decr 2, PPUBlockAddrLo
		jsr ld51c
		sta $ac,x
		stx $a4
		lda #3
		jsr lc856
		jsr UploadPPUAndMaskBuffer
		ldx $a4
		lda PPUBlockAddrLo
		aslr 3
		clc
		adc #16
		sta $b2,x
		lda PPUBlockAddrHi
		aslr 3
		sta $b5,x
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
		sta PropellerType,x			; /
		lda PPUBlockAddrLo
		aslr 3
		adc #12
		sta PropellerXPos,x
		lda PPUBlockAddrHi
		aslr 3
		adc #12
		sta PropellerYPos,x
		lda #0
		sta $0604,x
		jsr ld4fb_setppuaddr_render
		sta $05e6,x
		lda Temp13
		sta $05f0,x
		jsr ld56c
		jsr ld53c
		incr 2, PPUBlockAddrLo
		jsr ld53c
		incr 2, PPUBlockAddrHi
		jsr ld53c
		decr 2, PPUBlockAddrLo
		jsr ld53c
		inx
		jmp @PropellerLoop	; Load another propeller data
	@FinishPropellers:
	dex			; \ Write amount of propellers to RAM
	stx $05d1	; /
	jsr GetByteFromLoadPointer	; \
	sta DataPointerLo					; | Load Enemy Data Pointer
	jsr GetByteFromLoadPointer	; |
	sta DataPointerHi					; /
	ldy #0				; \
	lda (DataPointer),y	; | Load Enemy Amount
	tax					; |
	dex					; |
	bpl LoadEnemy		; /
	inc PhaseType		; If No Enemies then it's a Bonus Phase Type
	jmp LoadCollision	; Skip Enemy Loading
LoadEnemy:
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
LoadCollision:
	jsr GetByteFromLoadPointer	; \ Load Amount of Platforms
	sta PlatformCount			; /
	jsr GetByteFromLoadPointer	; \
	sta LeftPointerLo			; | Load Platform Collision Pointer
	jsr GetByteFromLoadPointer	; | Left Side
	tay							; |
	sta LeftPointerHi			; /
	lda LeftPointerLo			; \
	jsr NextPlatformPointer	; | Load Right Side Platform Collision Pointer
	sta RightPointerLo			; |
	sty RightPointerHi			; /
	jsr NextPlatformPointer	; \
	sta TopPointerLo			; | Load Top Side Platform Collision Pointer
	sty TopPointerHi			; /
	jsr NextPlatformPointer	; \
	sta BottomPointerLo			; | Load Bottom Side Platform Collision Pointer
	sty BottomPointerHi			; /
ld3e1:
	jsr ld5d9
	jsr LoadPalette
	jsr EnableNMI
	jmp UploadPPUAndMask

LoadPalette:
	ldx #34					; \
	@PaletteLoop:
		lda BasePalette,x	; | Copy Base Palette
		sta $57,x			; | to PPU Temp Block
		dex					; |
		bpl @PaletteLoop	; /
	lda PhaseType			; \ Check Phase Type...
	bne @LoadBonusPalette	; /
	lda CurrentPhaseHeader	; \ ...If Normal Phase
	and #%1100				; | Select Palette based
	ora #%0011				; | on current level header
	tay						; /
	ldx #3					; \
	@GrassLoop:
		lda GrassPalettes,y	; | Copy Single Palette Data
		sta $5a,x			; | to Background Palette 1
		dey					; | to PPU Temp Block
		dex					; |
		bpl @GrassLoop		; /
	@Next:
	jmp CopyPPUTempBlock
	@LoadBonusPalette:
		ldx BonusPhaseIntensity	; ...If Bonus Phase
		lda BalloonPalPointerLower,x	; \
		sta LoadPointerLo				; | Select Balloon Palette
		lda BalloonPalPointerUpper,x	; | based on Intensity Level
		sta LoadPointerHi				; /
		ldx #3					; \
		ldy #7					; |
		@CopyLoop1:
			lda (LoadPointer),y	; | Copy Second Palette Data
			sta $72,x			; | to Sprite Palette 2
			dey					; | to PPU Temp Block
			dex					; |
			bpl @CopyLoop1		; /
		lda GameMode	; \ If Balloon Trip mode
		bne @Next		; / then stop and copy PPU Temp Block as is
		@CopyLoop2:
			lda (LoadPointer),y	; \ Copy First Palette Data
			sta $005a,y			; | to Background Palette 1
			dey					; | to PPU Temp Block
			bpl @CopyLoop2		; /
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
	ldy #0
	lda (LoadPointer),y
	inc LoadPointerLo
	bne :+
	inc LoadPointerHi
	:rts

GetByteFromDataPointer:
	ldy #0
	lda (DataPointer),y
	inc DataPointerLo
	bne :+
	inc DataPointerHi
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

ld51c:
	lda PPUBlockAddrHi
	and #$fc
	asl
	sta Temp12
	lda $54
	lsrr 2
	ora Temp12
	ora #$c0
	pha
	lda $55
	and #2
	sta Temp12
	lda $54
	and #2
	lsr
	ora Temp12
	tay
	pla
	rts

ld53c:
	lda #$23
	sta PPUADDR
	jsr ld51c
	sta PPUADDR
	lda PPUDATA
	lda PPUDATA
	and ld564,y
	ora ld568,y
	pha
	lda #$23
	sta PPUADDR
	jsr ld51c
	sta PPUADDR
	pla
	sta PPUDATA
	rts

ld564:	;This is attribute related
.BYTE $fc,$f3,$cf,$3f
ld568:
.BYTE $01,$04,$10,$40
ld56c:
	jsr lcccb
	jmp UploadPPUAndMaskBuffer

InitBalloonTrip:	; Initialize Balloon Trip Game Mode
	lday $23C0
	jsr ld593
	lday $27C0
	jsr ld593
	ldya $2360
	jsr ld5b8
	ldya $2760
	jsr ld5b8
	inc PhaseType
	jmp ld3e1
ld593:
	styappuaddr
	ldx #0
ld59b:
	lda BGAttributes,x
	sta PPUDATA
	inx
	cpx #8
	bne ld59b
	lda #0
	ldx #$28
	jsr ld5b1
	lda #$aa
	ldx #$10
ld5b1:
	sta PPUDATA
	dex
	bne ld5b1
	rts

ld5b8:
	styappuaddr
	ldx #$20
	lda #$58
	jsr ld5c9
	ldx #$40
	lda #$5c
ld5c9:
	sta Temp12
ld5cb:
	txa
	and #3
	eor #3
	ora Temp12
	sta PPUDATA
	dex
	bne ld5cb
	rts

ld5d9:
	ldx #0
ld5db:
	jsr ld651
	jsr ld5f1
	lda PPUAddressLo
	ora #$04
	sta PPUAddressLo
	jsr ld5f1
	inxr 2
	cpx #$80
	bne ld5db
	rts

ld5f1:
	lda PPUAddressLo
	sta PPUADDR
	lda PPUAddressHi
	sta PPUADDR
	lda PPUDATA
	lda PPUDATA
	cmp #$24
	bne :+
	txa
	and #3
	tay
	jmp ld63b
	:rts

UpdateStarBG:
	lda StarUpdateFlag	; \ If [$4C] == 0
	beq :+				; / Then Do Nothing
	dec StarUpdateFlag
	lda $4f		; \
	clc			; |
	adc #2		; | Update and Get Current
	and #$3f	; | Star ID
	sta $4f		; |
	tax			; /
	jsr ld651			; \
	lda PPUAddressLo	; |
	sta PPUADDR			; | Set PPU Address for Star Tile
	lda PPUAddressHi	; |
	sta PPUADDR			; /
	lda PPUDATA				; \
	lda PPUDATA				; |
	ldy #3					; | Check if Tile is part of
	@Loop:
		cmp StarAnimTiles,y	; | Star Animation tiles
		beq ld63b			; | If not: Stop
		dey					; |
		bpl @Loop			; /
	:rts

ld63b:
	lda PPUAddressLo		; \
	sta PPUADDR				; |
	lda PPUAddressHi		; | Write Next Star Tile
	sta PPUADDR				; |
	lda StarAnimTiles+1,y	; |
	sta PPUDATA				; /
	rts

StarAnimTiles:	;Star Tile Animation Frames
	.BYTE $24,$ed,$ee,$ef,$24	;Empty, Low, middle, high, empty

ld651:
	lda StarPositions,x
	sta PPUAddressHi
	lda StarPositions+1,x
	sta PPUAddressLo
	rts

StarPositions:	;PPU Addresses of each BG star
	.WORD $2163,$21A5,$20CB,$20B7,$217D,$229B,$20F2,$2249
	.WORD $216D,$220B,$2292,$2195,$211C,$2148,$20E0,$230B
	.WORD $20CE,$21D0,$2106,$2119,$2230,$228A,$2288,$20A4
	.WORD $2242,$2168,$223C,$2136,$21CA,$20BC,$2196,$214C
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
	beq @Continue	; | then go to @Continue
	:rts				; / Else return
	@Continue:
	ldx TargetUpdateScore	; \ If [TargetUpdateScore] >= 2
	cpx #2					; | Then return
	bcs :-					; /
	lda P1Lives,x	; \ If Player X has no lives
	bmi :-			; / Then return

	ldy #100			; \ Process Score to add up
	jsr DivideByY		; | Score to add / 100
	clc					; |
	adc ScoreDigit5		; |
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
	jsr ld78f		; | First Digit
	sta P1Score0,x	; /

	lda P1Score1,x	; \ Add Score 000X0
	adc ScoreDigit2	; | Lock Score between 0 and 9
	jsr ld78f		; | Second Digit
	sta P1Score1,x	; /
	
	lda P1Score2,x	; \ Add Score 00X00
	adc ScoreDigit3	; | Lock Score between 0 and 9
	jsr ld78f		; | Third Digit
	sta P1Score2,x	; /
	
	lda P1Score3,x	; \ Add Score 0X000
	adc ScoreDigit4	; | Lock Score between 0 and 9
	jsr ld78f		; | Fourth Digit
	sta P1Score3,x	; /
	
	lda P1Score4,x	; \ Add Score X0000
	adc #0			; | Lock Score between 0 and 9
	jsr ld78f		; | Fifth Digit
	sta P1Score4,x	; /

	inxr 4
	ldy #4					; \ From highest digit
ld746:
	lda P1Score,x			; | If this score digit is
	cmp (ScorePointer),y	; | under Highest Top Score Digit
	bcc ld765				; | then Top Score was not beaten
	bne ld752				; | so go to ld765 (stop checking)
	dex						; | if not equal then Top score is beaten
	dey						; | if equal then check the lower digit
	bpl ld746				; / until the last.
ld752:
	ldy #0
	lda TargetUpdateScore	; \
	aslr 2					; | X = [TargetUpdateScore] * 5
	ora TargetUpdateScore	; |
	tax						; /
	@CopyLoop:
		lda P1Score,x			; \
		sta (ScorePointer),y	; | Copy Current Score
		inx						; | to Highest Top Score
		iny						; |
		cpy #5					; |
		bne @CopyLoop			; /
ld765:
	ldy #4						; \
	@CopyLoop:
		lda (ScorePointer),y	; | Copy Highest Top Score
		sta TopScore,y			; | back to Current Top Score
		dey						; | 
		bpl @CopyLoop			; /
	inc $46		; Status Bar Update Flag
	lda GameMode			; \
	beq :+					; | If Balloon Trip Mode then
	jsr UpdateBTRank	; / Ranking Update
	:rts

TopScoreAddrLo:	;Lower byte for each of the static top score memory locations
	.BYTE <GameATopScore,<GameBTopScore,<GameCTopScore

DivideByY:	; Divide [$43] by Y
	sty Temp12
	ldx #$ff
	lda $43
ld782:
	sec			; \
	sbc Temp12	; | Subtract Y 
	inx			; | X + 1
	bcs ld782	; / If it doesn't overflow then continue
	cadc Temp12	; Add Y value again to cancel overflow
	sta $43		; [$43] = Reminder
	txa			; A and X = Result
	rts

ld78f:
	cmp #10		; \ Check if Score Digit >= 10
	bcs ld794	; | Then ...
	rts			; / Else return
	ld794:
		sec			; \ Then subtract 10
		sbc #10		; / from digit
		rts

UpdateStatusBar:
	ldy StatusUpdateFlag	; \ Check status bar update flag
	dey						; | (Do operation to update registers)
	beq DrawStatusBar		; | If flag was 1, redraw status bar
	bpl ld805				; | If flag was >1, 
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

ld805:
	dec StatusUpdateFlag
	stppuaddr $2062	; PPUADDR = $2062 GAME OVER Player 1 Status Bar
	lda P1Lives	; \ If Player 1 Lives is negative
	jsr ld826	; / Then upload GAME OVER
	lda TwoPlayerFlag	; \ If Single Player
	beq :+				; / then return
	stppuaddr $2075
	lda P2Lives			; \ If Player 2 Lives is negative
ld826:
	bmi DrawGameOverB	; / Then upload GAME OVER
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

ld871:
	sta Temp12
	stx Temp13
	sty Temp14
	ldx #$01
ld879:
	lda $061a,x
	bmi ld88c
	dex
	bpl ld879
	ldx #1
	lda $0619
	cmp $0618
	bcc ld88c
	dex
ld88c:
	lda #$64
	sta $0618,x
	lda Temp12
	sta $061a,x
	tay
	txa
	aslr 3
	tax
	lda ScorePopupLeft,y
	sta $02f1,x
	lda ScorePopupRight,y
	sta $02f5,x
	ldy Temp13
	lda $009a,y
	sec
	sbc #8
	sta $02f0,x
	sta $02f4,x
	lda $0091,y
	sta $02f3,x
	cadc #8
	sta $02f7,x
	lda TargetUpdateScore
	sta $02f2,x
	sta $02f6,x
	ldy Temp14
	ldx Temp13
	lda Temp12
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

ld8ff:
	ldx #1
ld901:
	lda #0
	sta $0618,x
	lda #$ff
	sta $061a,x
	dex
	bpl ld901
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

.include "TitleScreenData.asm"

GotoTitleScreen:	; Manage Title Screen
	jsr EnableNMI
	jsr DisplayTitleScreen
	lda #0				; \ Reset Frame Counter
	sta FrameCounter	; /
TitleScreenLoop:
	jsr FinishFrame
	lda FrameCounter	; \ Start demo if Frame Counter overflows
	beq StartDemo		; / 
	jsr ldb08	; Set Modes & Cursor
	jsr PollController0
	tax
	and #StartBtn	; \ If Start button is pressed
	bne :+			; / then exit Title Screen loop
	txa
	and #SelectBtn	; \ If Select button is NOT pressed
	beq ldaed		; / then loop again
	lda #0				; \ Reset Frame Counter
	sta FrameCounter	; /
	ldx MainMenuCursor	; \
	lda TSNextOption,x	; | Select Next Mode
	sta MainMenuCursor	; /
ldaed:
	jmp TitleScreenLoop	; Loop
	:rts

StartDemo:
	inc DemoFlag		; Set Demo Flag
	inc TwoPlayerFlag		; Set to 2 Players
	lda #0		; \ Disable All Sound Channels
	sta $4015	; /
	sta GameMode		; Set Game Mode to 00 (Balloon Fight)
	jsr StartDemoGame
	lda #0
	sta DemoFlag
	beq GotoTitleScreen

TSNextOption:	;Title Screen choices
.BYTE 1,2,0

ldb08:
	lda MainMenuCursor	; \
	lsr					; | Set Game Mode
	sta GameMode		; / depending on selected mode
	lda MainMenuCursor		; \
	tax						; | Set Amount of players
	and #1					; | depending on selected mode
	sta TwoPlayerFlag		; /
	lda MenuCursorYOptions,x	; \ Set Y position of menu cursor balloon
	sta $057b					; /
	lda #44		; \ Set X position of menu cursor balloon
	sta $0567	; /
	ldx #0		; \ Set graphics of menu cursor balloon
	stx $055d	; /
	jmp lce2f_balloonxspritemanage

MenuCursorYOptions:
	.BYTE 140,156,172

.include "PhaseData.asm"

.include "SpriteAnimData.asm"

le3a4:
	lda OAMObjectOrder1,x	; \ Set Pointer from the first table
	sta DataPointerLo		; /
	lda FrameCounter		; \
	lsr						; | Every 2 frames
	bcc @Skip				; | Set Pointer from the second table
	lda OAMObjectOrder2,x	; |
	sta DataPointerLo		; /
	@Skip:
	lda #$02			; \ Set Pointer to $02xx
	sta DataPointerHi	; / (OAM)
	lda ObjectBalloons,x	; \ If Object X Balloons >= 0
	bpl le3cf				; /
	cmp #$ff	; \ If Object X Balloons == -1
	beq le3c2	; /
	jmp le4d5
	le3c2:
	ldy #20					; \
	@ClearLoop:
		lda #$f0			; |
		sta (DataPointer),y	; |
		deyr 4				; |
		bpl @ClearLoop		; /
	rts
	le3cf:
	cpx #8		; \ If Object is Fish
	beq le41b	; /
	lda ObjectStatus,x		; \
	aslr 2					; | (Object Status * 4) + Animation Frame
	adc ObjectAnimFrame,x	; /
	cpx #2		; \ If Object is a player
	bcs le408	; /
	ldy ObjectBalloons,x	; \ 
	adc le34c,y				; | Y = (Object Status * 4) + Animation Frame
	tay						; / + [le34c + Balloons]
	lda PlayerAnimLower,y	; \
	sta $1d					; | Set pointer
	lda PlayerAnimUpper,y	; |
	sta $1e					; /
	lda $bd,x	; \ If Player X is invincible
	beq le429	; /
	ldy $88,x	; Y = Player X Balloons
	lda le34c+3,y	; \
	adc ObjectAnimFrame,x		; | Y = [le34c+3+Balloons+Frame]
	tay				; /
	lda PlayerFlashAnimLower,y	; \
	sta LoadPointerLo			; | Set pointer
	lda PlayerFlashAnimUpper,y	; |
	sta LoadPointerHi			; /
	jmp le429
	le408:
	ldy ObjectBalloons,x
	cadcy le352
	tay
	lda EnemyAnimLower,y
	sta LoadPointerLo
	lda EnemyAnimUpper,y
	sta LoadPointerHi
	bne le429
	le41b:
	ldy ObjectStatus,x
	bmi le3c2
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
	ldy $88,x
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
	tpa le043, ScorePointer
	lda ObjectDirection,x
	beq le47c
	tpa le079, ScorePointer
	le47c:
	ldy #0
	lda (LoadPointer),y
	inc LoadPointerLo
	bne le486
	inc LoadPointerHi
	le486:
	asl
	sta Temp13
	asl
	adc Temp13
	adc ScorePointerLo
	sta ScorePointerLo
	bcc le494
	inc ScorePointerHi
	le494:
	phx
	ldx #5
	ldy #0
	le49a:
		lda Temp12
		clc
		adc le03d,x
		sta (DataPointer),y
		sta Temp12
		iny
		sty Temp13
		ldy #0
		lda (LoadPointer),y
		inc LoadPointerLo
		bne le4b1
		inc LoadPointerHi
		le4b1:
		ldy Temp13
		sta (DataPointer),y
		iny
		lda Temp14
		sta (DataPointer),y
		iny
		sty Temp13
		ldy #0
		lda Temp15
		clc
		adc (ScorePointer),y
		inc ScorePointerLo
		bne le4ca
		inc ScorePointerHi
		le4ca:
		ldy Temp13
		sta (DataPointer),y
		iny
		dex
		bpl le49a
	plx
	rts

le4d5:
	phx
	ldy DataPointerLo
	lda ObjectYPosInt,x
	sta OAM+0,y
	sta OAM+4,y
	cadc #8
	sta OAM+8,y
	sta OAM+12,y
	lda #$f0
	sta OAM+16,y
	sta OAM+20,y
	lda ObjectXPosInt,x
	sta OAM+3,y
	sta OAM+11,y
	cadc #8
	sta OAM+7,y
	sta OAM+15,y
	lda ObjectYPosInt,x
	cmp #$d0
	lda #3
	bcc le50d
	lda #$23
	le50d:
	sta OAM+2,y
	lda ObjectStatus,x
	bne le553
	lda OAM+2,y
	sta OAM+6,y
	sta OAM+10,y
	sta OAM+14,y
	lda #$da
	sta OAM+1,y
	lda #$db
	sta OAM+5,y
	lda #$dc
	sta OAM+9,y
	lda #$dd
	sta OAM+13,y
	ldx $1f
	lda FrameCounter
	and #$20
	beq le550
	lda FrameCounter
	and #$40
	bne le54a
	inc OAM+0,x
	inc OAM+4,x
	bne le550
	le54a:
	inc OAM+3,x
	inc OAM+11,x
	le550:
	plx
	rts

le553:
	lda OAM+2,y
	ora #$40
	sta OAM+6,y
	ora #$80
	sta OAM+14,y
	and #$bf
	sta OAM+10,y
	lda #$de
	sta OAM+1,y
	sta OAM+5,y
	sta OAM+9,y
	sta OAM+13,y
	dec ObjectCountdown,x
	bpl le584
	lda #$ff
	sta ObjectBalloons,x
	lda #$f0
	sta ObjectYPosInt,x
	lda #4
	sta $f1
	le584:
	plx
	rts

SplashManage:
	ldx SplashAnim
	bmi :+
	lda SplashAnimLo,x
	sta LoadPointerLo
	lda SplashAnimHi,x
	sta LoadPointerHi
	ldy #0
	ldx #0
	@Loop1:
		lda (LoadPointer),y
		sta OAM+$e0,x
		iny
		inx
		cmp #$f0
		bne @Skip
		inxr 3
		@Skip:
		cpx #16
		bne @Loop1
	ldy #15
	@Loop2:
		lda OAM+$e0,y		; \
		clc					; | Offset the splash sprites by the Splash X Offset
		adc SplashXOffset	; |
		sta OAM+$e0,y		; /
		deyr 4
		bpl @Loop2
	lda FrameCounter
	and #3
	bne :+
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

le601:	;YVelFrac by object type
	.BYTE $04,$04,$05,$06,$03,$03,$03,$06,$0a,$0a,$0a,$0a
le60d:
	.BYTE $28,$32,$46,$78,$00,$00,$00,$64,$00,$00,$00,$00
le619:
	.BYTE $0a,$1e,$32,$70,$00,$00,$00,$70,$00,$00,$00,$00
le625:
	.BYTE $14,$3c,$64,$a0,$00,$00,$00,$a0,$00,$00,$00,$00
le631:
	.BYTE $70,$b0,$e0,$40,$80,$80,$80,$40,$00,$00,$00,$00
le63d:
	.BYTE $00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$00
le649:
	.BYTE $90,$50,$20,$c0,$80,$80,$80,$c0,$00,$00,$00,$00
le655:
	.BYTE $ff,$ff,$ff,$fe,$ff,$ff,$ff,$fe,$00,$00,$00,$00
le661:
	.BYTE $50,$90,$c0,$40,$40,$40,$40,$40,$00,$00,$00,$00
le66d:
	.BYTE $00,$00,$00,$01,$00,$00,$00,$01,$02,$02,$02,$02
le679:
	.BYTE $b0,$70,$40,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
le685:
	.BYTE $ff,$ff,$ff,$fe,$ff,$ff,$ff,$fe,$fe,$01,$fe,$fe

ObjectManage:
	jsr lee25_collision
	ldx #7					; \
le696:
	lda ObjectBalloons,x	; | Check all Object's Balloons
	bpl le6a4				; | If >= 0 then proceed
	cmp #$ff				; | else if == -1 then go to next object
	beq le6e2				; | else ? and go to next object
	jsr lecba				; |
	jmp le6e2				; /
le6a4:
	cpx #2		; \ Object is Player
	bcc le6b8	; /
	cmp #1		; \ One balloon
	bne le6b8	; /
	lda ObjectStatus,x	; \ Object Status >= 2
	cmp #2				; |
	bcs le6b8			; /
	lda $f1		; \
	ora #$20	; | Play SFX
	sta $f1		; /
le6b8:
	dec ObjectAnimTimer,x	; \ Object's Anim Timer != 0
	bne le6d9				; /
	lda #3					; \ Object's ? = 3
	sta ObjectAnimTimer,x	; /
	cpx #2		; \ Object is not Player
	bcs le6ce	; /
	dec PlayerInvTimer,x	; \ Player Invincibility Timer Handle
	bne le6ce				; | Decrease Time until 0
	lda #0					; | Then disable invincibility
	sta PlayerInvincible,x	; /
le6ce:
	jsr ObjectUpdateAnim
	stx TargetUpdateScore
	jsr lebc4
	jsr le796
le6d9:
	jsr lea58
	jsr led28
	jsr le983
le6e2:
	jsr le3a4
	dex
	bpl le696	; Loop back
	rts

ObjectUpdateAction:
	cpx #2			; \ If Enemy then rely on RNG
	bcs AutoInput	; / If Player then rely on joypad
	lda FrameCounter	; \
	and #15				; | Enemy only reacts every 16 frames
	bne GetPlayerInput	; /
	jsr UpdateRNG		; \ Update Enemy Action
	sta ObjectAction,x	; /
GetPlayerInput:
	lda DemoFlag	; \ If Demo Play then
	bne AutoInput	; / do automatic inputs
	jsr PollControllerX
	lda Joy1Press,x		; \ Read Pressed Buttons
	sta ObjectAction,x	; / into Player Action
	:rts
AutoInput:	; Demo Players & Enemies
	lda ObjectYPosInt,x	; \ If Player Y > #$A0 (Low on screen)
	cmp #$a0			; | Then
	bcc @YCheckSkip		; /
	lda ObjectAction,x	; \
	ora #BBtn			; | Do rapid fire
	sta ObjectAction,x	; / (B Button)
	rts
	@YCheckSkip:
	dec ObjectCountdown,x	; \
	bne :-					; / then return
	jsr UpdateRNG
	ldy ObjectType,x
	and le762,y
	adc le765,y
	sta ObjectCountdown,x
	stx Temp12
	lda FrameCounter
	rolr 2
	eor Temp12
	and #1
	tay
	lda ObjectBalloons,y
	bmi @ForceB
	lda PlayerInvincible,y
	bne @ForceB
	lda #0
	sta ObjectAction,x
	lda ObjectYPosInt,y
	sec
	sbc #4
	cmp ObjectYPosInt,x
	bcs @ChooseDirection
	@ForceB:
		lda #BBtn			; \ Make character hold B (ascend)
		sta ObjectAction,x	; /
	@ChooseDirection:
		lda ObjectXPosInt,x
		cmp ObjectXPosInt,y
		bcs @GoLeft
		lda ObjectAction,x
		ora #RightDPad
		sta ObjectAction,x
		rts
		@GoLeft:
		lda ObjectAction,x
		ora #LeftDPad
		sta ObjectAction,x
		rts

le762:
	.BYTE 31,15,7
le765:
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

le796:
	lda ObjectBalloons,x	; \ If object has balloons
	bne le7a3				; / then continue
le79a:
	lda #0					; \ If no balloons:
	sta ObjectXVelFrac,x	; | X Velocity = 0
	sta ObjectXVelInt,x		; /
	rts	; Return
le7a3:
	cmp #2		; \ If 2 balloons
	beq le7e8	; /
	cpx #2		; \ If object is a player
	bcc le7e8	; /
	lda ObjectStatus,x	; \ If Object Status >= 2
	cmp #2				; | then zero X velocity
	bcs le79a			; /
le7b1:
	lda ObjectXVelFrac,x
	sta Temp12
	lda ObjectXVelInt,x
	sta Temp13
	jsr lf1a6
	lda ObjectUnknown2,x
	cadc Temp12
	sta ObjectUnknown2,x
	sta Temp12
	lda ObjectUnknown3,x
	adc Temp13
	sta ObjectUnknown3,x
	sta Temp13
	jsr lf1a6
	lda ObjectXVelFrac,x
	sec
	sbc Temp12
	sta ObjectXVelFrac,x
	lda ObjectXVelInt,x
	sbc Temp13
	sta ObjectXVelInt,x
	rts
le7e8:
	lda ObjectStatus,x
	cmp #6
	bcc le7ef
	rts
le7ef:
	lda ObjectStatus,x
	cmp #4
	bne le811
	lda ObjectAction,x
	and #LeftDPad
	beq le802
	lda ObjectDirection,x
	beq le811
	bne le80d
le802:
	lda ObjectAction,x
	and #RightDPad
	beq le811
	lda ObjectDirection,x
	bne le811
le80d:
	lda #5
	sta ObjectStatus,x
le811:
	lda ObjectStatus,x
	cmp #2
	bne le832
	lda ObjectAction,x
	and #LeftDPad
	beq le821
	lda #0
	beq le829
le821:
	lda ObjectAction,x
	and #RightDPad
	beq le82e
	lda #1
le829:
	cmp ObjectDirection,x
	beq le832
le82e:
	lda #4
	sta ObjectStatus,x
le832:
	lda ObjectStatus,x
	cmp #4
	bcc le854
	lda ObjectAction,x
	and #LeftDPad
	beq le845
	lda ObjectDirection,x
	bne le854
	beq le850
le845:
	lda ObjectAction,x
	and #RightDPad
	beq le854
	lda ObjectDirection,x
	beq le854
le850:
	lda #2
	sta ObjectStatus,x
le854:
	lda ObjectStatus,x
	cmp #3
	bne le864
	lda ObjectAction,x
	and #LeftDPad | RightDPad
	beq le864
	lda #2
	sta ObjectStatus,x
le864:
	lda ObjectStatus,x
	cmp #4
	bcs le87f
	lda ObjectAction,x
	and #LeftDPad
	beq le874
	lda #0
	beq le87c
le874:
	lda ObjectAction,x
	and #RightDPad
	beq le87f
	lda #1
le87c:
	sta ObjectDirection,x
le87f:
	lda ObjectStatus,x
	cmp #4
	bcc le8b8
	lda ObjectAnimFrame,x
	cmp #1
	bne le8b8
	ldy ObjectType,x
	lda ObjectDirection,x
	beq le8a6
	lda ObjectXVelFrac,x
	sec
	sbc le625,y
	sta ObjectXVelFrac,x
	lda ObjectXVelInt,x
	sbc #0
	jmp le901
le8a6:
	lda ObjectXVelFrac,x
	cadcy le625
	sta ObjectXVelFrac,x
	lda ObjectXVelInt,x
	adc #0
	jmp le901
le8b8:
	lda ObjectStatus,x
	beq le8c7
	cmp #2
	beq le907
	cmp #3
	beq le8c7
	jmp le951
le8c7:
	lda ObjectAnimFrame,x
	cmp #1
	beq le8d1
	jmp le951
le8d1:
	ldy ObjectType,x
	lda ObjectAction,x
	and #LeftDPad
	beq le8ec
	lda ObjectXVelFrac,x
	sec
	sbc le619,y
	sta ObjectXVelFrac,x
	lda ObjectXVelInt,x
	sbc #0
	jmp le901
le8ec:
	lda ObjectAction,x
	and #RightDPad
	beq le951
	lda ObjectXVelFrac,x
	cadcy le619
	sta ObjectXVelFrac,x
	lda ObjectXVelInt,x
	adc #0
le901:
	sta ObjectXVelInt,x
	jmp le951
le907:
	lda ObjectAnimFrame,x
	cmp #1
	bne le951
	ldy ObjectType,x
	lda ObjectAction,x
	and #LeftDPad
	beq le929
	lda ObjectXVelFrac,x
	sec
	sbc le625,y
	sta ObjectXVelFrac,x
	lda ObjectXVelInt,x
	sbc #0
	jmp le93e
le929:
	lda ObjectAction,x
	and #RightDPad
	beq le951
	lda ObjectXVelFrac,x
	cadcy le625
	sta ObjectXVelFrac,x
	lda ObjectXVelInt,x
	adc #0
le93e:
	sta ObjectXVelInt,x
	lda ObjectAction,x
	and #LeftDPad | RightDPad
	beq le951
	cpx #2
	bcs le951
	lda SFX1Req
	ora #8
	sta SFX1Req
le951:
	lda ObjectStatus,x
	cmp #4
	bcc :+
	lda ObjectDirection,x
	bne le963
	lda ObjectXVelInt,x
	bmi :+
	bpl le968
le963:
	lda ObjectXVelInt,x
	bpl :+
le968:
	lda ObjectStatus,x
	cmp #5
	bne le976
	lda ObjectDirection,x
	eor #1
	sta ObjectDirection,x
le976:
	lda #3
	sta ObjectStatus,x
	lda #0
	sta ObjectXVelFrac,x
	sta ObjectXVelInt,x
	:rts

le983:
	lda $cb
	bne le9b6
	lda PlayerInvincible,x
	beq le99a
	lda BTPlatformX
	beq le99a
	sec
	sbc ObjectXPosInt,x
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
	lda #1
	sta ObjectStatus,x
	sta ObjectCountdown,x
	rts
le9b6:
	lda #0
	sta ObjectYVelFrac,x
	sta ObjectYVelInt,x
	sta ObjectYPosFrac,x
	sta $cb
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
	lda #0
	sta ObjectXVelFrac,x
	sta ObjectXVelInt,x
	sta ObjectUnknown2,x
	sta ObjectUnknown3,x
	lda #$40	; \ Play SFX
	sta $f1		; /
	:rts
le9f3:
	lda #0
	sta ObjectStatus,x
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

lea58:
	lda ObjectUnknown4,x
	beq lea60
	dec ObjectUnknown4,x
lea60:
	cpx #2
	bcs lea8c
	lda PlayerFreeze,x
	beq lea8c
	lda FrameCounter
	lsr
	bcc :+
	inc ObjectAnimFrame,x
	lda ObjectAnimFrame,x
	and #3
	sta ObjectAnimFrame,x
	lda #1
	sta ObjectStatus,x
	dec ObjectCountdown,x
	bne :+
	lda #0
	sta PlayerFreeze,x
	sta ObjectStatus,x
	lda #$20
	sta SFX1Req
	:rts
lea8c:
	lda ObjectYVelFrac,x
	clc
	ldy ObjectType,x
	adc le601,y
	sta ObjectYVelFrac,x
	bcc lea9e
	inc ObjectYVelInt,x
lea9e:
	lda ObjectYVelInt,x
	bmi leac1
	cmp le66d,y
	bcc leadc
	bne leab2
	lda ObjectYVelFrac,x
	cmp le661,y
	bcc leadc
leab2:
	lda le661,y
	sta ObjectYVelFrac,x
	lda le66d,y
	sta ObjectYVelInt,x
	jmp leadc
leac1:
	cmp le685,y
	bcc lead0
	bne leadc
	lda ObjectYVelFrac,x
	cmp le679,y
	bcs leadc
lead0:
	lda le679,y
	sta ObjectYVelFrac,x
	lda le685,y
	sta ObjectYVelInt,x
leadc:
	jsr ObjectApplyYVelocity
	cmp #$f8
	bcs leb0d
	cmp #$e8
	bcc leb0d
	lda #$ff
	sta ObjectBalloons,x
	lda #4		; \ Do Water Plonk
	sta $bb		; / Animation
	lda ObjectXPosInt,x
	sta SplashXOffset
	cpx #2
	bcc leb05
	lda #$80
	sta ObjectBalloons,x
	lda #0
	sta ObjectStatus,x
	lda #1
	sta $f3
	bne leb0d
leb05:
	lda PhaseType
	bne leb0d
	lda #$40
	sta SFX1Req
leb0d:
	lda ObjectXVelInt,x
	bmi leb30
	cmp le63d,y
	bcc leb4b
	bne leb21
	lda ObjectXVelFrac,x
	cmp le631,y
	bcc leb4b
leb21:
	lda le631,y
	sta ObjectXVelFrac,x
	lda le63d,y
	sta ObjectXVelInt,x
	jmp leb4b
leb30:
	cmp le655,y
	bcc leb3f
	bne leb4b
	lda ObjectXVelFrac,x
	cmp le649,y
	bcs leb4b
leb3f:
	lda le649,y
	sta ObjectXVelFrac,x
	lda le655,y
	sta ObjectXVelInt,x
leb4b:
	jsr ObjectApplyXVelocity
	lda GameMode
	beq leb62
	lda ObjectXPosInt,x
	cmp #$10
	bcs leb5a
	lda #$10
leb5a:
	cmp #$e0
	bcc leb60
	lda #$e0
leb60:
	sta ObjectXPosInt,x
leb62:
	lda PhaseType
	beq :+
	lda ObjectBalloons,x
	bne :+
	lda ObjectYPosInt,x
	cmp #$c8
	bcc :+
	lda #$c7
	sta ObjectYPosInt,x
	lda ObjectType,x
	cmp #$0b
	bne leb84
	dec ObjectType,x
	jsr ObjectBounceY
	jmp lf18c
leb84:
	lda #2
	sta ObjectBalloons,x
	lda #3
	sta ObjectType,x
	:rts

ObjectApplyXVelocity:
	lda ObjectXPosFrac,x	; \
	clc						; | Apply Velocity to
	adc ObjectXVelFrac,x	; | X Position (Frac)
	sta ObjectXPosFrac,x	; /
	lda ObjectXPosInt,x	; \ Apply Velocity to
	adc ObjectXVelInt,x	; | X Position (Int)
	sta ObjectXPosInt,x	; /
	rts

ObjectApplyYVelocity:
	lda ObjectYPosFrac,x	; \
	clc						; | Apply Velocity to
	adc ObjectYVelFrac,x	; | Y Position (Frac)
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

lebc4:
	cpx #2		; \ If not player
	bcs lebe3	; /
	lda ObjectBalloons,x	; \ If player still has balloons
	bne lebd6				; /
	lda ObjectAnimFrame,x	; \ If player animation frame != 0
	bne lebd6				; /
	lda #0				; \ Then Player Status = 0 (Dead)
	sta ObjectStatus,x	; /
	rts
lebd6:	; Player
	lda ObjectStatus,x	; \ If Player Status < 6
	cmp #6				; | Then ?
	bcc lec38			; /
	lda #1				; \ Else Status = 1
	sta ObjectStatus,x	; /
	dec ObjectBalloons,x	; Decrease one balloon
	rts
lebe3:	; Enemy
	lda ObjectBalloons,x	; \ If Enemy Status == 2
	cmp #2					; | Then ?
	beq lec38				; /
	lda ObjectAnimFrame,x	; \ If enemy animation frames != 0
	bne :+					; / Then
	lda ObjectBalloons,x	; \ If Enemy Status != 0
	bne lebf7				; / Then
	lda #0				; \ Enemy Status = 0 (Dead)
	sta ObjectStatus,x	; /
	rts
lebf7:
	lda ObjectStatus,x	; \ If Enemy Status != 0
	bne lebfe			; / Then
	inc ObjectStatus,x	; Increase Enemy Status
	:rts
lebfe:
	cmp #2	; \ If Player
	bcc :-	; / then return
	dec ObjectCountdown,x
	bne :+
	lda EnemyInflateSpeed
	sta ObjectCountdown,x
	inc ObjectStatus,x
	lda ObjectStatus,x
	cmp #7
	bcc :+
	lda #2
	sta ObjectBalloons,x
	lda #0
	sta ObjectStatus,x
	ldy ObjectType,x
	lda lecae,y
	ldy ObjectUnknown5,x
	bne lec2f
	dec ObjectUnknown5,x
	lda ObjectType,x
	and #3
	lec2f:
	sta ObjectType,x
	lda #$fe
	sta ObjectYVelInt,x
	:rts
lec38:
	jsr ObjectUpdateAction
	lda ObjectAction,x	; Limit action to valid inputs
	and #ABtn | BBtn | LeftDPad | RightDPad
	beq @ApplyObjectAction
	cpx #2					; \ If Enemy
	bcs @ApplyObjectAction	; / Skip
	lda #0					; \ If Player
	sta PlayerInvincible,x	; / Disable invincibility
	@ApplyObjectAction:
		lda ObjectAction,x	; \
		and #BBtn			; | B button
		bne @BPressed		; /
		lda ObjectAction,x	; \
		and #ABtn			; | A button
		bne @APressed		; /
		lda #0				; \
		sta ABtnCooldown,x	; | Mark the A button as lifted
		beq :+				; / Return
	@APressed:
		lda ABtnCooldown,x	; \ Return if the A Button was already being pressed
		bne :+				; /
	@BPressed:
	lda ObjectStatus,x
	cmp #2
	bcc lec75
	dec ObjectYPosInt,x
	dec ObjectYPosInt,x
	lda #0
	sta ObjectYVelFrac,x
	sta ObjectYVelInt,x
	beq lec7e
lec75:
	cmp #1
	beq lec7e
	lda ObjectAnimFrame,x
	bne :+	; Return
lec7e:
	lda #0
	sta ObjectStatus,x
	lda #1
	sta ObjectAnimFrame,x
	lda #1
	sta ABtnCooldown,x
	ldy #0
	cpx #2
	bcc lec93
	iny
lec93:
	lda SFX1Req,y
	ora #$10
	sta SFX1Req,y
	lda ObjectYVelFrac,x
	sec
	ldy ObjectType,x
	sbc le60d,y
	sta ObjectYVelFrac,x
	bcs :+
	dec ObjectYVelInt,x
	:rts

lecae:
	.BYTE $01,$02,$02,$03
	.BYTE $01,$02,$02,$03
	.BYTE $01,$02,$02,$03

lecba:
	lda ObjectStatus,x	; \ If Object(x).Status != 0
	bne :+	; / then don't do anything
	jsr le7b1
	jsr ObjectApplyXVelocity
	lda ObjectYPosFrac,x
	sec
	sbc #$60
	sta ObjectYPosFrac,x
	lda ObjectYPosInt,x
	sbc #0
	sta ObjectYPosInt,x
	cmp #$f1
	bcc lecdb
	lda #$ff
	sta ObjectBalloons,x
lecdb:
	phx
	ldy #1
	@Loop:
		lda ObjectBalloons,y
		beq @Next
		bmi @Next
		lda ObjectYPosInt,x
		sec
		sbc ObjectYPosInt,y
		jsr GetAbsoluteValue
		cmp #24
		bcs @Next
		lda ObjectXPosInt,x
		sec
		sbc ObjectXPosInt,y
		jsr GetAbsoluteValue
		cmp #16
		bcs @Next
		lda #$ff
		sta ObjectStatus,x
		lda #3
		sta ObjectCountdown,x
		lda #120
		sta ScrollLockTimer
		lda #2
		sta SFX1Req
		lda #50
		sty TargetUpdateScore
		jsr AddScore
		lda #1
		ldx TargetUpdateScore
		jsr ld871
		plx
		rts
		@Next:
		dey
		bpl @Loop
	plx
	:rts

led28:
	ldy ObjectBalloons,x
	dey
	bpl led2e
	:rts
led2e:
	lda ObjectYPosInt,x
	cmp #$f9
	bcc led40
	lda ObjectYVelInt,x
	bpl :-
	lda #0
	sta CollisionFlags
	jmp lede1
led40:
	ldy PlatformCount
	bmi :--
led44:
	lda #0
	sta CollisionFlags
	lda (TopPointer),y
	sec
	sbc #$18
	cmp ObjectYPosInt,x
	bcs ledb6
	adc #3
	cmp ObjectYPosInt,x
	bcc led5b
	lda #1
	bne led69
led5b:
	lda (BottomPointer),y
	cmp ObjectYPosInt,x
	bcc ledb6
	sbc #3
	cmp ObjectYPosInt,x
	bcs led89
	lda #2
led69:
	sta CollisionFlags
	lda (LeftPointer),y
	cmp #$10
	beq led78
	sec
	sbc #$0c
	cmp ObjectXPosInt,x
	bcs led85
led78:
	lda (RightPointer),y
	cmp #$ff
	beq led89
	sec
	sbc #4
	cmp ObjectXPosInt,x
	bcs led89
led85:
	lda #0
	sta CollisionFlags
led89:
	lda (LeftPointer),y
	sec
	sbc #$10
	beq leda0
	cmp ObjectXPosInt,x
	bcs ledb6
	adc #4
	cmp ObjectXPosInt,x
	bcc leda0
	lda CollisionFlags
	ora #4
	bne ledb4
leda0:
	lda (RightPointer),y
	cmp #$ff
	beq ledb6
	cmp ObjectXPosInt,x
	bcc ledb6
	sbc #4
	cmp ObjectXPosInt,x
	bcs ledb6
	lda CollisionFlags
	ora #8
ledb4:
	sta CollisionFlags
ledb6:
	lda CollisionFlags
	bne ledc1
	dey
	bmi :+
	jmp led44
	:rts

ledc1:
	lsr CollisionFlags
	bcc ledd6
	lda ObjectYVelInt,x
	bmi ledd6
	lda (TopPointer),y
	sbc #$18
	sta ObjectYPosInt,x
	inc ObjectYPosInt,x
	lda #1
	sta $cb
ledd6:
	lsr CollisionFlags
	bcc ledf4
	lda ObjectYVelInt,x
	bpl ledf4
	lda (BottomPointer),y
lede1:
	sta ObjectYPosInt,x
	jsr ObjectBounceY
	jsr lf18c
	cpx #2
	bcs ledf0
	jsr PlayBumpSFX
ledf0:
	lda $cb
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
	jsr lf172
	lda ObjectXVelInt,x
	ora ObjectXVelFrac,x
	beq :+
	lda ObjectDirection,x
	eor #1
	sta ObjectDirection,x
	lda $f1
	ora #2
	sta $f1
	:rts

lee25_collision:
	ldx #7	; Seems to compare balloons from objects
lee27:
	stx Temp12
	ldy Temp12
	dey
	bpl lee31
lee2e:
	jmp lef2a
lee31:
	lda ObjectBalloons,x	; \ If Object(x).Balloon <= 0
	bmi lee2e				; | then skip
	beq lee2e				; /
	lda ObjectBalloons,y	; \ If Object(y).Balloon <= 0
	bmi lee2e				; | then skip
	beq lee2e				; /
	lda #0
	sta CollisionFlags
	lda ObjectYPosInt,y		; \
	sec						; | 
	sbc ObjectYPosInt,x		; | If abs(Object(y).Y - Object(x).Y)
	jsr GetAbsoluteValue	; | <= #$18
	cmp #24					; | then
	bcs leec0				; /
	lda ObjectYPosInt,x		; \
	clc						; | If
	adc #24					; | abs((Object(y).Y + 7)
	sta Temp12				; |   - (Object(x).Y + #$18))
	lda ObjectYPosInt,y		; | >= 4 then
	clc						; |
	adc #7					; |
	sec						; |
	sbc Temp12				; |
	jsr GetAbsoluteValue	; |
	cmp #4					; |
	bcs lee6a				; /
	lda #1
	bne lee7c
lee6a:
	lda ObjectYPosInt,y		; \
	clc						; | If abs(Object(y).Y + #$11 - Object(x).Y)
	adc #17					; | >= 4 then
	sec						; |
	sbc ObjectYPosInt,x		; |
	jsr GetAbsoluteValue	; |
	cmp #$04				; |
	bcs lee8f				; /
	lda #$02
lee7c:
	sta CollisionFlags
	lda $0091,y		; \
	sec				; | If abs(Object(y).X - Object(x).X)
	sbc ObjectXPosInt,x		; | < #$10 then
	jsr GetAbsoluteValue	; |
	cmp #$10		; |
	bcc lee8f		; /
	lda #$00
	sta CollisionFlags
lee8f:
	lda ObjectXPosInt,x		; \
	clc				; |
	adc #$10		; | If abs((Object(y).X + 7)
	sta Temp12			; |      - (Object(x).X + #$10))
	lda $0091,y		; | >= 4 then
	clc				; |
	adc #$07		; |
	sec				; |
	sbc Temp12			; |
	jsr GetAbsoluteValue	; |
	cmp #$04		; |
	bcs leeaa		; /
	lda #$04
	bne leebc
leeaa:
	lda ObjectXPosInt,y		; \
	clc						; | If abs(Object(y).X + 9 - Object(x).X)
	adc #9					; | >= 4 then
	sec						; |
	sbc ObjectXPosInt,x		; |
	jsr GetAbsoluteValue	; |
	cmp #4					; |
	bcs leec0				; /
	lda #8
leebc:
	ora CollisionFlags
	sta CollisionFlags
leec0:
	lda #$00	; \
	sta $4b		; /
	lsr CollisionFlags		; \ [$CC].bit0 = Velocity Y related
	bcc leecd	; |
	jsr ObjectYVelSbcYX	; | 
	bmi leed6	; /
leecd:
	lsr CollisionFlags		; \ [$CC].bit1 = Velocity Y related
	bcc leef1	; |
	jsr ObjectYVelSbcYX	; |
	bmi leef1	; /
leed6:
	jsr lf0bd	; \ Do both object X and Y exist?
	bcs leeed	; /
	jsr ObjectBounceY
	jsr lf18c
	jsr SwapXY
	jsr ObjectBounceY
	jsr lf18c
	jsr SwapXY
leeed:
	lda #$01
	sta $4b
leef1:
	lsr CollisionFlags		; \ [$CC].bit2 = Velocity X related
	bcc leefa	; |
	jsr ObjectXVelSbcYX	; |
	bmi lef03	; /
leefa:
	lsr CollisionFlags		; \ [$CC].bit3 = Velocity X related
	bcc lef1e	; |
	jsr ObjectXVelSbcYX	; |
	bmi lef1e	; /
lef03:
	jsr lf0bd	; \ Do both object X and Y exist?
	bcs lef1a	; /
	jsr ObjectBounceX
	jsr lf172
	jsr SwapXY
	jsr ObjectBounceX
	jsr lf172
	jsr SwapXY
lef1a:
	lda #$01
	sta $4b
lef1e:
	jsr lef37
	jsr SwapXY
	jsr lef37
	jsr SwapXY
lef2a:
	dey			; \
	bmi lef30	; | Loop Y Objects
	jmp lee31	; /
lef30:
	dex			; \
	bmi :+		; | Loop X Objects
	jmp lee27	; /
	:rts

lef37:
	cpx #2		; \ Is Object X a player?
	bcc lef42	; |
	cpy #2		; | Is Object Y a player?
	bcc lef42	; /
	jmp lf043	; Skip
lef42:
	lda #0
	sta $0487
	lda ObjectUnknown4,x
	beq lef4f
	jmp lf043	; Skip
lef4f:
	lda $4b
	bne lef56
	jmp lf043	; Skip
lef56:
	cpx #2
	bcs lef61
	lda PlayerInvincible,x
	beq lef72
	jmp lf043	; Skip
lef61:
	lda ObjectBalloons,x
	cmp #1
	bne lef72
	lda ObjectStatus,x
	cmp #2
	bcs lef7f
	lda #1
	sta $0487
lef72:
	lda ObjectYPosInt,y
	cadc #4
	cmp ObjectYPosInt,x
	bcc lef7f
	jmp lf043	; Skip
lef7f:
	lda #$14
	sta ObjectUnknown4,x
	lda #0
	sta ObjectAnimFrame,x
	cpy #2
	bcc lef97
	lda ObjectBalloons,y
	cmp #2
	beq lef97
	jmp lf043	; Skip
lef97:
	lda SFX1Req
	ora #2
	sta SFX1Req
	lda ObjectBalloons,x
	cmp #2
	bne lefc0
	cpx #2
	bcs lefc0
	sty Temp12
	ldy ObjectStatus,x
	lda lf053,y
	ldy Temp12
	pha
	pla
	bne lefb7
	jmp lf043	; Skip
lefb7:
	sta ObjectStatus,x
	lda #0
	sta ObjectAnimFrame,x
	beq lefea
lefc0:
	dec ObjectBalloons,x
	bne lefce
	lda #$ff
	sta ObjectYVelInt,x
	lda #0
	sta ObjectYVelFrac,x
lefce:
	lda #0
	sta ObjectStatus,x
	sta ObjectXVelFrac,x
	sta ObjectXVelInt,x
	lda ObjectXPosInt,x
	bmi lefe0
	lda #$ff
	bne lefe2
lefe0:
	lda #0
lefe2:
	sta ObjectUnknown3,x
	lda #$80
	sta ObjectUnknown2,x
lefea:
	sty Temp12
	ldy ObjectType,x
	lda lf05e,y
	sta ObjectType,x
	lda #1
	sta ObjectUnknown5,x
	ldy Temp12
	cpy #2
	bcs lf043	; Skip
	lda ObjectType,x
	cmp #7
	beq lf011
	cmp #8
	bcc lf011
	lda $f1
	ora #$80
	sta $f1
lf011:
	ldy ObjectType,x
	lda lf06a,y
	sta Temp13
	lda $0487
	beq lf023
	lda lf076,y
	sta Temp13
lf023:
	lda lf082,y
	cadc $0487
	sta Temp14
	lda Temp12
	sta TargetUpdateScore
	pha
	phx
	lda Temp13
	pha
	lda Temp14
	jsr ld871
	pla
	jsr AddScore
	plx
	ply
lf043:
	lda ObjectType,x	; \ If Object X is not dead
	cmp #11				; | then don't play any SFX
	bne :+				; /
	lda PhaseType	; \ If it's Bonus Phase
	bne :+			; / then don't play any SFX
	lda #$20	; \ Play SFX
	sta SFX1Req	; /
	:rts

lf053:
	.BYTE $06,$06,$07,$08,$09,$0a,$00,$00,$00,$00,$00
lf05e:
	.BYTE $04,$05,$06,$07,$08,$09,$0a,$0b,$08,$09,$0a,$0b
lf06a:
	.BYTE $00,$00,$00,$00,$32,$4b,$64,$64,$4b,$64,$96,$64
lf076:
	.BYTE $00,$00,$00,$00,$32,$4b,$64,$64,$64,$96,$c8,$64
lf082:
	.BYTE $00,$00,$00,$00,$01,$02,$03,$03,$02,$03,$04,$03

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

lf0bd:
	cpx #2	; \
	bcc :+	; /
	lda ObjectStatus,x	; \ If Object(x).Status < 2
	cmp #2				; |
	bcc :+				; /
	lda #1					; \ If 1 - Object(x).Balloons >= 0
	cmp ObjectBalloons,x	; |
	bcs :+					; |
	cpy #2					; | If 1 - Object(x).Balloons - 2 < 0
	bcc :+					; /
	lda ObjectStatus,y	; \ If Object(y).Status < 2
	cmp #2				; |
	bcc :+				; /
	lda #1					; \ If 1 - Object(y).Balloons
	cmp ObjectBalloons,y	; /
	:rts

ObjectBounceX:
	lda #0					; \
	sec						; |
	sbc ObjectXVelFrac,x	; | Reverse X Velocity of Object X
	sta ObjectXVelFrac,x	; | (Bounce Horizontally)
	lda #0					; |
	sbc ObjectXVelInt,x		; |
	sta ObjectXVelInt,x		; /
	lda #0					; \
	sec						; | ?
	sbc ObjectUnknown2,x	; |
	sta ObjectUnknown2,x	; /
	lda #0					; \
	sbc ObjectUnknown3,x	; | ?
	sta ObjectUnknown3,x	; /
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

lf119:
	sta $2d
	lda $2c		; \ If Velocity Int >= 0
	bpl lf143	; / then goto lf143
	lda #0		; \
	sec			; | Get absolute value of Velocity Frac
	sbc $2b		; |
	sta $2b		; /
	lda #0		; \
	sbc $2c		; | Get absolute value of Velocity Int
	sta $2c		; /
	jsr lf143
	lda #0
	sec
	sbc $2e
	sta $2e
	lda #0
	sbc $2f
	sta $2f
	lda #0
	sbc $30
	sta $30
	rts

lf143:
	phx
	lda #0		; \
	sta $2e		; | Init
	sta $2f		; |
	sta $30		; /
	ldx #8				; \ -Loop 8 times
	@Loop:
		asl $2e			; |
		rol $2f			; |
		rol $30			; |
		asl $2d			; |
		bcc @Next		; |
		clc				; |
		lda TempWordLo	; | Old Velocity Frac
		adc $2e			; |
		sta $2e			; |
		lda TempWordHi	; | Old Velocity Int
		adc $2f			; |
		sta $2f			; |
		lda #0			; |
		adc $30			; |
		sta $30			; |
		@Next:
		dex				; |
		bne @Loop		; /
	plx
	rts

lf172:
	lda ObjectXVelFrac,x	; \ X Velocity Frac
	sta TempWordLo			; /
	lda ObjectXVelInt,x	; \ X Velocity Int
	sta TempWordHi		; /
	lda #$cd	; \ ?
	jsr lf119	; /
	lda $2f		; \ Update X Velocity Frac
	sta ObjectXVelFrac,x	; /
	lda $30		; \ Update X Velocity Int
	sta ObjectXVelInt,x	; /
	rts

lf18c:
	lda ObjectYVelFrac,x	; \ Y Velocity Frac
	sta $2b					; /
	lda ObjectYVelInt,x	; \ Y Velocity Int
	sta $2c				; /
	lda #$cd	; \ ?
	jsr lf119	; /
	lda $2f					; \ Update Y Velocity Frac
	sta ObjectYVelFrac,x	; /
	lda $30				; \ Update Y Velocity Int
	sta ObjectYVelInt,x	; /
	rts

lf1a6:
	ldy #4
	@Loop:
		lda Temp13
		asl
		ror Temp13
		ror Temp12
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
	stx BTPlatformX
	stx CurrentPhaseHeader		; Current Level Header = 0
	stx CurrentPhaseNum		; Current Phase = 0
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
	tax
	lda EnemyStartDelayData,x
	sta EnemyStartDelay
	lda EnemyInflateSpeedData,x
	sta EnemyInflateSpeed
	lda CurrentPhaseNum			; \
	cmp #2						; | If Current Phase >= 2
	bcs @FinishSetDifficulty	; / then finish
	lda #3					; \ For Phases 1 & 2:
	sta EnemyStartDelay		; | Set enemy start delay to 3 (Very short! They get going almost immediately)
	sta EnemyInflateSpeed	; / Set inflate speed to 3 (Moderate)
	@FinishSetDifficulty:
	ldx #7						; \
	@ClearLoop:
		lda #0					; | Initialize variables for each object (except Fish?)
		sta ObjectDirection,x	; | - Direction (0 = Left, 1 = Right)
		sta ObjectUnknown4,x	; |
		sta ObjectUnknown5,x	; |
		sta ObjectXVelFrac,x	; | - X Velocity (Frac)
		sta ObjectXVelInt,x		; | - X Velocity (Int)
		sta ObjectYVelFrac,x	; | - Y Velocity (Frac)
		sta ObjectYVelInt,x		; | - Y Velocity (Int)
		sta ObjectUnknown2,x	; |
		sta ObjectUnknown3,x	; |
		sta ObjectXPosFrac,x	; | - X Positions (Frac)
		sta ObjectYPosFrac,x	; | - Y Positions (Frac)
		lda #1					; |
		sta ObjectAnimTimer,x	; |
		sta ObjectCountdown,x	; |
		lda #3					; |
		sta ObjectAnimFrame,x	; | - Animation Frame
		dex						; |
		bpl @ClearLoop			; /
	ldx #5						; \
	@EnemyClearLoop:
		lda #$ff				; | Initialize Enemies
		sta ObjectBalloons+2,x	; |
		dex						; |
		bpl @EnemyClearLoop		; /
	ldx TwoPlayerFlag				; \
	@PlayerInitLoop:
		jsr lf386_initializeplayerx	; | Initialize Players
		dex							; |
		bpl @PlayerInitLoop			; /
	jsr ClearPPU
	jsr InitGameMode
	lda EnemyStartDelay
	cmp #16
	bcs lf28e
	lda #$58
	sta EnemyStartDelay
	lf28e:
	jsr InitializeFish
	jsr ld8ff
	lda GameMode
	beq lf29b	; Balloon Fight Game Mode
	jmp BalloonTripInit	; Balloon Trip Game Mode
lf29b:
	lda PhaseType
	beq lf2a2_balloonfight_load	; Normal Phase Type
	jmp lcf13	; Bonus Phase Type
lf2a2_balloonfight_load:
	jsr InitializeSparks
	lda CurrentPhaseHeader	; \ Level Header?
	and #3					; |
	bne lf2b3				; /
	lda #8			; \
	sta MusicReq	; / Play Stage Start jingle
	ldx DemoFlag				; \
	bne lf2b9_balloonfight_loop	; / Demo Flag
lf2b3:
	lda #$ff				; \ Show Phase Number for
	sta PhaseDisplayTimer	; / 255 frames
	inc CurrentPhaseNum		; Increment Current Phase Number
lf2b9_balloonfight_loop:	; Balloon Fight Game Loop
	jsr Pause
	lda PhaseDisplayTimer	; \
	beq lf2c5				; | Display Phase Number
	dec PhaseDisplayTimer	; | if the time is not 0
	jsr lf3cc_phasedisplay	; /
lf2c5:
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
	ldx TwoPlayerFlag		; X = 2 Player Flag
lf2e4:
	lda ObjectBalloons,x	; \ If Player X has balloons
	bpl lf30d				; / then skip respawn code
	lda DemoFlag	; \ If Demo Play
	bne :+			; / then return
	lda P1Lives,x	; \ If Player X Lives < 0
	bmi lf30d		; / then skip respawn code
	dec $c3,x	; \ Decrease Player X Respawn Delay
	bne lf327	; / If not 0 then ?
	phx
	jsr lc726
	plx
	ldy #2
	dec P1Lives,x	; Decrement Player X Lives
	sty $46		; Update Status Bar
	bmi lf30d	; If Player X has no more lives then don't respawn
	jsr lf386_initializeplayerx
	jsr InitPlayerType
	lda #$80		; \ Play Respawn Jingle
	sta MusicReq	; /
lf30d:
	dex			; \ Loop with Player 1
	bpl lf2e4	; /
	lda P1Lives		; \ If Player 1 has lives
	bpl lf318	; / continue
	lda P2Lives		; \ If Player 1 & 2 have 0 lives
	bmi lf366	; / then game over
lf318:
	lda DemoFlag	; \ If Demo Play
	beq lf327		; / then skip joypad read
	jsr PollController0
	lda Joy1Press				; \
	and #SelectBtn | StartBtn	; | If START or SELECT is pressed
	beq lf2b9_balloonfight_loop	; / then loop
	:rts
lf327:
	ldx #$05	; Enemy Check
lf329:
	lda $8a,x	; \ If Enemy Balloons
	beq lf32f	; | == 0 then ?
	bpl lf2b9_balloonfight_loop	; /  > 0 then loop
lf32f:
	dex			; \ Check next enemy
	bpl lf329	; /
	lda $bb		; Loop if water plonk effect
	bpl lf2b9_balloonfight_loop	; is not finished yet.
	ldx TwoPlayerFlag		; Player Check
lf338:
	ldy $88,x	; \ If Player X has balloons
	dey			; | then 
	bpl lf34c	; /
	lda P1Lives,x	; \ If Player X has no lives
	bmi lf34c	; / then skip
	lda #$ff	; \ Set Player X balloons
	sta $88,x	; / to none
	lda #$01	; \ Set Player X Respawn Delay
	sta $c3,x	; / to 1 frame
	jmp lf2b9_balloonfight_loop	; loop
lf34c:
	dex			; \ Loop player checks until
	bpl lf338	; / we can assume phase is cleared.
	lda #$02		; \ Play Stage Clear jingle
	sta MusicReq	; /
lf353:
	ldx #$96				; \ Wait 150 frames
	jsr WaitYFrames	; /
	ldx $3b		; \
	inx			; | Get to next level
	cpx #16		; | if past level ID #16
	bne lf361	; |
	ldx #4		; | then loop back to level ID 4
lf361:
	stx $3b		; /
	jmp LoadPhase	; Load Next Level
lf366:		; Manage Game Over
	lda #$01		; \ Play Game Over jingle
	sta MusicReq	; /
lf36a:
	lda #0
	sta BTXScroll		; Reset PPUSCROLL Shadow
	sta PPUCTRLShadowBT	; Reset PPUCTRL Shadow
	sta Temp15		; Set time
	jsr DrawGameOverText
lf375:
	jsr FinishFrame
	jsr PollController0	; \ Press START or SELECT
	and #$30				; | to come back to Title Screen
	bne lf383				; /
	dec Temp15		; \ Wait for 256 frames
	bne lf375	; / to come back to Title Screen
lf383:
	jmp StartGame	; Back to Title Screen

lf386_initializeplayerx:
	lda P1Lives,x	; \ If Player X has negative lives
	bmi :+			; / Then don't do anything
	lda PlayerStartingX,x	; \ Set up X coordinate for Player X
	sta ObjectXPosInt,x		; /
	lda #$b8			; \ Set up Y coordinate for Player X
	sta ObjectYPosInt,x	; /
	sta $bd,x	; Set up invincibility for Player X
	lda #$c8	; \ Set up invincibility time
	sta $bf,x	; / for Player X
	lda #$5a	; \
	ldy P1Lives,x	; | If Player X has lives
	bpl lf3a1	; | Then set respawn delay to #$5A
	lda #1		; | Else set respawn delay to #$01
lf3a1:
	sta $c3,x	; /
	lda #0
	sta $c1,x	; Clear Player X Freeze Flag
	sta ObjectXVelInt,x	; \ Set up Player X's X Velocity to $00
	sta ObjectXVelFrac,x ; /
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

lf3cc_phasedisplay:
	lda PhaseDisplayTimer	; \ Toggle between "PHASE-??"
	and #$20				; | and empty
	beq lf3ee				; / every #$20 frames?
	ldx #$0a	; \
lf3d4:
	lda PhaseText,x	; | Copy "PHASE-  " PPU Block
	sta $57,x	; |
	dex			; |
	bpl lf3d4	; /
	ldy #10			; \
	lda $3c			; | Add 1st digit of
	sta $43			; | Phase Number
	jsr DivideByY	; | (Divide by 10)
	sta $60			; /
	lda $43		; \ Add 2nd digit of
	sta $61		; / Phase Number
	jmp CopyPPUTempBlock
lf3ee:
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
	ldx #1				; \
	@Loop:
		lda lf43b,x			; | Prepare Game Over
		ldy lf43b+2,x		; | PPU blocks
		jsr CopyPPUBlock	; | to upload
		dex					; |
		bpl @Loop			; /
	ldx #$0f	; \
lf41e:
	lda #$24	; | Prepare 16 empty tiles
	sta $5a,x	; | to upload
	dex			; |
	bpl lf41e	; /
	lda #16	; \ Size: 16 bytes
	sta $59	; /
	lda #$21	; \ PPUADDR = $21xx
	sta $57		; /
	ldx #$02				; \
lf42f:
	lda lf43f,x				; | Prepare uploading
	sta $58					; | empty tiles to nametable
	jsr CopyPPUTempBlock	; | ($2188, $21A8, $21E8)
	dex						; | to PPU Buffer
	bpl lf42f				; /
	rts

.define PPUBlockPointers lf442, lf455
lf43b:	; Pointers to PPU Blocks
	.LOBYTES PPUBlockPointers
lf43d:
	.HIBYTES PPUBlockPointers

lf43f:	; Empty tiles lower PPUADDR
	.BYTE $88,$a8,$e8
lf442:	; "   GAME  OVER   "
	.BYTE $21,$c8,$10	;16 bytes to $21C8
	.BYTE $24,$24,$24,$10,$0a,$16,$0e,$24,$24,$18,$1f,$0e,$1b,$24,$24,$24
lf455:	; Tile Attributes?
	.BYTE $23,$da,$04,$aa,$aa,$aa,$aa

Wait20Frames:
	ldx #20
WaitYFrames:
	jsr FinishFrame
	dex
	bne WaitYFrames
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
	sta FishUnknown1	; \ Set Unused Variables?
	sta FishUnknown2	; /
	lda #$ff		; \ Reset Water Plonk Animation
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