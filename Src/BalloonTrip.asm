; Balloon Trip Game Mode code

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
		inc BalloonXPosInt,x	; Scroll balloon 1px to the right
		lda BalloonXPosInt,x	; \
		cmp #248				; | If balloon's X position
		bne @SkipBalloon		; | reaches #$F8
		lda #<-1				; | then make it disappear
		sta BalloonStatus,x		; |
		lda #240				; |
		sta BalloonYPosInt,x	; | And reset the balloon counter
		lda #0					; |
		sta P1TripBalloons		; /
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
		jsr CheckBalloonXCollision
		lda P1BonusBalloons	; \
		beq @SkipBalloon	; | Every balloon touched
		dec P1BonusBalloons	; | counts towards the
		inc P1TripBalloons	; / main counter
		phx
		lda BalloonPts	; \ Add Score
		jsr AddScore	; /
		plx
		@SkipBalloon:
		jsr ManageBalloonXSprite
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
		lda SparkAnimFrames,y			; | (Unused Note: Animation is 8 frames
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
	jsr WaitXFrames	; /
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
	jmp EndGameNoJingle	; Put Game Over on Screen

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
		lda #1				; \ Set Balloon Status to 1
		sta BalloonStatus,x	; /
		lda #0					; \ Set Balloon X position to 0
		sta BalloonXPosInt,x	; /
		lda Temp15				; \ Set Balloon Y position to [$15]
		sta BalloonYPosInt,x	; /
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
