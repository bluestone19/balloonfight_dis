
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
		lda BonusBalloonStock	; \ Repeat game loop here if there are balloons left
		bne BonusPhaseGameLoop	; /
		ldx #9
		@BalloonCheckLoop:
			lda BalloonStatus,x		; \
			bpl BonusPhaseGameLoop	; | Repeat game loop if there are active balloons
			dex						; /
			bpl @BalloonCheckLoop
		lda FrameCounter		; \ If all balloons are gone,
		bne BonusPhaseGameLoop	; / Wait until FrameCounter == 0 to go to end screen
	jsr ClearPPU	; Clear screen
	ldx #2					; \ Update Status bar
	stx StatusUpdateFlag	; /
	jsr WaitXFrames	; Wait 2 frames

	lday BonusEndScreenPalette	; \ Upload Bonus End Screen Palette
	jsr CopyPPUBlock			; /
	lday SuperBonusTextAttribute	; \ Upload attributes for Super Bonus text
	jsr CopyPPUBlock				; /
	lday BonusStatusAttribute	; \ Upload attributes for status bar
	jsr CopyPPUBlock			; /
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
	@EndBalloonLoop:				; |
		jsr ManageBalloonXSprite	; | Draw only the balloons necessary for the number of players
		dex							; /
		bpl @EndBalloonLoop
	jsr Wait20Frames

	lda #$2b			; \
	sta PPUTempBlock	; |
	lda #$24			; | "x  "
	sta PPUTempBlock+1	; |
	sta PPUTempBlock+2	; /
	lda #12			; \
	sta UploadTileX	; | Put multiplication text at tile 12,11
	lda #11			; |
	sta UploadTileY	; /
	lda #5				; \ Upload 5 tiles
	sta UploadBlockSize	; /
	lda P1BonusBalloons		; \ Upload Player 1's Balloon count
	jsr UploadBalloonCount	; /
	lda TwoPlayerFlag	; \ Skip uploading Player 2's Balloon count if only 1 player
	beq @SkipP2			; /
	lda #15			; \ Upload player 2's balloon count text at 12,15
	sta UploadTileY	; /
	lda P2BonusBalloons		; \ Upload Player 2's Balloon count
	jsr UploadBalloonCount	; /
	@SkipP2:
	jsr Wait20Frames

	lda BalloonPts		; \
	sta PPUTempBlock	; | Put balloon point value into PPUTempBlock
	lda #0				; | 
	sta PPUTempBlock+1	; | BalloonPts .. "00"
	sta PPUTempBlock+2	; /
	lda #8			; \
	sta UploadTileX	; | Put Balloon Points at tile 8,11
	lda #11			; |
	sta UploadTileY	; /
	lda #3				; \ Balloon Points is 3 tiles
	sta UploadBlockSize	; /
	lda BalloonPts
	jsr UploadPPU
	lda TwoPlayerFlag	; \
	beq @SkipP22		; |
	lda #15				; |
	sta UploadTileY		; | If two player, also display points at tile 8,15
	jsr UploadPPU		; /
	@SkipP22:
	lda #<-1			; \
	sta BalloonStatus	; | Make both balloons pop
	sta BalloonStatus+1	; /
	ldx TwoPlayerFlag				; \
	@EndBalloonLoop2:				; | Update balloon sprites again to be popping
		jsr ManageBalloonXSprite	; |
		dex							; /
		bpl @EndBalloonLoop2
	lda #$02	; \ Play Pop SFX
	sta SFX1Req	; /
	ldx #2			; \ Wait 2 frames
	jsr WaitXFrames	; /

	ldx TwoPlayerFlag				; \
	@EndBalloonLoop3:				; | Update balloon sprites again to disappear
		jsr ManageBalloonXSprite	; |
		dex							; /
		bpl @EndBalloonLoop3
	jsr DrawBalloonPointTotals
	jsr Wait20Frames

	lda #$01	; \ End SFX
	sta SFX1Req	; /
	jsr CheckTotalBonusBalloons	; \ Check if players collected all the balloons
	bne @SkipSuperBonus			; / If they failed, skip the super bonus
	lday PerfectText	; \ Upload the "P E R F E C T !!!" text
	jsr CopyPPUBlock	; /
	jsr FinishFrame

	ldx #26
	@SBLoadLoop:
		lda SuperBonusText,x	; \
		sta PPUTempBlock,x		; | Load Super Bonus Text to PPUTempBlock
		dex						; /
		bpl @SBLoadLoop
	lda SuperBonusPtsUpper	; \
	sta PPUTempBlock+17		; | Update value in Super Bonus Text
	lda SuperBonusPtsLower	; |
	sta PPUTempBlock+18		; /
	jsr CopyPPUTempBlock
	lda #$10		; \ Play super bonus jingle
	sta MusicReq	; /
	@SkipSuperBonus:
	ldx #120		; \ Wait 120 frames
	jsr WaitXFrames	; /

	jsr DrawBalloonPointTotals	; Redraw point totals so the PPU temp blocks are in memory again
	@PointAwardLoop:
		lda #0					; \
		sta TargetUpdateScore	; | Countdown P1's bonus point total into P1's score
		ldx #4					; |
		jsr BonusPointCountdown	; |
		jsr CopyPPUTempBlock	; /
		lda TwoPlayerFlag	; \ If single player, skip player 2
		beq @SkipP23		; /
		inc TargetUpdateScore	; \
		ldx #18					; |
		jsr BonusPointCountdown	; | Countdown P2's bonus point total into P2's score
		lday (PPUTempBlock+14)	; |
		jsr CopyPPUBlock		; /
		@SkipP23:
		lda #$01	; \ Play point count SFX
		sta SFX2Req	; /
		ldx #2			; \ Wait 2 frames between counting up points
		jsr WaitXFrames	; /
		lda PPUTempBlock+6	; \
		cmp #$24			; | Continue until P1's 100's place is empty
		bne @PointAwardLoop	; /
		lda TwoPlayerFlag		; \ If there's a second player
		beq @FinishMainBonusPts	; | Continue until P2's 100's place is empty
		lda a:PPUTempBlock+20	; | Absolute addressing on ZP location?
		cmp #$24				; /
		bne @PointAwardLoop
	@FinishMainBonusPts:
	ldx #10			; \ Wait 10 frames
	jsr WaitXFrames	; /

	jsr CheckTotalBonusBalloons
	bne @SkipSuperBonus2
	lda SuperBonusPtsUpper	; \
	sta ScoreDigit4			; | Put Super Bonus points into upper ScoreDigits for awarding
	lda SuperBonusPtsLower	; |
	sta ScoreDigit5			; /
	lda TwoPlayerFlag			; \
	sta TargetUpdateScore		; |
	@SBPlayerLoop:				; |
		jsr UpdateScore			; | Update score for each player
		dec TargetUpdateScore	; /
		bpl @SBPlayerLoop
	lda #$01	; \ Play point count SFX
	sta SFX2Req	; /
	jsr Wait20Frames

	@SkipSuperBonus2:
	lda #0			; \
	sta ScoreDigit4	; | Clear upper two ScoreDigits
	sta ScoreDigit5	; /
	ldx #1
	@LifeCheckLoop:
		lda P1Lives,x		; \ Continue if player x has lives
		bpl @PlayerXAlive	; /
		sta ObjectBalloons,x	; Set Player[X].Balloons to 0 if they have no lives
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
	.BYTE $29,$00,$00,$00,$00,$00,$24,$19,$1d,$1c,$26	; "=00000 PTS." Score value at offsets 4-8
	;P2 Bonus Total Text (Second block loaded simultaneously, starts 14 bytes in)
	.BYTE $21,$f3,11	; 11 Bytes to 0x21F3
	.BYTE $29,$00,$00,$00,$00,$00,$24,$19,$1d,$1c,$26	; "=00000 PTS." Score value at offsets 18-22
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

DrawBalloonPointTotals:
	ldx #28
	@Loop:
		lda BonusTotalText,x	; \ Load base Bonus Total text
		sta PPUTempBlock,x		; /
		dex
		bpl @Loop
	ldx #4	; P1's Balloon score total starts 4 tiles in
	ldy P1BonusBalloons			; \ Multiply P1BonusBalloons * BalloonPts
	jsr BonusBalloonPtsMultiply	; /

	ldx #18	; P2's Balloon score total starts 18 tiles in
	ldy P2BonusBalloons			; \ Multiply P2BonusBalloons * BalloonPts
	jsr BonusBalloonPtsMultiply	; /

	jsr CopyPPUTempBlock	; Upload P1's balloon score total
	lda TwoPlayerFlag
	bne @UploadP2	; \ If single player, then return here
	rts				; /
	@UploadP2:
	lday (PPUTempBlock+14)	; \ Upload P2's balloon score total
	jmp CopyPPUBlock		; / Then return

UploadBalloonCount:
	ldy #0	; Y = 10's place
	@Loop:
		cmp #10		; \ If remainder < 10, then it's done
		bcc @Finish	; /
		iny	; If remainder >= 10, increase 10's place by one
		sbc #10	; Then reduce remainder by 10
		jmp @Loop
	@Finish:
	sty PPUTempBlock+3	; Put 10's place into PPUTempBlock
	sta PPUTempBlock+4	; Put 1's place into PPUTempBlock
	jmp UploadPPU

BonusBalloonPtsMultiply:	; Y = Player's Bonus Balloons, X = Total Text Offset
	dey					; \ If Y == 0, erase lead zeroes then return
	bmi EraseLeadZeroes	; /
	lda BalloonPts			; \ Add BalloonPts 100's place to total 100's place
	cadcx PPUTempBlock+2	; /
	cmp #10					; \ If 100's place overflowed,
	bcc @SkipMod10			; |
	sbc #10					; | Reduce 100's place by 10
	inc PPUTempBlock+1,x	; / And increase 1000's place by 1
	@SkipMod10:
	sta PPUTempBlock+2,x	; Store 100's place
	lda PPUTempBlock+1,x	; \
	cmp #10					; | If 1000's place overflowed,
	bcc @SkipMod100			; |
	sbc #10					; | Reduce 1000's place by 10
	inc PPUTempBlock,x		; | Increase 10000's place by 1
	sta PPUTempBlock+1,x	; / And update 1000's place
	@SkipMod100:
	jmp BonusBalloonPtsMultiply ; Repeat Y times

EraseLeadZeroes:
	ldy #0
	@Loop:
		lda PPUTempBlock,x	; Get tile X in PPUTempBlock
		beq @Continue	; If this tile is 0, continue
		cmp #$24	; \ Return if not empty tile
		bne :+		; /
		@Continue:
		lda #$24			; \ Set 0 tile to blank
		sta PPUTempBlock,x	; /
		inx	; Go to next tile
		iny		; \ Only loop up to 4 times
		cpy #4	; /
		bne @Loop
	:rts

BonusPointCountdown:
	lda PPUTempBlock+2,x	; \
	cmp #$24				; | Finish if 100's place is empty
	beq @Finish				; /
	tay				; \
	bne @Add100Pts	; / If there are points in 100's place to add, add them
	lda PPUTempBlock+1,x	; \
	cmp #$24				; | Finish if 1000's place is empty
	beq @Finish				; /
	lda PPUTempBlock+1,x	; \ If 100's place is empty but 1000's has points,
	bne @Borrow1000s		; / Borrow from 1000's place then add points
	lda PPUTempBlock,x	; \
	cmp #$24			; | Finish if 10000's place is empty
	beq @Finish			; /
	lda #10					; \ If 100's and 1000's place are empty but 10000 has points,
	sta PPUTempBlock+1,x	; | Borrow 1 from 10000's place into 1000's place
	dec PPUTempBlock,x		; /
	@Borrow1000s:
	lda #10					; \
	sta PPUTempBlock+2,x	; | Borrow 1 from 1000's place into 100's place
	dec PPUTempBlock+1,x	; /
	@Add100Pts:
	dec PPUTempBlock+2,x	; Remove 100 points from total
	phx	; Preserve X
	lda #10			; \ Add 100 points
	jsr AddScore	; /
	plx	; Restore X
	@Finish:
	jmp EraseLeadZeroes