PropellerManage:
	ldx PropellerCount	; X = Propeller Count (-1 if none)
	bmi :+	; Skip if there's no propellers
	@Loop:
		jsr PropellerObjectCollisionCheck
		lda PropellerState,x	; \ If not spinning, move on
		beq @Next				; /
		txa					; \
		eor FrameCounter	; | Every other frame, manage spinning
		and #1				; | XOR'd with propeller index so they don't all spin at the same time
		bne @Next			; /
		ldy PropellerAngle,x	; \
		iny						; | Increment angle
		tya						; | But keep angle between 0-3 (inclusive)
		and #3					; |
		sta PropellerAngle,x	; /
		jsr DrawPropellerX
		lda PropellerAngle,x		; \
		cmp #1						; | Decrement countdown everytime angle hits 1 (Vertical)
		bne @Next					; |
		dec PropellerCountdown,x	; /
		bne @Next				; \ When countdown == 0,
		dec PropellerState,x	; / Stop spinning
		@Next:
			dex
			bpl @Loop
	:rts

PropellerObjectCollisionCheck:
	ldy #7
	lda PropellerState,x	; \ Check for objects to bounce if spinning
	bne @Loop				; /
	jmp PropellerActivationCheck	; If stationary, check if any object is activating this propeller
	@Loop:
		lda ObjectBalloons,y	; \ If Object Y has <=0 Balloons,
		bmieq @Next				; / Skip
		cpy #2				; \ If Object Y is player,
		bcc @ValidObject	; / continue
		cmp #1		; \ If Object Y is enemy with Balloons == 1,
		beq @Next	; / Skip
		@ValidObject:
		lda ObjectXPosInt,y		; \
		cadc #8					; | |Object(y).XPos + 8 - Propeller(x).XPos| >= 18
		ssbcx PropellerXPos		; | Then skip (Too far horizontally)
		sta Temp12				; |	Temp12 = X Offset
		jsr GetAbsoluteValue	; |
		cmp #18					; |
		bcs @Next				; /
		lda ObjectYPosInt,y		; \
		cadc #12				; | |Object(y).YPos + 12 - Propeller(x).YPos| >= 18
		ssbcx PropellerYPos		; | Then skip (Too far vertically)
		sta Temp13				; | Temp13 = Y Offset
		jsr GetAbsoluteValue	; |
		cmp #18					; |
		bcs @Next				; /

		lda Temp12			; \ If X offset is negative, 
		bmi @LeftSideHit	; / then the left side of the propeller was hit
		cmp #3				; \ If X offset is between 0 and 2 then
		bcc @CheckYOffset	; / Don't bounce them vertically
		lda #2						; \ Set Object(y).YVelInt to 2
		sta ObjectYVelInt,y			; | (Send them flying downward if they hit the right side)
		jsr PlayBumpSFX				; | Also play Bump SFX
		jsr ObjectYApplyYVelocity	; /
		bne @CheckYOffset	; Always go on to Check Y
		@LeftSideHit:
			cmp #<-3			; \ If X offset is between -3 and 0 then
			bcs @CheckYOffset	; / Don't bounce them vertically
			lda #<-2					; \ Set Object(y).YVelInt to -2
			sta ObjectYVelInt,y			; | (Send them flying upward if they hit the left side)
			jsr ObjectYApplyYVelocity	; |
			jsr PlayBumpSFX				; / Also play Bump SFX
		@CheckYOffset:
			lda Temp13		; \ If Y offset is negative, 
			bmi @TopSideHit	; / then the top side of the propeller was hit
			cmp #3		; \ If Y offset is between 0 and 2 then
			bcc @Next	; / Don't bounce them horizontally
			lda #2						; \ Set Object(y).XVelInt to 2
			sta ObjectXVelInt,y			; | (Send them flying rightward if they hit the bottom side)
			jsr ObjectYApplyXVelocity	; |
			jsr PlayBumpSFX				; / Also play Bump SFX
			bne @Next
		@TopSideHit:
			cmp #<-3	; \ If Y offset is between -3 and 0 then
			bcs @Next	; / Don't bounce them horizontally
			lda #<-2					; \ Set Object(y).XVelInt to -2
			sta ObjectXVelInt,y			; | (Send them flying leftward if they hit the top side)
			jsr ObjectYApplyXVelocity	; |
			jsr PlayBumpSFX				; / Also play Bump SFX
		@Next:
		dey
		bpl @Loop
	rts

PlayBumpSFX:
	lda SFX2Req	; \
	ora #$02	; | Play Bump SFX
	sta SFX2Req	; /
	rts

PropellerActivationCheck:
	lda ObjectBalloons,y
	bmieq @Next
	cpy #2					; \ Continue with normal check if Object Y is player
	bcc @NotEnemyPlatform	; /
	lda PropellerAngle,x	; \
	cmp #3					; | Continue with normal check if propeller isn't horizontal
	bne @NotEnemyPlatform	; /
	lda PropellerXPos,x		; \
	ssbc #10				; | Continue with normal check if
	cmp ObjectXPosInt,y		; | Propeller[X].XPos - 10 >= Object[Y].XPos
	bcs @NotEnemyPlatform	; /
	adc #4					; \ Continue with normal check if
	cmp ObjectXPosInt,y		; | Propeller[X].XPos - 6 < Object[Y].XPos
	bcc @NotEnemyPlatform	; /
	lda PropellerYPos,x		; \
	ssbc #28				; | Continue with normal check if
	cmp ObjectYPosInt,y		; | Propeller[X].YPos - 28 >= Object[Y].YPos
	bcs @NotEnemyPlatform	; /
	adc #4					; \ Continue with normal check if
	cmp ObjectYPosInt,y		; | Propeller[X].YPos - 24 < Object[Y].YPos
	bcc @NotEnemyPlatform	; /
	jsr EnemyLandOnPropeller	; If all checks passed, check for enemy resting on propeller
	@NotEnemyPlatform:
	lda ObjectXPosInt,y		; \
	cadc #8					; |
	ssbcx PropellerXPos		; | Temp12 = |Object[Y].XPos + 8 - Propeller[X].XPos|
	jsr GetAbsoluteValue	; |
	sta Temp12				; /
	lda ObjectYPosInt,y		; \
	cadc #12				; |
	ssbcx PropellerYPos		; | Temp13 = |Object[Y].YPos + 12 - Propeller[X].YPos|
	jsr GetAbsoluteValue	; |
	sta Temp13				; /
	lda PropellerAngle,x	; \
	cmp #3					; | If horizontal, skip the swap
	beq @SkipSwap			; /
	lda Temp12	; \
	pha			; |
	lda Temp13	; | Swap Temp13 and Temp12
	sta Temp12	; |
	pla			; |
	sta Temp13	; /
	@SkipSwap:
	lda Temp12	; \
	cmp #20		; | If Temp12 >= 20, don't activate
	bcs @Next	; /
	lda Temp13	; \
	cmp #11		; | If Temp13 >= 11, don't activate
	bcs @Next	; /
	lda #1					; \ Activate propeller
	sta PropellerState,x	; /
	lda #50						; \ Set countdown to 50
	sta PropellerCountdown,x	; /
	@Next:
	dey		; \ Try next object
	bmi :+	; / Return when finished
	jmp PropellerActivationCheck
	:rts

EnemyLandOnPropeller:
	phx	; Preserve X
	tya	; \ Transfer Y to X
	tax	; /
	inc LandingFlag
	jsr ManageObjectLanding
	plx	; Restore X
	rts

DrawPropellerX:
	lda PropellerAddrHi,x	; \
	sta PPUTempBlock		; | Upload starting at Propeller's PPU Address 
	lda PropellerAddrLo,x	; |
	sta PPUTempBlock+1		; /
	lda #3				; \ Upload 3 bytes at a time
	sta PPUTempBlock+2	; /
	ldy PropellerAngle,x	; Y = Propeller Angle
	lda PropellerTileUL,y	; \
	sta PPUTempBlock+3		; | Top row
	lda PropellerTileUM,y	; |
	sta PPUTempBlock+4		; |
	lda PropellerTileUR,y	; |
	sta PPUTempBlock+5		; /
	jsr @UploadRow
	lda PropellerTileML,y	; \
	sta PPUTempBlock+3		; | Middle row
	lda PropellerTileMM,y	; |
	sta PPUTempBlock+4		; |
	lda PropellerTileMR,y	; |
	sta PPUTempBlock+5		; /
	jsr @UploadRow
	lda PropellerTileLL,y	; \
	sta PPUTempBlock+3		; | Bottom row
	lda PropellerTileLM,y	; |
	sta PPUTempBlock+4		; |
	lda PropellerTileLR,y	; |
	sta PPUTempBlock+5		; /
	@UploadRow:
		phy	; Preserve Y
		lday PPUTempBlock	; \ Upload the current row
		jsr CopyPPUBlock	; /
		ply	; Resore Y
		lda PPUTempBlock+1	; \
		cadc #32			; |
		sta PPUTempBlock+1	; | Add 32 to PPU Address (Move 1 row down)
		bcc :+				; |
		inc PPUTempBlock	; /
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