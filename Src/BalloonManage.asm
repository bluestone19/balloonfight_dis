InitBalloons:
	ldx #9						; \ Reset all 10 balloons
	@ClearLoop:
		lda #$ff				; | Status = -1
		sta BalloonStatus,x		; |
		lda #$f0				; | Y Positions = #$F0
		sta BalloonYPosInt,x	; |
		dex						; |
		bpl @ClearLoop			; /
	rts

ManageBonusBalloonSpawn:
	dec BonusBalloonDelay	; Countdown until next balloon spawns
	beq @Continue	; \ 
	rts				; / If not 0, return without spawning a new one
	@Continue:
	lda RNGOutput			; \
	and #63					; | Set delay until next spawn
	adc #40					; | 40-103 frames
	sta BonusBalloonDelay	; /
	ldx #9
	@SlotCheck:
		lda BalloonStatus,x	; \
		bmi @EmptySlotFound	; | If balloon status is negative, the space is free
		dex					; | otherwise check next
		bpl @SlotCheck		; /
	rts
	@EmptySlotFound:
	lda #0					; \
	sta BalloonStatus,x		; | Clear status
	sta BalloonXVelFrac,x	; | Set velocity to 0
	sta BalloonXVelInt,x	; /
	lda #128				; \
	sta BalloonXPosFrac,x	; | Set Fractional position to half
	sta BalloonYPosFrac,x	; /
	lda #208				; \ Set Balloon Y to 208px
	sta BalloonYPosInt,x	; /
	jsr UpdateRNG	; \
	and #3			; | Randomly choose pipe 0-3 to spawn from
	tay				; /
	lda BalloonSpawnXPosData,y	; \ Set balloon X pos to pipe position
	sta BalloonXPosInt,x		; /
	ldy #0
	lda RNGOutput			; \ Randomize drift starting acceleration
	sta BalloonAccelFrac,x	; /
	bpl @SetY				; \ If RNG was positive, set Int portion of acceleration to 0
	dey						; | If RNG was negative, set Int portion to 0xFF
	@SetY:					; |
	tya						; |
	sta BalloonAccelInt,x	; /
	dec BonusBalloonStock	; Decrease remaining balloons & return
	rts

ManageBonusBalloons:
	ldx #9
	@Loop:
		lda BalloonStatus,x
		bmi @Finish
		beq @SkipDrift
		lda BalloonXVelFrac,x	; \
		sta TempDriftVelLo		; | TempDriftVel = BalloonXVel
		lda BalloonXVelInt,x	; |
		sta TempDriftVelHi		; /
		jsr DivideDriftVel	; Divide TempDriftVel by 16
		lda BalloonAccelFrac,x	; \
		cadc TempDriftVelLo		; |
		sta BalloonAccelFrac,x	; | BalloonAccel += TempDriftVel
		sta TempDriftVelLo		; | TempDriftVel = BalloonAccel
		lda BalloonAccelInt,x	; |
		adc TempDriftVelHi		; |
		sta BalloonAccelInt,x	; |
		sta TempDriftVelHi		; /
		jsr DivideDriftVel	; Divide TempDriftVel by 16
		lda BalloonXVelFrac,x	; \
		ssbc TempDriftVelLo		; |
		sta BalloonXVelFrac,x	; | BalloonXVel -= TempDriftVel
		lda BalloonXVelInt,x	; |
		sbc TempDriftVelHi		; |
		sta BalloonXVelInt,x	; /
		lda BalloonXPosFrac,x	; \
		cadcx BalloonXVelFrac	; |
		sta BalloonXPosFrac,x	; | BalloonXPos += BalloonXVel
		lda BalloonXPosInt,x	; |
		adc BalloonXVelInt,x	; |
		sta BalloonXPosInt,x	; /
		@SkipDrift:
		lda BalloonYPosFrac,x	; \
		ssbc BalloonRiseSpeed	; | Move balloon upward by rise speed
		sta BalloonYPosFrac,x	; /
		bcs @SkipYPosDec		; \
		dec BalloonYPosInt,x	; / Move up by 1px if fractional part carried over
		@SkipYPosDec:
		lda BalloonYPosInt,x	; \
		cmp #$f0				; | If offscreen, unload
		beq @RemoveBalloon		; /
		cmp #168			; \ 
		bcs @Finish			; | When above Y Pos 168px,
		lda #1				; | Set status to 1 (Start drifting)
		sta BalloonStatus,x	; /
		bne @Finish	; Always taken: Finish update
		@RemoveBalloon:
			lda #<-1			; \ Set status to -1
			sta BalloonStatus,x	; /
		@Finish:
		jsr ManageBalloonXSprite	; Update sprite
		jsr CheckBalloonXCollision	; Check for collision with this balloon
		dex
		bmi :+
		jmp @Loop
	:rts

ManageBalloonXSprite:
	ldy BalloonStatus,x	; \ Y = Status + 1
	iny					; /
	lda BalloonAttributeData,y	; \ Store Attribute data for Objects in Temp13
	sta Temp13					; / Palette is 2, put behind BG if status == 0 (In pipe)
	txa			; \
	sta Temp12	; |
	asl			; | Y = X * 12
	adc Temp12	; | (Each balloon takes up 3 Objects, 4 bytes each)
	aslr 2		; |
	tay			; /
	lda BalloonYPosInt,x	; \
	sta OAM+$50,y			; | Top two sprites Y Pos matches the Balloon's Y Pos
	sta OAM+$54,y			; /
	cadc #8			; \ For bottom sprite, put 8px below
	sta OAM+$58,y	; /
	lda BalloonXPosInt,x	; \ First sprite, X Pos matches Balloon's position
	sta OAM+$53,y			; /
	cadc #4			; \ Bottom sprite offset 4px right
	sta OAM+$5b,y	; /
	cadc #4			; \ Top right sprite offset 4px more right (8px total)
	sta OAM+$57,y	; /
	lda Temp13		; \
	sta OAM+$52,y	; | Set attributes for Objects (palette & priority)
	sta OAM+$56,y	; |
	sta OAM+$5a,y	; /
	lda BalloonStatus,x		; \ If status is negative, draw balloon popped
	bmi PopBalloonXSprite	; /
	lda #$a8		; \
	sta OAM+$51,y	; | Set tile index for upper objects (top of balloon)
	lda #$a9		; |
	sta OAM+$55,y	; /
	lda FrameCounter	; \
	lsrr 4				; | For Balloon animation, index = (FrameCounter / 16) && 7
	and #7				; / Frame changes every 16 frames, index from 0-7
	stx Temp13	; Preserve X
	tax								; \
	lda BalloonStringIndexData,x	; | Get frame index, load tile index from table
	sta OAM+$59,y					; /
	lda OAM+$5a,y				; \
	eor BalloonStringAttrData,x	; | Set horizontal flip
	sta OAM+$5a,y				; /
	ldx Temp13	; Resore X
	rts

PopBalloonXSprite:
	lda #$f0				; \ Set position offscreen, so it disappears next frame
	sta BalloonYPosInt,x	; /
	lda #$ac		; \
	sta OAM+$51,y	; | Set the tile indexes for each object in this balloon
	lda #$ad		; | Top two become the popped pieces,
	sta OAM+$55,y	; |
	lda #$fc		; | Bottom object becomes invisible
	sta OAM+$59,y	; /
	rts

BalloonSpawnXPosData:
	.BYTE $20,$50,$a0,$d0
BalloonAttributeData:
	.BYTE $02,$22,$02
BalloonStringIndexData:
	.BYTE $aa,$ab,$ab,$aa,$aa,$ab,$ab,$aa
BalloonStringAttrData:
	.BYTE $00,$00,$40,$40,$40,$40,$00,$00

; This data isn't referenced anywhere. But based on the bytes and the context, it seems like it might've been used for the "END" sign.
; There's unused graphics carried over from Vs. Balloon Fight, for a sign that says "END" and is supposed to be tied to the last balloon.
; Maybe this was initially for that, but either it was forgotten or cut?
UnusedEndTicketData:
	.BYTE $fc,$fc,$df
	.BYTE $fc,$fc,$e0
	.BYTE $e2,$e1,$fc

CheckBalloonXCollision:
	ldy #1	;Check Balloon X against both players
	@Loop:
		lda ObjectBalloons,y	; \ If Player Y has no Balloons, try next
		bmieq @Next				; /
		lda BalloonStatus,x	; \ If this Balloon is not currently real, skip
		bmi :+				; /
		lda ObjectYPosInt,y	; \
		cmp #192			; | If this Player's Y Position >= 192, then no collision
		bcs @Next			; /
		ssbcx BalloonYPosInt	; \
		jsr GetAbsoluteValue	; | If |Player.Y - Balloon.Y| >= 24, then no collision
		cmp #24					; |
		bcs @Next				; /
		lda ObjectXPosInt,y		; \
		ssbcx BalloonXPosInt	; | If |Player.X - Balloon.X| >= 16, then no collision
		jsr GetAbsoluteValue	; |
		cmp #16					; |
		bcs @Next				; /
		lda #<-1			; \ If all the checks passed so far,
		sta BalloonStatus,x	; / Pop the balloon
		lda P1BonusBalloons,y	; \
		cadc #1					; | Add a Balloon to the count for the player that popped it
		sta P1BonusBalloons,y	; /
		lda #$02	; \ Play Pop SFX
		sta SFX1Req	; /
		rts	; If popped one player, it can't be popped by the other too
		@Next:
		dey	; Try next player
		bpl @Loop
	:rts
