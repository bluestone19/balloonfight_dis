InitializeSparks:
	ldx #1						; \
	@InitLoop:
		lda #<-1				; | Reset 2 Sparks
		sta SparkAnim,x			; |
		sta SparkLightning,x	; |
		dex						; |
		bpl @InitLoop			; /
	jsr SelectRandomCloud	; Select next Cloud to make a spark
InitializeSparkDifficulty:
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
	jmp UpdateCloudAttribute	; Blink selected cloud

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

SelectRandomCloud:	; Randomly select a cloud to send bolts?
	lda CloudCount	; \ If cloud count == -1, there's no clouds to just finish
	bpl @SkipSet	; / else randomly select a cloud
	@SelectCloud:
		sta SelectedCloud	; \ Select Cloud & finish
		rts					; /
		@SkipSet:
		jsr UpdateRNG
	@ModuloLoop:
		cmp CloudCount		; \ If RNG value <= amount of Clouds
		bcc @SelectCloud	; | then select cloud based on value
		beq @SelectCloud	; /
		clc				; \
		sbc CloudCount	; | A = A - CloudCount - 1
		jmp @ModuloLoop	; / Loop until <= CloudCount

ManageCloudBolt:
	lda FrameCounter	; \
	and #$7f			; | Every 128 frames...
	beq @Continue		; /
	:rts
	@Continue:
	dec SparkCountdown	; \ Do Lightning Bolt Countdown
	bne :-				; / ...once it reaches zero...
	ldx #0			; \
	lda SparkAnim,x	; |
	bmi @SlotFound	; | Only continue if there's a spark slot open
	inx				; |
	lda SparkAnim,x	; |
	bmi @SlotFound	; /
	lda #1				; \ If there's no room, set countdown back to 1
	sta SparkCountdown	; | and try again later
	rts					; /
	@SlotFound:
	ldy SelectedCloud
	sty CurrentSparkCloud
	bpl @ValidCloud
	rts
	@ValidCloud:
	lda #$80			; \
	sta SparkXPosFrac,x	; | Set spark's fractional position to 0.5 in both axes
	sta SparkYPosFrac,x	; /
	lda #0			; \ Set Spark's animation frame to 0
	sta SparkAnim,x	; /
	lda CloudXPos,y		; \
	sta SparkXPosInt,x	; | Move new spark to the selected cloud
	lda CloudYPos,y		; |
	sta SparkYPosInt,x	; /
	ldy SparkIntensity	; Y = Intensity level (Increases speed)
	jsr UpdateRNG			; \
	and #31					; | Set X Vel Fraction to random velocity to 0-31 (0-0.1211)
	adc SparkXVelFracData,y	; | + Defined value by intensity (0.375-0.6875)
	sta SparkXVelFrac,x		; /
	lda SparkYVelFracData,y	; \ Set Y Vel Fraction based on intensity
	sta SparkYVelFrac,x		; / (0.125-0.9375)
	lda SparkXVelIntData,y	; \ Set X Vel Int based on intensity
	sta SparkXVelInt,x		; / (Always 0) (With fraction, 0.375-0.8086)
	lda SparkYVelIntData,y	; \ Set Y Vel Int based on intensity
	sta SparkYVelInt,x		; / (0-1) (With fraction, 0.75-1.6875)
	jsr UpdateRNG			; \
	and #3					; |	Randomize lightning strike direction?
	sta SparkLightning,x	; /
	tay	; Y = Lightning direction
	lda SparkDirXOffsetData,y	; \
	cadcx SparkXPosInt			; | Offset Spark's X Position based on direction
	sta SparkXPosInt,x			; /
	lda SparkDirYOffsetData,y	; \
	cadcx SparkYPosInt			; | Offset Spark's Y Position based on direction
	sta SparkYPosInt,x			; /
	lda SparkDirXSignData,y	; \
	beq @SkipXReverse		; | Make spark's X velocity negative if direction is leftward
	jsr SparkBounceX		; /
	@SkipXReverse:
	lda SparkDirYSignData,y	; \
	beq @SkipYReverse		; | Make spark's Y velocity negative if direction is upward
	jsr SparkBounceY		; /
	@SkipYReverse:
	lda SparkIntensity	; \
	cmp #5				; | Every time lightning strikes, the next spark gets faster
	bcs @SkipIntensify	; | (Until intensity == 5)
	inc SparkIntensity	; /
	@SkipIntensify:
	lda #6				; \ SparkCountdown = 6 - SparkIntensity
	ssbc SparkIntensity	; | The more intense the sparks, the shorter the countdown between
	sta SparkCountdown	; / Minimum of 1
	lda SFX1Req	; \
	ora #$04	; | Play lightning strike SFX
	sta SFX1Req	; /
	jmp SelectRandomCloud	; Select the next cloud to shoot a spark then return

ManageCloudBlink:
	lda SparkCountdown	; \ If Lightning Bolt Countdown != 1
	cmp #1				; | then return
	bne :+				; /
	lda SparkAnim		; \ If Spark 0 doesn't exist
	bmi @AllowStrike	; / then prepare for one
	lda SparkAnim+1		; \ If Spark 1 doesn't exist
	bmi @AllowStrike	; / then prepare for one
	lda #2				; \ Else up the countdown to 2
	sta SparkCountdown	; /
	rts
	@AllowStrike:
		lda FrameCounter			; \
		and #127					; | If Frame Counter < 64
		cmp #64						; | then don't do anything
		bcc :+						; | If not equal to 64
		bne UpdateCloudAttribute	; / then don't play SFX
		lda SFX2Req	; \
		ora #$08	; | Play Sound Effect
		sta SFX2Req	; /
UpdateCloudAttribute:
	and #3	; \ Incoming A is flash frame
	tax		; / Limit to 0-3 and put in X
	lda CloudFlashAttrData,x	; \ Store attribute byte for new color into the PPU Block
	sta PPUTempBlock+3			; /
	ldx SelectedCloud	; \ Blink the selected cloud
	bmi :+				; /
	lda #$23			; \ Set Tile Attribute Palette
	sta PPUTempBlock	; / at PPUADDR[$23xx]
	lda CloudAttrAddrLo0,x	; \ Update first Attribute byte for this cloud
	sta PPUTempBlock+1		; /
	lda #1				; \ Upload 1 attribute byte at a time
	sta PPUTempBlock+2	; /
	jsr @UploadAttr	; Set 16x16 Tile Attribute 0
	lda CloudAttrAddrLo1,x	; \
	sta PPUTempBlock+1		; | Set 16x16 Tile Attribute 1
	jsr @UploadAttr			; /
	lda CloudAttrAddrLo2,x	; \
	sta PPUTempBlock+1		; | Set 16x16 Tile Attribute 2
	jsr @UploadAttr			; /
	lda CloudAttrAddrLo3,x	; \
	sta PPUTempBlock+1		; | Set 16x16 Tile Attribute 3
	@UploadAttr:			; /
		lday PPUTempBlock	; \ Copy Temp PPU Block
		jmp CopyPPUBlock	; / [$0057]
		:rts

CloudFlashAttrData:
	.BYTE %01010101	; Orange
	.BYTE %11111111	; Gray
	.BYTE %00000000	; Grass color
	.BYTE %11111111	; Gray

SparkDirXSignData:
	.BYTE $00,$00,$FF,$FF
SparkDirYSignData:
	.BYTE $FF,$00,$00,$FF
SparkDirXOffsetData:
	.BYTE 16,16,<-16,<-16
SparkDirYOffsetData:
	.BYTE <-34,34,34,<-34

SparkXVelFracData:
	.BYTE 96,112,128,144,160,176
SparkXVelIntData:
	.BYTE 0,0,0,0,0,0
SparkYVelFracData:
	.BYTE 192,240,32,80,128,176
SparkYVelIntData:
	.BYTE 0,0,1,1,1,1

ManageSparks:	; Only for Games A & B. Balloon Trip not included
	ldx #1
	@Loop:
		lda SparkAnim,x
		bpl @Continue
		jmp @ManageNextSpark
		@Continue:
		lda SparkLightning,x
		bmi @LeftScreenBounce
		tay	; Y = SparkLightning
		phx	; Preserve X
		ldx CurrentSparkCloud	; X = Cloud creating new spark
		lda CloudXPos,x							; \
		adc LightningStrikeXOffsets,y			; |
		sta OAM+$e3	;Lightning Strike Spr 0 X	  | Set Lightning Strike X Position, offset from cloud based on direction
		sta OAM+$e7	;Lightning Strike Spr 1 X	  |
		sta OAM+$eb	;Lightning Strike Spr 2 X	  /
		lda CloudYPos,x							; \
		adc LightningStrikeYOffsets,y			; |
		sta OAM+$e0	;Lightning Strike Spr 0 Y	  | Set Lightning Strike Y Position, offset from cloud based on direction
		adc LightningStrikeYDirectionOffsets,y	; |
		sta OAM+$e4	;Lightning Strike Spr 1 Y	  |
		adc LightningStrikeYDirectionOffsets,y	; |
		sta OAM+$e8	;Lightning Strike Spr 2 Y	  /
		tya		; \
		and #3	; |	X = Y AND 3
		tax		; /
		tya		; \
		lsrr 2	; | Y /= 4
		tay		; /
		lda FrameCounter	; \
		lsrr 2				; | Change lightning strike color every 2 frames
		bcs @SkipYInc		; /
		tya		; \
		adc #5	; | Add 5 to Y
		tay		; /
		@SkipYInc:
		lda LightningStrikeTileData0,y			; \
		sta OAM+$e1	;Lightning Strike Spr 0 Tile  |
		lda LightningStrikeTileData1,y			; |
		sta OAM+$e5	;Lightning Strike Spr 1 Tile  |
		lda LightningStrikeTileData2,y			; |
		sta OAM+$e9	;Lightning Strike Spr 2 Tile  /
		lda LightningStrikeAttributeData,x		; \
		sta OAM+$e2	;Lightning Strike Spr 0 Attr  |
		sta OAM+$e6	;Lightning Strike Spr 1 Attr  |
		sta OAM+$ea	;Lightning Strike Spr 2 Attr  /
		plx	; Restore X (Loop index)
		lda FrameCounter				; \
		and #7							; | Once every 8 frames
		bne @SkipSettingSparkLightning	; |
		lda SparkLightning,x			; | Increment SparkLightning by 4
		cadc #4							; |
		sta SparkLightning,x			; /
		cmp #$14						; \
		bcc @SkipSettingSparkLightning	; | If SparkLightning >= 14 set it to -1
		lda #<-1						; |
		sta SparkLightning,x			; /
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
			lda #<-1
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
		lda SparkAnimFrames,y
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
		lda #%00000000	; Set priority to 0 (In front of background)
		bcc @SetPriority
		lda #%00100000	; Set priority to 1 (Behind of background)
		@SetPriority:
		sta OAM+2,y
		@ManageNextSpark:
			dex
			bmi :+
			jmp @Loop
	:rts

UpdateSparkPos:
	lda SparkXVelFrac,x	; \ Update X Position (Frac)
	cadcx SparkXPosFrac	; |	X = X + XVel
	sta SparkXPosFrac,x	; /
	lda SparkXVelInt,x	; \
	adc SparkXPosInt,x	; | Update X Position (Int)
	sta SparkXPosInt,x	; /
	lda SparkYVelFrac,x	; \ Update Y Position (Frac)
	cadcx SparkYPosFrac	; | Y = Y + YVel
	sta SparkYPosFrac,x	; /
	lda SparkYVelInt,x	; \
	adc SparkYPosInt,x	; | Update Y Position (Int)
	sta SparkYPosInt,x	; /
	rts

SparkAnimFrames:
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
LightningStrikeTileData0:
	.BYTE $91,$93,$97,$97,$fc
	.BYTE $92,$95,$9a,$9a,$fc
LightningStrikeTileData1:
	.BYTE $fc,$94,$98,$98,$fc
	.BYTE $fc,$96,$9b,$9b,$fc
LightningStrikeTileData2:
	.BYTE $fc,$fc,$99,$99,$fc
	.BYTE $fc,$fc,$9c,$9c,$fc
LightningStrikeAttributeData:
	.BYTE %11000000
	.BYTE %01000000
	.BYTE %00000000
	.BYTE %10000000

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
		ssbc #8
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

SparkPlayerCollision:
	ldy #1
	@Loop:
		lda ObjectBalloons,y	; \
		bmi @Skip				; | Skip if Player Y has no balloons
		beq @Skip				; /
		lda PlayerInvincible,y	; \ Skip if Player Y is invincible
		bne @Skip				; /
		lda SparkXPosInt,x		; \
		ssbcy ObjectXPosInt		; | Skip if Player Y and Spark X are horizontally too far away
		jsr GetAbsoluteValue	; | |Spark[X].XPos - Player[Y].XPos| >= 8
		cmp #8					; |
		bcs @Skip				; /
		lda SparkYPosInt,x		; \
		ssbcy ObjectYPosInt		; | Skip if Player Y and Spark X are vertically too far away
		ssbc #8					; | |Spark[X].YPos - Player[Y].YPos - 8| >= 12
		jsr GetAbsoluteValue	; |
		cmp #12					; |
		bcs @Skip				; /
		lda #0					; \
		sta ObjectBalloons,y	; / Player Y's balloons = 0
		lda #1				; \
		sta ObjectStatus,y	; | Player Y's status = 1 
		sta PlayerFreeze,y	; / Player Y's freeze flag = 1
		lda #11				; \
		sta ObjectType,y	; / Player Y's type = 11
		lda #32					; \
		sta ObjectCountdown,y	; / Player Y's countdown = 32
		lda SFX1Req	; \
		ora #$80	; | Play Zapped SFX
		sta SFX1Req	; /
		lda #$f0			; \
		sta SparkYPosInt,x	; | Lightning Bolt X
		lda #$ff			; | disappears
		sta SparkAnim,x		; /
		@Skip:
		dey			; \ Check next player
		bpl @Loop	; /
	rts
