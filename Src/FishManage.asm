FishRiseAnimData:
	.BYTE $01,$02,$03,$03
FishFallAnimData:
	.BYTE $02,$01,$ff,$03,$04,$05,$06,$ff

ManageFishAnimation:
	lda FishFrameTime	; \
	lsrr 3				; | X = FishFrameTime / 8
	tax					; /
	lda FishAnimation		; \
	bne @LoadAnim1			; | If Fish Animation? == 0
	lda FishRiseAnimData,x	; / set Fish Status
	jmp @SetStatus
	@LoadAnim1:
		lda FishFallAnimData,x	; If Fish Animation? != 0
	@SetStatus:
	sta ObjectStatus+8	; Update Fish Status
	ldx #8
	jsr DrawObjectX
	lda FishTargetEaten	; \ If Fish Target Eaten Flag
	beq :+				; / is set
	ldx FishTargetID	; X = Fish Target
	lda FishFrameTime		; \
	cmp #32					; |
	bne @SkipTargetEat		; | If Fish Frame Time == 32
	lda #<-1				; | then target is eaten
	sta ObjectBalloons,x	; | (Balloons = -1)
	bmi @Finish				; /
	@SkipTargetEat:
	bcs :+	; If Fish Frame Time < $20
	lda ObjectDirection+8	; \ Depending on Fish Direction
	bne @MoveLeft			; /
	lda ObjectXPosInt+8	; \ Move Fish 4 pixels to the right
	cadc #4				; /
	bne @FinishMove
	@MoveLeft:
	lda ObjectXPosInt+8	; \ Move Fish 4 pixels to the left
	ssbc #4				; /
	@FinishMove:
	sta ObjectXPosInt,x
	lda ObjectYPosInt+8	; \ Fish Target's Y position =
	ssbc #10			; | (Fish Y - 10)
	sta ObjectYPosInt,x	; /
	@Finish:
	jsr DrawObjectX
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

ManageFishEat:
	lda FishYDirection	; \ If Fish Y Direction == Up
	bne @FishDescend	; /
	dec ObjectYPosInt+8	; Fish Y goes up by 1 pixel
	lda ObjectYPosInt+8	; \
	cmp #196			; | If Fish Y Position is about
	bcs @FinishYMove	; | to go above 196
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
	bne @NoTarget		; /
	ldx FishTargetID		; \
	lda ObjectBalloons,x	; | If Target exists...
	bmi @NoTarget			; / (has balloons)
	lda ObjectYPosInt,x	; \ If the target is above
	cadc #16			; | the fish by 16 pixels
	cmp ObjectYPosInt+8	; |
	bcc @NoTarget		; /
	ldy ObjectType,x			; \
	lda PostFishTargetType,y	; | Change Target Object Type
	sta ObjectType,x			; /
	lda #0					; \ Insta Kill
	sta ObjectStatus,x		; | Target Object Status = 0
	sta ObjectBalloons,x	; / Target Object Balloons = 0
	lda MusicReq	; \
	ora #$40		; | Play Fish Jingle
	sta MusicReq	; /
	inc FishTargetEaten	; Set Fish Target Eaten Flag
	@NoTarget:
	lda FishAnimation	; \ Fish Animation? != 0
	beq :+				; /
	lda FishFrameTime	; \
	cmp #40				; | If Fish Frame Time == 40
	beq @SetFishYMin	; | OR
	cmp #48				; | If Fish Frame Time == 48
	bne :+				; |
	@SetFishYMin:
	lda #204			; | then
	sta ObjectYPosInt+8	; / Fish Y Position = 204
	:rts

PostFishTargetType:
	.BYTE 8,9,10,11
	.BYTE 8,9,10,11
	.BYTE 8,9,10,11

FishFollowTarget:
	lda FishYDirection	; \ If Fish Direction is Up
	bne :+				; /
	ldx FishTargetID		; \
	lda ObjectBalloons,x	; | Does target still exist?
	bmi @FishRecede			; /
	lda ObjectYPosInt,x	; \
	cmp #$b4			; | Is target still below Y pos #$B4?
	bcc @FishRecede		; /
	lda ObjectXPosInt,x	; \
	cmp #$40			; | Is target between
	bcc @FishRecede		; | X positions #$40 and #$B1?
	cmp #$b1			; | If so, teleport fish
	bcc @MoveFish		; /
	@FishRecede:
		lda #$30			; \ Else
		sec					; | Fish Frame Time = $30 - itself
		sbc FishFrameTime	; |
		sta FishFrameTime	; /
		inc FishYDirection	; Set Fish Direction to Down
		bne :+
	@MoveFish:
		lda ObjectXPosInt,x	; \ Teleport Fish
		sta ObjectXPosInt+8	; / to Object's X position
		lda ObjectDirection,x	; \ Change Fish Direction
		sta ObjectDirection+8	; / to Object's Direction
		:rts

FishManage:
	lda ObjectStatus+8		; \ If Fish Status >= 0
	bpl ContinueFishAttack	; / then handle eating
	jsr FishMove	
	jsr FishSearchTarget
	lda FishTargetID	; \ If Target found
	bpl PlayFishSFX		; / then handle Fish attack
	rts
	PlayFishSFX:
		lda #$40	; \ Play Fish Eating SFX
		sta SFX3Req	; /
	ContinueFishAttack:
		jsr FishFollowTarget	; Handle Fish Teleport to Target
		jsr ManageFishEat	; Handle Fish Target Eating
		jmp ManageFishAnimation	; Handle Fish Target Eating Movement & Return