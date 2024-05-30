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