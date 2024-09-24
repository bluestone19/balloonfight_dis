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
