;Balloon Fight (JP/USA/EU) Disassembly - PRG ROM
;-----------------------
;Starting disassembly with nesrevplus v0.3b (2018-09-18 16:39, 5ms)
;Manual disassembly and commenting by LuigiBlood
;CA65 Conversion and multi-region support by Bluestone19

REGION	.set 0	;0 = JP, 1 = US, 2 = EU

.p02

.include "Macros.asm"
.include "MemoryDefines.asm"

.SEGMENT "CODE"
Reset:
	lda #0		; \
	sta PPUCTRL	; | Initialize PPU registers
	sta PPUMASK	; /

.IF REGION >= 1
FrameWaitLoop1:
	lda PPUSTATUS		; \
	bpl FrameWaitLoop1	; |
FrameWaitLoop2:
	lda PPUSTATUS		; | Get to next V-Blank
	bmi FrameWaitLoop2	; |
.ENDIF
FrameWaitLoop3:
	lda PPUSTATUS		; |
	bpl FrameWaitLoop3	; /

	sei			; Disable Interrupts
	cld			; Clear Decimal Mode (Useless on NES/Famicom)
	ldx #$ff	; \ Initialize Stack Pointer
	txs			; / to $01FF

	ldx #$12	; \
	lda #0		; | Initialize part of RAM
PartialRamInit:
	sta $00,x	; | $0012 - $00FF
	inx			; |
	bne PartialRamInit	; /

	ldx #$02			; \
HALCheckLoop:
	lda HALStringMem,x			; | Check if system was reset
	cmp HALString,x		; | by checking if $07FA has "HAL" string
	bne FullRAMInit		; |
	dex					; | If found, then skip to the end
	bpl HALCheckLoop	; | Else, proceed with initialization code
	bmi FinishRAMInit	; / (BUG: It gets rewritten during gameplay so it never skips.)

FullRAMInit:
	ldx #$00		; \
	txa				; | Initialize parts of RAM
RAMInitLoop:
	sta $00,x		; | $0000 - $00FF: Main RAM
	sta $0700,x		; | $0700 - $07FF: Balloon Trip RAM
	inx				; |
	bne RAMInitLoop	; /

	lda #50						; \
	sta Temp15					; | - Initialize Balloon Trip Ranking Scores -
BTRankScoreInit:
	lda #50						; | Add +50 to Score
	jsr ld6de_scoreadd			; | and update
	lda #0						; |
	sta $46						; | Clear Status Bar Update Flag
	jsr lc579_rankscoreupdate	; | Update Balloon Trip Rank 01 to 50 Scores
	dec Temp15					; | with multiples of 50
	bne BTRankScoreInit			; /

	ldx #14					; \
TopScoreInitLoop:
	lda DefaultTopScores,x	; | Write default High Scores
	sta $0629,x				; | for each game mode
	dex						; |
	bpl TopScoreInitLoop	; /

	ldx #$04		; \
P1ScoreInit:
	lda #$00		; | Initialize Player 1 Score
	sta P1Score,x	; |
	dex				; |
	bpl P1ScoreInit	; /

	lda #$00			; \
	jsr ld6de_scoreadd	; / Update Score

	ldx #2				; \
HALWriteLoop:
	lda HALString,x		; | Write "HAL" to $07FA
	sta HALStringMem,x			; | for future Reset checking
	dex					; |
	bpl HALWriteLoop	; /
FinishRAMInit:
	lda #%00011110		; \ PPUMASK Shadow
	sta PPUMASKShadow	; / Enable Background and Sprites
	lda #%10010000		; \ PPUCTRL Shadow
	sta PPUCTRLShadow	; / Enable NMI at V-Blank, BG Pattern Table at $1000
	jmp lf1d4	; Start

HALString:
.BYTE "HAL"

DefaultTopScores:
.BYTE 0,0,0,1,0	;1 Player Mode
.BYTE 0,0,0,1,0	;2 Player Mode
.BYTE 0,0,5,2,0	;Balloon Trip Mode

;----------------------
; NMI code
;----------------------

NMI:
	pha			; \
	phx			; | Push A, X, Y
	phy			; /
	lda #0		; \
	sta OAMADDR	; | Upload OAM Buffer
	lda #2		; | $0200 - $02FF to OAM (via DMA)
	sta OAMDMA	; /
	lda $52						; \ Check for PPU Buffer Upload
	cmp $53						; |
	beq lc0ac					; | If Position in buffer != Buffer Size
	jsr lc17c_uploadppubuffer	; / Then Upload PPU Buffer
lc0ac:
	jsr ld60d_updatestarbganim	; Update Star Animation
	jsr ld798_updatestatusbar	; Update Status Bar
	inc $19		; Increment Frame Counter
	lda #$20	; \
	sta PPUADDR	; | PPUADDR = $2000
	lda #$00	; | (Nametable 0)
	sta PPUADDR	; /
	lda #$00		; \
	sta PPUSCROLL	; | PPUSCROLL = X:0, Y:0
	sta PPUSCROLL	; /
	jsr GotoAudioMain	; Manage Audio
	lda #$01	; \ Set Video Frame Done Flag
	sta $02		; /

	lda $16		; \ If Game Mode is Balloon Fight mode
	beq lc0f1	; / then end NMI
lc0d1:
	lda $2002	; \ Wait for V-Blank End
	bmi lc0d1	; /
.IF REGION <= 1		; NTSC Timing for BT Scroll Shear (JP/US)
	ldx #$04
	ldy #$c6
.ELSEIF REGION = 2	; PAL Timing for BT Scroll Shear (EU)
	ldx #$08
	ldy #$10
.ENDIF
lc0da:
	dey			; \ Wait X*Y loops
	bne lc0da	; | for updating the scrolling
	dex			; | mid frame (under scoreboard)
	bne lc0da	; /

	lda $18		; \
	ora $00		; | PPUCTRL = [$0018] | [$0000]
	sta $2000	; /
	lda $17		; \
	sta PPUSCROLL	; | Input X scroll value
	lda #$00	; | PPUSCROLL = X:[$17], Y:0
	sta PPUSCROLL	; /

lc0f1:
	ply			; \
	plx			; | Pull A, X, Y
	pla			; /
	rti


;----------------------
; BRK code
;----------------------

BRKLoop:
	jmp BRKLoop	; Loop


;----------------------
; NMI/PPU Management code
;----------------------

lc0fa_disablenmi:
	lda $00		; \
	and #$7f	; / Disable NMI
lc0fe:
	sta $2000	; \
	sta $00		; | Update PPUCTRL
	rts			; /

lc104_enablenmi:
	lda $00		; \
	ora #$80	; | Enable NMI
	bne lc0fe	; /

lc10a_clearppumask:
	lda #$00	; Clear PPUMASK
lc10c_writeppumask:
	pha
	jsr lf465_clearframeflag
	pla
	sta $2001	; Update PPUMASK
	rts

lc115:
	lda $01		; Write PPUMASK Shadow to PPUMASK
	bne lc10c_writeppumask
lc119:
	jsr lc154_newppublock
	ldy #$00	; \
lc11e:
	lda $0057,y	; | Put PPU Data
	sta $0300,x	; | to upload to Nametable 1
	inx			; |
	iny			; |
	cpy $56		; |
	bne lc11e	; /
	stx $53		; Update PPU Buffer Size
	rts
;-----------------------

lc12d_copypputempblock:
	lda #$57	; \
	ldy #$00	; / [$21] = $0057
lc131_copyppublock:
	sta $21		; \ Update Pointer
	sty $22		; / [$21] = $YYAA
	txa
	pha
	ldy #$02	; \
	lda ($21),y	; | Get Data Size + 3
	clc			; | to include Address and Size info
	adc #$03	; |
	sta $12		; /
	ldx $53		; Get PPU Buffer Size
	ldy #$00	; \
lc144:
	lda ($21),y	; | Copy PPU Upload Block
	sta $0300,x	; |
	inx			; |
	iny			; |
	cpy $12		; |
	bne lc144	; /
	stx $53		; Update PPU Buffer Size
	pla
	tax
	rts
;-----------------------

lc154_newppublock:
	ldx $53		; X = PPU Buffer Size
	lda #$00	; \
	sta $12		; |
	lda $55		; |
	asl			; | Prepare PPUADDR
	asl			; | [$55] = 000XXDDD
	asl			; | 
	asl			; | PPUADDR_H = 001000XX
	rol $12		; | PPUADDR_L = DDD00000 | [$54]
	asl			; |
	rol $12		; |
	ora $54		; |
	pha			; /
	lda $12		; \
	ora #$20	; | Put PPUADDR High Byte
	sta $0300,x	; / (From Nametable 1)
	inx			; \
	pla			; | Put PPUADDR Low Byte
	sta $0300,x	; /
	inx			; \
	lda $56		; | Put Upload Size
	sta $0300,x	; /
	inx			; \ Return:
	rts			; / X = Current PPU Buffer Address
;-----------------------

lc17c_uploadppubuffer:
	phy			; \ Push Y & X
	phx			; /
	jsr lc188	; 
	plx			; \ Pull X & Y
	ply			; /
	rts
;-----------------------

lc188:
	ldx $52		; Get Current Position in PPU Upload Buffer
	lda $0300,x	; \
	inx			; |
	sta $50		; |
	sta PPUADDR	; | Get PPU Address
	lda $0300,x	; | And Set PPUADDR
	inx			; |
	sta PPUADDR	; /
	ldy $0300,x	; Get Upload Size to Y
	inx
lc19e:
	lda $0300,x	; \
	inx			; |
	sta PPUDATA	; | Upload Y bytes to PPU
	dey			; |
	bne lc19e	; /

	lda $50		; \
	cmp #$3f	; | If Upload Address != $3FXX (Palette Data)
	bne lc1be	; / Then Skip this section
	lda #$3f	; \
	sta PPUADDR	; |
	lda #$00	; | PPUADDR = $3F00
	sta PPUADDR	; | PPUADDR = $0000
	sta PPUADDR	; | ???
	sta PPUADDR	; /
lc1be:
	stx $52		; \
	cpx $53		; | If PPU Buffer Position != PPU Buffer Size
	bne lc188	; / Then Upload more data
	rts


;----------------------
; Balloon Trip Game Mode code
;----------------------

lc1c5:
	lda #$20	; \ Play Balloon Trip Music
	sta $f2		; /
	jsr lc527_setbonuspts10
	jsr lc539_rankupdate
	lda #$ff	; \ No platforms?
	sta $cd		; /
	tpa BTStartLayout, $23
	lda #$80	; \ Set Player 1 X Position
	sta $91		; / to #$80
	sta $0488	; Set Balloon Trip Starting Platform X Position to #$80
	lda #$70	; \ Set Player 1 Y Position
	sta $9a		; / to #$70
	jsr lcd4a_initballoons
	lda #$00
	sta $41		; 0 Lives to Player 1
	sta $c9		; Init Tile Scroll Counter
	sta $ca		; Init Screen Scroll Counter
	sta $ba		; Init 10 Screens Counter?
	sta $c5		; Init Scrolling Lock Time
	sta $c8		; Phase Type = 0
	jsr lf4a5_initfish
	ldx #$13	; \
lc1fc:
	lda #$ff	; | Reset All Lightning Bolts
	sta $0530,x	; | Animation Frame = -1
	lda #$f0	; |
	sta $04a4,x	; | No Vertical Direction?
	dex			; |
	bpl lc1fc	; /
lc209:
	jsr lf470_pause
	jsr le691_objectmanage
	lda $c5		; If Screen is locked
	bne lc216	; then don't manage Fish
	jsr lc6f9_fishmanage
lc216:
	lda $19		; \ Manage Screen Scrolling and stuff
	lsr			; | every 2 frames...
	bcs lc21e	; /
	jmp lc2d0
lc21e:
	lda $c5		; \ ...unless the scrolling
	beq lc227	; | is locked
	dec $c5		; /
	jmp lc2d0
lc227:
	lda $17		; \ If the scrolling X position
	bne lc231	; | is 0 then
	lda $18		; | Toggle between
	eor #$01	; | nametable 0 and 1
	sta $18		; /
lc231:
	dec $17		; Scroll 1 pixel from the left
	lda $0488	; \ Skip if starting platform
	beq lc24d	; / does not exist
	inc $0488	; Scroll starting platform 1px to the right
	lda $0488	; \
	cmp #$f0	; | If Starting Platform reaches
	bcc lc247	; | X position #$F0
	lda #$00	; | then disappear
	sta $0488	; /
lc247:
	lda $bd		; \ If Player is invincible
	beq lc24d	; | (has not yet moved)
	inc $91		; / then scroll 1px to the right
lc24d:
	ldx #$07
lc24f:
	lda $055d,x	; \ If balloon doesn't exist
	bmi lc26d	; / skip to the next one
	inc $0567,x	; Scroll balloon 1px to the right
	lda $0567,x	; \
	cmp #$f8	; | If balloon's X position
	bne lc26d	; | reaches #$F8
	lda #$ff	; | then make it disappear
	sta $055d,x	; |
	lda #$f0	; |
	sta $057b,x	; | And reset the balloon counter
	lda #$00	; |
	sta $05ce	; /
lc26d:
	dex			; \ Check next balloon
	bpl lc24f	; /

	ldx #$13
lc272:
	lda $0530,x	; \ If Lightning Bolt doesn't exist
	bmi lc289	; / then skip to next one
	inc $0490,x	; Scroll Bolt 1 pixel to the right
	lda $0490,x	; \
	cmp #$f8	; | If bolt's X position
	bcc lc289	; | reaches #$F8
	lda #$f0	; | then make it disappear
	sta $04a4,x	; |
	sta $0530,x	; /
lc289:
	dex			; \ Check next bolt
	bpl lc272	; /

	lda $17		; \ Every 8 pixel scrolled
	and #$07	; |
	bne lc2d0	; /
	ldx $88		; \ If Player still has balloons
	dex			; |
	bmi lc2d0	; /
	lda #$00			; \
	sta $3e				; | Add 10 to Player 1 Score
	lda #$01			; |
	jsr ld6de_scoreadd	; /
	inc $c9		; Increment Tile Scroll Counter
	lda $c9		; \
	and #$1f	; | If 32 tiles have been scrolled
	bne lc2bc	; /
	inc $ca		; Increment Screen Scroll Counter
	lda $ca		; \
	cmp #$0a	; | If 10 screens have been scrolled
	bne lc2bc	; /
	lda #$02	; \ Then reset to Screen #$02
	sta $ca		; /
	ldy $ba		; \
	iny			; | Increment
	tya			; | Lightning Bolt Intensity Level
	and #$03	; |
	sta $ba		; /
lc2bc:
	ldx $ca			; \
	lda lc3bf,x		; |
	asl				; |
	tay				; | Manage Screen Layout
	lda lc3b5,y		; | Jump to subroutines
	sta $25			; | dedicated to each screen layout
	lda lc3b5+1,y	; |
	sta $26			; |
	jsr lc3b2		; /
lc2d0:
	ldx #$07
lc2d2:
	lda $055d,x	; \ If Balloon X does not exist
	bmi lc2ef	; / then skip collision check
	jsr lcece_ballooncollision
	lda $05cd	; \
	beq lc2ef	; | Every balloon touched
	dec $05cd	; | counts towards the
	inc $05ce	; / main counter
	phx
	lda $0559			; \ Add Score
	jsr ld6de_scoreadd	; /
	plx
lc2ef:
	jsr lce2f_balloonxspritemanage
	dex			; \ Check next balloon
	bpl lc2d2	; /

	ldx #$13
lc2f7:
	lda $0530,x	; \ If Lightning Bolt exists?
	bmi lc317	; /
	lda $c5		; \ If Scrolling is locked
	bne lc314	; /
	jsr lc9b6_boltupdate	; Update Lightning Bolt Position
	lda $04a4,x	; \
	cmp #$02	; | If Y pos < #$02
	bcs lc30d	; | then
	jsr lca4f	; / Bounce Lightning Bolt Vertically
lc30d:
	cmp #$d8	; \ If Y pos >= #$D8
	bcc lc314	; | then
	jsr lca4f	; / Bounce Lightning Bolt Vertically
lc314:
	jsr lcb1c_bolt_playercollision
lc317:
	lda $19		; \
	and #$07	; | Get Lightning Bolt
	lsr			; | Animation Frame Tile
	tay			; |
	lda lc9dd,y	; | (Unused Note: Animation is 8 frames
	pha			; / but only half of them are used.)
	lda $19		; \
	lsr			; | Every 2 frames...
	txa			; |
	bcc lc32d	; /
	sta $12		; \
	lda #$13	; | ...count from the end
	sbc $12		; /
lc32d:
	asl			; \
	asl			; | Get OAM Sprite Address
	tay			; /
	pla			; \ Update Lightning Bolt Sprite
	sta $02b1,y	; / Tile ID
	lda $04a4,x	; \
	sta $02b0,y	; / Y position
	lda $0490,x	; \
	sta $02b3,y	; / X position
	lda #$00	; \
	sta $02b2,y	; / Use Palette 4
	dex			; \ Loop to next bolt
	bpl lc2f7	; /

	lda $05ce	; \ If you touched
	cmp #$14	; | 20 balloons in a row...
	bcc lc36f	; /
	inc $47				; \ Add 10000
	lda #$00			; | to score
	jsr ld6de_scoreadd	; /
	dec $47		; Reset score to add
	lda #$10	; \ Play Bonus Phase Perfect jingle
	sta $f2		; /
	inc $c8		; Set Bonus Phase Type
	jsr ld3ed_setpalette	; Update Balloon Palette
	jsr lc527_setbonuspts10
	dec $c8		; Reset to Normal Phase
	ldx #$64				; \ Wait for 100 frames
	jsr lf45e_waityframes	; /
	lda #$20	; \ Play Balloon Trip Music
	sta $f2		; /
lc36f:
	ldx #$f0	; \ If Balloon Trip Starting Platform
	lda $0488	; | X position is 0
	beq lc378	; / then don't make it appear on screen
	ldx #$88	; \ At Y position #$88:
lc378:
	stx $0200	; | Display Left and Right
	stx $0204	; / sides of Platform
	sta $0203	; \
	clc			; | Display Left and Right
	adc #$08	; | sides at current X position
	sta $0207	; /
	lda $19		; \
	and #$03	; | Switch between palettes
	sta $0202	; | on platform
	sta $0206	; /
	ldx #$e3	; \
	stx $0201	; | Display Tile #$E3 and #$E4
	inx			; |
	stx $0205	; /
	lda $88		; \ If Player is dead (no balloons)
	bmi lc3a1	; / then game over
	jmp lc209	; else game loop
lc3a1:
	jsr lc579_rankscoreupdate
	lda #$01	; \ Play SFX
	sta $f0		; /
	jsr lf465_clearframeflag
	lda #$02	; \ Play Stage Clear jingle
	sta $f2		; /
	jmp lf36a	; Put Game Over on Screen
lc3b2:
	jmp ($0025)

lc3b5:	; Screen Layout Subroutines
.WORD lc3c9, lc3f7, lc43e, lc45f, lc45e

lc3bf:	; Screen Layout Order
.BYTE 0,0,2,2,2,2,2,4,3,1

lc3c9:
	ldy #$00	; \
	lda ($23),y	; | Read Layout Data Byte
	inc $23		; | [$23] and Increment
	bne lc3d3	; | Bit format: BL0YYYYY
	inc $24		; / B = Balloon, L = Lightning Bolt, Y = Tile Y Position
lc3d3:
	tax
	beq lc3f6	; If Layout Byte = 00 then return
	asl			; \
	asl			; | Set Y position
	asl			; |
	sta $15		; /
	lda #$00	; \
	sta $14		; /
	txa
	and #$c0	; \
	cmp #$80	; | If bit B is set
	bne lc3ec	; | then
	jsr lc46b	; / Spawn Balloon
	jmp lc3c9	; Repeat
lc3ec:
	cmp #$00	; \ If bit L is set
	bne lc3f6	; | then
	jsr lc486	; / Spawn Lightning Bolt
	jmp lc3c9	; Repeat
lc3f6:
	rts

lc3f7:
	jsr lf1b3_rng	; \
	and #$7f		; |
	cmp #$04		; |
	bcc lc40c		; | If RNG value is between
	cmp #$18		; | 4 and 23
	bcs lc40c		; |
	asl				; | then spawn Balloon
	asl				; | at Tile Y position value
	asl				; |
	sta $15			; |
	jsr lc46b		; /
lc40c:
	jsr lf1b3_rng	; \
	and #$3f		; |
	cmp #$02		; | If RNG value is between
	bcc lc439		; | 2 and 23
	cmp #$18		; |
	bcs lc439		; |
	asl				; | then spawn Lightning Bolt
	asl				; | at Tile Y position value
	asl				; | and set Y velocity value
	sta $15			; | using BTSparkVelOptions value
	jsr lf1b3_rng	; | (depending on full loop)
	and #$3f		; | + RNG value up to 63
	ldx $ba			; |
	adc BTSparkVelOptions,x		; |
	sta $14			; |
	jsr lc486		; /
	jsr lf1b3_rng	; \
	lsr				; | Make Y velocity value negative
	bcc lc40c		; | 50% of the time
	jsr lca4f		; |
	jmp lc40c		; /
lc439:
	rts
BTSparkVelOptions:
.BYTE $20,$30,$40,$60

lc43e:
	jsr lf1b3_rng	; \ Only bits 00XX0000 have to be set
	and #$cf		; | else spawn something at random
	bne lc3f7		; /
	ldy $89		; \ If Player 2 exists (???)
	iny			; |
	bne lc3f7	; /
	lda #$e6	; \ Player 2 Y Position = $E6
	sta $9b		; /
	lda $1b		; \
	and #$7f	; | Player 2 X Position = $40 + RNG (up to 127)
	adc #$40	; |
	sta $92		; /
	lda #$80	; \ Player 2 Balloons = -128
	sta $89		; /
	lda #$00	; \ Player 2 Status = 00
	sta $80		; /
lc45e:
	rts

lc45f:
	jsr lc40c		; Randomly Spawn Lightning Bolt
	jsr lf1b3_rng	; \
	and #$7f		; | Set X Velocity (Frac)
	sta $0508,x		; / RNG up to 127
	rts

lc46b:
	ldx #$07	; \
lc46d:
	lda $055d,x	; | Find Balloon that
	bmi lc476	; | hasn't spawned yet
	dex			; |
	bpl lc46d	; /
	rts
lc476:
	lda #$01	; \ Set Balloon Type/GFX? to 01
	sta $055d,x	; /
	lda #$00	; \ Set Balloon X position to 00
	sta $0567,x	; /
	lda $15		; \ Set Balloon Y position to [$15]
	sta $057b,x	; /
	rts

lc486:
	ldx #$13	; \
lc488:
	lda $0530,x	; | Find Lightning Bolt
	bmi lc491	; | that hasn't spawned yet
	dex			; |
	bpl lc488	; /
	rts
lc491:
	lda #$00
	sta $0530,x	; Set Animation Frame to 00
	sta $0490,x	; Set X position to 00
	sta $04f4,x	; Set Y velocity to 00
	sta $0508,x	; \ Set X velocity to 00 (Int and Frac)
	sta $04e0,x	; /
	lda $14		; \ Set Y velocity (Frac) to [$14]
	sta $051c,x	; /
	lda $15		; \ Set Y position to [$15]
	sta $04a4,x	; /
	rts

BTStartLayout:	; Screen Premade Layout Data
.BYTE $00,$00
.BYTE $09,$00,$08,$8c,$00,$07,$18,$00,$18,$00,$19,$00,$1a,$00,$84
.BYTE $94,$1a,$00,$1a,$00,$1a,$00,$0b,$12,$00,$0c,$13,$00,$0d,$14
.BYTE $00,$14,$00,$00,$90,$00,$07,$00,$07,$8c,$96,$00,$08,$00,$09
.BYTE $00,$00,$18,$00,$17,$00,$16,$00,$00,$00,$00,$00,$00,$8a,$90
.BYTE $00,$00,$00,$08,$00,$09,$98,$00,$0a,$00,$00,$00,$86,$8a,$15
.BYTE $00,$14,$00,$8e,$13,$00,$00,$03,$0d,$00,$0d,$0e,$00,$0c,$0d
.BYTE $00,$0d,$19,$00,$86,$92,$00,$00,$98,$00,$00,$0a,$12,$00,$09
.BYTE $13,$00,$08,$14,$00,$07,$15,$00,$07,$16,$00,$07,$00,$00,$00

lc527_setbonuspts10:
	jsr ld0e2_setbonusphase	; Set up Balloon Points
	asl $0559	; \
	lda $0559	; | ([$0559] * 2) * 5
	asl			; | Multiply Balloon Points
	asl			; | by 10
	adc $0559	; |
	sta $0559	; /
	rts

lc539_rankupdate:
	lda #$00	; \ Set Balloon Trip Rank 01 (00 + 1)
	sta $12		; /
lc53d:
	lda $12		; \
	asl			; | 
	asl			; | Setup Pointer to
	adc $12		; | 0700 + (Rank)*5
	sta $1d		; |
	lda #$07	; |
	sta $1e		; /
	ldy #$04	; \
lc54b:
	lda ($1d),y	; | Check each digit
	cmp $0003,y	; | If P1 Score Digit < Rank Score
	bcc lc563	; | then stop
	bne lc559	; | If >= then check next Rank Score
	dey			; |
	bpl lc54b	; / Else check next digit
	bmi lc563	; When done, update current Rank
lc559:
	inc $12		; \
	lda $12		; | If (Rank+1) != 50 (!)
	cmp #$32	; | then check the next rank
	bne lc53d	; | else update current rank
	dec $12		; /
lc563:
	inc $12				; \
	lda $12				; |
	pha					; | Update Current Rank variable
	sta $43				; |
	ldy #$0a			; |
	jsr ld77c_divide	; | (Rank+1) / 10
	sta $4a				; | Write second digit
	lda $43				; |
	sta $49				; | Write first digit (modulo)
	pla					; |
	sta $12				; /
	rts

lc579_rankscoreupdate:
	jsr lc539_rankupdate	; Update Balloon Trip Rank
	dec $12					; \
	lda #$31				; | A = (Rank - 49)
	sec						; |
	sbc $12					; /
	sta $13		; \
	asl			; |
	asl			; | Y = A * 5
	adc $13		; |
	tay			; /
	lda $12		; \
	asl			; |
	asl			; | [$1D] = Pointer to Score Rank
	adc $12		; |
	sta $1d		; |
	clc			; |
	adc #$05	; |
	sta $1f		; | [$1F] = Pointer to Score Rank+1
	lda #$07	; |
	sta $1e		; |
	sta $20		; /
	tya			; \ If Rank == 49 then
	beq lc5ac	; / only update one rank score.
	dey			; \
lc5a1:
	lda ($1d),y	; | Shift Balloon Trip
	sta ($1f),y	; | Score Ranking
	dey			; | by one rank above
	bne lc5a1	; |
	lda ($1d),y	; |
	sta ($1f),y	; /
lc5ac:
	ldy #$04	; \
lc5ae:
	lda $0003,y	; | Copy current score
	sta ($1d),y	; | to current Score Rank
	dey			; |
	bpl lc5ae	; /
	rts


;----------------------
; Fish code
;----------------------

lc5b7:	; Fish Animation 0
.BYTE $01,$02,$03,$03
lc5bb:	; Fish Animation 1
.BYTE $02,$01,$ff,$03,$04,$05,$06,$ff

lc5c3:
	lda $048d
	lsr
	lsr
	lsr
	tax
	lda $048a	; \
	bne lc5d5	; | If Fish Animation? == 0
	lda lc5b7,x	; / set Fish Status
	jmp lc5d8
lc5d5:
	lda lc5bb,x	; If Fish Animation? != 0
lc5d8:
	sta $87		; Update Fish Status
	ldx #$08
	jsr le3a4
	lda $048c	; \ If Fish Target Eaten Flag
	beq lc613	; / is set
	ldx $048b	; X = Fish Target
	lda $048d	; \
	cmp #$20	; |
	bne lc5f4	; | If Fish Frame Time == $20
	lda #$ff	; | then target is eaten
	sta $88,x	; | (Balloons = -1)
	bmi lc610	; /
lc5f4:
	bcs lc613	; If Fish Frame Time < $20
	lda $0450	; \ Depending on Fish Direction
	bne lc602	; /
	lda $99		; \
	clc			; | Move Fish 4 pixels to the right
	adc #$04	; /
	bne lc607
lc602:
	lda $99		; \ or
	sec			; | Move Fish 4 pixels to the left
	sbc #$04	; /
lc607:
	sta $91,x
	lda $a2		; \
	sec			; | Fish Target's Y position =
	sbc #$0a	; | (Fish Y - $0A)
	sta $9a,x	; /
lc610:
	jsr le3a4
lc613:
	rts

lc614_fishsearchtarget:
	lda #$ff	; \ Reset Target
	sta $048b	; / to none
	ldx #$07	; \
lc61b:
	lda $88,x	; | Check each object
	bmi lc62b	; | if it exists,
	lda $9a,x	; | if Y pos >= #$9A
	cmp #$b4	; | if X pos == Fish X pos
	bcc lc62b	; | then the first one
	lda $91,x	; | that meets these conditions
	cmp $99		; | is the target
	beq lc62f	; |
lc62b:
	dex			; | else check next object
	bpl lc61b	; /
	rts
lc62f:
	stx $048b	; Update Target
	lda $0448,x	; \ Update Fish Direction
	sta $0450	; / with Target Object's Direction
	lda #$00
	sta $048a	; Reset Fish Animation?
	sta $048d	; Reset Fish Frame Time
	sta $048c	; Reset Fish Target Eaten Flag
	sta $0489	; Reset Fish Y Direction to Up
	lda #$dc	; \ Set Fish Y position to #$DC
	sta $a2		; /
	rts

lc64b_fishmove:
	inc $99		; Move Fish +1 pixel to the right
	lda $99		; \
	cmp #$b1	; | If Fish X position >= #$B1
	bcc lc657	; | then go back to X pos = #$40
	lda #$40	; |
	sta $99		; /
lc657:
	rts


lc658:
	lda $0489	; \ If Fish Y Direction == Up
	bne lc66f	; /
	dec $a2		; Fish Y goes up by 1 pixel
	lda $a2		; \
	cmp #$c4	; | If Fish Y Position is about
	bcs lc671	; | to go above $C4
	inc $a2		; | then
	inc $048a	; | Set Fish Animation? to 1
	inc $0489	; | and Fish Y Direction to down
	bne lc671	; /
lc66f:
	inc $a2		; Fish Y goes down by 1 pixel
lc671:
	inc $048d	; Increase Fish Frame Time
	lda $048d	; \
	cmp #$18	; | If Fish Frame Time == $18
	bne lc6a3	; /
	ldx $048b	; \
	lda $88,x	; | If Target exists...
	bmi lc6a3	; / (has balloons)
	lda $9a,x	; \
	clc			; | If the target is above
	adc #$10	; | the fish by 10 pixel
	cmp $a2		; |
	bcc lc6a3	; /
	ldy $0451,x	; \
	lda lc6b8,y	; | Change Target Object Type
	sta $0451,x	; /
	lda #$00	; \ Insta Kill
	sta $7f,x	; | Target Object Status = 00
	sta $88,x	; / Target Object Balloons == 0
	lda $f2		; \
	ora #$40	; | Play Fish Jingle
	sta $f2		; /
	inc $048c	; Set Fish Target Eaten Flag
lc6a3:
	lda $048a	; \ Fish Animation? != 0
	beq lc6b7	; /
	lda $048d	; \
	cmp #$28	; | If Fish Frame Time? == $28
	beq lc6b3	; / OR
	cmp #$30	; \ If Fish Frame Time? == $30
	bne lc6b7	; /
lc6b3:
	lda #$cc	; \ then
	sta $a2		; / Fish Y Position = $CC
lc6b7:
	rts

lc6b8:
    ;12 bytes
.BYTE $08,$09,$0a,$0b,$08,$09,$0a,$0b,$08,$09,$0a,$0b

lc6c4:
	lda $0489	; \ If Fish Direction is Up
	bne lc6f8	; /
	ldx $048b	;
	lda $88,x	; \ Do Object X exist?
	bmi lc6e0	; /
	lda $9a,x	; \
	cmp #$b4	; | Is Object X >= Y pos #$B4?
	bcc lc6e0	; /
	lda $91,x	; \
	cmp #$40	; | Is Object X between
	bcc lc6e0	; | X positions #$40 and #$B1?
	cmp #$b1	; | If so, teleport fish
	bcc lc6ee	; /
lc6e0:
	lda #$30	; \ Else
	sec			; | Fish Frame Time? = $30 - itself
	sbc $048d	; |
	sta $048d	; /
	inc $0489	; Set Fish Direction to Down
	bne lc6f8
lc6ee:
	lda $91,x	; \ Teleport Fish
	sta $99		; / to Object's X position
	lda $0448,x	; \ Change Fish Direction
	sta $0450	; / to Object's Direction
lc6f8:
	rts

lc6f9_fishmanage:
	lda $87		; \ If Fish Status >= 0
	bpl lc70d	; / then handle eating
	jsr lc64b_fishmove	
	jsr lc614_fishsearchtarget
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
; Lightning Bolts Code
;----------------------

lc716_initcloudbolt:
	ldx #$01	; \
lc718:
	lda #$ff	; | Reset 2 Lightning Bolts
	sta $0530,x	; |
	sta $0544,x	; |
	dex			; |
	bpl lc718	; /
	jsr lc77a_cloudboltselect	; Select Cloud that sends the bolt?
lc726:
	ldx $3c		; \
	cpx #$18	; | There are only 25 (#$18) phases
	bcc lc72e	; | X = Current Phase OR X = 24
	ldx #$18	; /
lc72e:
	lda lc748,x	; \ Change Lightning Bolt Intensity
	sta $ba		; /
	lda lc761,x	; \ Change Lightning Bolt Countdown
	sta $b8		; / depending on current phase
	lda #$f0	; \
	sta $02e0	; | Hide last 3 sprites
	sta $02e4	; |
	sta $02e8	; /
	lda #$03
	jmp lc856	; Blink selected cloud
lc748:
    ;25 bytes
.BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01
.BYTE $01,$02,$01,$01,$01,$01,$01,$01,$01,$01
lc761:
    ;25 bytes
.BYTE $0f,$0f,$0c,$0c,$0c,$0c,$0a,$0a,$0a,$0a,$0c,$0c,$0a,$0a,$0a
.BYTE $08,$0a,$0a,$08,$08,$08,$08,$08,$08,$05

lc77a_cloudboltselect:	; Randomly select a cloud to send bolts?
	lda $a3		; \ If there are clouds then select one
	bpl lc781	; / else don't do anything
lc77e:
	sta $a4		; Select Cloud
	rts
lc781:
	jsr lf1b3_rng
lc784:
	cmp $a3		; \ If RNG value <= amount of Clouds
	bcc lc77e	; | then select cloud based on value
	beq lc77e	; /
	clc			; \ Subtract to the RNG
	sbc $a3		; | the amount of clouds
	jmp lc784	; / until the condition is right

lc790_cloudbolt:
	lda $19		; \
	and #$7f	; | Every 128 frames...
	beq lc797	; /
lc796:
	rts
lc797:
	dec $b8		; \ Do Lightning Bolt Countdown
	bne lc796	; / ...once it reaches zero...
	ldx #$00
	lda $0530,x
	bmi lc7ad
	inx
	lda $0530,x
	bmi lc7ad
	lda #$01
	sta $b8
	rts
lc7ad:
	ldy $a4
	sty $a5
	bpl lc7b4
	rts
lc7b4:
	lda #$80
	sta $04b8,x
	sta $04cc,x
	lda #$00
	sta $0530,x
	lda $00b2,y
	sta $0490,x
	lda $00b5,y
	sta $04a4,x
	ldy $ba
	jsr lf1b3_rng
	and #$1f
	adc lc89f,y
	sta $0508,x
	lda lc8ab,y
	sta $051c,x
	lda lc8a5,y
	sta $04e0,x
	lda lc8b1,y
	sta $04f4,x
	jsr lf1b3_rng
	and #$03
	sta $0544,x
	tay
	lda lc897,y
	clc
	adc $0490,x
	sta $0490,x
	lda lc89b,y
	clc
	adc $04a4,x
	sta $04a4,x
	lda lc88f,y
	beq lc811
	jsr lca3d
lc811:
	lda lc893,y
	beq lc819
	jsr lca55
lc819:
	lda $ba
	cmp #$05
	bcs lc821
	inc $ba
lc821:
	lda #$06
	sec
	sbc $ba
	sta $b8
	lda $f0
	ora #$04
	sta $f0
	jmp lc77a_cloudboltselect

lc831_cloudblink:
	lda $b8		; \ If Lightning Bolt Countdown != 1
	cmp #$01	; | then return
	bne lc88a	; /
	lda $0530	; \ If Lightning Bolt 0 doesn't exist
	bmi lc846	; / then prepare for one
	lda $0531	; \ If Lightning Bolt 1 doesn't exist
	bmi lc846	; / then prepare for one
	lda #$02	; \ Else up the countdown to 2
	sta $b8		; /
	rts
lc846:
	lda $19		; \
	and #$7f	; | If Frame Counter < 64
	cmp #$40	; | then don't do anything
	bcc lc88a	; | If not equal to 64
	bne lc856	; / then don't play SFX
	lda $f1		; \
	ora #$08	; | Play Sound Effect
	sta $f1		; /
lc856:
	and #$03
	tax
	lda lc88b,x
	sta $5a
	ldx $a4		; \ Blink the selected cloud
	bmi lc88a	; /
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
	lda #$57				; \
	ldy #$00				; | Copy Temp PPU Block
	jmp lc131_copyppublock	; / [$0057]
lc88a:
	rts
;-----------------------

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
lc89f:
.BYTE $60,$70,$80,$90,$a0,$b0
lc8a5:
.BYTE $00,$00,$00,$00,$00,$00
lc8ab:
.BYTE $c0,$f0,$20,$50,$80,$b0
lc8b1:
.BYTE $00,$00,$01,$01,$01,$01

lc8b7:
	ldx #$01
lc8b9:
	lda $0530,x
	bpl lc8c1
	jmp lc9af
lc8c1:
	lda $0544,x
	bmi lc941
	tay
	txa
	pha
	ldx $a5
	lda $b2,x
	adc lc9e5,y
	sta $02e3
	sta $02e7
	sta $02eb
	lda $b5,x
	adc lc9f5,y
	sta $02e0
	adc lca05,y
	sta $02e4
	adc lca05,y
	sta $02e8
	tya
	and #$03
	tax
	tya
	lsr
	lsr
	tay
	lda $19
	lsr
	lsr
	bcs lc8ff
	tya
	adc #$05
	tay
lc8ff:
	lda lca15,y
	sta $02e1
	lda lca1f,y
	sta $02e5
	lda lca29,y
	sta $02e9
	lda lca33,x
	sta $02e2
	sta $02e6
	sta $02ea
	pla
	tax
	lda $19
	and #$07
	bne lc937
	lda $0544,x
	clc
	adc #$04
	sta $0544,x
	cmp #$14
	bcc lc937
	lda #$ff
	sta $0544,x
lc937:
	lda $0544,x
	cmp #$10
	bcs lc941
	jmp lc9af
lc941:
	jsr lc9b6_boltupdate
	lda $0490,x
	cmp #$02
	bcs lc94e
	jsr lca37
lc94e:
	lda $0490,x
	cmp #$f7
	bcc lc958
	jsr lca37
lc958:
	lda $04a4,x
	cmp #$02
	bcs lc962
	jsr lca4f
lc962:
	lda $04a4,x
	cmp #$e0
	bcc lc976
	lda #$ff
	sta $0530,x
	lda #$f0
	sta $04a4,x
	jmp lc9af
lc976:
	jsr lca67
	jsr lcb1c_bolt_playercollision
	ldy $0530,x
	iny
	tya
	and #$07
	sta $0530,x
	ldy $0530,x
	lda lc9dd,y
	sta $12
	txa
	asl
	asl
	clc
	tay
	lda $04a4,x
	cmp #$d0
	sta $0200,y
	lda $0490,x
	sta $0203,y
	lda $12
	sta $0201,y
	lda #$00
	bcc lc9ac
	lda #$20
lc9ac:
	sta $0202,y
lc9af:
	dex
	bmi lc9b5
	jmp lc8b9
lc9b5:
	rts
;-----------------------

lc9b6_boltupdate:
	lda $0508,x	; \
	clc			; | Update X Position (Frac)
	adc $04b8,x	; |
	sta $04b8,x	; /
	lda $04e0,x	; \
	adc $0490,x	; | Update X Position (Int)
	sta $0490,x	; /
	lda $051c,x	; \
	clc			; | Update Y Position (Frac)
	adc $04cc,x	; |
	sta $04cc,x	; /
	lda $04f4,x	; \
	adc $04a4,x	; | Update Y Position (Int)
	sta $04a4,x	; /
	rts

lc9dd:
.BYTE $9d,$9e,$9f,$9e,$9d,$a0,$a1,$a0
lc9e5:
    ;16 bytes
.BYTE $08,$08,$f0,$f0,$08,$08,$f0,$f0,$08,$08,$f0,$f0,$08,$08,$f0
.BYTE $f0
lc9f5:
    ;16 bytes
.BYTE $ee,$0a,$0a,$ee,$ee,$0a,$0a,$ee,$ee,$0a,$0a,$ee,$ee,$0a,$0a
.BYTE $ee
lca05:
    ;16 bytes
.BYTE $f8,$08,$08,$f8,$f8,$08,$08,$f8,$f8,$08,$08,$f8,$f8,$08,$08
.BYTE $f8
lca15:
    ;10 bytes
.BYTE $91,$93,$97,$97,$fc,$92,$95,$9a,$9a,$fc
lca1f:
    ;10 bytes
.BYTE $fc,$94,$98,$98,$fc,$fc,$96,$9b,$9b,$fc
lca29:
    ;10 bytes
.BYTE $fc,$fc,$99,$99,$fc,$fc,$fc,$9c,$9c,$fc
lca33:
.BYTE $c0,$40,$00,$80

lca37:
	lda $f3		; \
	ora #$80	; | Play SFX
	sta $f3		; /
lca3d:
	lda #$00	; \
	sec			; |
	sbc $0508,x	; | Lightning Bolt
	sta $0508,x	; | Reverse X Velocity
	lda #$00	; |
	sbc $04e0,x	; |
	sta $04e0,x	; /
	rts

lca4f:
	lda $f3		; \
	ora #$80	; | Play SFX
	sta $f3		; /
lca55:
	lda #$00	; \
	sec			; |
	sbc $051c,x	; | Lightning Bolt
	sta $051c,x	; | Reverse Y Velocity
	lda #$00	; |
	sbc $04f4,x	; |
	sta $04f4,x	; /
	rts

lca67:		;Lightning Bolt Platform Collision?
	ldy $cd
lca69:
	lda #$00
	sta $cc
	lda ($27),y
	sec
	sbc #$08
	cmp $04a4,x
	bcs lcadd
	adc #$03
	cmp $04a4,x
	bcc lca82
	lda #$01
	bne lca92
lca82:
	lda ($29),y
	cmp $04a4,x
	bcc lcadd
	sbc #3
	cmp $04a4,x
	bcs lcaad
	lda #2
lca92:
	sta $cc
	lda ($23),y
	cmp #$10
	beq lcaa2
	sec
	sbc #4
	cmp $0490,x
	bcs lcaa9
lcaa2:
	lda ($25),y
	cmp $0490,x
	bcs lcaad
lcaa9:
	lda #0
	sta $cc
lcaad:
	lda ($23),y
	cmp #$10
	beq lcac8
	sec
	sbc #8
	cmp $0490,x
	bcs lcadd
	adc #3
	cmp $0490,x
	bcc lcac8
	lda $cc
	ora #4
	bne lcadb
lcac8:
	lda ($25),y
	cmp #$ff
	beq lcadd
	cmp $0490,x
	bcc lcadd
	sbc #3
	bcs lcadd
	lda $cc
	ora #8
lcadb:
	sta $cc
lcadd:
	lda $cc
	bne lcae8
lcae1:
	dey
	bmi lcae7
	jmp lca69
lcae7:
	rts
lcae8:
	lsr $cc
	bcc lcaf4
	lda $04f4,x
	bmi lcaf4
	jsr lca4f
lcaf4:
	lsr $cc
	bcc lcb00
	lda $04f4,x
	bpl lcb00
	jsr lca4f
lcb00:
	lsr $cc
	bcc lcb0c
	lda $04e0,x
	bmi lcb0c
	jsr lca37
lcb0c:
	lsr $cc
	bcc lcb18
	lda $04e0,x
	bpl lcb18
	jsr lca37
lcb18:
	jmp lcae1
	rts

lcb1c_bolt_playercollision:		; Lightning Bolt Player Collision
	ldy #$01
lcb1e:
	lda $0088,y	; \
	bmi lcb70	; | If Player Y has balloons...
	beq lcb70	; /
	lda $00bd,y	; \ and if Player Y is not invincible...
	bne lcb70	; /
	lda $0490,x		; \
	sec				; | If Player Y's X position
	sbc $0091,y		; | is within the X position
	jsr lf08e_abs	; | of Lightning Bolt X
	cmp #$08		; | (size 8 pixels)
	bcs lcb70		; /
	lda $04a4,x		; \
	sec				; |
	sbc $009a,y		; | If Player Y's Y position
	sec				; | is within the Y position
	sbc #$08		; | of Lightning Bolt X
	jsr lf08e_abs	; | (size 12 pixels high
	cmp #$0c		; | to take balloons into account)
	bcs lcb70		; /
	lda #$00
	sta $0088,y	; Player Y's balloons = 00
	lda #$01
	sta $007f,y	; Player Y's status = 01 
	sta $00c1,y	; Player Y's freeze flag = 01
	lda #$0b
	sta $0451,y	; Player Y's type = 0B
	lda #$20
	sta $045a,y	; Player Y's ? = 20
	lda $f0		; \
	ora #$80	; | Play SFX
	sta $f0		; /
	lda #$f0	; \
	sta $04a4,x	; | Lightning Bolt X
	lda #$ff	; | disappears
	sta $0530,x	; /
lcb70:
	dey			; \ Check next player
	bpl lcb1e	; /
	rts


;----------------------
; Flipper code
;----------------------

lcb74_flippermanage:
	ldx $05d1
	bmi lcba7
lcb79:
	jsr lcba8
	lda $0604,x
	beq lcba4
	txa
	eor $19
	and #1
	bne lcba4
	ldy $05fa,x
	iny
	tya
	and #3
	sta $05fa,x
	jsr lcccb
	lda $05fa,x
	cmp #1
	bne lcba4
	dec $060e,x
	bne lcba4
	dec $0604,x
lcba4:
	dex
	bpl lcb79
lcba7:
	rts

lcba8:
	ldy #7
	lda $0604,x
	bne lcbb2
	jmp lcc3a
lcbb2:
	lda $0088,y
	bmi lcc2f
	beq lcc2f
	cpy #2
	bcc lcbc1
	cmp #1
	beq lcc2f
lcbc1:
	lda $0091,y
	clc
	adc #8
	sec
	sbc $05d2,x
	sta $12
	jsr lf08e_abs
	cmp #$12
	bcs lcc2f
	lda $009a,y
	clc
	adc #$0c
	sec
	sbc $05dc,x
	sta $13
	jsr lf08e_abs
	cmp #$12
	bcs lcc2f
	lda $12
	bmi lcbfc
	cmp #3
	bcc lcc0b
	lda #2
	sta $041b,y
	jsr lcc33
	jsr lebbb
	bne lcc0b
lcbfc:
	cmp #$fd
	bcs lcc0b
	lda #$fe
	sta $041b,y
	jsr lebbb
	jsr lcc33
lcc0b:
	lda $13
	bmi lcc20
	cmp #3
	bcc lcc2f
	lda #2
	sta $042d,y
	jsr lebb2
	jsr lcc33
	bne lcc2f
lcc20:
	cmp #$fd
	bcs lcc2f
	lda #$fe
	sta $042d,y
	jsr lebb2
	jsr lcc33
lcc2f:
	dey
	bpl lcbb2
	rts

lcc33:
	lda $f1
	ora #2
	sta $f1
	rts

lcc3a:
	lda $0088,y
	bmi lccb8
	beq lccb8
	cpy #2
	bcc lcc73
	lda $05fa,x
	cmp #3
	bne lcc73
	lda $05d2,x
	sec
	sbc #$0a
	cmp $0091,y
	bcs lcc73
	adc #4
	cmp $0091,y
	bcc lcc73
	lda $05dc,x
	sec
	sbc #$1c
	cmp $009a,y
	bcs lcc73
	adc #4
	cmp $009a,y
	bcc lcc73
	jsr lccbf
lcc73:
	lda $0091,y
	clc
	adc #8
	sec
	sbc $05d2,x
	jsr lf08e_abs
	sta $12
	lda $009a,y
	clc
	adc #$0c
	sec
	sbc $05dc,x
	jsr lf08e_abs
	sta $13
	lda $05fa,x
	cmp #3
	beq lcca2
	lda $12
	pha
	lda $13
	sta $12
	pla
	sta $13
lcca2:
	lda $12
	cmp #$14
	bcs lccb8
	lda $13
	cmp #$0b
	bcs lccb8
	lda #1
	sta $0604,x
	lda #$32
	sta $060e,x
lccb8:
	dey
	bmi lccbe
	jmp lcc3a
lccbe:
	rts

lccbf:
	txa
	pha
	tya
	tax
	inc $cb
	jsr le983
	pla
	tax
	rts

lcccb:
	lda $05f0,x
	sta $57
	lda $05e6,x
	sta $58
	lda #3
	sta $59
	ldy $05fa,x
	lda lcd26,y
	sta $5a
	lda lcd2a,y
	sta $5b
	lda lcd2e,y
	sta $5c
	jsr lcd0f
	lda lcd32,y
	sta $5a
	lda lcd36,y
	sta $5b
	lda lcd3a,y
	sta $5c
	jsr lcd0f
	lda lcd3e,y
	sta $5a
	lda lcd42,y
	sta $5b
	lda lcd46,y
	sta $5c
lcd0f:
	tya
	pha
	lda #$57
	ldy #0
	jsr lc131_copyppublock
	pla
	tay
	lda $58
	clc
	adc #$20
	sta $58
	bcc lcd25
	inc $57
lcd25:
	rts

lcd26:
.BYTE $a1,$24,$24,$24
lcd2a:
.BYTE $a2,$9e,$ab,$24
lcd2e:
.BYTE $24,$24,$ac,$24
lcd32:
.BYTE $a3,$24,$ad,$a8
lcd36:
.BYTE $a4,$9f,$ae,$a9
lcd3a:
.BYTE $a5,$24,$af,$aa
lcd3e:
.BYTE $24,$24,$b0,$24
lcd42:
.BYTE $a6,$a0,$b1,$24
lcd46:
.BYTE $a7,$24,$24,$24


;----------------------
; Balloon code
;----------------------

lcd4a_initballoons:
	ldx #$09	; \ Reset all 10 balloons
lcd4c:
	lda #$ff	; | GFX = #$FF
	sta $055d,x	; |
	lda #$f0	; | Y Positions = #$F0
	sta $057b,x	; |
	dex			; |
	bpl lcd4c	; /
	rts

lcd5a:
	dec $05cc
	beq lcd60
	rts
lcd60:
	lda $1b
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
	jsr lf1b3_rng
	and #3
	tay
	lda lceae,y
	sta $0567,x
	ldy #0
	lda $1b
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
	sta $12
	lda $058f,x
	sta $13
	jsr lf1a6
	lda $05b7,x
	clc
	adc $12
	sta $05b7,x
	sta $12
	lda $05c1,x
	adc $13
	sta $05c1,x
	sta $13
	jsr lf1a6
	lda $0599,x
	sec
	sbc $12
	sta $0599,x
	lda $058f,x
	sbc $13
	sta $058f,x
	lda $0571,x
	clc
	adc $0599,x
	sta $0571,x
	lda $0567,x
	adc $058f,x
	sta $0567,x
lcdfc:
	lda $0585,x
	sec
	sbc $055a
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
	bmi lce2e
	jmp lcdac
lce2e:
	rts

lce2f_balloonxspritemanage:
	ldy $055d,x
	iny
	lda lceb2,y
	sta $13
	txa
	sta $12
	asl
	adc $12
	asl
	asl
	tay
	lda $057b,x
	sta $0250,y
	sta $0254,y
	clc
	adc #8
	sta $0258,y
	lda $0567,x
	sta $0253,y
	clc
	adc #4
	sta $025b,y
	clc
	adc #4
	sta $0257,y
	lda $13
	sta $0252,y
	sta $0256,y
	sta $025a,y
	lda $055d,x
	bmi lce99
	lda #$a8
	sta $0251,y
	lda #$a9
	sta $0255,y
	lda $19
	lsr
	lsr
	lsr
	lsr
	and #7
	stx $13
	tax
	lda lceb2+3,x
	sta $0259,y
	lda $025a,y
	eor lcebd,x
	sta $025a,y
	ldx $13
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
	bmi lcf12
	lda $009a,y
	cmp #$c0
	bcs lcf0f
	sec
	sbc $057b,x
	jsr lf08e_abs
	cmp #$18
	bcs lcf0f
	lda $0091,y
	sec
	sbc $0567,x
	jsr lf08e_abs
	cmp #$10
	bcs lcf0f
	lda #$ff
	sta $055d,x
	lda $05cd,y
	clc
	adc #$01
	sta $05cd,y
	lda #$02
	sta $f0
	rts
lcf0f:
	dey
	bpl lced0
lcf12:
	rts


;----------------------
; Bonus Phase code
;----------------------

lcf13:
	lda #$20
	sta $f2
	jsr ld0e2_setbonusphase
	jsr lcd4a_initballoons
	ldx $40
lcf1f:
	lda $41,x
	bmi lcf26
	jsr lf3b0_initplayertype
lcf26:
	dex
	bpl lcf1f
	ldx #0
	stx $bd
	stx $be
	lda #$14
	sta $05cb
lcf34:
	jsr lf470_pause
	inc $4c
	jsr ld8dd
	jsr le691_objectmanage
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
	lda $19
	bne lcf34
	jsr ld246_clearppu
	ldx #2
	stx $46
	jsr lf45e_waityframes
	lday ld12b
	jsr lc131_copyppublock
	lday ld15a
	jsr lc131_copyppublock
	lday ld165
	jsr lc131_copyppublock
	ldx $40
lcf7e:
	lda #$20
	sta $91,x
	lda ld19e,x
	sta $9a,x
	lda #3
	sta $7f,x
	lda #1
	sta $0448,x
	jsr lf3b0_initplayertype
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
	ldx $40
lcfb5:
	jsr lce2f_balloonxspritemanage
	dex
	bpl lcfb5
	jsr lf45c_wait20frames
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
	lda $05cd
	jsr ld1c9
	lda $40
	beq lcfe8
	lda #$0f
	sta $55
	lda $05ce
	jsr ld1c9
lcfe8:
	jsr lf45c_wait20frames
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
	jsr lc119
	lda $40
	beq ld013
	lda #$0f
	sta $55
	jsr lc119
ld013:
	lda #$ff
	sta $055d
	sta $055e
	ldx $40
ld01d:
	jsr lce2f_balloonxspritemanage
	dex
	bpl ld01d
	lda #2
	sta $f0
	ldx #2
	jsr lf45e_waityframes
	ldx $40
ld02e:
	jsr lce2f_balloonxspritemanage
	dex
	bpl ld02e
	jsr ld1a0
	jsr lf45c_wait20frames
	lda #1
	sta $f0
	jsr ld121
	bne ld068
	lday ld170
	jsr lc131_copyppublock
	jsr lf465_clearframeflag
	ldx #$1a
ld04f:
	lda ld184,x
	sta $57,x
	dex
	bpl ld04f
	lda $055b
	sta $68
	lda $055c
	sta $69
	jsr lc12d_copypputempblock
	lda #$10
	sta $f2
ld068:
	ldx #$78
	jsr lf45e_waityframes
	jsr ld1a0
ld070:
	lda #0
	sta $3e
	ldx #4
	jsr ld213
	jsr lc12d_copypputempblock
	lda $40
	beq ld08e
	inc $3e
	ldx #$12
	jsr ld213
	lda #$65
	ldy #0
	jsr lc131_copyppublock
ld08e:
	lda #1
	sta $f1
	ldx #2
	jsr lf45e_waityframes
	lda $5d
	cmp #$24
	bne ld070
	lda $40
	beq ld0a8
	lda a:$006b	;Absolute addressing on ZP location?
	cmp #$24
	bne ld070
ld0a8:
	ldx #$0a
	jsr lf45e_waityframes
	jsr ld121
	bne ld0ce
	lda $055b
	sta $47
	lda $055c
	sta $48
	lda $40
	sta $3e
ld0c0:
	jsr ld6dc_scoreupdate
	dec $3e
	bpl ld0c0
	lda #1
	sta $f1
	jsr lf45c_wait20frames
ld0ce:
	lda #0
	sta $47
	sta $48
	ldx #1
ld0d6:
	lda $41,x
	bpl ld0dc
	sta $88,x
ld0dc:
	dex
	bpl ld0d6
	jmp lf353

ld0e2_setbonusphase:
	ldx $0558	; \ Set up Bonus Phase
	lda ld10d,x	; | according to Intensity (max 4)
	sta $0559	; | 
	lda ld112,x	; | Set points per balloon
	sta $055a	; | Set rising speed
	lda ld117,x	; | Set super bonus points
	sta $055b	; |
	lda ld11c,x	; |
	sta $055c	; /
	cpx #$04	; \ Increment Bonus Phase Intensity
	bcs ld104	; | until maximum (4)
	inc $0558	; /
ld104:
	lda #$00	; \
	sta $05cd	; | Initialize Balloon Counters
	sta $05ce	; /
	rts

ld10d:	; Points per balloon
.BYTE $03,$05,$07,$07,$07
ld112:	; Rising Speed
.BYTE $80,$90,$98,$a0,$a8
ld117:	; Super Bonus x0000 Points
.BYTE 1,1,2,2,3
ld11c:	; Super Bonus 0x000 Points
.BYTE 0,5,0,5,0

ld121:
	lda $05cd
	clc
	adc $05ce
	cmp #$14
	rts
;-----------------------

ld12b:
.BYTE $3f,$00,$10,$0f,$30,$30,$30,$0f,$30,$27,$15,$0f,$30,$02,$21,$0f,$16,$16,$16
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
	ldx #$1c
ld1a2:
	lda ld13e,x
	sta $57,x
	dex
	bpl ld1a2
	ldx #4
	ldy $05cd
	jsr ld1dc
	ldx #$12
	ldy $05ce
	jsr ld1dc
	jsr lc12d_copypputempblock
	lda $40
	bne ld1c2
	rts

ld1c2:
	lda #$65
	ldy #0
	jmp lc131_copyppublock
ld1c9:
	ldy #0
ld1cb:
	cmp #$0a
	bcc ld1d5
	iny
	sbc #$0a
	jmp ld1cb
ld1d5:
	sty $5a
	sta $5b
	jmp lc119
ld1dc:
	dey
	bmi ld1fe
	lda $0559
	clc
	adc $59,x
	cmp #$0a
	bcc ld1ed
	sbc #$0a
	inc $58,x
ld1ed:
	sta $59,x
	lda $58,x
	cmp #$0a
	bcc ld1fb
	sbc #$0a
	inc $57,x
	sta $58,x
ld1fb:
	jmp ld1dc
ld1fe:
	ldy #0
ld200:
	lda $57,x
	beq ld208
	cmp #$24
	bne ld212
ld208:
	lda #$24
	sta $57,x
	inx
	iny
	cpy #4
	bne ld200
ld212:
	rts
;-----------------------

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
	lda #$0a
	sta $58,x
	dec $57,x
ld232:
	lda #$0a
	sta $59,x
	dec $58,x
ld238:
	dec $59,x
	txa
	pha
	lda #$0a
	jsr ld6de_scoreadd
	pla
	tax
ld243:
	jmp ld1fe

ld246_clearppu:	;Clear PPU?
	jsr lc10a_clearppumask
	jsr lc0fa_disablenmi
	lda #$20	; \
	sta PPUADDR	; | PPUADDR = $2000
	lda #$00	; | Nametable 0 Address
	sta PPUADDR	; /
	jsr ld275_clearnametable	; \ Clear Nametable 0
	jsr ld275_clearnametable	; / Clear Nametable 1
	jsr lc104_enablenmi
	jsr lc115
	ldx #$3f
	ldy #$00
	sty $4c
ld268:
	lda #$f0	; \
	sta $0200,y	; |
	iny			; |
	iny			; | Hide all sprites
	iny			; |
	iny			; |
	dex			; |
	bpl ld268	; /
	rts
;-----------------------

ld275_clearnametable:
	ldx #$f0	; \
	lda #$24	; |
ld279:
	sta PPUDATA	; |
	sta PPUDATA	; | Fill PPU Nametable with empty tiles
	sta PPUDATA	; | $3C0 bytes
	sta PPUDATA	; |
	dex			; |
	bne ld279	; /

	ldx #$40	; \
	lda #$00	; |
ld28c:
	sta PPUDATA	; | Fill rest of nametable with Tile 00
	dex			; | $40 bytes
	bne ld28c	; /
	rts			; Total: $400 bytes
;-----------------------

ld293_initgamemode:
	jsr lc10a_clearppumask
	jsr lc0fa_disablenmi
	lda $16		; Check Game Mode for Initialization
	beq ld2a0
	jmp ld572
ld2a0:			; Initialize Balloon Fight Game Mode
	ldy $3b						; \
	lda PhasePointersLow,y		; |
	sta $1d						; | Load Phase Graphics
	lda PhasePointersHigh,y		; |
	sta $1e						; |
	jsr ld497_uploadbackground	; /
	ldx #$00							; \
ld2b1:
	jsr ld4e5_getbytefromloadpointer	; |
	cmp #$ff							; | Load Clouds (XX YY)
	beq ld322							; | until one has $FF as
	sta $54								; | X coordinate
	jsr ld4e5_getbytefromloadpointer	; |
	sta $55								; /
	ldy #$03					; \
ld2c1:
	jsr ld4fb_setppuaddr_render	; |
	lda #$04					; |
	sta $12						; | Render Cloud
	lda ld493,y					; | to the screen
ld2cb:
	sta PPUDATA					; |
	clc							; |
	adc #$04					; |
	dec $12						; |
	bne ld2cb					; |
	inc $55						; |
	dey							; |
	bpl ld2c1					; /
	lda $55
	sec
	sbc #$04
	sta $55
	jsr ld51c
	sta $a6,x
	inc $54
	inc $54
	jsr ld51c
	sta $a9,x
	inc $55
	inc $55
	jsr ld51c
	sta $af,x
	dec $54
	dec $54
	jsr ld51c
	sta $ac,x
	stx $a4
	lda #$03
	jsr lc856
	jsr lc17c_uploadppubuffer
	ldx $a4
	lda $54
	asl
	asl
	asl
	clc
	adc #$10
	sta $b2,x
	lda $55
	asl
	asl
	asl
	sta $b5,x
	inx
	jmp ld2b1	; Load another cloud data
ld322:
	dex		; \ Write amount of clouds to RAM
	stx $a3	; /
	ldx #$00							; \
ld327:
	jsr ld4e5_getbytefromloadpointer	; |
	cmp #$ff							; | Load Flippers (XX YY TT)
	beq ld37e							; | until one has $FF as
	sta $54								; | X coordinate
	jsr ld4e5_getbytefromloadpointer	; |
	sta $55								; |
	jsr ld4e5_getbytefromloadpointer	; |
	sta $05fa,x							; /
	lda $54
	asl
	asl
	asl
	adc #$0c
	sta $05d2,x
	lda $55
	asl
	asl
	asl
	adc #$0c
	sta $05dc,x
	lda #$00
	sta $0604,x
	jsr ld4fb_setppuaddr_render
	sta $05e6,x
	lda $13
	sta $05f0,x
	jsr ld56c
	jsr ld53c
	inc $54
	inc $54
	jsr ld53c
	inc $55
	inc $55
	jsr ld53c
	dec $54
	dec $54
	jsr ld53c
	inx
	jmp ld327	; Load another flipper data
ld37e:
	dex			; \ Write amount of flippers to RAM
	stx $05d1	; /
	jsr ld4e5_getbytefromloadpointer	; \
	sta $1f								; | Load Enemy Data Pointer
	jsr ld4e5_getbytefromloadpointer	; |
	sta $20								; /
	ldy #$00	; \
	lda ($1f),y	; | Load Enemy Amount
	tax			; |
	dex			; |
	bpl ld399	; /
	inc $c8		; If No Enemies then it's a Bonus Phase Type
	jmp ld3ba	; Skip Enemy Loading
ld399:
	iny
ld39a:
	lda ($1f),y	; \ Load Enemy X Position
	iny			; |
	sta $93,x	; /
	lda ($1f),y	; \ Load Enemy Y Position
	iny			; |
	sta $9c,x	; /
	lda ($1f),y	; \ Load Enemy Type
	iny			; |
	sta $0453,x	; /
	lda #$02	; \ Initialize Enemy Status
	sta $81,x	; / (02 = Sitting)
	lda #$01	; \ Initialize Enemy Balloons
	sta $8a,x	; / (01 = Sitting/Umbrella)
	lda $c6		; \ Initialize Enemy ?
	sta $0441,x	; /
	dex
	bpl ld39a	; Load another enemy data
ld3ba:
	jsr ld4e5_getbytefromloadpointer	; \ Load Amount of Platforms
	sta $cd								; /
	jsr ld4e5_getbytefromloadpointer	; \
	sta $23								; | Load Platform Collision Pointer
	jsr ld4e5_getbytefromloadpointer	; | Left Side
	tay									; |
	sta $24								; /
	lda $23						; \
	jsr ld48c_nextplatformptr	; | Load Right Side Platform Collision Pointer
	sta $25						; |
	sty $26						; /
	jsr ld48c_nextplatformptr	; \
	sta $27						; | Load Top Side Platform Collision Pointer
	sty $28						; /
	jsr ld48c_nextplatformptr	; \
	sta $29						; | Load Bottom Side Platform Collision Pointer
	sty $2a						; /
ld3e1:
	jsr ld5d9
	jsr ld3ed_setpalette
	jsr lc104_enablenmi
	jmp lc115

ld3ed_setpalette:
	ldx #$22	; \
ld3ef:
	lda ld437,x	; | Copy ld437 (Palette)
	sta $57,x	; | to PPU Temp Block
	dex			; |
	bpl ld3ef	; /
	lda $c8		; \ Check Phase Type...
	bne ld410	; /
	lda $3b		; \ ...If Normal Phase
	and #$0c	; | Select Palette based
	ora #$03	; | on current level header
	tay			; /
	ldx #$03	; \
ld404:
	lda ld45a,y	; | Copy Single Palette Data
	sta $5a,x	; | to Background Palette 1
	dey			; | to PPU Temp Block
	dex			; |
	bpl ld404	; /
ld40d:
	jmp lc12d_copypputempblock
ld410:
	ldx $0558	; ...If Bonus Phase
	lda BalloonPalPointerLower,x	; \
	sta $1d		; | Select Balloon Palette
	lda BalloonPalPointerUpper,x	; | based on Intensity Level
	sta $1e		; /
	ldx #$03	; \
	ldy #$07	; |
ld421:
	lda ($1d),y	; | Copy Second Palette Data
	sta $72,x	; | to Sprite Palette 2
	dey			; | to PPU Temp Block
	dex			; |
	bpl ld421	; /
	lda $16		; \ If Balloon Trip mode
	bne ld40d	; / then stop and copy PPU Temp Block as is
ld42d:
	lda ($1d),y	; \ Copy First Palette Data
	sta $005a,y	; | to Background Palette 1
	dey			; | to PPU Temp Block
	bpl ld42d	; /
	bmi ld40d
ld437:
    ;35 bytes
.BYTE $3f,$00,$20,$0f,$2a,$09,$07,$0f,$30,$27,$15,$0f,$30,$02,$21
.BYTE $0f,$30,$00,$10,$0f,$16,$12,$37,$0f,$12,$16,$37,$0f,$17,$11
.BYTE $35,$0f,$17,$11,$2b
ld45a:
    ;16 bytes
.BYTE $0f,$2a,$09,$07,$0f,$26,$06,$07,$0f,$1b,$0c,$07,$0f,$2c,$01
.BYTE $06

.define BonusBalloonPalettes GreenBalloonPalette, OrangeBalloonPalette, RedBalloonPalette, RedBalloonPalette, RedBalloonPalette
BalloonPalPointerLower:
.LOBYTES BonusBalloonPalettes
BalloonPalPointerUpper:
.HIBYTES BonusBalloonPalettes

GreenBalloonPalette:
.BYTE $0f,$02,$08,$06,$0f,$2b,$30,$12
OrangeBalloonPalette:
.BYTE $0f,$07,$0a,$19,$0f,$26,$30,$2b
RedBalloonPalette:
.BYTE $0f,$07,$0c,$1c,$0f,$15,$30,$26

ld48c_nextplatformptr:
	sec
	adc $cd
	bcc ld492
	iny
ld492:
	rts
;-----------------------

ld493:
.BYTE $7f,$7e,$7d,$7c

ld497_uploadbackground:	;Argument: $001D = Pointer to pointers to screen data
	jsr ld4e5_getbytefromloadpointer
	sta $1f
	jsr ld4e5_getbytefromloadpointer
	sta $20
	tax
	beq ld4e4
ld4a4:
	jsr ld4f0_getbytefromgfxpointer
	tax
	beq ld497_uploadbackground
	and #$7f
	sta PPUADDR
	jsr ld4f0_getbytefromgfxpointer
	sta PPUADDR
	jsr ld4f0_getbytefromgfxpointer
	sta $12
	txa
	and #$80
	lsr
	lsr
	lsr
	lsr
	lsr
	ora $00
	sta $2000
	txa
	and #$40
	bne ld4d8
ld4cc:
	jsr ld4f0_getbytefromgfxpointer
	sta PPUDATA
	dec $12
	bne ld4cc
	beq ld4a4
ld4d8:
	jsr ld4f0_getbytefromgfxpointer
ld4db:
	sta PPUDATA
	dec $12
	bne ld4db
	beq ld4a4
ld4e4:
	rts
;-----------------------

ld4e5_getbytefromloadpointer:
	ldy #$00
	lda ($1d),y
	inc $1d
	bne ld4ef
	inc $1e
ld4ef:
	rts
;-----------------------

ld4f0_getbytefromgfxpointer:
	ldy #0
	lda ($1f),y
	inc $1f
	bne ld4fa
	inc $20
ld4fa:
	rts
;-----------------------

ld4fb_setppuaddr_render:
	lda $55
	sta $12
	lda #$00
	asl $12
	asl $12
	asl $12
	asl $12
	rol
	asl $12
	rol
	ora #$20
	sta PPUADDR
	sta $13
	lda $12
	ora $54
	sta PPUADDR
	rts
;-----------------------

ld51c:
	lda $55
	and #$fc
	asl
	sta $12
	lda $54
	lsr
	lsr
	ora $12
	ora #$c0
	pha
	lda $55
	and #2
	sta $12
	lda $54
	and #2
	lsr
	ora $12
	tay
	pla
	rts
;-----------------------

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
;-----------------------

ld564:
.BYTE $fc,$f3,$cf,$3f
ld568:
.BYTE $01,$04,$10,$40
ld56c:
	jsr lcccb
	jmp lc17c_uploadppubuffer
ld572:			; Initialize Balloon Trip Game Mode
	lda #$c0
	ldy #$23
	jsr ld593
	lda #$c0
	ldy #$27
	jsr ld593
	ldy #$23
	lda #$60
	jsr ld5b8
	ldy #$27
	lda #$60
	jsr ld5b8
	inc $c8
	jmp ld3e1
ld593:
	sty PPUADDR
	sta PPUADDR
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
;-----------------------

ld5b8:
	sty PPUADDR
	sta PPUADDR
	ldx #$20
	lda #$58
	jsr ld5c9
	ldx #$40
	lda #$5c
ld5c9:
	sta $12
ld5cb:
	txa
	and #3
	eor #3
	ora $12
	sta PPUDATA
	dex
	bne ld5cb
	rts
;-----------------------

ld5d9:
	ldx #$00
ld5db:
	jsr ld651
	jsr ld5f1
	lda $51
	ora #$04
	sta $51
	jsr ld5f1
	inx
	inx
	cpx #$80
	bne ld5db
	rts
;-----------------------

ld5f1:
	lda $51
	sta PPUADDR
	lda $50
	sta PPUADDR
	lda PPUDATA
	lda PPUDATA
	cmp #$24
	bne ld60c
	txa
	and #3
	tay
	jmp ld63b
ld60c:
	rts
;-----------------------

ld60d_updatestarbganim:
	lda $4c		; \ If [$4C] == 0
	beq ld63a	; / Then Do Nothing
	dec $4c
	lda $4f		; \
	clc			; |
	adc #$02	; | Update and Get Current
	and #$3f	; | Star ID
	sta $4f		; |
	tax			; /
	jsr ld651	; \
	lda $51		; |
	sta PPUADDR	; | Set PPU Address for Star Tile
	lda $50		; |
	sta PPUADDR	; /
	lda PPUDATA	; \
	lda PPUDATA	; |
	ldy #$03	; | Check if Tile is part of
ld632:
	cmp StarAnimTiles,y	; | Star Animation tiles
	beq ld63b	; | If not: Stop
	dey			; |
	bpl ld632	; /
ld63a:
	rts

ld63b:
	lda $51					; \
	sta PPUADDR				; |
	lda $50					; | Write Next Star Tile
	sta PPUADDR				; |
	lda StarAnimTiles+1,y	; |
	sta PPUDATA				; /
	rts

StarAnimTiles:	;Star Tile Animation Frames
.BYTE $24,$ed,$ee,$ef,$24

ld651:
	lda StarPositions,x
	sta $50
	lda StarPositions+1,x
	sta $51
	rts

StarPositions:
    ;128 bytes
.WORD $2163,$21A5,$20CB,$20B7,$217D,$229B,$20F2,$2249
.WORD $216D,$220B,$2292,$2195,$211C,$2148,$20E0,$230B
.WORD $20CE,$21D0,$2106,$2119,$2230,$228A,$2288,$20A4
.WORD $2242,$2168,$223C,$2136,$21CA,$20BC,$2196,$214C
.WORD $2235,$20EF,$2268,$20A6,$21BB,$217A,$20EA,$21F1
.WORD $20C2,$2177,$2154,$20BA,$22C5,$20BE,$20FA,$21AE
.WORD $2146,$219A,$20D2,$213D,$222B,$20B0,$21B6,$20AC
.WORD $20B3,$20DB,$20F6,$212C,$20E7,$2162,$21E4,$214E

ld6dc_scoreupdate:
	lda #$00	; Only Update Score
ld6de_scoreadd:
	sta $43		; Score to Add
	lda $3a		; \ If not Demo Play
	beq ld6e5	; | then go to ld6e5
ld6e4:
	rts			; / Else return

ld6e5:
	ldx $3e		; \ If [$3E] >= 2
	cpx #$02	; | Then return
	bcs ld6e4	; /
	lda $41,x	; \ If Player X has no lives
	bmi ld6e4	; / Then return
	ldy #$64			; \ Process Score to add up
	jsr ld77c_divide	; | Score to add / 100
	clc					; |
	adc $48				; |
	sta $45				; |
	ldy #$0a			; |
	jsr ld77c_divide	; | Modulo Result / 10
	sta $44				; /
	ldx $3f		; \ Selected Game Mode?
	lda ld779,x	; |
	sta $21		; | Setup Pointer to Default Top Score
	lda #$06	; | [$21] = 06XX
	sta $22		; /
	lda $3e		; \
	asl			; | Select Score to update
	asl			; | X = [$3E] * 5
	ora $3e		; |
	tax			; /
	clc
	lda P1Score,x	; \ Add Score 0000X
	adc $43		; | Lock Score between 0 and 9
	jsr ld78f	; | First Digit
	sta P1Score,x	; /
	lda $04,x	; \ Add Score 000X0
	adc $44		; | Lock Score between 0 and 9
	jsr ld78f	; | Second Digit
	sta $04,x	; /
	lda $05,x	; \ Add Score 00X00
	adc $45		; | Lock Score between 0 and 9
	jsr ld78f	; | Third Digit
	sta $05,x	; /
	lda $06,x	; \ Add Score 0X000
	adc $47		; | Lock Score between 0 and 9
	jsr ld78f	; | Fourth Digit
	sta $06,x	; /
	lda $07,x	; \ Add Score X0000
	adc #$00	; | Lock Score between 0 and 9
	jsr ld78f	; | Fifth Digit
	sta $07,x	; /
	inx			; \
	inx			; | Goes to last digit
	inx			; |
	inx			; /
	ldy #$04	; \ From highest digit
ld746:
	lda P1Score,x	; | If this score digit is
	cmp ($21),y	; | under Highest Top Score Digit
	bcc ld765	; | then Top Score was not beaten
	bne ld752	; | so go to ld765 (stop checking)
	dex			; | if not equal then Top score is beaten
	dey			; | if equal then check the lower digit
	bpl ld746	; / until the last.
ld752:
	ldy #$00
	lda $3e		; \
	asl			; | Select Score ???
	asl			; | X = [$3E] * 5
	ora $3e		; |
	tax			; /
ld75b:
	lda P1Score,x	; \
	sta ($21),y	; | Copy Current Score
	inx			; | to Highest Top Score
	iny			; |
	cpy #$05	; |
	bne ld75b	; /
ld765:
	ldy #$04	; \
ld767:
	lda ($21),y	; | Copy Highest Top Score
	sta $000d,y	; | back to Current Top Score
	dey			; | 
	bpl ld767	; /
	inc $46		; Status Bar Update Flag
	lda $16					; \
	beq ld778				; | If Balloon Trip Mode then
	jsr lc539_rankupdate	; / Ranking Update
ld778:
	rts

ld779:
.BYTE $29,$2e,$33

ld77c_divide:	; Divide [$43] by Y
	sty $12
	ldx #$ff
	lda $43
ld782:
	sec			; \
	sbc $12		; | Subtract Y 
	inx			; | X + 1
	bcs ld782	; / If it doesn't overflow then continue
	clc
	adc $12		; Add Y value again to cancel overflow
	sta $43		; [$43] = Reminder
	txa			; A and X = Result
	rts

ld78f:
	cmp #$0a	; \ Check if Score Digit >= 0x0A
	bcs ld794	; | Then ...
	rts			; / Else return
ld794:
	sec			; \ Then subtract 0x0A
	sbc #$0a	; / from digit
	rts
;-----------------------

ld798_updatestatusbar:
	ldy $46		; \
	dey			; | 
	beq ld7a0	; |
	bpl ld805	; /
	rts

ld7a0:
	lda #$20	; \
	sta PPUADDR	; | PPUADDR = $2043
	lda #$43	; |
	sta PPUADDR	; /
	lda #$8e	; \ Upload I- to PPU
	sta PPUDATA	; /

	ldx #$04	; \
ld7b1:
	lda P1Score,x	; | Upload Player 1 Score to PPU
	sta PPUDATA	; |
	dex			; |
	bpl ld7b1	; |
	lda #$00	; |
	sta PPUDATA	; /

	lda #$24	; \
	sta PPUDATA	; | Upload 2 empty spaces to PPU
	sta PPUDATA	; /
	ldx #$8c	; \
	stx PPUDATA	; | Upload TOP- to PPU
	inx			; |
	stx PPUDATA	; /

	ldx #$04	; \
ld7d1:
	lda $0d,x	; | Upload Top Score to PPU
	sta PPUDATA	; |
	dex			; |
	bpl ld7d1	; |
	lda #0		; |
	sta PPUDATA	; /

	lda #$24	; \
	sta PPUDATA	; | Upload 2 empty spaces to PPU
	sta PPUDATA	; /

	lda $16		; \ If Game Mode is Balloon Trip Mode
	bne ld854	; / then render RANK 
	lda $40		; \ If Single Player
	beq ld802	; / then don't render Player 2 Score

	lda #$8f	; \ Upload II- to PPU
	sta PPUDATA	; /

	ldx #$04	; \
ld7f5:
	lda $08,x	; | Upload Player 2 Score to PPU
	sta PPUDATA	; |
	dex			; |
	bpl ld7f5	; |
	lda #0		; |
	sta PPUDATA	; /
ld802:
	dec $46
	rts
;-----------------------

ld805:
	dec $46
	lda #$20	; \
	sta PPUADDR	; | PPUADDR = $2062
	lda #$62	; | GAME OVER Player 1 Status Bar
	sta PPUADDR	; /
	lda $41		; \ If Player 1 Lives is negative
	jsr ld826	; / Then upload GAME OVER
	lda $40		; \ If Single Player
	beq ld83a	; / then return
	lda #$20	; \
	sta PPUADDR	; | PPUADDR = $2075
	lda #$75	; | GAME OVER Player 2 Status Bar
	sta PPUADDR	; /
	lda $42		; \ If Player 2 Lives is negative
ld826:
	bmi ld83b	; / Then upload GAME OVER
ld828:
	sta $50		; \
	ldx #$06	; |
ld82c:
	lda #$24	; | Upload amount of lives to PPU
	cpx $50		; |
	bcs ld834	; |
	lda #$2a	; |
ld834:
	sta PPUDATA	; |
	dex			; |
	bpl ld82c	; /
ld83a:
	rts

ld83b:
	lda $40		; \ If Single Player
	beq ld828	; / then go back
	ldx #$08			; \
ld841:
	lda GameOverText,x	; | Upload GAME OVER to PPU
	sta PPUDATA			; |
	dex					; |
	bpl ld841			; /
	rts

GameOverText:	;GAME OVER
.BYTE $1b,$0e,$1f,$18,$24,$0e,$16,$0a,$10

ld854:
	ldy #$04	; \
ld856:
	lda RankText,y	; | Upload RANK- to PPU
	sta PPUDATA	; |
	dey			; |
	bpl ld856	; /
	lda $4a		; \
	sta PPUDATA	; | Upload Rank Number to PPU
	lda $49		; |
	sta PPUDATA	; /
	dec $46
	rts

RankText:	;RANK-
.BYTE $fb,$fa,$f9,$f8,$f7

ld871:
	sta $12
	stx $13
	sty $14
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
	lda $12
	sta $061a,x
	tay
	txa
	asl
	asl
	asl
	tax
	lda ScorePopupLeft,y
	sta $02f1,x
	lda ScorePopupRight,y
	sta $02f5,x
	ldy $13
	lda $009a,y
	sec
	sbc #8
	sta $02f0,x
	sta $02f4,x
	lda $0091,y
	sta $02f3,x
	clc
	adc #8
	sta $02f7,x
	lda $3e
	sta $02f2,x
	sta $02f6,x
	ldy $14
	ldx $13
	lda $12
	rts
;-----------------------

ScorePopupLeft:
.BYTE $f4,$f5,$f6,$f7,$f8,$f9
ScorePopupRight:
.BYTE $fb,$fb,$fa,$fb,$fb,$fb

ld8dd:
	ldx #$01
ld8df:
	lda $061a,x
	bmi ld8fb
	dec $0618,x
	bne ld8fb
	lda #$ff
	sta $061a,x
	txa
	asl
	asl
	asl
	tay
	lda #$f0
	sta $02f0,y
	sta $02f4,y
ld8fb:
	dex
	bpl ld8df
	rts
;-----------------------

ld8ff:
	ldx #$01
ld901:
	lda #$00
	sta $0618,x
	lda #$ff
	sta $061a,x
	dex
	bpl ld901
	rts
;-----------------------

ld90f_uploadtitlescreen:
	jsr ld246_clearppu
	jsr lc10a_clearppumask
	jsr lf465_clearframeflag
	jsr lc0fa_disablenmi
	tpa TitleScreenHeader, $1d
	jsr ld497_uploadbackground
	jsr lc104_enablenmi
	jmp lc115

.include "TitleScreenData.asm"

ldac1_titlescreenloop:	; Manage Title Screen
	jsr lc104_enablenmi
	jsr ld90f_uploadtitlescreen
	lda #$00	; \ Reset Frame Counter
	sta $19		; /
ldacb:
	jsr lf465_clearframeflag
	lda $19		; \ If Frame Counter is 0
	beq ldaf1	; / then do demo?
	jsr ldb08	; Set Modes & Cursor
	jsr le768_polljoypad0
	tax
	and #$10	; \ If Start button is pressed
	bne ldaf0	; / then exit Title Screen loop
	txa
	and #$20	; \ If Select button is NOT pressed
	beq ldaed	; / then loop again
	lda #$00	; \ Reset Frame Counter
	sta $19		; /
	ldx $3f		; \
	lda ldb05,x	; | Select Next Mode
	sta $3f		; /
ldaed:
	jmp ldacb	; Loop
ldaf0:
	rts
;-----------------------

ldaf1:
	inc $3a		; Set Demo Flag
	inc $40		; Set to 2 Players
	lda #$00	; \ Disable All Sound Channels
	sta $4015	; /
	sta $16		; Set Game Mode to 00 (Balloon Fight)
	jsr lf1f2
	lda #$00
	sta $3a
	beq ldac1_titlescreenloop

ldb05:	;Title Screen choices
.BYTE 1,2,0

ldb08:
	lda $3f		; \
	lsr			; | Set Game Mode
	sta $16		; / depending on selected mode
	lda $3f		; \
	tax			; | Set Amount of players
	and #$01	; | depending on selected mode
	sta $40		; /
	lda MenuCursorYOptions,x	; \ Set Y position of menu cursor balloon
	sta $057b					; /
	lda #$2c	; \ Set X position of menu cursor balloon
	sta $0567	; /
	ldx #$00	; \ Set graphics of menu cursor balloon
	stx $055d	; /
	jmp lce2f_balloonxspritemanage

MenuCursorYOptions:
.BYTE $8c,$9c,$ac

.include "PhaseData.asm"

.include "SpriteAnimData.asm"

le3a4:
	lda OAMObjectOrder1,x	; \ Set Pointer from the first table
	sta $1f		; /
	lda $19		; \
	lsr			; | Every 2 frames
	bcc le3b3	; | Set Pointer from the second table
	lda OAMObjectOrder2,x	; |
	sta $1f		; /
le3b3:
	lda #$02	; \ Set Pointer to $02xx
	sta $20		; / (OAM)
	lda $88,x	; \ If Object X Balloons >= 0
	bpl le3cf	; /
	cmp #$ff	; \ If Object X Balloons == -1
	beq le3c2	; /
	jmp le4d5
le3c2:
	ldy #$14	; \
le3c4:
	lda #$f0	; |
	sta ($1f),y	; |
	dey			; | Hide 20 sprites
	dey			; |
	dey			; |
	dey			; |
	bpl le3c4	; /
	rts
le3cf:
	cpx #$08	; \ If Object is Fish
	beq le41b	; /
	lda $7f,x	; \
	asl			; | (Object Status * 4) + Animation Frame
	asl			; |
	adc $0436,x	; /
	cpx #$02	; \ If Object is a player
	bcs le408	; /
	ldy $88,x	; \ 
	adc le34c,y	; | Y = (Object Status * 4) + Animation Frame
	tay			; / + [le34c + Balloons]
	lda PlayerAnimLower,y	; \
	sta $1d					; | Set pointer
	lda PlayerAnimUpper,y	; |
	sta $1e					; /
	lda $bd,x	; \ If Player X is invincible
	beq le429	; /
	ldy $88,x	; Y = Player X Balloons
	lda le34c+3,y	; \
	adc $0436,x		; | Y = [le34c+3+Balloons+Frame]
	tay				; /
	lda PlayerFlashAnimLower,y	; \
	sta $1d						; | Set pointer
	lda PlayerFlashAnimUpper,y	; |
	sta $1e						; /
	jmp le429
le408:
	ldy $88,x
	clc
	adc le352,y
	tay
	lda EnemyAnimLower,y
	sta $1d
	lda EnemyAnimUpper,y
	sta $1e
	bne le429
le41b:
	ldy $7f,x
	bmi le3c2
	lda FishSprPointersLower,y
	sta $1d
	lda FishSprPointersUpper,y
	sta $1e
le429:
	lda $91,x
	sta $15
	lda $9a,x
	sta $12
	txa
	beq le444
	cpx #1
	bne le43c
	lda #1
	bne le444
le43c:
	lda $0451,x
	clc
	adc #2
	and #3
le444:
	ldy $0448,x
	beq le44b
	ora #$40
le44b:
	ldy $88,x
	cpy #2
	bne le459
	ldy $7f,x
	cpy #5
	bne le459
	eor #$40
le459:
	ldy $9a,x
	cpy #$c9
	bcs le463
	cpx #9
	bne le465
le463:
	ora #$20
le465:
	sta $14
	tpa le043, $21
	lda $0448,x
	beq le47c
	tpa le079, $21
le47c:
	ldy #0
	lda ($1d),y
	inc $1d
	bne le486
	inc $1e
le486:
	asl
	sta $13
	asl
	adc $13
	adc $21
	sta $21
	bcc le494
	inc $22
le494:
	txa
	pha
	ldx #5
	ldy #0
le49a:
	lda $12
	clc
	adc le03d,x
	sta ($1f),y
	sta $12
	iny
	sty $13
	ldy #0
	lda ($1d),y
	inc $1d
	bne le4b1
	inc $1e
le4b1:
	ldy $13
	sta ($1f),y
	iny
	lda $14
	sta ($1f),y
	iny
	sty $13
	ldy #0
	lda $15
	clc
	adc ($21),y
	inc $21
	bne le4ca
	inc $22
le4ca:
	ldy $13
	sta ($1f),y
	iny
	dex
	bpl le49a
	pla
	tax
	rts

le4d5:
	txa
	pha
	ldy $1f
	lda $9a,x
	sta $0200,y
	sta $0204,y
	clc
	adc #8
	sta $0208,y
	sta $020c,y
	lda #$f0
	sta $0210,y
	sta $0214,y
	lda $91,x
	sta $0203,y
	sta $020b,y
	clc
	adc #8
	sta $0207,y
	sta $020f,y
	lda $9a,x
	cmp #$d0
	lda #3
	bcc le50d
	lda #$23
le50d:
	sta $0202,y
	lda $7f,x
	bne le553
	lda $0202,y
	sta $0206,y
	sta $020a,y
	sta $020e,y
	lda #$da
	sta $0201,y
	lda #$db
	sta $0205,y
	lda #$dc
	sta $0209,y
	lda #$dd
	sta $020d,y
	ldx $1f
	lda $19
	and #$20
	beq le550
	lda $19
	and #$40
	bne le54a
	inc $0200,x
	inc $0204,x
	bne le550
le54a:
	inc $0203,x
	inc $020b,x
le550:
	pla
	tax
	rts
;-----------------------

le553:
	lda $0202,y
	ora #$40
	sta $0206,y
	ora #$80
	sta $020e,y
	and #$bf
	sta $020a,y
	lda #$de
	sta $0201,y
	sta $0205,y
	sta $0209,y
	sta $020d,y
	dec $045a,x
	bpl le584
	lda #$ff
	sta $88,x
	lda #$f0
	sta $9a,x
	lda #4
	sta $f1
le584:
	pla
	tax
	rts
;-----------------------

le587:
	ldx $bb
	bmi le5c4
	lda le5c5,x
	sta $1d
	lda le5ca,x
	sta $1e
	ldy #$00
	ldx #$00
le599:
	lda ($1d),y
	sta $02e0,x
	iny
	inx
	cmp #$f0
	bne le5a7
	inx
	inx
	inx
le5a7:
	cpx #$10
	bne le599
	ldy #$0f
le5ad:
	lda $02e0,y
	clc
	adc $bc
	sta $02e0,y
	dey
	dey
	dey
	dey
	bpl le5ad
	lda $19
	and #$03
	bne le5c4
	dec $bb		; Go to next water plonk animation frame
le5c4:
	rts
;-----------------------

.define SplashPointers Splash5, Splash4, Splash3, Splash2, Splash1
le5c5:
.LOBYTES SplashPointers
le5ca:
.HIBYTES SplashPointers

Splash1:
.BYTE $d0,$ae,$03,$04,$f0,$f0,$f0
Splash2:
.BYTE $c8,$af,$03,$04,$d0,$b0,$03,$04,$f0,$f0
Splash3:
.BYTE $c8,$b1,$03,$fc,$c8,$b2,$03,$04,$d0,$b3,$03,$04,$f0
Splash4:
.BYTE $c8,$b4,$03,$00,$c8,$b4,$43,$08,$d0,$b5,$03,$00,$d0,$b5,$43,$08
Splash5:
.BYTE $f0,$f0,$f0,$f0

le601:
    ;12 bytes
.BYTE $04,$04,$05,$06,$03,$03,$03,$06,$0a,$0a,$0a,$0a
le60d:
    ;12 bytes
.BYTE $28,$32,$46,$78,$00,$00,$00,$64,$00,$00,$00,$00
le619:
    ;12 bytes
.BYTE $0a,$1e,$32,$70,$00,$00,$00,$70,$00,$00,$00,$00
le625:
    ;12 bytes
.BYTE $14,$3c,$64,$a0,$00,$00,$00,$a0,$00,$00,$00,$00
le631:
    ;12 bytes
.BYTE $70,$b0,$e0,$40,$80,$80,$80,$40,$00,$00,$00,$00
le63d:
    ;12 bytes
.BYTE $00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$00
le649:
    ;12 bytes
.BYTE $90,$50,$20,$c0,$80,$80,$80,$c0,$00,$00,$00,$00
le655:
    ;12 bytes
.BYTE $ff,$ff,$ff,$fe,$ff,$ff,$ff,$fe,$00,$00,$00,$00
le661:
    ;12 bytes
.BYTE $50,$90,$c0,$40,$40,$40,$40,$40,$00,$00,$00,$00
le66d:
    ;12 bytes
.BYTE $00,$00,$00,$01,$00,$00,$00,$01,$02,$02,$02,$02
le679:
    ;12 bytes
.BYTE $b0,$70,$40,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
le685:
    ;12 bytes
.BYTE $ff,$ff,$ff,$fe,$ff,$ff,$ff,$fe,$fe,$01,$fe,$fe

le691_objectmanage:
	jsr lee25_collision
	ldx #$07	; \
le696:
	lda $88,x	; | Check all Object's Balloons
	bpl le6a4	; | If >= 0 then proceed
	cmp #$ff	; | else if == -1 then go to next object
	beq le6e2	; | else ? and go to next object
	jsr lecba	; |
	jmp le6e2	; /
le6a4:
	cpx #$02	; \ Object is Player
	bcc le6b8	; /
	cmp #$01	; \ One balloon
	bne le6b8	; /
	lda $7f,x	; \ Object Status >= 2
	cmp #$02	; |
	bcs le6b8	; /
	lda $f1		; \
	ora #$20	; | Play SFX
	sta $f1		; /
le6b8:
	dec $043f,x	; \ Object's ? != 0
	bne le6d9	; /
	lda #$03	; \ Object's ? = 3
	sta $043f,x	; /
	cpx #$02	; \ Object is not Player
	bcs le6ce	; /
	dec $bf,x	; \ Player Invincibility Handle
	bne le6ce	; | Decrease Time until 0
	lda #$00	; | Then disable invincibility
	sta $bd,x	; /
le6ce:
	jsr lea18_objectupdateanim
	stx $3e
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
;-----------------------

le6e9_objectupdateaction:
	cpx #$02	; \ If Enemy then rely on RNG
	bcs le705	; / If Player then rely on joypad
	lda $19		; \
	and #$0f	; | Enemy only reacts every 16 frames
	bne le6f8	; /
	jsr lf1b3_rng	; \ Update Enemy Action
	sta $31,x		; /
le6f8:
	lda $3a			; \ If Demo Play then
	bne le705		; / do automatic inputs
	jsr le76a_polljoypadx
	lda $061c,x		; \ Read Pressed Buttons
	sta $31,x		; / into Player Action
le704:
	rts
le705:		; Demo Play
	lda $9a,x	; \ If Player Y above #$A0
	cmp #$a0	; | Then
	bcc le712	; /
	lda $31,x	; \
	ora #$40	; | Do rapid fire
	sta $31,x	; / (B Button)
	rts
le712:
	dec $045a,x		; \
	bne le704		; / then return
	jsr lf1b3_rng
	ldy $0451,x
	and le762,y
	adc le762+3,y
	sta $045a,x
	stx $12
	lda $19
	rol
	rol
	eor $12
	and #$01
	tay
	lda $0088,y
	bmi le749
	lda $00bd,y
	bne le749
	lda #$00
	sta $31,x
	lda $009a,y
	sec
	sbc #$04
	cmp $9a,x
	bcs le74d
le749:
	lda #$40
	sta $31,x
le74d:
	lda $91,x
	cmp $0091,y
	bcs le75b
	lda $31,x
	ora #$01
	sta $31,x
	rts
le75b:
	lda $31,x
	ora #$02
	sta $31,x
	rts

le762:
.BYTE $1f,$0f,$07,$20,$10,$08


;----------------------
; Joypad Code
;----------------------

le768_polljoypad0:
	ldx #$00	; Read Controller 0
le76a_polljoypadx:
	lda #$01	; \
	sta JOY1	; | Output Strobe to both controllers
	lda #$00	; |
	sta JOY1	; /
	ldy #$07
le776:
	lda JOY1,x	; \
	sta $12		; |
	lsr			; | Poll Controller X
	ora $12		; | to $061C + X
	lsr			; |
	rol $061c,x	; |
	dey			; |
	bpl le776	; /
	ldy $061e,x	; \
	lda $061c,x	; |
	sta $061e,x	; | Check for pressed buttons
	tya			; |
	eor $061c,x	; |
	and $061c,x	; /
	rts			; Returns pressed buttons in A
;-----------------------

le796:
	lda $88,x	; \ If object has balloons
	bne le7a3	; / then continue
le79a:
	lda #$00	; \ If no balloons:
	sta $0424,x	; | X Velocity = 0
	sta $042d,x	; /
	rts			; Return
le7a3:
	cmp #$02	; \ If 2 balloons
	beq le7e8	; /
	cpx #$02	; \ If object is a player
	bcc le7e8	; /
	lda $7f,x	; \ If Object Status >= 2
	cmp #$02	; | then zero X velocity
	bcs le79a	; /
le7b1:
	lda $0424,x
	sta $12
	lda $042d,x
	sta $13
	jsr lf1a6
	lda $0463,x
	clc
	adc $12
	sta $0463,x
	sta $12
	lda $046c,x
	adc $13
	sta $046c,x
	sta $13
	jsr lf1a6
	lda $0424,x
	sec
	sbc $12
	sta $0424,x
	lda $042d,x
	sbc $13
	sta $042d,x
	rts
le7e8:
	lda $7f,x
	cmp #$06
	bcc le7ef
	rts
le7ef:
	lda $7f,x
	cmp #$04
	bne le811
	lda $31,x
	and #$02
	beq le802
	lda $0448,x
	beq le811
	bne le80d
le802:
	lda $31,x
	and #$01
	beq le811
	lda $0448,x
	bne le811
le80d:
	lda #$05
	sta $7f,x
le811:
	lda $7f,x
	cmp #$02
	bne le832
	lda $31,x
	and #$02
	beq le821
	lda #$00
	beq le829
le821:
	lda $31,x
	and #$01
	beq le82e
	lda #$01
le829:
	cmp $0448,x
	beq le832
le82e:
	lda #$04
	sta $7f,x
le832:
	lda $7f,x
	cmp #$04
	bcc le854
	lda $31,x
	and #$02
	beq le845
	lda $0448,x
	bne le854
	beq le850
le845:
	lda $31,x
	and #$01
	beq le854
	lda $0448,x
	beq le854
le850:
	lda #$02
	sta $7f,x
le854:
	lda $7f,x
	cmp #$03
	bne le864
	lda $31,x
	and #$03
	beq le864
	lda #$02
	sta $7f,x
le864:
	lda $7f,x
	cmp #$04
	bcs le87f
	lda $31,x
	and #$02
	beq le874
	lda #$00
	beq le87c
le874:
	lda $31,x
	and #$01
	beq le87f
	lda #$01
le87c:
	sta $0448,x
le87f:
	lda $7f,x
	cmp #$04
	bcc le8b8
	lda $0436,x
	cmp #$01
	bne le8b8
	ldy $0451,x
	lda $0448,x
	beq le8a6
	lda $0424,x
	sec
	sbc le625,y
	sta $0424,x
	lda $042d,x
	sbc #$00
	jmp le901
le8a6:
	lda $0424,x
	clc
	adc le625,y
	sta $0424,x
	lda $042d,x
	adc #$00
	jmp le901
le8b8:
	lda $7f,x
	beq le8c7
	cmp #$02
	beq le907
	cmp #$03
	beq le8c7
	jmp le951
le8c7:
	lda $0436,x
	cmp #$01
	beq le8d1
	jmp le951
le8d1:
	ldy $0451,x
	lda $31,x
	and #$02
	beq le8ec
	lda $0424,x
	sec
	sbc le619,y
	sta $0424,x
	lda $042d,x
	sbc #$00
	jmp le901
le8ec:
	lda $31,x
	and #$01
	beq le951
	lda $0424,x
	clc
	adc le619,y
	sta $0424,x
	lda $042d,x
	adc #$00
le901:
	sta $042d,x
	jmp le951
le907:
	lda $0436,x
	cmp #$01
	bne le951
	ldy $0451,x
	lda $31,x
	and #$02
	beq le929
	lda $0424,x
	sec
	sbc le625,y
	sta $0424,x
	lda $042d,x
	sbc #$00
	jmp le93e
le929:
	lda $31,x
	and #$01
	beq le951
	lda $0424,x
	clc
	adc le625,y
	sta $0424,x
	lda $042d,x
	adc #$00
le93e:
	sta $042d,x
	lda $31,x
	and #$03
	beq le951
	cpx #$02
	bcs le951
	lda $f0
	ora #$08
	sta $f0
le951:
	lda $7f,x
	cmp #$04
	bcc le982
	lda $0448,x
	bne le963
	lda $042d,x
	bmi le982
	bpl le968
le963:
	lda $042d,x
	bpl le982
le968:
	lda $7f,x
	cmp #$05
	bne le976
	lda $0448,x
	eor #$01
	sta $0448,x
le976:
	lda #$03
	sta $7f,x
	lda #$00
	sta $0424,x
	sta $042d,x
le982:
	rts
;-----------------------

le983:
	lda $cb
	bne le9b6
	lda $bd,x
	beq le99a
	lda $0488
	beq le99a
	sec
	sbc $91,x
	jsr lf08e_abs
	cmp #5
	bcc le9b6
le99a:
	cpx #2
	bcc le9a4
	lda $88,x
	cmp #2
	bne le9f2
le9a4:
	lda $7f,x
	cmp #2
	bcc le9f2
	cmp #6
	bcs le9f2
	lda #1
	sta $7f,x
	sta $045a,x
	rts
le9b6:
	lda #0
	sta $0412,x
	sta $041b,x
	sta $0409,x
	sta $cb
	cpx #2
	bcc le9fd
	lda $88,x
	cmp #2
	beq le9f3
	cmp #1
	bne le9f2
	lda $7f,x
	cmp #2
	bcs le9f2
	lda #2
	sta $7f,x
	lda $c6
	sta $043f,x
	lda #0
	sta $0424,x
	sta $042d,x
	sta $0463,x
	sta $046c,x
	lda #$40	; \ Play SFX
	sta $f1		; /
le9f2:
	rts
le9f3:
	lda #0
	sta $7f,x
	lda #1
	sta $045a,x
	rts
le9fd:
	lda $7f,x
	cmp #1
	bne lea17
	cmp #6
	bcs lea17
	lda $0424,x
	ora $042d,x
	bne lea13
	lda #3
	bne lea15
lea13:
	lda #2
lea15:
	sta $7f,x
lea17:
	rts


;----------------------
; Object Code
;----------------------

lea18_objectupdateanim:
	cpx #$02	; \ Object is not Player
	bcs lea2c	; /
	lda $bd,x	; \ If Player X Invincible
	bne lea44	; /
	lda $7f,x	; \ If Player X Status == 1
	cmp #$01	; | Then update animation every 8th frame
	beq lea3e	; /
	cmp #$03	; \ If Player X Status != 3
	bne lea44	; | Then update animation
	beq lea3e	; / Else update animation every 8th frame
lea2c:
	lda $7f,x	; \ If Enemy Status == 1
	cmp #$01	; | Then update animation every 8th frame
	beq lea3e	; /
	cmp #$03	; \ If Enemy Status < 1
	bcc lea44	; / Then update animation
	lda $19		; \
	and #$03	; | Update Animation Frame
	bne lea47	; | every 4 frames
	beq lea44	; /
lea3e:
	lda $19		; \ Update Animation Frame
	and #$07	; | every 8 frames
	bne lea47	; /
lea44:
	inc $0436,x	; Increment Animation Frame
lea47:
	lda $0436,x	; \
	and #$03	; | Stay within Frame 0 to 3
	sta $0436,x	; /
	bne lea57	; 
	lda $7f,x	; \
	bne lea57	; | Increment Status if not 0
	inc $7f,x	; /
lea57:
	rts
;-----------------------

lea58:
	lda $0475,x
	beq lea60
	dec $0475,x
lea60:
	cpx #2
	bcs lea8c
	lda $c1,x
	beq lea8c
	lda $19
	lsr
	bcc lea8b
	inc $0436,x
	lda $0436,x
	and #3
	sta $0436,x
	lda #1
	sta $7f,x
	dec $045a,x
	bne lea8b
	lda #0
	sta $c1,x
	sta $7f,x
	lda #$20
	sta $f0
lea8b:
	rts
lea8c:
	lda $0412,x
	clc
	ldy $0451,x
	adc le601,y
	sta $0412,x
	bcc lea9e
	inc $041b,x
lea9e:
	lda $041b,x
	bmi leac1
	cmp le66d,y
	bcc leadc
	bne leab2
	lda $0412,x
	cmp le661,y
	bcc leadc
leab2:
	lda le661,y
	sta $0412,x
	lda le66d,y
	sta $041b,x
	jmp leadc
leac1:
	cmp le685,y
	bcc lead0
	bne leadc
	lda $0412,x
	cmp le679,y
	bcs leadc
lead0:
	lda le679,y
	sta $0412,x
	lda le685,y
	sta $041b,x
leadc:
	jsr leba0_objectapplyyvelocity
	cmp #$f8
	bcs leb0d
	cmp #$e8
	bcc leb0d
	lda #$ff
	sta $88,x
	lda #$04	; \ Do Water Plonk
	sta $bb		; / Animation
	lda $91,x
	sta $bc
	cpx #2
	bcc leb05
	lda #$80
	sta $88,x
	lda #0
	sta $7f,x
	lda #1
	sta $f3
	bne leb0d
leb05:
	lda $c8
	bne leb0d
	lda #$40
	sta $f0
leb0d:
	lda $042d,x
	bmi leb30
	cmp le63d,y
	bcc leb4b
	bne leb21
	lda $0424,x
	cmp le631,y
	bcc leb4b
leb21:
	lda le631,y
	sta $0424,x
	lda le63d,y
	sta $042d,x
	jmp leb4b
leb30:
	cmp le655,y
	bcc leb3f
	bne leb4b
	lda $0424,x
	cmp le649,y
	bcs leb4b
leb3f:
	lda le649,y
	sta $0424,x
	lda le655,y
	sta $042d,x
leb4b:
	jsr leb8e_objectapplyxvelocity
	lda $16
	beq leb62
	lda $91,x
	cmp #$10
	bcs leb5a
	lda #$10
leb5a:
	cmp #$e0
	bcc leb60
	lda #$e0
leb60:
	sta $91,x
leb62:
	lda $c8
	beq leb8d
	lda $88,x
	bne leb8d
	lda $9a,x
	cmp #$c8
	bcc leb8d
	lda #$c7
	sta $9a,x
	lda $0451,x
	cmp #$0b
	bne leb84
	dec $0451,x
	jsr lf107_reverseyvelocity
	jmp lf18c
leb84:
	lda #2
	sta $88,x
	lda #3
	sta $0451,x
leb8d:
	rts
;-----------------------

leb8e_objectapplyxvelocity:
	lda $0400,x	; \
	clc			; | Apply Velocity to
	adc $0424,x	; | X Position (Frac)
	sta $0400,x	; /
	lda $91,x	; \ Apply Velocity to
	adc $042d,x	; | X Position (Int)
	sta $91,x	; /
	rts
;-----------------------

leba0_objectapplyyvelocity:
	lda $0409,x	; \
	clc			; | Apply Velocity to
	adc $0412,x	; | Y Position (Frac)
	sta $0409,x	; /
	lda $9a,x	; \ Apply Velocity to
	adc $041b,x	; | Y Position (Int)
	sta $9a,x	; /
	rts
;-----------------------

lebb2:
	jsr lf0b4_swapxy
	jsr leb8e_objectapplyxvelocity
	jmp lf0b4_swapxy
lebbb:
	jsr lf0b4_swapxy
	jsr leba0_objectapplyyvelocity
	jmp lf0b4_swapxy

lebc4:
	cpx #$02	; \ If not player
	bcs lebe3	; /
	lda $88,x	; \ If player still has balloons
	bne lebd6	; /
	lda $0436,x	; \ If player animation frame != 0
	bne lebd6	; /
	lda #$00	; \ Then Player Status = 0 (Dead)
	sta $7f,x	; /
	rts
lebd6:	; Player
	lda $7f,x	; \ If Player Status < 6
	cmp #$06	; | Then ?
	bcc lec38	; /
	lda #$01	; \ Else Status = 1
	sta $7f,x	; /
	dec $88,x	; Decrease one balloon
	rts
lebe3:	; Enemy
	lda $88,x	; \ If Enemy Status == 2
	cmp #$02	; | Then ?
	beq lec38	; /
	lda $0436,x	; \ If enemy animation frames != 0
	bne lebfd	; / Then
	lda $88,x	; \ If Enemy Status != 0
	bne lebf7	; / Then
	lda #$00	; \ Enemy Status = 0 (Dead)
	sta $7f,x	; /
	rts
lebf7:
	lda $7f,x	; \ If Enemy Status != 0
	bne lebfe	; / Then
	inc $7f,x	; Increase Enemy Status
lebfd:
	rts
lebfe:
	cmp #$02	; \ If Player
	bcc lebfd	; / then return
	dec $045a,x
	bne lec37
	lda $c7
	sta $045a,x
	inc $7f,x
	lda $7f,x
	cmp #$07
	bcc lec37
	lda #$02
	sta $88,x
	lda #$00
	sta $7f,x
	ldy $0451,x
	lda lecae,y
	ldy $047e,x
	bne lec2f
	dec $047e,x
	lda $0451,x
	and #$03
lec2f:
	sta $0451,x
	lda #$fe
	sta $041b,x
lec37:
	rts
lec38:
	jsr le6e9_objectupdateaction
	lda $31,x	; \ Check valid actions
	and #$c3	; | Left/Right/B/A
	beq lec49	; /
	cpx #$02	; \ If Enemy
	bcs lec49	; / Skip
	lda #$00	; \ If Player
	sta $bd,x	; / Disable invincibility
lec49:
	lda $31,x	; \
	and #$40	; | B button
	bne lec61	; /
	lda $31,x	; \
	and #$80	; | A button
	bne lec5c	; /
	lda #$00	; \
	sta $0620,x	; | ?
	beq lecad	; / Return
lec5c:
	lda $0620,x	; \
	bne lecad	; / Return
lec61:
	lda $7f,x
	cmp #$02
	bcc lec75
	dec $9a,x
	dec $9a,x
	lda #$00
	sta $0412,x
	sta $041b,x
	beq lec7e
lec75:
	cmp #1
	beq lec7e
	lda $0436,x
	bne lecad	; Return
lec7e:
	lda #0
	sta $7f,x
	lda #1
	sta $0436,x
	lda #1
	sta $0620,x
	ldy #0
	cpx #2
	bcc lec93
	iny
lec93:
	lda $00f0,y
	ora #$10
	sta $00f0,y
	lda $0412,x
	sec
	ldy $0451,x
	sbc le60d,y
	sta $0412,x
	bcs lecad
	dec $041b,x
lecad:
	rts
;-----------------------

lecae:
    ;12 bytes
.BYTE $01,$02,$02,$03,$01,$02,$02,$03,$01,$02,$02,$03

lecba:
	lda $7f,x	; \ If Object(x).Status != 0
	bne led27	; / then don't do anything
	jsr le7b1
	jsr leb8e_objectapplyxvelocity
	lda $0409,x
	sec
	sbc #$60
	sta $0409,x
	lda $9a,x
	sbc #0
	sta $9a,x
	cmp #$f1
	bcc lecdb
	lda #$ff
	sta $88,x
lecdb:
	txa
	pha
	ldy #1
lecdf:
	lda $0088,y
	beq led22
	bmi led22
	lda $9a,x
	sec
	sbc $009a,y
	jsr lf08e_abs
	cmp #$18
	bcs led22
	lda $91,x
	sec
	sbc $0091,y
	jsr lf08e_abs
	cmp #$10
	bcs led22
	lda #$ff
	sta $7f,x
	lda #3
	sta $045a,x
	lda #$78
	sta $c5
	lda #2
	sta $f0
	lda #$32
	sty $3e
	jsr ld6de_scoreadd
	lda #1
	ldx $3e
	jsr ld871
	pla
	tax
	rts
led22:
	dey
	bpl lecdf
	pla
	tax
led27:
	rts
;-----------------------

led28:
	ldy $88,x
	dey
	bpl led2e
led2d:
	rts
led2e:
	lda $9a,x
	cmp #$f9
	bcc led40
	lda $041b,x
	bpl led2d
	lda #0
	sta $cc
	jmp lede1
led40:
	ldy $cd
	bmi led27
led44:
	lda #0
	sta $cc
	lda ($27),y
	sec
	sbc #$18
	cmp $9a,x
	bcs ledb6
	adc #3
	cmp $9a,x
	bcc led5b
	lda #1
	bne led69
led5b:
	lda ($29),y
	cmp $9a,x
	bcc ledb6
	sbc #3
	cmp $9a,x
	bcs led89
	lda #2
led69:
	sta $cc
	lda ($23),y
	cmp #$10
	beq led78
	sec
	sbc #$0c
	cmp $91,x
	bcs led85
led78:
	lda ($25),y
	cmp #$ff
	beq led89
	sec
	sbc #4
	cmp $91,x
	bcs led89
led85:
	lda #0
	sta $cc
led89:
	lda ($23),y
	sec
	sbc #$10
	beq leda0
	cmp $91,x
	bcs ledb6
	adc #4
	cmp $91,x
	bcc leda0
	lda $cc
	ora #4
	bne ledb4
leda0:
	lda ($25),y
	cmp #$ff
	beq ledb6
	cmp $91,x
	bcc ledb6
	sbc #4
	cmp $91,x
	bcs ledb6
	lda $cc
	ora #8
ledb4:
	sta $cc
ledb6:
	lda $cc
	bne ledc1
	dey
	bmi ledc0
	jmp led44
ledc0:
	rts
;-----------------------

ledc1:
	lsr $cc
	bcc ledd6
	lda $041b,x
	bmi ledd6
	lda ($27),y
	sbc #$18
	sta $9a,x
	inc $9a,x
	lda #1
	sta $cb
ledd6:
	lsr $cc
	bcc ledf4
	lda $041b,x
	bpl ledf4
	lda ($29),y
lede1:
	sta $9a,x
	jsr lf107_reverseyvelocity
	jsr lf18c
	cpx #2
	bcs ledf0
	jsr lcc33
ledf0:
	lda $cb
	bne lee24
ledf4:
	lsr $cc
	bcc ledff
	lda $042d,x
	bmi ledff
	bpl lee08
ledff:
	lsr $cc
	bcc lee24
	lda $042d,x
	bpl lee24
lee08:
	jsr lf0de_reversexvelocity
	jsr lf172
	lda $042d,x
	ora $0424,x
	beq lee24
	lda $0448,x
	eor #1
	sta $0448,x
	lda $f1
	ora #2
	sta $f1
lee24:
	rts
;-----------------------

lee25_collision:
	ldx #$07	; Seems to compare balloons from objects
lee27:
	stx $12
	ldy $12
	dey
	bpl lee31
lee2e:
	jmp lef2a
lee31:
	lda $88,x	; \ If Object(x).Balloon <= 0
	bmi lee2e	; | then skip
	beq lee2e	; /
	lda $0088,y	; \ If Object(y).Balloon <= 0
	bmi lee2e	; | then skip
	beq lee2e	; /
	lda #$00
	sta $cc
	lda $009a,y		; \
	sec				; | 
	sbc $9a,x		; | If abs(Object(y).Y - Object(x).Y)
	jsr lf08e_abs	; | <= #$18
	cmp #$18		; | then
	bcs leec0		; /
	lda $9a,x		; \
	clc				; | If
	adc #$18		; | abs((Object(y).Y + 7)
	sta $12			; |   - (Object(x).Y + #$18))
	lda $009a,y		; | >= 4 then
	clc				; |
	adc #$07		; |
	sec				; |
	sbc $12			; |
	jsr lf08e_abs	; |
	cmp #$04		; |
	bcs lee6a		; /
	lda #$01
	bne lee7c
lee6a:
	lda $009a,y		; \
	clc				; | If abs(Object(y).Y + #$11 - Object(x).Y)
	adc #$11		; | >= 4 then
	sec				; |
	sbc $9a,x		; |
	jsr lf08e_abs	; |
	cmp #$04		; |
	bcs lee8f		; /
	lda #$02
lee7c:
	sta $cc
	lda $0091,y		; \
	sec				; | If abs(Object(y).X - Object(x).X)
	sbc $91,x		; | < #$10 then
	jsr lf08e_abs	; |
	cmp #$10		; |
	bcc lee8f		; /
	lda #$00
	sta $cc
lee8f:
	lda $91,x		; \
	clc				; |
	adc #$10		; | If abs((Object(y).X + 7)
	sta $12			; |      - (Object(x).X + #$10))
	lda $0091,y		; | >= 4 then
	clc				; |
	adc #$07		; |
	sec				; |
	sbc $12			; |
	jsr lf08e_abs	; |
	cmp #$04		; |
	bcs leeaa		; /
	lda #$04
	bne leebc
leeaa:
	lda $0091,y		; \
	clc				; | If abs(Object(y).X + 9 - Object(x).X)
	adc #$09		; | >= 4 then
	sec				; |
	sbc $91,x		; |
	jsr lf08e_abs	; |
	cmp #$04		; |
	bcs leec0		; /
	lda #$08
leebc:
	ora $cc
	sta $cc
leec0:
	lda #$00	; \
	sta $4b		; /
	lsr $cc		; \ [$CC].bit0 = Velocity Y related
	bcc leecd	; |
	jsr lf0a6	; | 
	bmi leed6	; /
leecd:
	lsr $cc		; \ [$CC].bit1 = Velocity Y related
	bcc leef1	; |
	jsr lf0a6	; |
	bmi leef1	; /
leed6:
	jsr lf0bd	; \ Do both object X and Y exist?
	bcs leeed	; /
	jsr lf107_reverseyvelocity
	jsr lf18c
	jsr lf0b4_swapxy
	jsr lf107_reverseyvelocity
	jsr lf18c
	jsr lf0b4_swapxy
leeed:
	lda #$01
	sta $4b
leef1:
	lsr $cc		; \ [$CC].bit2 = Velocity X related
	bcc leefa	; |
	jsr lf098	; |
	bmi lef03	; /
leefa:
	lsr $cc		; \ [$CC].bit3 = Velocity X related
	bcc lef1e	; |
	jsr lf098	; |
	bmi lef1e	; /
lef03:
	jsr lf0bd	; \ Do both object X and Y exist?
	bcs lef1a	; /
	jsr lf0de_reversexvelocity
	jsr lf172
	jsr lf0b4_swapxy
	jsr lf0de_reversexvelocity
	jsr lf172
	jsr lf0b4_swapxy
lef1a:
	lda #$01
	sta $4b
lef1e:
	jsr lef37
	jsr lf0b4_swapxy
	jsr lef37
	jsr lf0b4_swapxy
lef2a:
	dey			; \
	bmi lef30	; | Loop Y Objects
	jmp lee31	; /
lef30:
	dex			; \
	bmi lef36	; | Loop X Objects
	jmp lee27	; /
lef36:
	rts
;-----------------------

lef37:
	cpx #$02	; \ Is Object X a player?
	bcc lef42	; |
	cpy #$02	; | Is Object Y a player?
	bcc lef42	; /
	jmp lf043	; Skip
lef42:
	lda #$00
	sta $0487
	lda $0475,x
	beq lef4f
	jmp lf043	; Skip
lef4f:
	lda $4b
	bne lef56
	jmp lf043	; Skip
lef56:
	cpx #$02
	bcs lef61
	lda $bd,x
	beq lef72
	jmp lf043	; Skip
lef61:
	lda $88,x
	cmp #$01
	bne lef72
	lda $7f,x
	cmp #$02
	bcs lef7f
	lda #$01
	sta $0487
lef72:
	lda $009a,y
	clc
	adc #$04
	cmp $9a,x
	bcc lef7f
	jmp lf043	; Skip
lef7f:
	lda #$14
	sta $0475,x
	lda #$00
	sta $0436,x
	cpy #$02
	bcc lef97
	lda $0088,y
	cmp #$02
	beq lef97
	jmp lf043	; Skip
lef97:
	lda $f0
	ora #$02
	sta $f0
	lda $88,x
	cmp #$02
	bne lefc0
	cpx #$02
	bcs lefc0
	sty $12
	ldy $7f,x
	lda lf053,y
	ldy $12
	pha
	pla
	bne lefb7
	jmp lf043	; Skip
lefb7:
	sta $7f,x
	lda #$00
	sta $0436,x
	beq lefea
lefc0:
	dec $88,x
	bne lefce
	lda #$ff
	sta $041b,x
	lda #$00
	sta $0412,x
lefce:
	lda #$00
	sta $7f,x
	sta $0424,x
	sta $042d,x
	lda $91,x
	bmi lefe0
	lda #$ff
	bne lefe2
lefe0:
	lda #$00
lefe2:
	sta $046c,x
	lda #$80
	sta $0463,x
lefea:
	sty $12
	ldy $0451,x
	lda lf05e,y
	sta $0451,x
	lda #$01
	sta $047e,x
	ldy $12
	cpy #$02
	bcs lf043	; Skip
	lda $0451,x
	cmp #$07
	beq lf011
	cmp #$08
	bcc lf011
	lda $f1
	ora #$80
	sta $f1
lf011:
	ldy $0451,x
	lda lf06a,y
	sta $13
	lda $0487
	beq lf023
	lda lf076,y
	sta $13
lf023:
	lda lf082,y
	clc
	adc $0487
	sta $14
	lda $12
	sta $3e
	pha
	txa
	pha
	lda $13
	pha
	lda $14
	jsr ld871
	pla
	jsr ld6de_scoreadd
	pla
	tax
	pla
	tay
lf043:
	lda $0451,x	; \ If Object X is not dead
	cmp #$0b	; | then don't play any SFX
	bne lf052	; /
	lda $c8		; \ If it's Bonus Phase
	bne lf052	; / then don't play any SFX
	lda #$20	; \ Play SFX
	sta $f0		; /
lf052:
	rts
;-----------------------

lf053:
    ;11 bytes
.BYTE $06,$06,$07,$08,$09,$0a,$00,$00,$00,$00,$00
lf05e:
    ;12 bytes
.BYTE $04,$05,$06,$07,$08,$09,$0a,$0b,$08,$09,$0a,$0b
lf06a:
    ;12 bytes
.BYTE $00,$00,$00,$00,$32,$4b,$64,$64,$4b,$64,$96,$64
lf076:
    ;12 bytes
.BYTE $00,$00,$00,$00,$32,$4b,$64,$64,$64,$96,$c8,$64
lf082:
    ;12 bytes
.BYTE $00,$00,$00,$00,$01,$02,$03,$03,$02,$03,$04,$03

lf08e_abs:
	pha			; \
	pla			; |
	bpl lf097	; | Get Absolute Value of A
	eor #$ff	; |
	clc			; |
	adc #$01	; |
lf097:
	rts			; /
;-----------------------

lf098:
	lda $0424,y ; \ Object(y).XVelocityFrac - Object(x).XVelocityFrac
	sec			; |
	sbc $0424,x	; /
	lda $042d,y	; \ Object(y).XVelocity - Object(x).XVelocity
	sbc $042d,x	; /
	rts
;-----------------------

lf0a6:
	lda $0412,y	; \ Object(y).YVelocityFrac - Object(x).YVelocityFrac
	sec			; |
	sbc $0412,x	; /
	lda $041b,y	; \ Object(y).YVelocity - Object(x).YVelocity
	sbc $041b,x	; /
	rts
;-----------------------

lf0b4_swapxy:
	stx $12
	sty $13
	ldx $13
	ldy $12
	rts
;-----------------------

lf0bd:
	cpx #$02	; \
	bcc lf0dd	; /
	lda $7f,x	; \ If Object(x).Status < 2
	cmp #$02	; |
	bcc lf0dd	; /
	lda #$01	; \ If 1 - Object(x).Balloons >= 0
	cmp $88,x	; |
	bcs lf0dd	; |
	cpy #$02	; | If 1 - Object(x).Balloons - 2 < 0
	bcc lf0dd	; /
	lda $007f,y	; \ If Object(y).Status < 2
	cmp #$02	; |
	bcc lf0dd	; /
	lda #$01	; \ If 1 - Object(y).Balloons
	cmp $0088,y	; /
lf0dd:
	rts
;-----------------------

lf0de_reversexvelocity:
	lda #$00	; \
	sec			; |
	sbc $0424,x	; | Reverse X Velocity of Object X
	sta $0424,x	; | (Bounce Horizontally)
	lda #$00	; |
	sbc $042d,x	; |
	sta $042d,x	; /
	lda #$00	; \
	sec			; | ?
	sbc $0463,x	; |
	sta $0463,x	; /
	lda #$00	; \
	sbc $046c,x	; | ?
	sta $046c,x	; /
	lda $31,x	; \
	and #$40	; | ?
	sta $31,x	; /
	rts
;-----------------------

lf107_reverseyvelocity:
	lda #$00	; \
	sec			; |
	sbc $0412,x	; | Reverse Y Velocity of Object X
	sta $0412,x	; | (Bounce Vertically)
	lda #$00	; |
	sbc $041b,x	; |
	sta $041b,x	; |
	rts			; /
;-----------------------

lf119:
	sta $2d
	lda $2c		; \ If Velocity Int >= 0
	bpl lf143	; / then goto lf143
	lda #$00	; \
	sec			; | Get absolute value of Velocity Frac
	sbc $2b		; |
	sta $2b		; /
	lda #$00	; \
	sbc $2c		; | Get absolute value of Velocity Int
	sta $2c		; /
	jsr lf143
	lda #$00
	sec
	sbc $2e
	sta $2e
	lda #$00
	sbc $2f
	sta $2f
	lda #$00
	sbc $30
	sta $30
	rts

lf143:
	phx
	lda #$00	; \
	sta $2e		; | Init
	sta $2f		; |
	sta $30		; /
	ldx #$08	; \ -Loop 8 times
lf14f:
	asl $2e		; |
	rol $2f		; |
	rol $30		; |
	asl $2d		; |
	bcc lf16c	; |
	clc			; |
	lda $2b		; | Old Velocity Frac
	adc $2e		; |
	sta $2e		; |
	lda $2c		; | Old Velocity Int
	adc $2f		; |
	sta $2f		; |
	lda #$00	; |
	adc $30		; |
	sta $30		; |
lf16c:
	dex			; |
	bne lf14f	; /
	plx
	rts
;-----------------------

lf172:
	lda $0424,x	; \ X Velocity Frac
	sta $2b		; /
	lda $042d,x	; \ X Velocity Int
	sta $2c		; /
	lda #$cd	; \ ?
	jsr lf119	; /
	lda $2f		; \ Update X Velocity Frac
	sta $0424,x	; /
	lda $30		; \ Update X Velocity Int
	sta $042d,x	; /
	rts
;-----------------------

lf18c:
	lda $0412,x	; \ Y Velocity Frac
	sta $2b		; /
	lda $041b,x	; \ Y Velocity Int
	sta $2c		; /
	lda #$cd	; \ ?
	jsr lf119	; /
	lda $2f		; \ Update Y Velocity Frac
	sta $0412,x	; /
	lda $30		; \ Update Y Velocity Int
	sta $041b,x	; /
	rts
;-----------------------

lf1a6:
	ldy #$04
lf1a8:
	lda $13
	asl
	ror $13
	ror $12
	dey
	bne lf1a8
	rts
;-----------------------

lf1b3_rng:
	phx
	ldx #11			; \ Loop 11 times
lf1b7:
	asl RNGLower	; |
	rol RNGUpper	; |
	rol				; |
	rol				; | Do Pseudo Random
	eor RNGLower	; | Number Generator stuff?
	rol				; |
	eor RNGLower	; |
	lsr				; |
	lsr				; |
	eor #$ff		; |
	and #$01		; |
	ora RNGLower	; |
	sta RNGLower	; |
	dex				; |
	bne lf1b7		; /
	plx
	lda RNGOutput	; Return A = [$1B]
	rts
;-----------------------

lf1d4:
	jsr ldac1_titlescreenloop
	ldx #$09	; \
lf1d9:
	lda #$00	; | Player 1 Score to 000000
	sta P1Score,x	; |
	dex			; |
	bpl lf1d9	; /
	sta $3e		; Update Player 1 Score
	inc $41		; +1 Life to Player 1
	jsr ld6de_scoreadd	; Update Player Score
	lda #$0f	; \ Enable Sound Channels
	sta $4015	; /
	lda #$01	; \ Stop All Sounds
	sta $f0		; /
	lda #$02
lf1f2:
	sta $41		; Set Player 1 Lives to 2
	ldy $40		; \ If it's 2 players
	bne lf1fa	; | Then give lives to Player 2
	lda #$ff	; / Else no lives
lf1fa:
	sta $42		; Set Player 2 Lives to -1 or 2
	ldx #$00
	stx $0488
	stx $3b		; Current Level Header = 0
	stx $3c		; Current Phase = 0
	stx $0558	; Bonus Phase Level = 0
	dex
	stx $89		; Set Player 2 Balloons to -1
	ldx $40						; \
lf20d:
	jsr lf3b0_initplayertype	; | Set up both player types
	dex							; |
	bpl lf20d					; /
lf213:
	lda #$00	; \ Set to Regular Phase
	sta $c8		; /
	lda $3c		; \
	lsr			; |
	lsr			; | (Current Phase >> 2) cannot
	cmp #$08	; | be higher than 8
	bcc lf221	; |
	lda #$08	; /
lf221:
	tax
	lda lf3ba,x
	sta $c6
	lda lf3c3,x
	sta $c7
	lda $3c		; \
	cmp #$02	; | If Current Phase >= 2
	bcs lf238	; / then
	lda #$03
	sta $c6
	sta $c7
lf238:
	ldx #$07	; \
lf23a:
	lda #$00	; | Initialize variables for each object (except Fish?)
	sta $0448,x	; | - Direction (0 = Left, 1 = Right)
	sta $0475,x	; |
	sta $047e,x	; |
	sta $0424,x	; | - X Velocity (Frac)
	sta $042d,x	; | - X Velocity (Int)
	sta $0412,x	; | - Y Velocity (Frac)
	sta $041b,x	; | - Y Velocity (Int)
	sta $0463,x	; |
	sta $046c,x	; |
	sta $0400,x	; | - X Positions (Frac)
	sta $0409,x	; | - Y Positions (Frac)
	lda #$01	; |
	sta $043f,x	; |
	sta $045a,x	; |
	lda #$03	; |
	sta $0436,x	; | - Animation Frame
	dex			; |
	bpl lf23a	; /
	ldx #$05	; \
lf26f:
	lda #$ff	; | Initialize Enemies
	sta $8a,x	; |
	dex			; |
	bpl lf26f	; /
	ldx $40						; \
lf278:
	jsr lf386_initializeplayerx	; | Initialize Players
	dex							; |
	bpl lf278					; /
	jsr ld246_clearppu
	jsr ld293_initgamemode
	lda $c6
	cmp #$10
	bcs lf28e
	lda #$58
	sta $c6
lf28e:
	jsr lf4a5_initfish
	jsr ld8ff
	lda $16
	beq lf29b	; Balloon Fight Game Mode
	jmp lc1c5	; Balloon Trip Game Mode
lf29b:
	lda $c8
	beq lf2a2_balloonfight_load	; Normal Phase Type
	jmp lcf13	; Bonus Phase Type
lf2a2_balloonfight_load:
	jsr lc716_initcloudbolt
	lda $3b		; \ Level Header?
	and #$03	; |
	bne lf2b3	; /
	lda #$08	; \
	sta $f2		; / Play Stage Start jingle
	ldx $3a		; \
	bne lf2b9_balloonfight_loop	; / Demo Flag
lf2b3:
	lda #$ff	; \ Show Phase Number for
	sta $3d		; / 255 frames
	inc $3c		; Increment Current Phase Number
lf2b9_balloonfight_loop:	; Balloon Fight Game Loop
	jsr lf470_pause
	lda $3d					; \
	beq lf2c5				; | Display Phase Number
	dec $3d					; | if the time is not 0
	jsr lf3cc_phasedisplay	; /
lf2c5:
	jsr lf1b3_rng
	jsr le691_objectmanage
	jsr lc6f9_fishmanage
	jsr lc790_cloudbolt
	jsr lc831_cloudblink
	jsr lc8b7
	jsr ld8dd
	jsr le587
	jsr lcb74_flippermanage
	inc $4c
	ldx $40		; X = 2 Player Flag
lf2e4:
	lda $88,x	; \ If Player X has balloons
	bpl lf30d	; / then skip respawn code
	lda $3a		; \ If Demo Play
	bne lf326	; / then return
	lda $41,x	; \ If Player X Lives < 0
	bmi lf30d	; / then skip respawn code
	dec $c3,x	; \ Decrease Player X Respawn Delay
	bne lf327	; / If not 0 then ?
	phx
	jsr lc726
	plx
	ldy #$02
	dec $41,x	; Decrement Player X Lives
	sty $46		; Update Status Bar
	bmi lf30d	; If Player X has no more lives then don't respawn
	jsr lf386_initializeplayerx
	jsr lf3b0_initplayertype
	lda #$80	; \ Play Respawn Jingle
	sta $f2		; /
lf30d:
	dex			; \ Loop with Player 1
	bpl lf2e4	; /
	lda $41		; \ If Player 1 has lives
	bpl lf318	; / continue
	lda $42		; \ If Player 1 & 2 have 0 lives
	bmi lf366	; / then game over
lf318:
	lda $3a		; \ If Demo Play
	beq lf327	; / then skip joypad read
	jsr le768_polljoypad0
	lda $061c	; \
	and #$30	; | If START or SELECT is pressed
	beq lf2b9_balloonfight_loop	; / then loop
lf326:
	rts
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
	ldx $40		; Player Check
lf338:
	ldy $88,x	; \ If Player X has balloons
	dey			; | then 
	bpl lf34c	; /
	lda $41,x	; \ If Player X has no lives
	bmi lf34c	; / then skip
	lda #$ff	; \ Set Player X balloons
	sta $88,x	; / to none
	lda #$01	; \ Set Player X Respawn Delay
	sta $c3,x	; / to 1 frame
	jmp lf2b9_balloonfight_loop	; loop
lf34c:
	dex			; \ Loop player checks until
	bpl lf338	; / we can assume phase is cleared.
	lda #$02	; \ Play Stage Clear jingle
	sta $f2		; /
lf353:
	ldx #$96				; \ Wait 150 frames
	jsr lf45e_waityframes	; /
	ldx $3b		; \
	inx			; | Get to next level
	cpx #$10	; | if past level ID #$10
	bne lf361	; |
	ldx #$04	; | then loop back to level ID #$04
lf361:
	stx $3b		; /
	jmp lf213	; Load Next Level
lf366:		; Manage Game Over
	lda #$01	; \ Play Game Over jingle
	sta $f2		; /
lf36a:
	lda #$00
	sta $17		; Reset PPUSCROLL Shadow
	sta $18		; Reset PPUCTRL Shadow
	sta $15		; Set time
	jsr lf40b_uploadgameovertext
lf375:
	jsr lf465_clearframeflag
	jsr le768_polljoypad0	; \ Press START or SELECT
	and #$30				; | to come back to Title Screen
	bne lf383				; /
	dec $15		; \ Wait for 256 frames
	bne lf375	; / to come back to Title Screen
lf383:
	jmp lf1d4	; Back to Title Screen

lf386_initializeplayerx:
	lda $41,x	; \ If Player X has negative lives
	bmi lf3ad	; / Then don't do anything
	lda lf3ae,x	; \ Set up X coordinate for Player X
	sta $91,x	; /
	lda #$b8	; \ Set up Y coordinate for Player X
	sta $9a,x	; /
	sta $bd,x	; Set up invincibility for Player X
	lda #$c8	; \ Set up invincibility time
	sta $bf,x	; / for Player X
	lda #$5a	; \
	ldy $41,x	; | If Player X has lives
	bpl lf3a1	; | Then set respawn delay to #$5A
	lda #$01	; | Else set respawn delay to #$01
lf3a1:
	sta $c3,x	; /
	lda #$00
	sta $c1,x	; Clear Player X Freeze Flag
	sta $042d,x	; \ Set up Player X's X Velocity to $00
	sta $0424,x ; /
lf3ad:
	rts
;-----------------------

lf3ae:
.BYTE $20,$d0

lf3b0_initplayertype:
	lda #$03	; \ Set Player X type to 03 (2 Balloons)
	sta $0451,x	; /
	lda #$02	; \ Set Player X Balloons to 02
	sta $88,x	; /
	rts
;-----------------------

lf3ba:
    ;9 bytes
.BYTE $58,$50,$58,$50,$50,$40,$38,$30,$28
lf3c3:
    ;9 bytes
.BYTE $04,$04,$03,$03,$02,$02,$02,$02,$02

lf3cc_phasedisplay:
	lda $3d		; \ Toggle between "PHASE-??"
	and #$20	; | and empty
	beq lf3ee	; / every #$20 frames?
	ldx #$0a	; \
lf3d4:
	lda lf3f5,x	; | Copy "PHASE-  " PPU Block
	sta $57,x	; |
	dex			; |
	bpl lf3d4	; /
	ldy #$0a			; \
	lda $3c				; | Add 1st digit of
	sta $43				; | Phase Number
	jsr ld77c_divide	; | (Divide by 10)
	sta $60				; /
	lda $43				; \ Add 2nd digit of
	sta $61				; / Phase Number
	jmp lc12d_copypputempblock
lf3ee:
	lday lf400				; \ Copy Empty PPU Block
	jmp lc131_copyppublock	; /
lf3f5:	; $206C - $08 - "PHASE-  "
.BYTE $20,$6c,$08,$19,$11,$0a,$1c,$0e,$25,$00,$00
lf400:	; $206C - $08 - "        "
.BYTE $20,$6c,$08,$24,$24,$24,$24,$24,$24,$24,$24

lf40b_uploadgameovertext:
	jsr lf465_clearframeflag
	ldx #$01				; \
lf410:
	lda lf43b,x				; | Prepare Game Over
	ldy lf43b+2,x			; | PPU blocks
	jsr lc131_copyppublock	; | to upload
	dex						; |
	bpl lf410				; /
	ldx #$0f	; \
lf41e:
	lda #$24	; | Prepare 16 empty tiles
	sta $5a,x	; | to upload
	dex			; |
	bpl lf41e	; /
	lda #$10	; \ Size: 16 bytes
	sta $59		; /
	lda #$21	; \ PPUADDR = $21xx
	sta $57		; /
	ldx #$02					; \
lf42f:
	lda lf43f,x					; | Prepare uploading
	sta $58						; | empty tiles to nametable
	jsr lc12d_copypputempblock	; | ($2188, $21A8, $21E8)
	dex							; | to PPU Buffer
	bpl lf42f					; /
	rts

.define PPUBlockPointers lf442, lf455
lf43b:	; Pointers to PPU Blocks
.LOBYTES PPUBlockPointers
lf43d:
.HIBYTES PPUBlockPointers
lf43f:	; Empty tiles lower PPUADDR
.BYTE $88,$a8,$e8
lf442:	; "   GAME  OVER   "
.BYTE $21,$c8,$10,$24,$24,$24,$10,$0a,$16,$0e,$24,$24,$18,$1f,$0e,$1b,$24,$24,$24
lf455:	; Tile Attributes?
.BYTE $23,$da,$04,$aa,$aa,$aa,$aa

lf45c_wait20frames:
	ldx #$14
lf45e_waityframes:
	jsr lf465_clearframeflag
	dex
	bne lf45e_waityframes
	rts
;-----------------------

lf465_clearframeflag:
	lda #$00
	sta $02
lf469_waitnextframe:
	lda $02
	beq lf469_waitnextframe
	dec $02
lf46f:
	rts
;-----------------------

lf470_pause:
	jsr lf469_waitnextframe
	lda $3a		; \ If Demo Flag Set
	bne lf46f	; / then don't do anything
	jsr le768_polljoypad0	; \
	and #$10				; | If START is not pressed
	beq lf46f				; / then don't do anything
	lda #$04	; \ Play Pause jingle
	sta $f2		; /
	lda $01		; \
	and #$ef	; | Hide Sprites
	sta $2001	; /
lf489:
	jsr lf465_clearframeflag	; \
	jsr le768_polljoypad0		; |
	and #$10					; | If START is not pressed
	beq lf489					; / then loop
	lda $01		; \
	sta $2001	; / Show Sprites
	ldy #$04	; \
	lda $c8		; | Play Pause jingle if
	ora $16		; | it's a Normal Phase in Balloon Fight Game Mode
	beq lf4a2	; | Else play Balloon Trip / Bonus Phase music
	ldy #$20	; |
lf4a2:
	sty $f2		; /
	rts
;-----------------------

lf4a5_initfish:
	lda #$01
	sta $048e	; \ Set Unused Variables?
	sta $048f	; /
	lda #$ff	; \ Reset Water Plonk Animation
	sta $bb		; /
	sta $87		; Fish Status = #$FF
	sta $048c	; Fish Target Eaten Flag = #$FF
	ldx #$01
	stx $0459	; Fish Type = #$01
	stx $90		; Fish Balloons = #$01
	inx			; \ Update Status Bar
	stx $46		; /
	lda #$40	; \ Set Fish X position
	sta $99		; / to #$40
	rts
;-----------------------

.include "Sound.asm"

.SEGMENT "VECTORS"
.WORD NMI		;NMI
.WORD Reset		;RESET
.WORD BRKLoop	;IRQ/BRK
