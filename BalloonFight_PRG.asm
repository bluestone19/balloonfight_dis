//Balloon Fight (USA) Disassembly - PRG ROM
//-----------------------
//disassembled by nesrevplus v0.3b (2018-09-18 16:39, 5ms)

arch nes.cpu

// RAM Map:
// $0000 = PPUCTRL Shadow
// $0001 = PPUMASK Shadow
// $0002 = Video Frame Processed Flag
// $0003-$0007 = Player 1 Score
// $0008-$000C = Player 2 Score
// $000D-$0011 = Top Score
// $0012 = ???
// $0016 = Game Mode (0 = Balloon Fight, 1 = Balloon Trip)
// $0019 = Frame Counter

// $001D = Loading Pointer
// $001F = Graphics/Enemy Data Pointer
// $0021 = Top Score Pointer
// $0023 = Left Side   - Platform Collision Pointer
// $0025 = Right Side  - Platform Collision Pointer
// $0027 = Top Side    - Platform Collision Pointer
// $0029 = Bottom Side - Platform Collision Pointer

// $003A = Demo Flag
// $003B = Current Level Header
// $003C = Current Phase
// $003D = ???
// $003E = ???
// $003F = Main Menu Cursor
// $0040 = 2 Player Flag
// $0041 = Player 1 Lives
// $0042 = Player 2 Lives
// $0043 = ???

// $0049 = Balloon Trip Rank 0x
// $004A = Balloon Trip Rank x0

// Star Animation:
// $004C = Star Update?
// $004F = Star Animation - Star ID
// $0050 = PPU Address Low
// $0051 = PPU Address High

// PPU Upload Buffer:
// $0050 = PPU Address High
// $0051 = ???
// $0052 = PPU Upload Buffer Position
// $0053 = PPU Upload Buffer Size
// $0054 = Cloud/Flipper X coordinate used for rendering
// $0055 = Cloud/Flipper Y coordinate used for rendering

// $0081-$0086 = Enemies Status

// $0088 = Player 1 Balloons
// $0089 = Player 2 Balloons
// $008A-$008F = Enemies Balloons

// $0093 = Enemies X Positions
// $009C = Enemies Y Positions

// $00A3 = Amount of Clouds
// $00A4 = Current Cloud ID?
// $00A6 = Cloud related
// $00A9 = Cloud related
// $00AC = Cloud related
// $00AF = Cloud related
// $00B2 = Cloud related
// $00B5 = Cloud related

// $00BD = Player 1 Invincibility Flag
// $00BE = Player 2 Invincibility Flag
// $00BF = Player 1 Invincibility Time
// $00C0 = Player 2 Invincibility Time
// $00C1 = Player 1 Freeze Flag
// $00C2 = Player 2 Freeze Flag
// $00C3 = Player 1 Respawn Delay
// $00C4 = Player 2 Respawn Delay
// $00C8 = Phase Type (00 = Regular, 01 = Bonus)
// $00CD = Amount of Platforms

// Sound:
// $00D0 = ?
// $00D1 = ?
// $00D2 = ?
// $00D3 = ?

// $00DC = ?
// $00DD = ?

// $00DF = ?
// $00E0 = Track Pointer
// $00E2 = Track Pointer
// $00E4 = Track Pointer
// $00E6 = Track Pointer

// $00E8 = ?
// $00E9 = ?
// $00EA = ?
// $00EB = ?

// $00F0 = SFX 1
// $00F1 = SFX 2
// $00F2 = Music/Jingle
// $00F3 = SFX 3

// $00F4 = Current SFX?
// $00F5 = 
// $00F6 = Currently Playing Music/Jingle
// $00F7 = 

// $0200-$02FF = OAM Buffer
// $0300-$03FF = PPU Upload Buffer
//					(16-bit PPU Address,
//					 8-bit Size,
//					 Data, and repeat)

// $0453 = Enemies Type

// $0558 = Bonus Phase Intensity Level
// $0559 = Bonus Phase x00 points per balloon
// $055A = Balloon Rising Speed
// $055B = Bonus Phase Super Bonus x0000 points
// $055C = Bonus Phase Super Bonus 0x000 points
// $055D = Balloon GFX
// $0567 = Balloon X positions
// $057B = Balloon Y positions

// $05D1 = Amount of Flippers
// $05D2 = Flipper X positions
// $05DC = Flipper Y positions
// $05FA = Flippers Type

// $061C = Controller 1 Pressed Buttons
// $061D = Controller 2 Pressed Buttons
// $061E = Controller 1 Held Buttons
// $061F = Controller 2 Held Buttons
// $0629 = Default 1-Player Game Top Score
// $062E = Default 2-Player Game Top Score
// $0633 = Default Balloon Trip Top Score

// $07E8 = Balloon Trip Music Flag
// $07F0 = Audio related?
// $07F5 = $F0 SFX Flags for Balloon Trip?
// $07FA = Audio related?
// $07FB = "AL" Reset Check

base $c000

//----------------------
// RESET code
//----------------------

lc000_reset:
	lda #$00	// \
	sta $2000	// | Initialize PPU registers
	sta $2001	// /
lc008:
	lda $2002	// \
	bpl lc008	// |
lc00d:
	lda $2002	// | Get to next V-Blank
	bmi lc00d	// |
lc012:
	lda $2002	// |
	bpl lc012	// /

	sei			// Disable Interrupts
	cld			// Clear Decimal Mode (Useless on NES/Famicom)
	ldx #$ff	// \ Initialize Stack Pointer
	txs			// / to $01FF

	ldx #$12	// \
	lda #$00	// | Initialize part of RAM
lc020:
	sta $00,x	// | $0012 - $00FF
	inx			// |
	bne lc020	// /

	ldx #$02	// \
lc027:
	lda $07fa,x	// | Check if system was reset
	cmp lc082,x	// | by checking if $07FA has "HAL" string
	bne lc034	// |
	dex			// | If found, then skip to the end
	bpl lc027	// | Else, proceed with initialization code
	bmi lc077	// /

lc034:
	ldx #$00	// \
	txa			// | Initialize parts of RAM
lc037:
	sta $00,x	// | $0000 - $00FF
	sta $0700,x	// | $0700 - $07FF
	inx			// |
	bne lc037	// /

	lda #$32	// \
	sta $15		// |
lc043:
	lda #$32	// |
	jsr ld6de	// |
	lda #$00	// |
	sta $46		// |
	jsr lc579	// |
	dec $15		// |
	bne lc043	// /

	ldx #$0e	// \
lc055:
	lda lc085,x	// | Write default High Scores
	sta $0629,x	// | for each game mode
	dex			// |
	bpl lc055	// /

	ldx #$04	// \
lc060:
	lda #$00	// | Initialize Player 1 Score
	sta $03,x	// |
	dex			// |
	bpl lc060	// /

	lda #$00
	jsr ld6de

	ldx #$02	// \
lc06e:
	lda lc082,x	// | Write "HAL" to $07FA
	sta $07fa,x	// | for future Reset checking
	dex			// |
	bpl lc06e	// /
lc077:
	lda #$1e	// \ PPUMASK Shadow
	sta $01		// / Enable Background and Sprites
	lda #$90	// \ PPUCTRL Shadow
	sta $00		// / Enable NMI at V-Blank, BG Pattern Table at $1000
	jmp lf1d4

lc082:	//"HAL" string
db $48,$41,$4c

lc085:	// Default High Scores
db $00,$00,$00,$01,$00	//1 Player Mode
db $00,$00,$00,$01,$00	//2 Player Mode
db $00,$00,$05,$02,$00	//Balloon Trip Mode

//----------------------
// NMI code
//----------------------

lc094_nmi:
	pha			// \
	txa			// |
	pha			// | Push A, X, Y
	tya			// |
	pha			// /
	lda #$00	// \
	sta $2003	// | Upload OAM Buffer
	lda #$02	// | $0200 - $02FF to OAM (DMA)
	sta $4014	// /
	lda $52						// \ Check for PPU Buffer Upload
	cmp $53						// |
	beq lc0ac					// | If Position in buffer != Buffer Size
	jsr lc17c_uploadppubuffer	// / Then Upload PPU Buffer
lc0ac:
	jsr ld60d_updatestarbganim	// Update Star Animation
	jsr ld798_updatestatusbar	// Update Status Bar
	inc $19		// Increment Frame Counter
	lda #$20	// \
	sta $2006	// | PPUADDR = $2000
	lda #$00	// | (Nametable 0)
	sta $2006	// /
	lda #$00	// \
	sta $2005	// | PPUSCROLL = X:0, Y:0
	sta $2005	// /
	jsr lfff7_audiomain	// Manage Audio
	lda #$01	// \ Set Video Frame Done Flag
	sta $02		// /

	lda $16		// \ If Game Mode is Balloon Fight mode
	beq lc0f1	// / then end NMI
lc0d1:
	lda $2002	// \ Wait for V-Blank
	bmi lc0d1	// /

	ldx #$04	// \
	ldy #$c6	// |
lc0da:
	dey			// | Wait 6125 cycles
	bne lc0da	// |
	dex			// |
	bne lc0da	// /

	lda $18		// \
	ora $00		// | PPUCTRL = [$0018] | [$0000]
	sta $2000	// /
	lda $17		// \
	sta $2005	// | Input X scroll value
	lda #$00	// | PPUSCROLL = X:[$17], Y:0
	sta $2005	// /

lc0f1:
	pla			// \
	tay			// |
	pla			// | Pull A, X, Y
	tax			// |
	pla			// /
	rti


//----------------------
// BRK code
//----------------------

lc0f7_brk:
	jmp lc0f7_brk	// Loop


//----------------------
// ? code
//----------------------

lc0fa_disablenmi:
	lda $00		// \
	and #$7f	// |
lc0fe:
	sta $2000	// | Disable NMI
	sta $00		// |
	rts			// /

lc104_enablenmi:
	lda $00
	ora #$80
	bne lc0fe	// Enable NMI

lc10a_clearppumask:
	lda #$00	// Clear PPUMASK
lc10c_writeppumask:
	pha
	jsr lf465_clearframeflag
	pla
	sta $2001	// Update PPUMASK
	rts

lc115:
	lda $01		// Write PPUMASK Shadow
	bne lc10c_writeppumask
lc119:
	jsr lc154
	ldy #$00
lc11e:
	lda $0057,y
	sta $0300,x
	inx
	iny
	cpy $56
	bne lc11e
	stx $53
	rts
//-----------------------

lc12d:
	lda #$57
	ldy #0
lc131:
	sta $21
	sty $22
	txa
	pha
	ldy #2
	lda ($21),y
	clc
	adc #3
	sta $12
	ldx $53
	ldy #0
lc144:
	lda ($21),y
	sta $0300,x
	inx
	iny
	cpy $12
	bne lc144
	stx $53
	pla
	tax
	rts
//-----------------------

lc154:
	ldx $53
	lda #$00
	sta $12
	lda $55
	asl
	asl
	asl
	asl
	rol $12
	asl
	rol $12
	ora $54
	pha
	lda $12
	ora #$20
	sta $0300,x
	inx
	pla
	sta $0300,x
	inx
	lda $56
	sta $0300,x
	inx
	rts
//-----------------------

lc17c_uploadppubuffer:
	tya			// \
	pha			// | Push X & Y
	txa			// |
	pha			// /
	jsr lc188	// 
	pla			// \
	tax			// | Pull X & Y
	pla			// |
	tay			// /
	rts
//-----------------------

lc188:
	ldx $52		// Get Current Position in PPU Upload Buffer
	lda $0300,x	// \
	inx			// |
	sta $50		// |
	sta $2006	// | Get PPU Address
	lda $0300,x	// | And Set PPUADDR
	inx			// |
	sta $2006	// /
	ldy $0300,x	// Get Upload Size to Y
	inx
lc19e:
	lda $0300,x	// \
	inx			// |
	sta $2007	// | Upload Y bytes to PPU
	dey			// |
	bne lc19e	// /

	lda $50		// \
	cmp #$3f	// | If Upload Address != $3FXX (Palette Data)
	bne lc1be	// / Then Skip this section
	lda #$3f	// \
	sta $2006	// |
	lda #$00	// | PPUADDR = $3F00
	sta $2006	// | PPUADDR = $0000
	sta $2006	// | ???
	sta $2006	// /
lc1be:
	stx $52		// \
	cpx $53		// | If PPU Buffer Position != PPU Buffer Size
	bne lc188	// / Then Upload more data
	rts
//-----------------------

lc1c5:
	lda #$20
	sta $f2
	jsr lc527
	jsr lc539
	lda #$ff
	sta $cd
	lda #$ad
	sta $23
	lda #$c4
	sta $24
	lda #$80
	sta $91
	sta $0488
	lda #$70
	sta $9a
	jsr lcd4a
	lda #0
	sta $41
	sta $c9
	sta $ca
	sta $ba
	sta $c5
	sta $c8
	jsr lf4a5
	ldx #$13
lc1fc:
	lda #$ff
	sta $0530,x
	lda #$f0
	sta $04a4,x
	dex
	bpl lc1fc
lc209:
	jsr lf470
	jsr le691
	lda $c5
	bne lc216
	jsr lc6f9
lc216:
	lda $19
	lsr
	bcs lc21e
	jmp lc2d0
lc21e:
	lda $c5
	beq lc227
	dec $c5
	jmp lc2d0
lc227:
	lda $17
	bne lc231
	lda $18
	eor #1
	sta $18
lc231:
	dec $17
	lda $0488
	beq lc24d
	inc $0488
	lda $0488
	cmp #$f0
	bcc lc247
	lda #0
	sta $0488
lc247:
	lda $bd
	beq lc24d
	inc $91
lc24d:
	ldx #7
lc24f:
	lda $055d,x
	bmi lc26d
	inc $0567,x
	lda $0567,x
	cmp #$f8
	bne lc26d
	lda #$ff
	sta $055d,x
	lda #$f0
	sta $057b,x
	lda #0
	sta $05ce
lc26d:
	dex
	bpl lc24f
	ldx #$13
lc272:
	lda $0530,x
	bmi lc289
	inc $0490,x
	lda $0490,x
	cmp #$f8
	bcc lc289
	lda #$f0
	sta $04a4,x
	sta $0530,x
lc289:
	dex
	bpl lc272
	lda $17
	and #7
	bne lc2d0
	ldx $88
	dex
	bmi lc2d0
	lda #0
	sta $3e
	lda #1
	jsr ld6de
	inc $c9
	lda $c9
	and #$1f
	bne lc2bc
	inc $ca
	lda $ca
	cmp #$0a
	bne lc2bc
	lda #2
	sta $ca
	ldy $ba
	iny
	tya
	and #3
	sta $ba
lc2bc:
	ldx $ca
	lda lc3bf,x
	asl
	tay
	lda lc3b5,y
	sta $25
	lda lc3b5+1,y
	sta $26
	jsr lc3b2
lc2d0:
	ldx #7
lc2d2:
	lda $055d,x
	bmi lc2ef
	jsr lcece
	lda $05cd
	beq lc2ef
	dec $05cd
	inc $05ce
	txa
	pha
	lda $0559
	jsr ld6de
	pla
	tax
lc2ef:
	jsr lce2f_balloonxspritemanage
	dex
	bpl lc2d2
	ldx #$13
lc2f7:
	lda $0530,x
	bmi lc317
	lda $c5
	bne lc314
	jsr lc9b6
	lda $04a4,x
	cmp #2
	bcs lc30d
	jsr lca4f
lc30d:
	cmp #$d8
	bcc lc314
	jsr lca4f
lc314:
	jsr lcb1c
lc317:
	lda $19
	and #7
	lsr
	tay
	lda lc9dd,y
	pha
	lda $19
	lsr
	txa
	bcc lc32d
	sta $12
	lda #$13
	sbc $12
lc32d:
	asl
	asl
	tay
	pla
	sta $02b1,y
	lda $04a4,x
	sta $02b0,y
	lda $0490,x
	sta $02b3,y
	lda #0
	sta $02b2,y
	dex
	bpl lc2f7
	lda $05ce
	cmp #$14
	bcc lc36f
	inc $47
	lda #0
	jsr ld6de
	dec $47
	lda #$10
	sta $f2
	inc $c8
	jsr ld3ed
	jsr lc527
	dec $c8
	ldx #$64
	jsr lf45e
	lda #$20
	sta $f2
lc36f:
	ldx #$f0
	lda $0488
	beq lc378
	ldx #$88
lc378:
	stx $0200
	stx $0204
	sta $0203
	clc
	adc #8
	sta $0207
	lda $19
	and #3
	sta $0202
	sta $0206
	ldx #$e3
	stx $0201
	inx
	stx $0205
	lda $88
	bmi lc3a1
	jmp lc209
lc3a1:
	jsr lc579
	lda #1
	sta $f0
	jsr lf465_clearframeflag
	lda #2
	sta $f2
	jmp lf36a
lc3b2:
	jmp ($0025)
lc3b5:
    //10 bytes
db $c9,$c3,$f7,$c3,$3e,$c4,$5f,$c4,$5e,$c4
lc3bf:
    //360 bytes
db $00,$00,$02,$02,$02,$02,$02,$04,$03,$01,$a0,$00,$b1,$23,$e6
db $23,$d0,$02,$e6,$24,$aa,$f0,$20,$0a,$0a,$0a,$85,$15,$a9,$00
db $85,$14,$8a,$29,$c0,$c9,$80,$d0,$06,$20,$6b,$c4,$4c,$c9,$c3
db $c9,$00,$d0,$06,$20,$86,$c4,$4c,$c9,$c3,$60,$20,$b3,$f1,$29
db $7f,$c9,$04,$90,$0c,$c9,$18,$b0,$08,$0a,$0a,$0a,$85,$15,$20
db $6b,$c4,$20,$b3,$f1,$29,$3f,$c9,$02,$90,$24,$c9,$18,$b0,$20
db $0a,$0a,$0a,$85,$15,$20,$b3,$f1,$29,$3f,$a6,$ba,$7d,$3a,$c4
db $85,$14,$20,$86,$c4,$20,$b3,$f1,$4a,$90,$d9,$20,$4f,$ca,$4c
db $0c,$c4,$60,$20,$30,$40,$60,$20,$b3,$f1,$29,$cf,$d0,$b2,$a4
db $89,$c8,$d0,$ad,$a9,$e6,$85,$9b,$a5,$1b,$29,$7f,$69,$40,$85
db $92,$a9,$80,$85,$89,$a9,$00,$85,$80,$60,$20,$0c,$c4,$20,$b3
db $f1,$29,$7f,$9d,$08,$05,$60,$a2,$07,$bd,$5d,$05,$30,$04,$ca
db $10,$f8,$60,$a9,$01,$9d,$5d,$05,$a9,$00,$9d,$67,$05,$a5,$15
db $9d,$7b,$05,$60,$a2,$13,$bd,$30,$05,$30,$04,$ca,$10,$f8,$60
db $a9,$00,$9d,$30,$05,$9d,$90,$04,$9d,$f4,$04,$9d,$08,$05,$9d
db $e0,$04,$a5,$14,$9d,$1c,$05,$a5,$15,$9d,$a4,$04,$60,$00,$00
db $09,$00,$08,$8c,$00,$07,$18,$00,$18,$00,$19,$00,$1a,$00,$84
db $94,$1a,$00,$1a,$00,$1a,$00,$0b,$12,$00,$0c,$13,$00,$0d,$14
db $00,$14,$00,$00,$90,$00,$07,$00,$07,$8c,$96,$00,$08,$00,$09
db $00,$00,$18,$00,$17,$00,$16,$00,$00,$00,$00,$00,$00,$8a,$90
db $00,$00,$00,$08,$00,$09,$98,$00,$0a,$00,$00,$00,$86,$8a,$15
db $00,$14,$00,$8e,$13,$00,$00,$03,$0d,$00,$0d,$0e,$00,$0c,$0d
db $00,$0d,$19,$00,$86,$92,$00,$00,$98,$00,$00,$0a,$12,$00,$09
db $13,$00,$08,$14,$00,$07,$15,$00,$07,$16,$00,$07,$00,$00,$00
lc527:
	jsr ld0e2
	asl $0559
	lda $0559
	asl
	asl
	adc $0559
	sta $0559
	rts
//-----------------------

lc539:
	lda #$00
	sta $12
lc53d:
	lda $12
	asl
	asl
	adc $12
	sta $1d
	lda #$07
	sta $1e
	ldy #$04
lc54b:
	lda ($1d),y
	cmp $0003,y
	bcc lc563
	bne lc559
	dey
	bpl lc54b
	bmi lc563
lc559:
	inc $12
	lda $12
	cmp #$32
	bne lc53d
	dec $12
lc563:
	inc $12
	lda $12
	pha
	sta $43
	ldy #$0a
	jsr ld77c
	sta $4a
	lda $43
	sta $49
	pla
	sta $12
	rts
//-----------------------

lc579:
	jsr lc539
	dec $12
	lda #$31
	sec
	sbc $12
	sta $13
	asl
	asl
	adc $13
	tay
	lda $12
	asl
	asl
	adc $12
	sta $1d
	clc
	adc #5
	sta $1f
	lda #7
	sta $1e
	sta $20
	tya
	beq lc5ac
	dey
lc5a1:
	lda ($1d),y
	sta ($1f),y
	dey
	bne lc5a1
	lda ($1d),y
	sta ($1f),y
lc5ac:
	ldy #4
lc5ae:
	lda $0003,y
	sta ($1d),y
	dey
	bpl lc5ae
	rts
//-----------------------

lc5b7:
db $01,$02,$03,$03
lc5bb:
db $02,$01,$ff,$03,$04,$05,$06,$ff
lc5c3:
	lda $048d
	lsr
	lsr
	lsr
	tax
	lda $048a
	bne lc5d5
	lda lc5b7,x
	jmp lc5d8
lc5d5:
	lda lc5bb,x
lc5d8:
	sta $87
	ldx #8
	jsr le3a4
	lda $048c
	beq lc613
	ldx $048b
	lda $048d
	cmp #$20
	bne lc5f4
	lda #$ff
	sta $88,x
	bmi lc610
lc5f4:
	bcs lc613
	lda $0450
	bne lc602
	lda $99
	clc
	adc #4
	bne lc607
lc602:
	lda $99
	sec
	sbc #4
lc607:
	sta $91,x
	lda $a2
	sec
	sbc #$0a
	sta $9a,x
lc610:
	jsr le3a4
lc613:
	rts
//-----------------------

lc614:
	lda #$ff
	sta $048b
	ldx #7
lc61b:
	lda $88,x
	bmi lc62b
	lda $9a,x
	cmp #$b4
	bcc lc62b
	lda $91,x
	cmp $99
	beq lc62f
lc62b:
	dex
	bpl lc61b
	rts
//-----------------------

lc62f:
	stx $048b
	lda $0448,x
	sta $0450
	lda #0
	sta $048a
	sta $048d
	sta $048c
	sta $0489
	lda #$dc
	sta $a2
	rts
//-----------------------

lc64b:
	inc $99
	lda $99
	cmp #$b1
	bcc lc657
	lda #$40
	sta $99
lc657:
	rts
//-----------------------

lc658:
	lda $0489
	bne lc66f
	dec $a2
	lda $a2
	cmp #$c4
	bcs lc671
	inc $a2
	inc $048a
	inc $0489
	bne lc671
lc66f:
	inc $a2
lc671:
	inc $048d
	lda $048d
	cmp #$18
	bne lc6a3
	ldx $048b
	lda $88,x
	bmi lc6a3
	lda $9a,x
	clc
	adc #$10
	cmp $a2
	bcc lc6a3
	ldy $0451,x
	lda lc6b8,y
	sta $0451,x
	lda #0
	sta $7f,x
	sta $88,x
	lda $f2
	ora #$40
	sta $f2
	inc $048c
lc6a3:
	lda $048a
	beq lc6b7
	lda $048d
	cmp #$28
	beq lc6b3
	cmp #$30
	bne lc6b7
lc6b3:
	lda #$cc
	sta $a2
lc6b7:
	rts
//-----------------------

lc6b8:
    //12 bytes
db $08,$09,$0a,$0b,$08,$09,$0a,$0b,$08,$09,$0a,$0b
lc6c4:
	lda $0489
	bne lc6f8
	ldx $048b
	lda $88,x
	bmi lc6e0
	lda $9a,x
	cmp #$b4
	bcc lc6e0
	lda $91,x
	cmp #$40
	bcc lc6e0
	cmp #$b1
	bcc lc6ee
lc6e0:
	lda #$30
	sec
	sbc $048d
	sta $048d
	inc $0489
	bne lc6f8
lc6ee:
	lda $91,x
	sta $99
	lda $0448,x
	sta $0450
lc6f8:
	rts
//-----------------------

lc6f9:
	lda $87
	bpl lc70d
	jsr lc64b
	jsr lc614
	lda $048b
	bpl lc709
	rts
//-----------------------

lc709:
	lda #$40
	sta $f3
lc70d:
	jsr lc6c4
	jsr lc658
	jmp lc5c3
lc716:
	ldx #1
lc718:
	lda #$ff
	sta $0530,x
	sta $0544,x
	dex
	bpl lc718
	jsr lc77a
lc726:
	ldx $3c
	cpx #$18
	bcc lc72e
	ldx #$18
lc72e:
	lda lc748,x
	sta $ba
	lda lc761,x
	sta $b8
	lda #$f0
	sta $02e0
	sta $02e4
	sta $02e8
	lda #3
	jmp lc856
lc748:
    //25 bytes
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01
db $01,$02,$01,$01,$01,$01,$01,$01,$01,$01
lc761:
    //25 bytes
db $0f,$0f,$0c,$0c,$0c,$0c,$0a,$0a,$0a,$0a,$0c,$0c,$0a,$0a,$0a
db $08,$0a,$0a,$08,$08,$08,$08,$08,$08,$05
lc77a:
	lda $a3
	bpl lc781
lc77e:
	sta $a4
	rts
//-----------------------

lc781:
	jsr lf1b3
lc784:
	cmp $a3
	bcc lc77e
	beq lc77e
	clc
	sbc $a3
	jmp lc784
lc790:
	lda $19
	and #$7f
	beq lc797
lc796:
	rts
//-----------------------

lc797:
	dec $b8
	bne lc796
	ldx #0
	lda $0530,x
	bmi lc7ad
	inx
	lda $0530,x
	bmi lc7ad
	lda #1
	sta $b8
	rts
//-----------------------

lc7ad:
	ldy $a4
	sty $a5
	bpl lc7b4
	rts
//-----------------------

lc7b4:
	lda #$80
	sta $04b8,x
	sta $04cc,x
	lda #0
	sta $0530,x
	lda $00b2,y
	sta $0490,x
	lda $00b5,y
	sta $04a4,x
	ldy $ba
	jsr lf1b3
	and #$1f
	adc lc89f,y
	sta $0508,x
	lda lc8ab,y
	sta $051c,x
	lda lc8a5,y
	sta $04e0,x
	lda lc8b1,y
	sta $04f4,x
	jsr lf1b3
	and #3
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
	cmp #5
	bcs lc821
	inc $ba
lc821:
	lda #6
	sec
	sbc $ba
	sta $b8
	lda $f0
	ora #4
	sta $f0
	jmp lc77a
lc831:
	lda $b8
	cmp #1
	bne lc88a
	lda $0530
	bmi lc846
	lda $0531
	bmi lc846
	lda #2
	sta $b8
	rts
//-----------------------

lc846:
	lda $19
	and #$7f
	cmp #$40
	bcc lc88a
	bne lc856
	lda $f1
	ora #8
	sta $f1
lc856:
	and #3
	tax
	lda lc88b,x
	sta $5a
	ldx $a4
	bmi lc88a
	lda #$23
	sta $57
	lda $a6,x
	sta $58
	lda #1
	sta $59
	jsr lc883
	lda $a9,x
	sta $58
	jsr lc883
	lda $ac,x
	sta $58
	jsr lc883
	lda $af,x
	sta $58
lc883:
	lda #$57
	ldy #0
	jmp lc131
lc88a:
	rts
//-----------------------

lc88b:
db $55,$ff,$00,$ff
lc88f:
db $00,$00,$ff,$ff
lc893:
db $ff,$00,$00,$ff
lc897:
db $10,$10,$f0,$f0
lc89b:
db $de,$22,$22,$de
lc89f:
db $60,$70,$80,$90,$a0,$b0
lc8a5:
db $00,$00,$00,$00,$00,$00
lc8ab:
db $c0,$f0,$20,$50,$80,$b0
lc8b1:
db $00,$00,$01,$01,$01,$01
lc8b7:
	ldx #1
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
	and #3
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
	adc #5
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
	and #7
	bne lc937
	lda $0544,x
	clc
	adc #4
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
	jsr lc9b6
	lda $0490,x
	cmp #2
	bcs lc94e
	jsr lca37
lc94e:
	lda $0490,x
	cmp #$f7
	bcc lc958
	jsr lca37
lc958:
	lda $04a4,x
	cmp #2
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
	jsr lcb1c
	ldy $0530,x
	iny
	tya
	and #7
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
	lda #0
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
//-----------------------

lc9b6:
	lda $0508,x
	clc
	adc $04b8,x
	sta $04b8,x
	lda $04e0,x
	adc $0490,x
	sta $0490,x
	lda $051c,x
	clc
	adc $04cc,x
	sta $04cc,x
	lda $04f4,x
	adc $04a4,x
	sta $04a4,x
	rts
//-----------------------

lc9dd:
db $9d,$9e,$9f,$9e,$9d,$a0,$a1,$a0
lc9e5:
    //16 bytes
db $08,$08,$f0,$f0,$08,$08,$f0,$f0,$08,$08,$f0,$f0,$08,$08,$f0
db $f0
lc9f5:
    //16 bytes
db $ee,$0a,$0a,$ee,$ee,$0a,$0a,$ee,$ee,$0a,$0a,$ee,$ee,$0a,$0a
db $ee
lca05:
    //16 bytes
db $f8,$08,$08,$f8,$f8,$08,$08,$f8,$f8,$08,$08,$f8,$f8,$08,$08
db $f8
lca15:
    //10 bytes
db $91,$93,$97,$97,$fc,$92,$95,$9a,$9a,$fc
lca1f:
    //10 bytes
db $fc,$94,$98,$98,$fc,$fc,$96,$9b,$9b,$fc
lca29:
    //10 bytes
db $fc,$fc,$99,$99,$fc,$fc,$fc,$9c,$9c,$fc
lca33:
db $c0,$40,$00,$80
lca37:
	lda $f3
	ora #$80
	sta $f3
lca3d:
	lda #0
	sec
	sbc $0508,x
	sta $0508,x
	lda #0
	sbc $04e0,x
	sta $04e0,x
	rts
//-----------------------

lca4f:
	lda $f3
	ora #$80
	sta $f3
lca55:
	lda #0
	sec
	sbc $051c,x
	sta $051c,x
	lda #0
	sbc $04f4,x
	sta $04f4,x
	rts
//-----------------------

lca67:
	ldy $cd
lca69:
	lda #0
	sta $cc
	lda ($27),y
	sec
	sbc #8
	cmp $04a4,x
	bcs lcadd
	adc #3
	cmp $04a4,x
	bcc lca82
	lda #1
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
//-----------------------

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
db $60
lcb1c:
	ldy #1
lcb1e:
	lda $0088,y
	bmi lcb70
	beq lcb70
	lda $00bd,y
	bne lcb70
	lda $0490,x
	sec
	sbc $0091,y
	jsr lf08e
	cmp #8
	bcs lcb70
	lda $04a4,x
	sec
	sbc $009a,y
	sec
	sbc #8
	jsr lf08e
	cmp #$0c
	bcs lcb70
	lda #0
	sta $0088,y
	lda #1
	sta $007f,y
	sta $00c1,y
	lda #$0b
	sta $0451,y
	lda #$20
	sta $045a,y
	lda $f0
	ora #$80
	sta $f0
	lda #$f0
	sta $04a4,x
	lda #$ff
	sta $0530,x
lcb70:
	dey
	bpl lcb1e
	rts
//-----------------------

lcb74:
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
//-----------------------

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
	jsr lf08e
	cmp #$12
	bcs lcc2f
	lda $009a,y
	clc
	adc #$0c
	sec
	sbc $05dc,x
	sta $13
	jsr lf08e
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
//-----------------------

lcc33:
	lda $f1
	ora #2
	sta $f1
	rts
//-----------------------

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
	jsr lf08e
	sta $12
	lda $009a,y
	clc
	adc #$0c
	sec
	sbc $05dc,x
	jsr lf08e
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
//-----------------------

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
//-----------------------

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
	jsr lc131
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
//-----------------------

lcd26:
db $a1,$24,$24,$24
lcd2a:
db $a2,$9e,$ab,$24
lcd2e:
db $24,$24,$ac,$24
lcd32:
db $a3,$24,$ad,$a8
lcd36:
db $a4,$9f,$ae,$a9
lcd3a:
db $a5,$24,$af,$aa
lcd3e:
db $24,$24,$b0,$24
lcd42:
db $a6,$a0,$b1,$24
lcd46:
db $a7,$24,$24,$24
lcd4a:
	ldx #9
lcd4c:
	lda #$ff
	sta $055d,x
	lda #$f0
	sta $057b,x
	dex
	bpl lcd4c
	rts
//-----------------------

lcd5a:
	dec $05cc
	beq lcd60
	rts
//-----------------------

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
//-----------------------

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
	jsr lf1b3
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
//-----------------------

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
	jsr lcece
	dex
	bmi lce2e
	jmp lcdac
lce2e:
	rts
//-----------------------

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
//-----------------------

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
//-----------------------

lceae:
db $20,$50,$a0,$d0
lceb2:
    //11 bytes
db $02,$22,$02,$aa,$ab,$ab,$aa,$aa,$ab,$ab,$aa
lcebd:
    //17 bytes
db $00,$00,$40,$40,$40,$40,$00,$00,$fc,$fc,$df,$fc,$fc,$e0,$e2
db $e1,$fc
lcece:
	ldy #1
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
	jsr lf08e
	cmp #$18
	bcs lcf0f
	lda $0091,y
	sec
	sbc $0567,x
	jsr lf08e
	cmp #$10
	bcs lcf0f
	lda #$ff
	sta $055d,x
	lda $05cd,y
	clc
	adc #1
	sta $05cd,y
	lda #2
	sta $f0
	rts
//-----------------------

lcf0f:
	dey
	bpl lced0
lcf12:
	rts
//-----------------------

lcf13:
	lda #$20
	sta $f2
	jsr ld0e2
	jsr lcd4a
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
	jsr lf470
	inc $4c
	jsr ld8dd
	jsr le691
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
	jsr lf45e
	lda #$2b
	ldy #$d1
	jsr lc131
	lda #$5a
	ldy #$d1
	jsr lc131
	lda #$65
	ldy #$d1
	jsr lc131
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
	jsr lf45c
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
	jsr lf45c
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
	jsr lf45e
	ldx $40
ld02e:
	jsr lce2f_balloonxspritemanage
	dex
	bpl ld02e
	jsr ld1a0
	jsr lf45c
	lda #1
	sta $f0
	jsr ld121
	bne ld068
	lda #$70
	ldy #$d1
	jsr lc131
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
	jsr lc12d
	lda #$10
	sta $f2
ld068:
	ldx #$78
	jsr lf45e
	jsr ld1a0
ld070:
	lda #0
	sta $3e
	ldx #4
	jsr ld213
	jsr lc12d
	lda $40
	beq ld08e
	inc $3e
	ldx #$12
	jsr ld213
	lda #$65
	ldy #0
	jsr lc131
ld08e:
	lda #1
	sta $f1
	ldx #2
	jsr lf45e
	lda $5d
	cmp #$24
	bne ld070
	lda $40
	beq ld0a8
	lda $006b
	cmp #$24
	bne ld070
ld0a8:
	ldx #$0a
	jsr lf45e
	jsr ld121
	bne ld0ce
	lda $055b
	sta $47
	lda $055c
	sta $48
	lda $40
	sta $3e
ld0c0:
	jsr ld6dc
	dec $3e
	bpl ld0c0
	lda #1
	sta $f1
	jsr lf45c
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
ld0e2:
	ldx $0558
	lda ld10d,x
	sta $0559
	lda ld112,x
	sta $055a
	lda ld117,x
	sta $055b
	lda ld11c,x
	sta $055c
	cpx #4
	bcs ld104
	inc $0558
ld104:
	lda #0
	sta $05cd
	sta $05ce
	rts
//-----------------------

ld10d:
db $03,$05,$07,$07,$07
ld112:
db $80,$90,$98,$a0,$a8
ld117:
db $01,$01,$02,$02,$03
ld11c:
db $00,$05,$00,$05,$00
ld121:
	lda $05cd
	clc
	adc $05ce
	cmp #$14
	rts
//-----------------------

    //19 bytes
db $3f,$00,$10,$0f,$30,$30,$30,$0f,$30,$27,$15,$0f,$30,$02,$21
db $0f,$16,$16,$16
ld13e:
    //70 bytes
db $21,$73,$0b,$29,$00,$00,$00,$00,$00,$24,$19,$1d,$1c,$26,$21
db $f3,$0b,$29,$00,$00,$00,$00,$00,$24,$19,$1d,$1c,$26,$23,$e8
db $08,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$23,$c0,$08,$40,$50,$50
db $50,$50,$90,$a0,$a0,$22,$88,$11,$19,$24,$0e,$24,$1b,$24,$0f
db $24,$0e,$24,$0c,$24,$1d,$24,$2c,$2c,$2c
ld184:
    //26 bytes
db $22,$c6,$17,$1c,$1e,$19,$0e,$1b,$24,$0b,$18,$17,$1e,$1c,$24
db $24,$24,$01,$00,$00,$00,$00,$19,$1d,$1c,$2c
ld19e:
db $50,$70
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
	jsr lc12d
	lda $40
	bne ld1c2
	rts
//-----------------------

ld1c2:
	lda #$65
	ldy #0
	jmp lc131
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
//-----------------------

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
	jsr ld6de
	pla
	tax
ld243:
	jmp ld1fe

ld246_clearppu:	//Clear PPU?
	jsr lc10a_clearppumask
	jsr lc0fa_disablenmi
	lda #$20	// \
	sta $2006	// | PPUADDR = $2000
	lda #$00	// | Nametable 0 Address
	sta $2006	// /
	jsr ld275_clearnametable	// \ Clear Nametable 0
	jsr ld275_clearnametable	// / Clear Nametable 1
	jsr lc104_enablenmi
	jsr lc115
	ldx #$3f
	ldy #$00
	sty $4c
ld268:
	lda #$f0	// \
	sta $0200,y	// |
	iny			// |
	iny			// | Hide all sprites
	iny			// |
	iny			// |
	dex			// |
	bpl ld268	// /
	rts
//-----------------------

ld275_clearnametable:
	ldx #$f0	// \
	lda #$24	// |
ld279:
	sta $2007	// |
	sta $2007	// | Fill PPU Nametable with empty tiles
	sta $2007	// | $3C0 bytes
	sta $2007	// |
	dex			// |
	bne ld279	// /

	ldx #$40	// \
	lda #$00	// |
ld28c:
	sta $2007	// | Fill rest of nametable with Tile 00
	dex			// | $40 bytes
	bne ld28c	// /
	rts			// Total: $400 bytes
//-----------------------

ld293:
	jsr lc10a_clearppumask
	jsr lc0fa_disablenmi
	lda $16		// Check Game Mode for Initialization
	beq ld2a0
	jmp ld572
ld2a0:			// Initialize Balloon Fight Game Mode
	ldy $3b						// \
	lda ldb2a,y					// |
	sta $1d						// | Load Phase Graphics
	lda ldb3a,y					// |
	sta $1e						// |
	jsr ld497_uploadbackground	// /
	ldx #$00							// \
ld2b1:
	jsr ld4e5_getbytefromloadpointer	// |
	cmp #$ff							// | Load Clouds (XX YY)
	beq ld322							// | until one has $FF as
	sta $54								// | X coordinate
	jsr ld4e5_getbytefromloadpointer	// |
	sta $55								// /
	ldy #$03					// \
ld2c1:
	jsr ld4fb_setppuaddr_render	// |
	lda #$04					// |
	sta $12						// | Render Cloud
	lda ld493,y					// | to the screen
ld2cb:
	sta $2007					// |
	clc							// |
	adc #$04					// |
	dec $12						// |
	bne ld2cb					// |
	inc $55						// |
	dey							// |
	bpl ld2c1					// /
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
	jmp ld2b1	// Load another cloud data
ld322:
	dex		// \ Write amount of clouds to RAM
	stx $a3	// /
	ldx #$00							// \
ld327:
	jsr ld4e5_getbytefromloadpointer	// |
	cmp #$ff							// | Load Flippers (XX YY TT)
	beq ld37e							// | until one has $FF as
	sta $54								// | X coordinate
	jsr ld4e5_getbytefromloadpointer	// |
	sta $55								// |
	jsr ld4e5_getbytefromloadpointer	// |
	sta $05fa,x							// /
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
	jmp ld327	// Load another flipper data
ld37e:
	dex			// \ Write amount of flippers to RAM
	stx $05d1	// /
	jsr ld4e5_getbytefromloadpointer	// \
	sta $1f								// | Load Enemy Data Pointer
	jsr ld4e5_getbytefromloadpointer	// |
	sta $20								// /
	ldy #$00	// \
	lda ($1f),y	// | Load Enemy Amount
	tax			// |
	dex			// |
	bpl ld399	// /
	inc $c8		// If No Enemies then it's a Bonus Phase Type
	jmp ld3ba	// Skip Enemy Loading
ld399:
	iny
ld39a:
	lda ($1f),y	// \ Load Enemy X Position
	iny			// |
	sta $93,x	// /
	lda ($1f),y	// \ Load Enemy Y Position
	iny			// |
	sta $9c,x	// /
	lda ($1f),y	// \ Load Enemy Type
	iny			// |
	sta $0453,x	// /
	lda #$02	// \ Initialize Enemy Status
	sta $81,x	// / (02 = Sitting)
	lda #$01	// \ Initialize Enemy Balloons
	sta $8a,x	// / (01 = Sitting/Umbrella)
	lda $c6
	sta $0441,x
	dex
	bpl ld39a	// Load another enemy data
ld3ba:
	jsr ld4e5_getbytefromloadpointer	// \ Load Amount of Platforms
	sta $cd								// /
	jsr ld4e5_getbytefromloadpointer	// \
	sta $23								// | Load Platform Collision Pointer
	jsr ld4e5_getbytefromloadpointer	// | Left Side
	tay									// |
	sta $24								// /
	lda $23						// \
	jsr ld48c_nextplatformptr	// | Load Right Side Platform Collision Pointer
	sta $25						// |
	sty $26						// /
	jsr ld48c_nextplatformptr	// \
	sta $27						// | Load Top Side Platform Collision Pointer
	sty $28						// /
	jsr ld48c_nextplatformptr	// \
	sta $29						// | Load Bottom Side Platform Collision Pointer
	sty $2a						// /
ld3e1:
	jsr ld5d9
	jsr ld3ed
	jsr lc104_enablenmi
	jmp lc115
ld3ed:
	ldx #$22
ld3ef:
	lda ld437,x
	sta $57,x
	dex
	bpl ld3ef
	lda $c8
	bne ld410
	lda $3b
	and #$0c
	ora #3
	tay
	ldx #3
ld404:
	lda ld45a,y
	sta $5a,x
	dey
	dex
	bpl ld404
ld40d:
	jmp lc12d
ld410:
	ldx $0558
	lda ld46a,x
	sta $1d
	lda ld46f,x
	sta $1e
	ldx #3
	ldy #7
ld421:
	lda ($1d),y
	sta $72,x
	dey
	dex
	bpl ld421
	lda $16
	bne ld40d
ld42d:
	lda ($1d),y
	sta $005a,y
	dey
	bpl ld42d
	bmi ld40d
ld437:
    //35 bytes
db $3f,$00,$20,$0f,$2a,$09,$07,$0f,$30,$27,$15,$0f,$30,$02,$21
db $0f,$30,$00,$10,$0f,$16,$12,$37,$0f,$12,$16,$37,$0f,$17,$11
db $35,$0f,$17,$11,$2b
ld45a:
    //16 bytes
db $0f,$2a,$09,$07,$0f,$26,$06,$07,$0f,$1b,$0c,$07,$0f,$2c,$01
db $06
ld46a:
db $74,$7c,$84,$84,$84
ld46f:
    //29 bytes
db $d4,$d4,$d4,$d4,$d4,$0f,$02,$08,$06,$0f,$2b,$30,$12,$0f,$07
db $0a,$19,$0f,$26,$30,$2b,$0f,$07,$0c,$1c,$0f,$15,$30,$26

ld48c_nextplatformptr:
	sec
	adc $cd
	bcc ld492
	iny
ld492:
	rts
//-----------------------

ld493:
db $7f,$7e,$7d,$7c

ld497_uploadbackground:	//Argument: $001D = Pointer to pointers to screen data
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
	sta $2006
	jsr ld4f0_getbytefromgfxpointer
	sta $2006
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
	sta $2007
	dec $12
	bne ld4cc
	beq ld4a4
ld4d8:
	jsr ld4f0_getbytefromgfxpointer
ld4db:
	sta $2007
	dec $12
	bne ld4db
	beq ld4a4
ld4e4:
	rts
//-----------------------

ld4e5_getbytefromloadpointer:
	ldy #$00
	lda ($1d),y
	inc $1d
	bne ld4ef
	inc $1e
ld4ef:
	rts
//-----------------------

ld4f0_getbytefromgfxpointer:
	ldy #0
	lda ($1f),y
	inc $1f
	bne ld4fa
	inc $20
ld4fa:
	rts
//-----------------------

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
	sta $2006
	sta $13
	lda $12
	ora $54
	sta $2006
	rts
//-----------------------

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
//-----------------------

ld53c:
	lda #$23
	sta $2006
	jsr ld51c
	sta $2006
	lda $2007
	lda $2007
	and ld564,y
	ora ld568,y
	pha
	lda #$23
	sta $2006
	jsr ld51c
	sta $2006
	pla
	sta $2007
	rts
//-----------------------

ld564:
db $fc,$f3,$cf,$3f
ld568:
db $01,$04,$10,$40
ld56c:
	jsr lcccb
	jmp lc17c_uploadppubuffer
ld572:			// Initialize Balloon Trip Game Mode
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
	sty $2006
	sta $2006
	ldx #0
ld59b:
	lda ldcae,x
	sta $2007
	inx
	cpx #8
	bne ld59b
	lda #0
	ldx #$28
	jsr ld5b1
	lda #$aa
	ldx #$10
ld5b1:
	sta $2007
	dex
	bne ld5b1
	rts
//-----------------------

ld5b8:
	sty $2006
	sta $2006
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
	sta $2007
	dex
	bne ld5cb
	rts
//-----------------------

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
//-----------------------

ld5f1:
	lda $51
	sta $2006
	lda $50
	sta $2006
	lda $2007
	lda $2007
	cmp #$24
	bne ld60c
	txa
	and #3
	tay
	jmp ld63b
ld60c:
	rts
//-----------------------

ld60d_updatestarbganim:
	lda $4c		// \ If [$4C] == 0
	beq ld63a	// / Then Do Nothing
	dec $4c
	lda $4f		// \
	clc			// |
	adc #$02	// | Update and Get Current
	and #$3f	// | Star ID
	sta $4f		// |
	tax			// /
	jsr ld651	// \
	lda $51		// |
	sta $2006	// | Set PPU Address for Star Tile
	lda $50		// |
	sta $2006	// /
	lda $2007	// \
	lda $2007	// |
	ldy #$03	// | Check if Tile is part of
ld632:
	cmp ld64c,y	// | Star Animation tiles
	beq ld63b	// | If not: Stop
	dey			// |
	bpl ld632	// /
ld63a:
	rts

ld63b:
	lda $51			// \
	sta $2006		// |
	lda $50			// | Write Next Star Tile
	sta $2006		// |
	lda ld64c+1,y	// |
	sta $2007		// /
	rts

ld64c:	//Star Tile Animation Frames
db $24,$ed,$ee,$ef,$24

ld651:
	lda ld65c,x
	sta $50
	lda ld65c+1,x
	sta $51
	rts

ld65c:
    //128 bytes
db $63,$21,$a5,$21,$cb,$20,$b7,$20,$7d,$21,$9b,$22,$f2,$20,$49
db $22,$6d,$21,$0b,$22,$92,$22,$95,$21,$1c,$21,$48,$21,$e0,$20
db $0b,$23,$ce,$20,$d0,$21,$06,$21,$19,$21,$30,$22,$8a,$22,$88
db $22,$a4,$20,$42,$22,$68,$21,$3c,$22,$36,$21,$ca,$21,$bc,$20
db $96,$21,$4c,$21,$35,$22,$ef,$20,$68,$22,$a6,$20,$bb,$21,$7a
db $21,$ea,$20,$f1,$21,$c2,$20,$77,$21,$54,$21,$ba,$20,$c5,$22
db $be,$20,$fa,$20,$ae,$21,$46,$21,$9a,$21,$d2,$20,$3d,$21,$2b
db $22,$b0,$20,$b6,$21,$ac,$20,$b3,$20,$db,$20,$f6,$20,$2c,$21
db $e7,$20,$62,$21,$e4,$21,$4e,$21

ld6dc:
	lda #$00
ld6de:
	sta $43
	lda $3a
	beq ld6e5
ld6e4:
	rts

ld6e5:
	ldx $3e		// \ If [$3E] >= 2
	cpx #$02	// | Then return
	bcs ld6e4	// /
	lda $41,x	// \ If Player X has no lives
	bmi ld6e4	// / Then return
	ldy #$64
	jsr ld77c
	clc
	adc $48
	sta $45
	ldy #$0a
	jsr ld77c
	sta $44
	ldx $3f
	lda ld779,x
	sta $21
	lda #$06
	sta $22
	lda $3e
	asl
	asl
	ora $3e
	tax
	clc
	lda $03,x
	adc $43
	jsr ld78f
	sta $03,x
	lda $04,x
	adc $44
	jsr ld78f
	sta $04,x
	lda $05,x
	adc $45
	jsr ld78f
	sta $05,x
	lda $06,x
	adc $47
	jsr ld78f
	sta $06,x
	lda $07,x
	adc #0
	jsr ld78f
	sta $07,x
	inx
	inx
	inx
	inx
	ldy #4
ld746:
	lda $03,x
	cmp ($21),y
	bcc ld765
	bne ld752
	dex
	dey
	bpl ld746
ld752:
	ldy #0
	lda $3e
	asl
	asl
	ora $3e
	tax
ld75b:
	lda $03,x
	sta ($21),y
	inx
	iny
	cpy #5
	bne ld75b
ld765:
	ldy #4
ld767:
	lda ($21),y
	sta $000d,y
	dey
	bpl ld767
	inc $46
	lda $16
	beq ld778
	jsr lc539
ld778:
	rts
//-----------------------

ld779:
db $29,$2e,$33

ld77c:
	sty $12
	ldx #$ff
	lda $43
ld782:
	sec
	sbc $12
	inx
	bcs ld782
	clc
	adc $12
	sta $43
	txa
	rts
//-----------------------

ld78f:
	cmp #$0a
	bcs ld794
	rts
//-----------------------

ld794:
	sec
	sbc #$0a
	rts
//-----------------------

ld798_updatestatusbar:
	ldy $46		// \
	dey			// | 
	beq ld7a0	// |
	bpl ld805	// /
	rts

ld7a0:
	lda #$20	// \
	sta $2006	// | PPUADDR = $2043
	lda #$43	// |
	sta $2006	// /
	lda #$8e	// \ Upload I- to PPU
	sta $2007	// /

	ldx #$04	// \
ld7b1:
	lda $03,x	// | Upload Player 1 Score to PPU
	sta $2007	// |
	dex			// |
	bpl ld7b1	// |
	lda #$00	// |
	sta $2007	// /

	lda #$24	// \
	sta $2007	// | Upload 2 empty spaces to PPU
	sta $2007	// /
	ldx #$8c	// \
	stx $2007	// | Upload TOP- to PPU
	inx			// |
	stx $2007	// /

	ldx #$04	// \
ld7d1:
	lda $0d,x	// | Upload Top Score to PPU
	sta $2007	// |
	dex			// |
	bpl ld7d1	// |
	lda #0		// |
	sta $2007	// /

	lda #$24	// \
	sta $2007	// | Upload 2 empty spaces to PPU
	sta $2007	// /

	lda $16		// \ If Game Mode is Balloon Trip Mode
	bne ld854	// / then render RANK 
	lda $40		// \ If Single Player
	beq ld802	// / then don't render Player 2 Score

	lda #$8f	// \ Upload II- to PPU
	sta $2007	// /

	ldx #$04	// \
ld7f5:
	lda $08,x	// | Upload Player 2 Score to PPU
	sta $2007	// |
	dex			// |
	bpl ld7f5	// |
	lda #0		// |
	sta $2007	// /
ld802:
	dec $46
	rts
//-----------------------

ld805:
	dec $46
	lda #$20	// \
	sta $2006	// | PPUADDR = $2062
	lda #$62	// | GAME OVER Player 1 Status Bar
	sta $2006	// /
	lda $41		// \ If Player 1 Lives is negative
	jsr ld826	// / Then upload GAME OVER
	lda $40		// \ If Single Player
	beq ld83a	// / then return
	lda #$20	// \
	sta $2006	// | PPUADDR = $2075
	lda #$75	// | GAME OVER Player 2 Status Bar
	sta $2006	// /
	lda $42		// \ If Player 2 Lives is negative
ld826:
	bmi ld83b	// / Then upload GAME OVER
ld828:
	sta $50		// \
	ldx #$06	// |
ld82c:
	lda #$24	// | Upload amount of lives to PPU
	cpx $50		// |
	bcs ld834	// |
	lda #$2a	// |
ld834:
	sta $2007	// |
	dex			// |
	bpl ld82c	// /
ld83a:
	rts

ld83b:
	lda $40		// \ If Single Player
	beq ld828	// / then go back
	ldx #$08	// \
ld841:
	lda ld84b,x	// | Upload GAME OVER to PPU
	sta $2007	// |
	dex			// |
	bpl ld841	// /
	rts

ld84b:	//GAME OVER
db $1b,$0e,$1f,$18,$24,$0e,$16,$0a,$10

ld854:
	ldy #$04	// \
ld856:
	lda ld86c,y	// | Upload RANK- to PPU
	sta $2007	// |
	dey			// |
	bpl ld856	// /
	lda $4a		// \
	sta $2007	// | Upload Rank Number to PPU
	lda $49		// |
	sta $2007	// /
	dec $46
	rts

ld86c:	//RANK-
db $fb,$fa,$f9,$f8,$f7

ld871:
	sta $12
	stx $13
	sty $14
	ldx #1
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
	lda ld8d1,y
	sta $02f1,x
	lda ld8d7,y
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
//-----------------------

ld8d1:
db $f4,$f5,$f6,$f7,$f8,$f9
ld8d7:
db $fb,$fb,$fa,$fb,$fb,$fb
ld8dd:
	ldx #1
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
//-----------------------

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
//-----------------------

ld90f_uploadtitlescreen:
	jsr ld246_clearppu
	jsr lc10a_clearppumask
	jsr lf465_clearframeflag
	jsr lc0fa_disablenmi
	lda #(ld92c & $FF)
	sta $1d
	lda #(ld92c >> 8)
	sta $1e
	jsr ld497_uploadbackground
	jsr lc104_enablenmi
	jmp lc115

ld92c:
dw ld930,$0000

ld930:	//Title Screen Data
db $3f,$00,$04,$0f,$30,$27,$2a,$3f,$18,$04,$0f
db $16,$30,$21,$20,$7c,$21,$f0,$f1,$24,$24,$24,$24,$e0,$e1,$e1
db $e2,$e0,$e1,$e1,$e2,$e0,$e2,$24,$e0,$e2,$24,$e0,$e1,$e1,$e2
db $e0,$e1,$e1,$e2,$e0,$ec,$24,$e0,$e2,$20,$a2,$1b,$e3,$e3,$e3
db $e5,$e3,$e3,$e3,$e5,$e3,$e5,$24,$e3,$e5,$24,$e3,$e3,$e3,$e5
db $e3,$e3,$e3,$e5,$e3,$e3,$f3,$e3,$e5,$20,$c2,$1b,$e3,$e4,$e3
db $e7,$e3,$e4,$e3,$e5,$e3,$e5,$24,$e3,$e5,$24,$e3,$e4,$e3,$e5
db $e3,$e4,$e3,$e5,$e3,$e3,$e3,$e3,$e5,$20,$e2,$1b,$e3,$e3,$e3
db $e2,$e3,$e3,$e3,$e5,$e3,$e5,$24,$e3,$e5,$24,$e3,$e3,$e3,$e5
db $e3,$e3,$e3,$e5,$e3,$e3,$e3,$e3,$e5,$21,$02,$1b,$e3,$e4,$e3
db $e5,$e3,$f2,$e3,$e5,$e3,$e3,$e2,$e3,$e3,$e2,$e3,$e3,$e3,$e5
db $e3,$e3,$e3,$e5,$e3,$f2,$e3,$e3,$e5,$21,$22,$1b,$e6,$e3,$e3
db $e7,$eb,$24,$e6,$e7,$e6,$e3,$e7,$e6,$e3,$e7,$e6,$e3,$e3,$e7
db $e6,$e3,$e3,$e7,$eb,$24,$e6,$e3,$e7,$21,$4c,$12,$e0,$e1,$e1
db $e2,$e0,$e2,$e0,$e1,$e1,$e2,$e8,$24,$e0,$e2,$e0,$e1,$e1,$e2
db $21,$6c,$12,$e3,$e3,$e3,$e7,$e3,$e5,$e3,$f5,$f6,$e7,$e3,$f3
db $e3,$e5,$e6,$e3,$e3,$e7,$21,$8c,$12,$e3,$e3,$ef,$24,$e3,$e5
db $e3,$24,$24,$24,$e3,$e3,$e3,$e5,$24,$e3,$e5,$24,$21,$ac,$12
db $e3,$e3,$e1,$ea,$e3,$e5,$e3,$e9,$e3,$e2,$e3,$e3,$e3,$e5,$24
db $e3,$e5,$24,$21,$cc,$12,$e3,$e3,$ef,$24,$e3,$e5,$e3,$f3,$e3
db $e5,$e3,$f2,$e3,$e5,$24,$e3,$e5,$24,$21,$ec,$12,$e6,$e7,$24
db $24,$e6,$e7,$e6,$e7,$e6,$e7,$eb,$24,$e6,$e7,$24,$e6,$e7,$24
db $22,$48,$10,$0a,$24,$24,$01,$25,$19,$15,$0a,$22,$0e,$1b,$24
db $10,$0a,$16,$0e,$22,$88,$10,$0b,$24,$24,$02,$25,$19,$15,$0a
db $22,$0e,$1b,$24,$10,$0a,$16,$0e,$22,$c8,$10,$0c,$24,$24,$0b
db $0a,$15,$15,$18,$18,$17,$24,$24,$1d,$1b,$12,$19,$23,$49,$0e
db $f4,$01,$09,$08,$04,$24,$17,$12,$17,$1d,$0e,$17,$0d,$18,$00

ldac1_titlescreenloop:	// Manage Title Screen
	jsr lc104_enablenmi
	jsr ld90f_uploadtitlescreen
	lda #$00	// \ Reset Frame Counter
	sta $19		// /
ldacb:
	jsr lf465_clearframeflag
	lda $19		// \ If Frame Counter is 0
	beq ldaf1	// / then do demo?
	jsr ldb08	// Set Modes & Cursor
	jsr le768_polljoypad0
	tax
	and #$10	// \ If Start button is pressed
	bne ldaf0	// / then exit Title Screen loop
	txa
	and #$20	// \ If Select button is NOT pressed
	beq ldaed	// / then loop again
	lda #$00	// \ Reset Frame Counter
	sta $19		// /
	ldx $3f		// \
	lda ldb05,x	// | Select Next Mode
	sta $3f		// /
ldaed:
	jmp ldacb	// Loop
ldaf0:
	rts
//-----------------------

ldaf1:
	inc $3a		// Set Demo Flag
	inc $40		// Set to 2 Players
	lda #$00	// \ Disable All Sound Channels
	sta $4015	// /
	sta $16		// Set Game Mode to 00 (Balloon Fight)
	jsr lf1f2
	lda #$00
	sta $3a
	beq ldac1_titlescreenloop

ldb05:	//Title Screen choices
	db $01,$02,$00

ldb08:
	lda $3f		// \
	lsr			// | Set Game Mode
	sta $16		// / depending on selected mode
	lda $3f		// \
	tax			// | Set Amount of players
	and #$01	// | depending on selected mode
	sta $40		// /
	lda ldb27,x	// \ Set Y position of menu cursor balloon
	sta $057b	// /
	lda #$2c	// \ Set X position of menu cursor balloon
	sta $0567	// /
	ldx #$00	// \ Set graphics of menu cursor balloon
	stx $055d	// /
	jmp lce2f_balloonxspritemanage

ldb27:
db $8c,$9c,$ac

ldb2a:
    //16 bytes
db $4a,$59,$f2,$6c,$77,$88,$e1,$6c,$99,$03,$ca,$6c,$ad,$17,$e1
db $6c
ldb3a:
    //372 bytes
db $db,$db,$db,$db,$db,$db,$db,$db,$db,$dc,$db,$db,$db,$dc,$db
db $db,$2b,$dc,$39,$dd,$00,$00,$10,$06,$ff,$ff,$96,$df,$02,$7a
db $de,$2b,$dc,$39,$dd,$4b,$dd,$00,$00,$18,$0c,$04,$0e,$ff,$ff
db $a0,$df,$04,$86,$de,$ca,$dc,$00,$00,$ff,$ff,$b0,$df,$00,$9a
db $de,$2b,$dc,$5e,$dd,$00,$00,$08,$06,$18,$0a,$ff,$ff,$b1,$df
db $06,$9e,$de,$2b,$dc,$7d,$dd,$00,$00,$04,$06,$12,$08,$ff,$ff
db $c1,$df,$07,$ba,$de,$2b,$dc,$a3,$dd,$00,$00,$06,$06,$14,$10
db $ff,$0f,$0d,$01,$ff,$d4,$df,$09,$da,$de,$2b,$dc,$ce,$dd,$00
db $00,$04,$06,$10,$0e,$ff,$08,$0e,$03,$0d,$09,$03,$12,$08,$03
db $17,$0d,$03,$ff,$e7,$df,$03,$02,$df,$2b,$dc,$5e,$dd,$00,$00
db $10,$06,$1a,$0c,$ff,$08,$08,$01,$18,$04,$01,$ff,$b1,$df,$06
db $9e,$de,$2b,$dc,$db,$dd,$00,$00,$0e,$06,$0c,$14,$ff,$ff,$f7
db $df,$06,$12,$df,$2b,$dc,$01,$de,$00,$00,$04,$08,$16,$10,$ff
db $ff,$07,$e0,$09,$2e,$df,$2b,$dc,$32,$de,$00,$00,$04,$10,$18
db $10,$ff,$0e,$06,$01,$ff,$17,$e0,$07,$56,$df,$2b,$dc,$5b,$de
db $00,$00,$04,$08,$0e,$10,$ff,$10,$07,$01,$ff,$2a,$e0,$07,$76
db $df,$23,$40,$88,$39,$38,$39,$38,$39,$38,$39,$33,$24,$24,$24
db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$30,$38
db $39,$38,$39,$38,$39,$38,$3c,$3b,$3c,$3b,$3c,$3b,$3c,$3d,$58
db $59,$5a,$5b,$58,$59,$5a,$5b,$58,$59,$5a,$5b,$58,$59,$5a,$5b
db $3a,$3b,$3c,$3b,$3c,$3b,$3c,$3b,$60,$61,$62,$63,$60,$61,$62
db $63,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d
db $5e,$5f,$60,$61,$62,$63,$60,$61,$62,$63,$5c,$5d,$5e,$5f,$5c
db $5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f
db $5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f
ldcae:
    //911 bytes
db $40,$50,$50,$50,$50,$90,$a0,$a0,$23,$f0,$10,$00,$00,$a0,$a0
db $a0,$a0,$00,$00,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$00,$a3,$04
db $04,$93,$94,$94,$94,$a3,$05,$04,$95,$96,$96,$96,$a2,$ea,$05
db $93,$94,$94,$94,$94,$a2,$eb,$05,$95,$96,$96,$96,$96,$a3,$34
db $03,$93,$94,$94,$a3,$35,$03,$95,$96,$96,$a3,$1a,$04,$93,$94
db $94,$94,$a3,$1b,$04,$95,$96,$96,$96,$63,$80,$20,$97,$23,$a0
db $20,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99
db $98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98,$99,$98
db $99,$98,$99,$23,$c0,$08,$40,$50,$50,$50,$50,$90,$a0,$a0,$63
db $e8,$10,$ff,$00,$22,$49,$0e,$30,$31,$32,$31,$32,$31,$32,$31
db $32,$31,$32,$31,$32,$33,$00,$21,$57,$06,$30,$31,$32,$31,$32
db $33,$21,$65,$06,$30,$31,$32,$31,$32,$33,$00,$21,$90,$03,$30
db $31,$33,$22,$26,$03,$30,$31,$33,$22,$57,$03,$30,$31,$33,$22
db $6c,$03,$30,$31,$33,$22,$f2,$03,$30,$31,$33,$00,$20,$cb,$03
db $30,$31,$33,$a1,$6d,$03,$3e,$3f,$40,$a1,$59,$04,$3e,$3f,$3f
db $40,$a1,$a5,$03,$3e,$3f,$40,$22,$aa,$03,$30,$31,$33,$22,$b3
db $03,$30,$31,$33,$00,$20,$e2,$02,$30,$33,$20,$fb,$02,$30,$33
db $21,$57,$02,$30,$33,$21,$93,$02,$30,$33,$22,$0b,$02,$30,$33
db $22,$47,$02,$30,$33,$22,$83,$02,$30,$33,$22,$cf,$04,$30,$31
db $32,$33,$00,$22,$ca,$03,$30,$31,$33,$22,$d2,$03,$30,$31,$33
db $00,$21,$08,$04,$30,$31,$32,$33,$21,$14,$04,$30,$31,$32,$33
db $a1,$a5,$04,$3e,$3f,$3f,$40,$a1,$ba,$04,$3e,$3f,$3f,$40,$22
db $6c,$06,$30,$31,$32,$31,$32,$33,$00,$22,$ee,$04,$30,$31,$32
db $33,$20,$f9,$03,$30,$31,$33,$a1,$1a,$03,$3f,$3f,$40,$21,$90
db $03,$30,$31,$33,$a1,$b1,$03,$3f,$3f,$40,$22,$28,$03,$30,$31
db $33,$a2,$49,$03,$3f,$3f,$40,$20,$ea,$02,$30,$33,$00,$a2,$6c
db $03,$3e,$3f,$40,$a2,$73,$03,$3e,$3f,$40,$20,$e4,$04,$30,$31
db $32,$33,$20,$f8,$04,$30,$31,$32,$33,$21,$a8,$04,$30,$31,$32
db $33,$21,$b5,$04,$30,$31,$32,$33,$00,$22,$64,$02,$30,$33,$22
db $08,$02,$30,$33,$21,$ac,$02,$30,$33,$21,$b4,$02,$30,$33,$22
db $18,$02,$30,$33,$22,$7c,$02,$30,$33,$00,$10,$c8,$48,$38,$ff
db $b8,$cf,$cf,$8f,$e0,$e0,$98,$10,$c4,$48,$b8,$28,$3c,$ff,$b8
db $e8,$58,$cf,$cf,$8f,$4f,$57,$e0,$e0,$98,$58,$60,$10,$ff,$df
db $ec,$10,$c8,$80,$30,$b8,$60,$90,$38,$ff,$98,$48,$d0,$78,$a8
db $cf,$cf,$5f,$87,$8f,$97,$b7,$e0,$e0,$68,$90,$98,$a0,$c0,$10
db $c8,$58,$68,$c8,$28,$50,$98,$38,$ff,$70,$78,$d0,$30,$68,$b0
db $cf,$cf,$2f,$57,$4f,$67,$a7,$a7,$e0,$e0,$38,$6c,$6c,$7c,$b0
db $b0,$10,$c8,$12,$da,$ba,$9a,$5a,$3a,$1a,$7a,$38,$ff,$1e,$e6
db $c6,$a6,$66,$46,$26,$96,$cf,$cf,$37,$37,$4f,$5f,$7f,$8f,$9f
db $af,$e0,$e0,$40,$40,$58,$68,$88,$98,$a8,$b8,$10,$c8,$52,$92
db $38,$ff,$66,$a6,$cf,$cf,$af,$af,$e0,$e0,$b8,$b8,$10,$c8,$40
db $a0,$28,$d0,$60,$38,$ff,$60,$c0,$30,$d8,$90,$cf,$cf,$3f,$3f
db $67,$67,$97,$e0,$e0,$48,$48,$88,$88,$a0,$10,$c8,$50,$c8,$88
db $40,$70,$d0,$88,$48,$38,$ff,$60,$e0,$98,$58,$90,$d8,$90,$50
db $cf,$cf,$37,$37,$5f,$87,$b7,$3c,$64,$8c,$e0,$e0,$40,$40,$68
db $90,$c0,$60,$80,$a8,$10,$c8,$20,$c0,$40,$a8,$60,$98,$38,$ff
db $40,$e0,$60,$c8,$68,$a0,$cf,$cf,$37,$37,$67,$67,$97,$97,$e0
db $e0,$40,$40,$70,$70,$b0,$b0,$10,$c8,$20,$40,$60,$a0,$c0,$e0
db $38,$ff,$30,$50,$70,$b0,$d0,$f0,$cf,$cf,$97,$7f,$67,$67,$7f
db $97,$e0,$e0,$a0,$88,$70,$70,$88,$a0,$03,$58,$78,$04,$78,$78
db $04,$98,$78,$04,$05,$c8,$38,$05,$38,$40,$05,$58,$78,$04,$78
db $78,$04,$98,$78,$04,$00,$05,$94,$a0,$06,$84,$48,$05,$34,$70
db $04,$64,$80,$04,$bc,$78,$04,$06,$5c,$18,$06,$24,$50,$05,$64
db $40,$05,$c4,$38,$05,$54,$90,$04,$9c,$90,$04,$06,$10,$20,$06
db $d8,$20,$06,$b8,$38,$05,$98,$48,$05,$58,$68,$05,$38,$78,$04
db $05,$54,$98,$05,$94,$98,$05,$6c,$39,$06,$94,$31,$06,$bc,$59
db $06,$05,$50,$28,$06,$a8,$28,$06,$24,$50,$06,$cc,$50,$06,$70
db $80,$04,$05,$50,$20,$06,$c8,$20,$06,$84,$48,$05,$44,$70,$05
db $78,$a0,$04,$06,$28,$20,$06,$c8,$20,$06,$48,$50,$05,$b0,$50
db $05,$5c,$80,$04,$94,$80,$04,$06,$20,$80,$04,$40,$68,$05,$60
db $50,$06,$a0,$50,$06,$c0,$68,$05,$e0,$80,$04
le03d:
    //527 bytes
db $08,$08,$f0,$08,$08,$00,$00,$00,$00,$08,$08,$08,$00,$00,$01
db $08,$08,$09,$00,$00,$ff,$08,$08,$07,$fe,$00,$00,$06,$08,$08
db $02,$00,$00,$0a,$08,$08,$02,$00,$01,$0a,$08,$09,$04,$00,$00
db $0c,$08,$08,$00,$00,$fe,$08,$08,$06,$fd,$fd,$fd,$05,$05,$05
db $08,$08,$08,$00,$00,$00,$08,$08,$07,$00,$00,$ff,$08,$08,$09
db $00,$00,$01,$0a,$08,$08,$02,$00,$00,$06,$08,$08,$fe,$00,$00
db $06,$08,$07,$fe,$00,$ff,$04,$08,$08,$fc,$00,$00,$08,$08,$0a
db $00,$00,$02,$0b,$0b,$0b,$03,$03,$03,$00,$00,$01,$02,$03,$04
db $05,$00,$00,$01,$02,$03,$06,$07,$00,$00,$01,$02,$03,$06,$08
db $00,$09,$0a,$02,$0b,$0c,$05,$00,$00,$01,$02,$03,$04,$05,$00
db $00,$01,$02,$0d,$0e,$05,$00,$1a,$1b,$1c,$1d,$1e,$1f,$01,$1a
db $1b,$20,$1d,$1e,$fc,$00,$1a,$21,$22,$1d,$23,$24,$00,$00,$38
db $35,$0d,$39,$37,$00,$00,$34,$35,$03,$36,$37,$00,$09,$3a,$35
db $0b,$3b,$37,$00,$ce,$cf,$d0,$d1,$d2,$d3,$00,$25,$26,$27,$28
db $29,$2a,$01,$28,$29,$2c,$25,$26,$2b,$00,$4c,$cc,$02,$4d,$cd
db $05,$00,$4c,$2f,$22,$4d,$30,$24,$07,$4c,$2f,$35,$4d,$30,$37
db $02,$4c,$2f,$27,$4d,$30,$2a,$07,$4c,$2f,$2b,$4d,$30,$2c,$00
db $0f,$10,$02,$11,$12,$05,$00,$0f,$10,$02,$11,$19,$07,$00,$0f
db $10,$02,$11,$19,$08,$00,$13,$14,$02,$15,$16,$05,$00,$0f,$10
db $02,$11,$12,$05,$03,$13,$17,$02,$15,$18,$05,$04,$13,$2d,$1c
db $15,$2e,$1f,$05,$13,$2d,$20,$15,$2e,$fc,$04,$13,$2f,$22,$15
db $30,$24,$00,$13,$3c,$35,$15,$3d,$37,$00,$0f,$40,$35,$11,$41
db $37,$03,$13,$3e,$35,$15,$3f,$37,$00,$d4,$d5,$d0,$d6,$d7,$d3
db $00,$25,$31,$27,$32,$33,$2a,$02,$25,$31,$27,$32,$33,$2a,$00
db $fc,$48,$42,$fc,$49,$43,$00,$fc,$48,$44,$fc,$49,$45,$00,$fc
db $4a,$46,$fc,$4b,$47,$00,$fc,$a4,$a5,$fc,$a6,$a7,$08,$fc,$71
db $fc,$fc,$72,$73,$08,$fc,$74,$fc,$fc,$75,$76,$08,$fc,$71,$77
db $fc,$72,$73,$08,$fc,$74,$77,$fc,$75,$76,$08,$fc,$71,$78,$fc
db $72,$73,$08,$fc,$74,$78,$fc,$75,$76,$08,$fc,$71,$79,$fc,$72
db $73,$08,$fc,$74,$79,$fc,$75,$76,$00,$4e,$4f,$50,$51,$52,$53
db $00,$4e,$4f,$50,$51,$5e,$58,$00,$4e,$4f,$50,$51,$5e,$5d,$00
db $54,$55,$50,$56,$57,$53,$00,$4e,$4f,$50,$51,$52,$53,$00,$59
db $5a,$50,$5b,$5c,$53,$00,$fc,$5f,$60,$fc,$61,$62,$06,$63,$64
db $60,$fc,$65,$62,$00,$66,$67,$60,$68,$69,$62,$00,$6a,$67,$60
db $6b,$69,$62,$00,$fc,$6c,$6d,$fc,$6e,$6f,$00,$fc,$6c,$6d,$fc
db $6e,$70
le24c:
    //76 bytes
db $af,$b6,$bd,$b6,$c4,$cb,$d2,$cb,$d9,$e0,$e7,$e0,$ee,$f5,$fc
db $f5,$0a,$0a,$0a,$0a,$11,$11,$11,$11,$18,$18,$18,$18,$1f,$1f
db $1f,$1f,$26,$26,$26,$26,$2d,$2d,$2d,$2d,$34,$34,$34,$34,$3b
db $42,$49,$42,$50,$57,$5e,$57,$65,$6c,$73,$6c,$7a,$81,$88,$81
db $96,$96,$96,$96,$9d,$9d,$9d,$9d,$a4,$ab,$b2,$ab,$b9,$a4,$b9
db $a4
le298:
    //76 bytes
db $e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0
db $e0,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1
db $e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1
db $e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1
db $e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1
db $e1
le2e4:
db $81,$81,$8f,$81,$f5,$f5,$03,$f5
le2ec:
db $e1,$e1,$e1,$e1,$e0,$e0,$e1,$e0
le2f4:
    //44 bytes
db $f8,$ff,$06,$ff,$0d,$14,$1b,$14,$3e,$3e,$22,$29,$30,$30,$30
db $30,$c0,$c0,$c0,$c0,$c0,$c7,$c0,$c7,$ce,$d5,$ce,$d5,$dc,$e3
db $dc,$e3,$ea,$f1,$ea,$f1,$3e,$45,$3e,$45,$37,$37,$37,$37
le320:
    //44 bytes
db $e1,$e1,$e2,$e1,$e2,$e2,$e2,$e2,$e2,$e2,$e2,$e2,$e2,$e2,$e2
db $e2,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1
db $e1,$e1,$e1,$e1,$e1,$e1,$e2,$e2,$e2,$e2,$e2,$e2,$e2,$e2
le34c:
db $44,$2c,$00,$00,$00,$04
le352:
    //50 bytes
db $24,$08,$00,$00,$fc,$fc,$fe,$fc,$00,$7b,$fc,$fc,$7c,$fc,$fc
db $00,$7d,$7e,$fc,$7f,$80,$fc,$00,$81,$82,$83,$84,$85,$86,$00
db $87,$88,$fc,$89,$8a,$fc,$00,$8b,$8c,$fc,$8d,$8e,$fc,$00,$8f
db $90,$fc,$fc,$fc,$fc
le384:
db $55,$5a,$61,$68,$6f,$76,$7d
le38b:
db $e3,$e3,$e3,$e3,$e3,$e3,$e3
le392:
    //9 bytes
db $20,$38,$50,$68,$80,$98,$b0,$c8,$08
le39b:
    //9 bytes
db $20,$38,$c8,$b0,$98,$80,$68,$50,$08
le3a4:
	lda le392,x
	sta $1f
	lda $19
	lsr
	bcc le3b3
	lda le39b,x
	sta $1f
le3b3:
	lda #2
	sta $20
	lda $88,x
	bpl le3cf
	cmp #$ff
	beq le3c2
	jmp le4d5
le3c2:
	ldy #$14
le3c4:
	lda #$f0
	sta ($1f),y
	dey
	dey
	dey
	dey
	bpl le3c4
	rts
//-----------------------

le3cf:
	cpx #8
	beq le41b
	lda $7f,x
	asl
	asl
	adc $0436,x
	cpx #2
	bcs le408
	ldy $88,x
	adc le34c,y
	tay
	lda le24c,y
	sta $1d
	lda le298,y
	sta $1e
	lda $bd,x
	beq le429
	ldy $88,x
	lda le34c+3,y
	adc $0436,x
	tay
	lda le2e4,y
	sta $1d
	lda le2ec,y
	sta $1e
	jmp le429
le408:
	ldy $88,x
	clc
	adc le352,y
	tay
	lda le2f4,y
	sta $1d
	lda le320,y
	sta $1e
	bne le429
le41b:
	ldy $7f,x
	bmi le3c2
	lda le384,y
	sta $1d
	lda le38b,y
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
	lda #$43
	sta $21
	lda #$e0
	sta $22
	lda $0448,x
	beq le47c
	lda #$79
	sta $21
	lda #$e0
	sta $22
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
//-----------------------

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
//-----------------------

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
//-----------------------

le587:
	ldx $bb
	bmi le5c4
	lda le5c5,x
	sta $1d
	lda le5ca,x
	sta $1e
	ldy #0
	ldx #0
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
	and #3
	bne le5c4
	dec $bb
le5c4:
	rts
//-----------------------

le5c5:
db $fd,$ed,$e0,$d6,$cf
le5ca:
    //55 bytes
db $e5,$e5,$e5,$e5,$e5,$d0,$ae,$03,$04,$f0,$f0,$f0,$c8,$af,$03
db $04,$d0,$b0,$03,$04,$f0,$f0,$c8,$b1,$03,$fc,$c8,$b2,$03,$04
db $d0,$b3,$03,$04,$f0,$c8,$b4,$03,$00,$c8,$b4,$43,$08,$d0,$b5
db $03,$00,$d0,$b5,$43,$08,$f0,$f0,$f0,$f0
le601:
    //12 bytes
db $04,$04,$05,$06,$03,$03,$03,$06,$0a,$0a,$0a,$0a
le60d:
    //12 bytes
db $28,$32,$46,$78,$00,$00,$00,$64,$00,$00,$00,$00
le619:
    //12 bytes
db $0a,$1e,$32,$70,$00,$00,$00,$70,$00,$00,$00,$00
le625:
    //12 bytes
db $14,$3c,$64,$a0,$00,$00,$00,$a0,$00,$00,$00,$00
le631:
    //12 bytes
db $70,$b0,$e0,$40,$80,$80,$80,$40,$00,$00,$00,$00
le63d:
    //12 bytes
db $00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$00
le649:
    //12 bytes
db $90,$50,$20,$c0,$80,$80,$80,$c0,$00,$00,$00,$00
le655:
    //12 bytes
db $ff,$ff,$ff,$fe,$ff,$ff,$ff,$fe,$00,$00,$00,$00
le661:
    //12 bytes
db $50,$90,$c0,$40,$40,$40,$40,$40,$00,$00,$00,$00
le66d:
    //12 bytes
db $00,$00,$00,$01,$00,$00,$00,$01,$02,$02,$02,$02
le679:
    //12 bytes
db $b0,$70,$40,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
le685:
    //12 bytes
db $ff,$ff,$ff,$fe,$ff,$ff,$ff,$fe,$fe,$01,$fe,$fe
le691:
	jsr lee25
	ldx #7
le696:
	lda $88,x
	bpl le6a4
	cmp #$ff
	beq le6e2
	jsr lecba
	jmp le6e2
le6a4:
	cpx #2
	bcc le6b8
	cmp #1
	bne le6b8
	lda $7f,x
	cmp #2
	bcs le6b8
	lda $f1
	ora #$20
	sta $f1
le6b8:
	dec $043f,x
	bne le6d9
	lda #3
	sta $043f,x
	cpx #2
	bcs le6ce
	dec $bf,x
	bne le6ce
	lda #0
	sta $bd,x
le6ce:
	jsr lea18
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
	bpl le696
	rts
//-----------------------

le6e9:
	cpx #2
	bcs le705
	lda $19
	and #$0f
	bne le6f8
	jsr lf1b3
	sta $31,x
le6f8:
	lda $3a
	bne le705
	jsr le76a_polljoypadx
	lda $061c,x
	sta $31,x
le704:
	rts
//-----------------------

le705:
	lda $9a,x
	cmp #$a0
	bcc le712
	lda $31,x
	ora #$40
	sta $31,x
	rts
//-----------------------

le712:
	dec $045a,x
	bne le704
	jsr lf1b3
	ldy $0451,x
	and le762,y
	adc le762+3,y
	sta $045a,x
	stx $12
	lda $19
	rol
	rol
	eor $12
	and #1
	tay
	lda $0088,y
	bmi le749
	lda $00bd,y
	bne le749
	lda #0
	sta $31,x
	lda $009a,y
	sec
	sbc #4
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
	ora #1
	sta $31,x
	rts
//-----------------------

le75b:
	lda $31,x
	ora #2
	sta $31,x
	rts
//-----------------------

le762:
db $1f,$0f,$07,$20,$10,$08

le768_polljoypad0:
	ldx #$00	// Read Controller 0
le76a_polljoypadx:
	lda #$01	// \
	sta $4016	// | Output Strobe to both controllers
	lda #$00	// |
	sta $4016	// /
	ldy #$07
le776:
	lda $4016,x	// \
	sta $12		// |
	lsr			// | Poll Controller X
	ora $12		// | to $061C + X
	lsr			// |
	rol $061c,x	// |
	dey			// |
	bpl le776	// /
	ldy $061e,x	// \
	lda $061c,x	// |
	sta $061e,x	// | Check for pressed buttons
	tya			// |
	eor $061c,x	// |
	and $061c,x	// /
	rts			// Returns pressed buttons in A
//-----------------------

le796:
	lda $88,x
	bne le7a3
le79a:
	lda #0
	sta $0424,x
	sta $042d,x
	rts
//-----------------------

le7a3:
	cmp #2
	beq le7e8
	cpx #2
	bcc le7e8
	lda $7f,x
	cmp #2
	bcs le79a
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
//-----------------------

le7e8:
	lda $7f,x
	cmp #6
	bcc le7ef
	rts
//-----------------------

le7ef:
	lda $7f,x
	cmp #4
	bne le811
	lda $31,x
	and #2
	beq le802
	lda $0448,x
	beq le811
	bne le80d
le802:
	lda $31,x
	and #1
	beq le811
	lda $0448,x
	bne le811
le80d:
	lda #5
	sta $7f,x
le811:
	lda $7f,x
	cmp #2
	bne le832
	lda $31,x
	and #2
	beq le821
	lda #0
	beq le829
le821:
	lda $31,x
	and #1
	beq le82e
	lda #1
le829:
	cmp $0448,x
	beq le832
le82e:
	lda #4
	sta $7f,x
le832:
	lda $7f,x
	cmp #4
	bcc le854
	lda $31,x
	and #2
	beq le845
	lda $0448,x
	bne le854
	beq le850
le845:
	lda $31,x
	and #1
	beq le854
	lda $0448,x
	beq le854
le850:
	lda #2
	sta $7f,x
le854:
	lda $7f,x
	cmp #3
	bne le864
	lda $31,x
	and #3
	beq le864
	lda #2
	sta $7f,x
le864:
	lda $7f,x
	cmp #4
	bcs le87f
	lda $31,x
	and #2
	beq le874
	lda #0
	beq le87c
le874:
	lda $31,x
	and #1
	beq le87f
	lda #1
le87c:
	sta $0448,x
le87f:
	lda $7f,x
	cmp #4
	bcc le8b8
	lda $0436,x
	cmp #1
	bne le8b8
	ldy $0451,x
	lda $0448,x
	beq le8a6
	lda $0424,x
	sec
	sbc le625,y
	sta $0424,x
	lda $042d,x
	sbc #0
	jmp le901
le8a6:
	lda $0424,x
	clc
	adc le625,y
	sta $0424,x
	lda $042d,x
	adc #0
	jmp le901
le8b8:
	lda $7f,x
	beq le8c7
	cmp #2
	beq le907
	cmp #3
	beq le8c7
	jmp le951
le8c7:
	lda $0436,x
	cmp #1
	beq le8d1
	jmp le951
le8d1:
	ldy $0451,x
	lda $31,x
	and #2
	beq le8ec
	lda $0424,x
	sec
	sbc le619,y
	sta $0424,x
	lda $042d,x
	sbc #0
	jmp le901
le8ec:
	lda $31,x
	and #1
	beq le951
	lda $0424,x
	clc
	adc le619,y
	sta $0424,x
	lda $042d,x
	adc #0
le901:
	sta $042d,x
	jmp le951
le907:
	lda $0436,x
	cmp #1
	bne le951
	ldy $0451,x
	lda $31,x
	and #2
	beq le929
	lda $0424,x
	sec
	sbc le625,y
	sta $0424,x
	lda $042d,x
	sbc #0
	jmp le93e
le929:
	lda $31,x
	and #1
	beq le951
	lda $0424,x
	clc
	adc le625,y
	sta $0424,x
	lda $042d,x
	adc #0
le93e:
	sta $042d,x
	lda $31,x
	and #3
	beq le951
	cpx #2
	bcs le951
	lda $f0
	ora #8
	sta $f0
le951:
	lda $7f,x
	cmp #4
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
	cmp #5
	bne le976
	lda $0448,x
	eor #1
	sta $0448,x
le976:
	lda #3
	sta $7f,x
	lda #0
	sta $0424,x
	sta $042d,x
le982:
	rts
//-----------------------

le983:
	lda $cb
	bne le9b6
	lda $bd,x
	beq le99a
	lda $0488
	beq le99a
	sec
	sbc $91,x
	jsr lf08e
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
//-----------------------

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
	lda #$40
	sta $f1
le9f2:
	rts
//-----------------------

le9f3:
	lda #0
	sta $7f,x
	lda #1
	sta $045a,x
	rts
//-----------------------

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
//-----------------------

lea18:
	cpx #2
	bcs lea2c
	lda $bd,x
	bne lea44
	lda $7f,x
	cmp #1
	beq lea3e
	cmp #3
	bne lea44
	beq lea3e
lea2c:
	lda $7f,x
	cmp #1
	beq lea3e
	cmp #3
	bcc lea44
	lda $19
	and #3
	bne lea47
	beq lea44
lea3e:
	lda $19
	and #7
	bne lea47
lea44:
	inc $0436,x
lea47:
	lda $0436,x
	and #3
	sta $0436,x
	bne lea57
	lda $7f,x
	bne lea57
	inc $7f,x
lea57:
	rts
//-----------------------

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
//-----------------------

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
	jsr leba0
	cmp #$f8
	bcs leb0d
	cmp #$e8
	bcc leb0d
	lda #$ff
	sta $88,x
	lda #4
	sta $bb
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
	jsr leb8e
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
	jsr lf107
	jmp lf18c
leb84:
	lda #2
	sta $88,x
	lda #3
	sta $0451,x
leb8d:
	rts
//-----------------------

leb8e:
	lda $0400,x
	clc
	adc $0424,x
	sta $0400,x
	lda $91,x
	adc $042d,x
	sta $91,x
	rts
//-----------------------

leba0:
	lda $0409,x
	clc
	adc $0412,x
	sta $0409,x
	lda $9a,x
	adc $041b,x
	sta $9a,x
	rts
//-----------------------

lebb2:
	jsr lf0b4
	jsr leb8e
	jmp lf0b4
lebbb:
	jsr lf0b4
	jsr leba0
	jmp lf0b4
lebc4:
	cpx #2
	bcs lebe3
	lda $88,x
	bne lebd6
	lda $0436,x
	bne lebd6
	lda #0
	sta $7f,x
	rts
//-----------------------

lebd6:
	lda $7f,x
	cmp #6
	bcc lec38
	lda #1
	sta $7f,x
	dec $88,x
	rts
//-----------------------

lebe3:
	lda $88,x
	cmp #2
	beq lec38
	lda $0436,x
	bne lebfd
	lda $88,x
	bne lebf7
	lda #0
	sta $7f,x
	rts
//-----------------------

lebf7:
	lda $7f,x
	bne lebfe
	inc $7f,x
lebfd:
	rts
//-----------------------

lebfe:
	cmp #2
	bcc lebfd
	dec $045a,x
	bne lec37
	lda $c7
	sta $045a,x
	inc $7f,x
	lda $7f,x
	cmp #7
	bcc lec37
	lda #2
	sta $88,x
	lda #0
	sta $7f,x
	ldy $0451,x
	lda lecae,y
	ldy $047e,x
	bne lec2f
	dec $047e,x
	lda $0451,x
	and #3
lec2f:
	sta $0451,x
	lda #$fe
	sta $041b,x
lec37:
	rts
//-----------------------

lec38:
	jsr le6e9
	lda $31,x
	and #$c3
	beq lec49
	cpx #2
	bcs lec49
	lda #0
	sta $bd,x
lec49:
	lda $31,x
	and #$40
	bne lec61
	lda $31,x
	and #$80
	bne lec5c
	lda #0
	sta $0620,x
	beq lecad
lec5c:
	lda $0620,x
	bne lecad
lec61:
	lda $7f,x
	cmp #2
	bcc lec75
	dec $9a,x
	dec $9a,x
	lda #0
	sta $0412,x
	sta $041b,x
	beq lec7e
lec75:
	cmp #1
	beq lec7e
	lda $0436,x
	bne lecad
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
//-----------------------

lecae:
    //12 bytes
db $01,$02,$02,$03,$01,$02,$02,$03,$01,$02,$02,$03
lecba:
	lda $7f,x
	bne led27
	jsr le7b1
	jsr leb8e
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
	jsr lf08e
	cmp #$18
	bcs led22
	lda $91,x
	sec
	sbc $0091,y
	jsr lf08e
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
	jsr ld6de
	lda #1
	ldx $3e
	jsr ld871
	pla
	tax
	rts
//-----------------------

led22:
	dey
	bpl lecdf
	pla
	tax
led27:
	rts
//-----------------------

led28:
	ldy $88,x
	dey
	bpl led2e
led2d:
	rts
//-----------------------

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
//-----------------------

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
	jsr lf107
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
	jsr lf0de
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
//-----------------------

lee25:
	ldx #7
lee27:
	stx $12
	ldy $12
	dey
	bpl lee31
lee2e:
	jmp lef2a
lee31:
	lda $88,x
	bmi lee2e
	beq lee2e
	lda $0088,y
	bmi lee2e
	beq lee2e
	lda #0
	sta $cc
	lda $009a,y
	sec
	sbc $9a,x
	jsr lf08e
	cmp #$18
	bcs leec0
	lda $9a,x
	clc
	adc #$18
	sta $12
	lda $009a,y
	clc
	adc #7
	sec
	sbc $12
	jsr lf08e
	cmp #4
	bcs lee6a
	lda #1
	bne lee7c
lee6a:
	lda $009a,y
	clc
	adc #$11
	sec
	sbc $9a,x
	jsr lf08e
	cmp #4
	bcs lee8f
	lda #2
lee7c:
	sta $cc
	lda $0091,y
	sec
	sbc $91,x
	jsr lf08e
	cmp #$10
	bcc lee8f
	lda #0
	sta $cc
lee8f:
	lda $91,x
	clc
	adc #$10
	sta $12
	lda $0091,y
	clc
	adc #7
	sec
	sbc $12
	jsr lf08e
	cmp #4
	bcs leeaa
	lda #4
	bne leebc
leeaa:
	lda $0091,y
	clc
	adc #9
	sec
	sbc $91,x
	jsr lf08e
	cmp #4
	bcs leec0
	lda #8
leebc:
	ora $cc
	sta $cc
leec0:
	lda #0
	sta $4b
	lsr $cc
	bcc leecd
	jsr lf0a6
	bmi leed6
leecd:
	lsr $cc
	bcc leef1
	jsr lf0a6
	bmi leef1
leed6:
	jsr lf0bd
	bcs leeed
	jsr lf107
	jsr lf18c
	jsr lf0b4
	jsr lf107
	jsr lf18c
	jsr lf0b4
leeed:
	lda #1
	sta $4b
leef1:
	lsr $cc
	bcc leefa
	jsr lf098
	bmi lef03
leefa:
	lsr $cc
	bcc lef1e
	jsr lf098
	bmi lef1e
lef03:
	jsr lf0bd
	bcs lef1a
	jsr lf0de
	jsr lf172
	jsr lf0b4
	jsr lf0de
	jsr lf172
	jsr lf0b4
lef1a:
	lda #1
	sta $4b
lef1e:
	jsr lef37
	jsr lf0b4
	jsr lef37
	jsr lf0b4
lef2a:
	dey
	bmi lef30
	jmp lee31
lef30:
	dex
	bmi lef36
	jmp lee27
lef36:
	rts
//-----------------------

lef37:
	cpx #2
	bcc lef42
	cpy #2
	bcc lef42
	jmp lf043
lef42:
	lda #0
	sta $0487
	lda $0475,x
	beq lef4f
	jmp lf043
lef4f:
	lda $4b
	bne lef56
	jmp lf043
lef56:
	cpx #2
	bcs lef61
	lda $bd,x
	beq lef72
	jmp lf043
lef61:
	lda $88,x
	cmp #1
	bne lef72
	lda $7f,x
	cmp #2
	bcs lef7f
	lda #1
	sta $0487
lef72:
	lda $009a,y
	clc
	adc #4
	cmp $9a,x
	bcc lef7f
	jmp lf043
lef7f:
	lda #$14
	sta $0475,x
	lda #0
	sta $0436,x
	cpy #2
	bcc lef97
	lda $0088,y
	cmp #2
	beq lef97
	jmp lf043
lef97:
	lda $f0
	ora #2
	sta $f0
	lda $88,x
	cmp #2
	bne lefc0
	cpx #2
	bcs lefc0
	sty $12
	ldy $7f,x
	lda lf053,y
	ldy $12
	pha
	pla
	bne lefb7
	jmp lf043
lefb7:
	sta $7f,x
	lda #0
	sta $0436,x
	beq lefea
lefc0:
	dec $88,x
	bne lefce
	lda #$ff
	sta $041b,x
	lda #0
	sta $0412,x
lefce:
	lda #0
	sta $7f,x
	sta $0424,x
	sta $042d,x
	lda $91,x
	bmi lefe0
	lda #$ff
	bne lefe2
lefe0:
	lda #0
lefe2:
	sta $046c,x
	lda #$80
	sta $0463,x
lefea:
	sty $12
	ldy $0451,x
	lda lf05e,y
	sta $0451,x
	lda #1
	sta $047e,x
	ldy $12
	cpy #2
	bcs lf043
	lda $0451,x
	cmp #7
	beq lf011
	cmp #8
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
	jsr ld6de
	pla
	tax
	pla
	tay
lf043:
	lda $0451,x
	cmp #$0b
	bne lf052
	lda $c8
	bne lf052
	lda #$20
	sta $f0
lf052:
	rts
//-----------------------

lf053:
    //11 bytes
db $06,$06,$07,$08,$09,$0a,$00,$00,$00,$00,$00
lf05e:
    //12 bytes
db $04,$05,$06,$07,$08,$09,$0a,$0b,$08,$09,$0a,$0b
lf06a:
    //12 bytes
db $00,$00,$00,$00,$32,$4b,$64,$64,$4b,$64,$96,$64
lf076:
    //12 bytes
db $00,$00,$00,$00,$32,$4b,$64,$64,$64,$96,$c8,$64
lf082:
    //12 bytes
db $00,$00,$00,$00,$01,$02,$03,$03,$02,$03,$04,$03
lf08e:
	pha
	pla
	bpl lf097
	eor #$ff
	clc
	adc #1
lf097:
	rts
//-----------------------

lf098:
	lda $0424,y
	sec
	sbc $0424,x
	lda $042d,y
	sbc $042d,x
	rts
//-----------------------

lf0a6:
	lda $0412,y
	sec
	sbc $0412,x
	lda $041b,y
	sbc $041b,x
	rts
//-----------------------

lf0b4:
	stx $12
	sty $13
	ldx $13
	ldy $12
	rts
//-----------------------

lf0bd:
	cpx #2
	bcc lf0dd
	lda $7f,x
	cmp #2
	bcc lf0dd
	lda #1
	cmp $88,x
	bcs lf0dd
	cpy #2
	bcc lf0dd
	lda $007f,y
	cmp #2
	bcc lf0dd
	lda #1
	cmp $0088,y
lf0dd:
	rts
//-----------------------

lf0de:
	lda #0
	sec
	sbc $0424,x
	sta $0424,x
	lda #0
	sbc $042d,x
	sta $042d,x
	lda #0
	sec
	sbc $0463,x
	sta $0463,x
	lda #0
	sbc $046c,x
	sta $046c,x
	lda $31,x
	and #$40
	sta $31,x
	rts
//-----------------------

lf107:
	lda #0
	sec
	sbc $0412,x
	sta $0412,x
	lda #0
	sbc $041b,x
	sta $041b,x
	rts
//-----------------------

lf119:
	sta $2d
	lda $2c
	bpl lf143
	lda #0
	sec
	sbc $2b
	sta $2b
	lda #0
	sbc $2c
	sta $2c
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
//-----------------------

lf143:
	txa
	pha
	lda #0
	sta $2e
	sta $2f
	sta $30
	ldx #8
lf14f:
	asl $2e
	rol $2f
	rol $30
	asl $2d
	bcc lf16c
	clc
	lda $2b
	adc $2e
	sta $2e
	lda $2c
	adc $2f
	sta $2f
	lda #0
	adc $30
	sta $30
lf16c:
	dex
	bne lf14f
	pla
	tax
	rts
//-----------------------

lf172:
	lda $0424,x
	sta $2b
	lda $042d,x
	sta $2c
	lda #$cd
	jsr lf119
	lda $2f
	sta $0424,x
	lda $30
	sta $042d,x
	rts
//-----------------------

lf18c:
	lda $0412,x
	sta $2b
	lda $041b,x
	sta $2c
	lda #$cd
	jsr lf119
	lda $2f
	sta $0412,x
	lda $30
	sta $041b,x
	rts
//-----------------------

lf1a6:
	ldy #4
lf1a8:
	lda $13
	asl
	ror $13
	ror $12
	dey
	bne lf1a8
	rts
//-----------------------

lf1b3:
	txa
	pha
	ldx #$0b
lf1b7:
	asl $1b
	rol $1c
	rol
	rol
	eor $1b
	rol
	eor $1b
	lsr
	lsr
	eor #$ff
	and #1
	ora $1b
	sta $1b
	dex
	bne lf1b7
	pla
	tax
	lda $1b
	rts
//-----------------------

lf1d4:
	jsr ldac1_titlescreenloop
	ldx #$09	// \
lf1d9:
	lda #$00	// | Player 1 Score to 000000
	sta $03,x	// |
	dex			// |
	bpl lf1d9	// /
	sta $3e
	inc $41
	jsr ld6de
	lda #$0f	// \ Enable Sound Channels
	sta $4015	// /
	lda #$01	// \ Stop All Sounds
	sta $f0		// /
	lda #$02
lf1f2:
	sta $41		// Set Player 1 Lives to 2
	ldy $40		// \ If it's 2 players
	bne lf1fa	// | Then give lives to Player 2
	lda #$ff	// / Else no lives
lf1fa:
	sta $42		// Set Player 2 Lives to -1 or 2
	ldx #$00
	stx $0488
	stx $3b		// Current Level Header = 0
	stx $3c		// Current Phase = 0
	stx $0558	// Bonus Phase Level = 0
	dex
	stx $89		// Set Player 2 Balloons to -1
	ldx $40						// \
lf20d:
	jsr lf3b0_initplayertype	// | Set up both player types
	dex							// |
	bpl lf20d					// /
lf213:
	lda #$00	// \ Set to Regular Phase
	sta $c8		// /
	lda $3c		// \
	lsr			// |
	lsr			// | (Current Phase >> 2) cannot
	cmp #$08	// | be higher than $08
	bcc lf221	// |
	lda #$08	// /
lf221:
	tax
	lda lf3ba,x
	sta $c6
	lda lf3c3,x
	sta $c7
	lda $3c
	cmp #$02
	bcs lf238
	lda #$03
	sta $c6
	sta $c7
lf238:
	ldx #$07	// \
lf23a:
	lda #$00	// | Initialize variables for each object
	sta $0448,x	// | - Direction (0 = Left, 1 = Right)
	sta $0475,x	// |
	sta $047e,x	// |
	sta $0424,x	// | - X Velocity (Frac)
	sta $042d,x	// | - X Velocity (Int)
	sta $0412,x	// | - Y Velocity (Frac)
	sta $041b,x	// | - Y Velocity (Int)
	sta $0463,x	// |
	sta $046c,x	// |
	sta $0400,x	// | - X Coordinates (Frac)
	sta $0409,x	// | - Y Coordinates (Frac)
	lda #$01	// |
	sta $043f,x	// |
	sta $045a,x	// |
	lda #$03	// |
	sta $0436,x	// | - Animation Frame
	dex			// |
	bpl lf23a	// /
	ldx #$05	// \
lf26f:
	lda #$ff	// | Initialize Enemies
	sta $8a,x	// |
	dex			// |
	bpl lf26f	// /
	ldx $40						// \
lf278:
	jsr lf386_initializeplayerx	// | Initialize Players
	dex							// |
	bpl lf278					// /
	jsr ld246_clearppu
	jsr ld293
	lda $c6
	cmp #$10
	bcs lf28e
	lda #$58
	sta $c6
lf28e:
	jsr lf4a5
	jsr ld8ff
	lda $16
	beq lf29b
	jmp lc1c5
lf29b:
	lda $c8
	beq lf2a2
	jmp lcf13
lf2a2:
	jsr lc716
	lda $3b
	and #3
	bne lf2b3
	lda #8
	sta $f2
	ldx $3a
	bne lf2b9
lf2b3:
	lda #$ff
	sta $3d
	inc $3c
lf2b9:
	jsr lf470
	lda $3d
	beq lf2c5
	dec $3d
	jsr lf3cc
lf2c5:
	jsr lf1b3
	jsr le691
	jsr lc6f9
	jsr lc790
	jsr lc831
	jsr lc8b7
	jsr ld8dd
	jsr le587
	jsr lcb74
	inc $4c
	ldx $40
lf2e4:
	lda $88,x
	bpl lf30d
	lda $3a
	bne lf326
	lda $41,x
	bmi lf30d
	dec $c3,x
	bne lf327
	txa
	pha
	jsr lc726
	pla
	tax
	ldy #2
	dec $41,x
	sty $46
	bmi lf30d
	jsr lf386_initializeplayerx
	jsr lf3b0_initplayertype
	lda #$80
	sta $f2
lf30d:
	dex
	bpl lf2e4
	lda $41
	bpl lf318
	lda $42
	bmi lf366
lf318:
	lda $3a
	beq lf327
	jsr le768_polljoypad0
	lda $061c
	and #$30
	beq lf2b9
lf326:
	rts
//-----------------------

lf327:
	ldx #5
lf329:
	lda $8a,x
	beq lf32f
	bpl lf2b9
lf32f:
	dex
	bpl lf329
	lda $bb
	bpl lf2b9
	ldx $40
lf338:
	ldy $88,x
	dey
	bpl lf34c
	lda $41,x
	bmi lf34c
	lda #$ff
	sta $88,x
	lda #1
	sta $c3,x
	jmp lf2b9
lf34c:
	dex
	bpl lf338
	lda #2
	sta $f2
lf353:
	ldx #$96
	jsr lf45e
	ldx $3b
	inx
	cpx #$10
	bne lf361
	ldx #4
lf361:
	stx $3b
	jmp lf213
lf366:
	lda #1
	sta $f2
lf36a:
	lda #0
	sta $17
	sta $18
	sta $15
	jsr lf40b
lf375:
	jsr lf465_clearframeflag
	jsr le768_polljoypad0
	and #$30
	bne lf383
	dec $15
	bne lf375
lf383:
	jmp lf1d4

lf386_initializeplayerx:
	lda $41,x	// \ If Player X has negative lives
	bmi lf3ad	// / Then don't do anything
	lda lf3ae,x	// \ Set up X coordinate for Player X
	sta $91,x	// /
	lda #$b8	// \ Set up Y coordinate for Player X
	sta $9a,x	// /
	sta $bd,x	// Set up invincibility for Player X
	lda #$c8	// \ Set up invincibility time
	sta $bf,x	// / for Player X
	lda #$5a	// \
	ldy $41,x	// | If Player X has lives
	bpl lf3a1	// | Then set respawn delay to $5A
	lda #$01	// | Else set respawn delay to $01
lf3a1:
	sta $c3,x	// /
	lda #$00
	sta $c1,x	// Clear Player X Freeze Flag
	sta $042d,x	// \ Set up Player X's X Velocity to $00
	sta $0424,x // /
lf3ad:
	rts
//-----------------------

lf3ae:
db $20,$d0

lf3b0_initplayertype:
	lda #$03	// \ Set Player X type to 03 (2 Balloons)
	sta $0451,x	// /
	lda #$02	// \ Set Player X Balloons to 02
	sta $88,x	// /
	rts
//-----------------------

lf3ba:
    //9 bytes
db $58,$50,$58,$50,$50,$40,$38,$30,$28
lf3c3:
    //9 bytes
db $04,$04,$03,$03,$02,$02,$02,$02,$02
lf3cc:
	lda $3d
	and #$20
	beq lf3ee
	ldx #$0a
lf3d4:
	lda lf3f5,x
	sta $57,x
	dex
	bpl lf3d4
	ldy #$0a
	lda $3c
	sta $43
	jsr ld77c
	sta $60
	lda $43
	sta $61
	jmp lc12d
lf3ee:
	lda #0
	ldy #$f4
	jmp lc131
lf3f5:
    //22 bytes
db $20,$6c,$08,$19,$11,$0a,$1c,$0e,$25,$00,$00,$20,$6c,$08,$24
db $24,$24,$24,$24,$24,$24,$24
lf40b:
	jsr lf465_clearframeflag
	ldx #1
lf410:
	lda lf43b,x
	ldy lf43b+2,x
	jsr lc131
	dex
	bpl lf410
	ldx #$0f
lf41e:
	lda #$24
	sta $5a,x
	dex
	bpl lf41e
	lda #$10
	sta $59
	lda #$21
	sta $57
	ldx #2
lf42f:
	lda lf43f,x
	sta $58
	jsr lc12d
	dex
	bpl lf42f
	rts
//-----------------------

lf43b:
db $42,$55,$f4,$f4
lf43f:
    //29 bytes
db $88,$a8,$e8,$21,$c8,$10,$24,$24,$24,$10,$0a,$16,$0e,$24,$24
db $18,$1f,$0e,$1b,$24,$24,$24,$23,$da,$04,$aa,$aa,$aa,$aa
lf45c:
	ldx #$14
lf45e:
	jsr lf465_clearframeflag
	dex
	bne lf45e
	rts
//-----------------------

lf465_clearframeflag:
	lda #$00
	sta $02
lf469:
	lda $02
	beq lf469
	dec $02
lf46f:
	rts
//-----------------------

lf470:
	jsr lf469
	lda $3a
	bne lf46f
	jsr le768_polljoypad0
	and #$10
	beq lf46f
	lda #4
	sta $f2
	lda $01
	and #$ef
	sta $2001
lf489:
	jsr lf465_clearframeflag
	jsr le768_polljoypad0
	and #$10
	beq lf489
	lda $01
	sta $2001
	ldy #4
	lda $c8
	ora $16
	beq lf4a2
	ldy #$20
lf4a2:
	sty $f2
	rts
//-----------------------

lf4a5:
	lda #1
	sta $048e
	sta $048f
	lda #$ff
	sta $bb
	sta $87
	sta $048c
	ldx #1
	stx $0459
	stx $90
	inx
	stx $46
	lda #$40
	sta $99
	rts
//-----------------------

    //59 bytes
db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
lf500:
	jsr lf78f
lf503:
	rts
//-----------------------

lf504:
	lda #0
	tax
	sta $fd
	beq lf51b
lf50b:
	txa
	lsr
	tax
lf50e:
	inx
	txa
	cmp #4
	beq lf503
	lda $fd
	clc
	adc #4
	sta $fd
lf51b:
	txa
	asl
	tax
	lda $e0,x
	sta $fe
	lda $e1,x
	sta $ff
	lda $e1,x
	beq lf50b
	txa
	lsr
	tax
	dec $d0,x
	bne lf50e
lf531:
	ldy $e8,x
	inc $e8,x
	lda ($fe),y
	beq lf500
	tay
	cmp #$ff
	beq lf547
	and #$c0
	cmp #$c0
	beq lf553
	jmp lf561
lf547:
	lda $d8,x
	beq lf55e
	dec $d8,x
	lda $ec,x
	sta $e8,x
	bne lf55e
lf553:
	tya
	and #$3f
	sta $d8,x
	dec $d8,x
	lda $e8,x
	sta $ec,x
lf55e:
	jmp lf531
lf561:
	tya
	bpl lf57b
	and #$0f
	clc
	adc $df
	tay
	lda lf660,y
	sta $d4,x
	tay
	txa
	cmp #2
	beq lf5c4
lf575:
	ldy $e8,x
	inc $e8,x
	lda ($fe),y
lf57b:
	tay
	txa
	cmp #3
	beq lf5e1
	pha
	tax
	cmp #1
	beq lf5b9
lf587:
	ldx $fd
	lda lf601,y
	beq lf599
	sta $4002,x
	lda lf600,y
	ora #8
	sta $4003,x
lf599:
	tay
	pla
	tax
	tya
	bne lf5aa
	ldy #0
	txa
	cmp #2
	beq lf5ac
	ldy #$10
	bne lf5ac
lf5aa:
	ldy $dc,x
lf5ac:
	tya
	ldy $fd
	sta $4000,y
lf5b2:
	lda $d4,x
	sta $d0,x
	jmp lf50e
lf5b9:
	lda $f5
	and #2
	beq lf587
	pla
	tax
	jmp lf5b2
lf5c4:
	tya
	ldy $07f0
	beq lf5ce
	lda #$ff
	bne lf5d9
lf5ce:
	clc
	adc #$fe
	asl
	asl
	cmp #$3c
	bcc lf5d9
	lda #$3c
lf5d9:
	sta $4008
	sta $de
	jmp lf575
lf5e1:
	lda $f4
	cmp #2
	beq lf5f9
	lda lf700,y
	sta $400c
	lda lf700+1,y
	sta $400e
	lda lf700+2,y
	sta $400f
lf5f9:
	jmp lf5b2
db $16,$ff,$10,$c5
lf600:
db $07
lf601:
    //95 bytes
db $f0,$00,$00,$00,$d4,$00,$c8,$00,$bd,$00,$b2,$00,$a8,$00,$9f
db $00,$96,$00,$8d,$00,$85,$00,$7e,$00,$76,$00,$70,$01,$ab,$01
db $93,$01,$7c,$01,$67,$01,$52,$01,$3f,$01,$2d,$01,$1c,$01,$0c
db $00,$fd,$00,$ee,$00,$e1,$03,$57,$03,$27,$02,$f9,$02,$cf,$02
db $a6,$02,$80,$02,$5c,$02,$3a,$02,$1a,$01,$fc,$01,$df,$01,$c4
db $03,$f8,$00,$69,$00,$63,$00,$5e,$00,$58,$00,$53,$00,$4f,$00
db $4a,$00,$46,$00,$42
lf660:
    //35 bytes
db $03,$06,$0c,$18,$30,$12,$24,$09,$08,$04,$07,$01,$04,$08,$10
db $20,$40,$18,$30,$0c,$01,$06,$0c,$18,$30,$60,$24,$48,$12,$10
db $08,$0e,$02,$03,$04
lf683:
	lda #0
	beq lf691
lf687:
	lda #8
	bne lf691
lf68b:
	lda #$0c
	bne lf691
lf68f:
	lda #4
lf691:
	sta $f9
	lda #$40
	sta $fa
	stx $fb
	sty $fc
	ldy #0
lf69d:
	lda ($fb),y
	sta ($f9),y
	iny
	tya
	cmp #4
	bne lf69d
	rts
//-----------------------

lf6a8_loadsndseq:
	tax			// \
	jsr lf798	// | Initialize Sound Channels
	stx $f6		// / and Sound Variables
	lda $07f5	// \
	beq lf6be	// | Check [$07F5] == $00 or $02
	cmp #$02	// |
	bne lf6be	// /
	sta $f0		// [$00F0] = [$07F5] (!= 00 or 02)
	lda #$00	// \
	sta $07f5	// / Clear [$07F5]
lf6be:
	lda lfbca,y	// \ Load Sound Sequence Pointer to Y
	tay			// /
	ldx #$00	// \
lf6c4:
	lda lfbca,y	// |
	sta $df,x	// | Load Sound Sequence Header
	iny			// | (9 bytes)
	inx			// |
	txa			// |
	cmp #$09	// |
	bne lf6c4	// /

	lda #$01	// \
	sta $d0		// |
	sta $d1		// |
	sta $d2		// | Initialize Sequence stuff
	sta $d3		// |
	lda #$00	// |
	sta $e8		// |
	sta $e9		// |
	sta $ea		// |
	sta $eb		// /
	rts
//-----------------------

    //27 bytes
db $94,$ab,$fd,$58,$00,$7f,$04,$18,$3f,$7f,$00,$00,$06,$7f,$0a
db $c0,$08,$7f,$05,$c0,$c1,$89,$02,$0f,$ff,$ff,$ff
lf700:
    //18 bytes
db $10,$00,$18,$10,$01,$18,$00,$01,$88,$02,$02,$40,$03,$05,$40
db $04,$07,$40
lf712:
	lda #$7f	// \ Set Pulse Channels:
	sta $4001	// | No Sweep
	sta $4005	// /
lf71a:
	stx $dc
	sty $dd
	rts
//-----------------------

lf71f:
	ldx #$e5
	ldy #$f6
	bne lf745
lf725:
	lda $f3
	lsr
	bcs lf736
	lda $f7
	lsr
	bcs lf749
	lda $f0
	and #$10
	bne lf71f
	rts
//-----------------------

lf736:
	lda $f7
	ora #1
	sta $f7
	lda #0
	sta $07e4
	ldx #$fc
	ldy #$f5
lf745:
	jsr lf683
	rts
//-----------------------

lf749:
	inc $07e4
	lda $07e4
	cmp #$58
	bne lf77b
	lda #0
	sta $f7
	rts
//-----------------------

lf758:
	lda #$c0	// \ Set Frame Counter
	sta $4017	// / to 4-step sequence, clear frame interrupt flag
	jsr lfb25_music
	jsr lf90a
	jsr lfa38
	jsr lfaac
	jsr lf824
	lda $f1
	sta $07e9
	lda #$00	// \
	sta $f0		// |
	sta $f1		// | Clear Music/SFX Flags
	sta $f2		// |
	sta $f3		// /
lf77b:
	rts
//-----------------------

lf77c:
	lda $f4
	and #6
	bne lf77b
	lda $f4
	and #$f0
	sta $f4
	ldx #$e9
	ldy #$f6
	jmp lf879
lf78f:
	lda $f6
	cmp #$20
	bne lf79f
	inc $07e8
lf798:
	and #$0f	// \ Initialize Sound Channels
	cmp #$0f	// | differently depending on
	bne lf7a5	// / Music/Jingle needs
	txa
lf79f:
	lda $f4
	and #$20
	bne lf7c7
lf7a5:
	lda #$10	// \ Constant volume on:
	sta $400c	// | - Noise Channel
	sta $4000	// | - Pulse 1 Channel
	sta $4004	// / - Pulse 2 Channel
	lda #$00
	sta $f4
lf7b4:
	sta $f5
	sta $f6		// Clear Current Music/Jingle
	sta $07fa
	sta $f7
	sta $4008	// Clear Triangle Channel Linear Counter
	sta $4011	// Clear DMC Channel Load Counter
	sta $07f0
	rts
//-----------------------

lf7c7:
	lda #$10	// \ Constant volume on:
	sta $4004	// | - Pulse 2 Channel
	sta $400c	// / - Noise Channel
	lda #$00
	beq lf7b4
lf7d3:
	lda #$00
	sta $07fa
	rts
//-----------------------

lf7d9:
	ldx #$f5
	ldy #$f6
	jmp lf879
lf7e0:
	inc $07fb
	lda $07fb
	cmp #$10
	beq lf7d9
	cmp #$20
	beq lf7d3
	rts
//-----------------------

lf7ef:
	lda #0
	sta $07fb
	lda #$f0
	sta $07fa
	ldx #$f1
	ldy #$f6
	jmp lf879
lf800:
	lda $f4
	and #$f0
	ora #2
	sta $f4
	lda #0
	sta $07f7
	ldx #$f1
	ldy #$f6
	jmp lf879
lf814:
	inc $07f7
	lda $07f7
	cmp #$10
	bne lf85b
	jmp lf89b
lf821:
	jmp lf7a5
lf824:
	lda $f0
	lsr
	bcs lf821
	lda $f6
	cmp #$df
	beq lf83b
	cmp #$7f
	beq lf83b
	cmp #$20
	beq lf83b
	lda $f6
	bne lf85b
lf83b:
	lda $07fa
	cmp #$0f
	beq lf7ef
	cmp #$f0
	beq lf7e0
	lda $f0
	lsr
	lsr
	bcs lf800
	lsr
	bcs lf85f
	lsr
	bcs lf85c
	lda $f4
	lsr
	lsr
	bcs lf814
	lsr
	bcs lf87d
lf85b:
	rts
//-----------------------

lf85c:
	jmp lf77c
lf85f:
	lda $f4
	and #$80
	bne lf85b
	lda $f4
	and #$f0
	ora #4
	sta $f4
	lda #0
	sta $07f3
	sta $07f1
	ldx #$ed
	ldy #$f6
lf879:
	jsr lf68b
	rts
//-----------------------

lf87d:
	inc $07f3
	lda $07f3
	cmp #3
	bne lf8a1
	lda #0
	sta $07f3
	inc $07f1
	lda $07f1
	cmp #$10
	bne lf8a2
	lda #$10
	sta $400c
lf89b:
	lda $f4
	and #$f0
	sta $f4
lf8a1:
	rts
//-----------------------

lf8a2:
	sta $400e
	rts
//-----------------------

lf8a6:
	lda #0
	sta $07e0
	clc
	lda $1b
	and #7
	adc #2
	sta $07e1
	lda $f7
	and #0
	ora #$80
	sta $f7
	bne lf8e8
lf8bf:
	inc $07e0
	lda $07e0
	cmp $07e1
	bne lf8e8
lf8ca:
	lda #$10
	sta $4000
	sta $4004
	lda #0
	sta $f7
	lda $f4
	and #$0f
	sta $f4
	rts
//-----------------------

lf8dd:
	jsr lf7a5
	lda #$80
	sta $f4
	lda #2
	sta $f0
lf8e8:
	ldx #$f9
	ldy #$f6
	jsr lf683
	lda $1b
	and #$0f
	sta $4002
	ldx #$f9
	ldy #$f6
	jsr lf68f
	lda $1b
	lsr
	lsr
	and #$0f
	sta $4006
	rts
//-----------------------

lf907:
	jmp lf8a6

lf90a:
	lda $f6		// \ Check if music is not playing
	beq lf91b	// / If not playing then continue as normal
	cmp #$df	// \ Songs #$DF?
	beq lf91b	// / Wouldn't that be redundant?
	lda $f0		// \
	and #$e0	// | Check for sound effects that stops the music
	beq lf94e	// / if found, then return
	jsr lf7a5
lf91b:
	lda $f0
	asl
	bcs lf8dd
	asl
	bcs lf952
	asl
	bcs lf965
	lda $f4
	asl
	bcs lf8e8
	lda $f4
	and #$e0
	bne lf94e
	lda $f6
	cmp #$df
	beq lf94b
	lda $f6
	bne lf94e
	lda $f3
	asl
	bcs lf907
	asl
	bcs lf977
	lda $f7
	asl
	bcs lf94f
	asl
	bcs lf993
lf94b:
	jsr lf725
lf94e:
	rts
//-----------------------

lf94f:
	jmp lf8bf
lf952:
	lda #$0f
	sta $07fa
	lda $f4
	and #$0f
	ora #$40
	sta $f4
	ldx #$d1
	ldy #$f9
	bne lf98f
lf965:
	lda #2
	sta $f0
	lda $f4
	and #$0f
	ora #$20
	sta $f4
	ldx #$cd
	ldy #$f9
	bne lf98f
lf977:
	lda #0
	sta $07fc
	lda $f7
	and #0
	ora #$40
	sta $f7
	ldx #$d5
	ldy #$f9
	jsr lf68f
	ldx #$d9
	ldy #$f9
lf98f:
	jsr lf683
	rts
//-----------------------

lf993:
	inc $07fc
	lda $07fc
	cmp #$12
	beq lf9ca
	cmp #6
	bcc lf9b1
	lda $1b
	ora #$10
	and #$7f
	sta $07fe
	rol
	sta $07fd
	jmp lf9bd
lf9b1:
	inc $07fd
	inc $07fd
	inc $07fe
	inc $07fe
lf9bd:
	lda $07fd
	sta $4006
	lda $07fe
	sta $4002
	rts
//-----------------------

lf9ca:
	jmp lf8ca
    //28 bytes
db $b8,$d5,$20,$00,$9f,$93,$80,$22,$3f,$ba,$e0,$06,$3f,$bb,$ce
db $06,$b8,$93,$50,$02,$80,$7f,$60,$68,$80,$7f,$62,$68
lf9e9:
	lda $f5
	and #2
	bne lfa26
	lda $f6
	cmp #$df
	beq lf9f9
	lda $f6
	bne lfa26
lf9f9:
	lda #0
	sta $07f9
	lda $f5
	and #$e0
	ora #2
	sta $f5
	ldx #$dd
	ldy #$f9
	bne lfa7f
lfa0c:
	inc $07f9
	lda $07f9
	cmp #7
	bne lfa26
	lda #$7f
	sta $4005
	lda #$10
	sta $4004
	lda $f5
	and #$e0
	sta $f5
lfa26:
	rts
//-----------------------

lfa27:
	jsr lf7a5
	ldx #$e1
	ldy #$f9
	jsr lf683
	ldx #$e5
	ldy #$f9
	jmp lfa7f
lfa38:
	lda $f6
	beq lfa42
	and #$0f
	cmp #$0f
	bne lfa63
lfa42:
	lda $f4
	and #$80
	bne lfa63
	lda $f7
	and #$c0
	bne lfa63
	lda $f1
	lsr
	bcs lfa27
	lsr
	bcs lf9e9
	lsr
	bcs lfa83
	lsr
	lsr
	bcs lfa64
	lda $f5
	lsr
	lsr
	bcs lfa0c
lfa63:
	rts
//-----------------------

lfa64:
	lda $f6
	bne lfa63
	lda $f5
	and #2
	bne lfa63
	ldx #$8a
	ldy #$fa
	jsr lf68f
	lda $1b
	and #$3f
	ora #$10
	sta $4006
	rts
//-----------------------

lfa7f:
	jsr lf68f
	rts
//-----------------------

lfa83:
	ldy #$0a
	lda #$ef
	jmp lfba5
    //12 bytes
db $d9,$86,$a8,$48,$08,$7f,$40,$28,$08,$7f,$45,$28
lfa96:
	inc $07f6
	lda $07f6
	cmp #4
	bne lfad8
	lda $f5
	and #$1f
	sta $f5
	ldx #$92
	ldy #$fa
	bne lfb00
lfaac:
	lda $f6
	beq lfaba
	cmp #8
	beq lfaba
	and #$0f
	cmp #$0f
	bne lfad8
lfaba:
	lda $f4
	and #$80
	bne lfad8
	lda $f1
	asl
	bcs lfb17
	asl
	bcs lfae2
	lda $f5
	asl
	asl
	bcs lfa96
	lda $f1
	and #$20
	beq lfad9
	lda $f6
	beq lfb04
lfad8:
	rts
//-----------------------

lfad9:
	lda $f6
	cmp #$df
	bne lfad8
	jmp lf79f
lfae2:
	lda $f5
	and #$1f
	ora #$40
	sta $f5
	lda #0
	sta $4008
	sta $f6
	sta $07f6
	lda #$10
	sta $4004
	sta $400c
	ldx #$8e
	ldy #$fa
lfb00:
	jsr lf687
	rts
//-----------------------

lfb04:
	lda $07e9
	and #$20
	bne lfb10
	lda #$02
	sta $07f5
lfb10:
	ldy #$08
	lda #$df
	jmp lfba5

lfb17:
	ldy #$04
	lda #$7f
	jmp lfba5

lfb1e:		// Music/Jingle: Stage Clear
	ldy #$00
	lda #$02
	jmp lfbc1

lfb25_music:
	lda $07e8	// \ Play Balloon Trip Music
	bne lfb5e	// /
	lda $f2		// \ Play Music/Jingle:
	lsr			// |
	bcs lfb82	// | #$01 = Game Over
	lsr			// |
	bcs lfb1e	// | #$02 = Stage Clear
	lsr			// |
	bcs lfb4c	// | #$04 = Pause
	lsr			// |
	bcs lfb7c	// | #$08 = Stage Start
	lsr			// |
	bcs lfb69	// | #$10 = Bonus Game Perfect
	lsr			// |
	bcs lfb5e	// | #$20 = Balloon Trip / Bonus Game Music
	lsr			// |
	bcs lfb58	// | #$40 = Fish
	lsr			// |
	bcs lfb52	// / #$80 = Respawn
	lda $f6		// \
	bne lfb49	// / Current Music/Jingle
	rts
//-----------------------

lfb49:
	jmp lf504

lfb4c:		// Music/Jingle: Pause
	ldy #$02
	lda #$04
	bne lfba5
lfb52:		// Music/Jingle: Respawn
	ldy #$09
	lda #$80
	bne lfb6d
lfb58:		// Music/Jingle: Fish
	ldy #$07
	lda #$40
	bne lfb6d
lfb5e:		// Music/Jingle: Balloon Trip / Bonus Game
	lda #$00
	sta $07e8
	ldy #$06
	lda #$20
	bne lfbc1
lfb69:		// Music/Jingle: Bonus Game Perfect
	ldy #$05
	lda #$10
lfb6d:
	jsr lf6a8_loadsndseq
	ldx #$fc
	ldy #$fc
	jsr lf712
	inc $07f0
	bne lfb49
lfb7c:		// Music/Jingle: Stage Start
	ldy #$03
	lda #$08
	bne lfb86
lfb82:		// Music/Jingle: Game Over
	ldy #$01
	lda #$01
lfb86:
	jsr lf6a8_loadsndseq
	ldx #$80
	ldy #$80
lfb8d:
	jsr lf71a
	lda #$83	// \ Pulse 1 Channel:
	sta $4001	// / Sweep, Shift = 3
	lda #$7f	// \ Pulse 2 Channel:
	sta $4005	// / No Sweep
	bne lfbaf
	jsr lf6a8_loadsndseq
	ldx #$04
	ldy #$04
	bne lfbac
lfba5:
	jsr lf6a8_loadsndseq
	ldx #$80
	ldy #$80
lfbac:
	jsr lf712
lfbaf:
	lda #$00
	sta $07f0
	lda $f4
	and #$20
	beq lfb49
	lda #$d5
	sta $4001
	bne lfb49
lfbc1:
	jsr lf6a8_loadsndseq
	ldx #$80
	ldy #$ba
	bne lfb8d
lfbca:
    //1069 bytes
db $0b,$14,$1d,$26,$2f,$38,$41,$4a,$53,$5c,$65,$0c,$02,$ff,$0b
db $ff,$1e,$ff,$31,$ff,$15,$18,$fe,$2a,$fe,$65,$fe,$86,$fe,$0c
db $0d,$fe,$00,$00,$13,$fe,$00,$00,$15,$38,$ff,$5a,$ff,$79,$ff
db $94,$ff,$00,$00,$00,$d7,$fe,$ed,$fe,$00,$00,$00,$b3,$ff,$c9
db $ff,$da,$ff,$ef,$ff,$15,$a5,$fc,$0a,$fd,$98,$fd,$e0,$fd,$15
db $b2,$fe,$00,$00,$c5,$fe,$00,$00,$15,$00,$00,$92,$fe,$a1,$fe
db $00,$00,$0c,$59,$fc,$72,$fc,$8c,$fc,$00,$00,$00,$00,$00,$38
db $fc,$49,$fc,$00,$00,$82,$02,$8b,$02,$80,$08,$02,$10,$02,$16
db $02,$52,$02,$02,$02,$1a,$00,$82,$02,$80,$10,$02,$16,$02,$52
db $02,$5a,$02,$02,$02,$56,$81,$02,$80,$12,$02,$0c,$02,$04,$02
db $0c,$02,$04,$02,$2a,$02,$81,$04,$02,$80,$04,$02,$81,$04,$88
db $02,$02,$00,$88,$02,$02,$80,$04,$02,$2a,$02,$24,$02,$2a,$02
db $24,$02,$1c,$02,$81,$22,$02,$80,$22,$02,$81,$24,$88,$02,$88
db $02,$80,$56,$02,$4e,$02,$12,$02,$4e,$02,$12,$02,$0c,$02,$81
db $10,$02,$80,$10,$02,$81,$12,$88,$02,$c3,$81,$02,$02,$1c,$02
db $02,$02,$1c,$1c,$ff,$c6,$88,$1c,$ff,$c7,$82,$4c,$4c,$2a,$4c
db $ff,$c6,$88,$1c,$ff,$c4,$81,$46,$02,$46,$02,$32,$02,$46,$80
db $2e,$2e,$ff,$c3,$82,$46,$46,$81,$32,$32,$46,$2e,$ff,$80,$0c
db $0c,$81,$46,$46,$46,$80,$04,$04,$81,$46,$46,$02,$c8,$82,$4c
db $4c,$2a,$4c,$ff,$c2,$81,$46,$80,$32,$32,$82,$46,$04,$81,$46
db $2a,$ff,$c2,$81,$0c,$0c,$80,$04,$04,$81,$04,$80,$2e,$2e,$81
db $2e,$82,$24,$ff,$00,$81,$32,$02,$02,$06,$0c,$32,$02,$02,$8a
db $2e,$8b,$02,$8a,$2e,$8b,$02,$8a,$2e,$8b,$02,$88,$2e,$32,$2e
db $d0,$8c,$2c,$24,$ff,$d0,$2e,$20,$ff,$c3,$80,$28,$02,$82,$02
db $80,$2c,$02,$32,$02,$24,$02,$82,$02,$81,$02,$80,$28,$02,$06
db $02,$28,$02,$81,$02,$80,$24,$02,$32,$02,$24,$02,$ff,$80,$28
db $02,$82,$02,$80,$2c,$02,$32,$02,$24,$02,$82,$02,$89,$0c,$0a
db $08,$06,$32,$30,$2e,$2c,$2a,$28,$26,$24,$02,$02,$02,$86,$02
db $c7,$84,$02,$ff,$c4,$80,$28,$02,$82,$02,$80,$2c,$02,$32,$02
db $24,$02,$82,$02,$81,$02,$80,$28,$02,$06,$02,$28,$02,$81,$02
db $80,$24,$02,$32,$02,$24,$02,$ff,$c8,$84,$02,$ff,$81,$14,$02
db $02,$14,$1a,$14,$02,$02,$88,$10,$10,$10,$10,$14,$10,$85,$3c
db $81,$44,$85,$4a,$81,$44,$88,$28,$24,$20,$46,$42,$40,$c6,$81
db $3c,$02,$02,$44,$02,$02,$02,$4a,$02,$46,$36,$36,$38,$38,$02
db $3a,$02,$80,$3c,$3c,$81,$02,$24,$02,$02,$2c,$24,$88,$24,$1e
db $46,$36,$38,$3a,$ff,$c4,$84,$02,$ff,$d8,$81,$06,$ff,$c6,$88
db $06,$ff,$c7,$81,$06,$06,$80,$06,$06,$81,$06,$06,$80,$06,$06
db $81,$06,$06,$ff,$c6,$88,$06,$ff,$e0,$81,$06,$06,$ff,$82,$0f
db $81,$06,$06,$ea,$06,$06,$06,$06,$ff,$c5,$80,$0e,$58,$ff,$00
db $c5,$80,$0e,$58,$ff,$82,$1c,$1c,$c3,$82,$1c,$1c,$81,$1c,$1c
db $1c,$02,$ff,$c7,$88,$1c,$ff,$00,$83,$02,$80,$0e,$02,$0e,$02
db $0c,$02,$0e,$02,$4e,$02,$02,$02,$0e,$02,$0c,$02,$02,$02,$0e
db $02,$0c,$02,$0e,$02,$4e,$02,$02,$02,$0e,$02,$0c,$02,$0e,$02
db $0e,$02,$0c,$02,$0e,$02,$4e,$02,$02,$02,$0e,$02,$0c,$02,$88
db $4e,$18,$16,$12,$0e,$0c,$0e,$83,$02,$81,$3e,$3e,$82,$46,$1c
db $46,$81,$02,$38,$3e,$02,$82,$46,$1c,$82,$48,$48,$81,$3e,$3e
db $82,$38,$88,$24,$20,$1c,$48,$46,$42,$3e,$82,$09,$09,$c6,$82
db $03,$0c,$ff,$c6,$88,$06,$ff,$ed,$89,$2a,$02,$04,$0c,$02,$04
db $08,$02,$30,$26,$02,$30,$ff,$80,$02,$ed,$89,$0c,$02,$12,$4e
db $02,$12,$18,$02,$0e,$08,$02,$0e,$ff,$80,$42,$02,$48,$02,$1e
db $02,$24,$02,$02,$02,$2a,$02,$c6,$8c,$30,$2a,$ff,$00,$80,$24
db $02,$2a,$02,$30,$02,$06,$02,$02,$02,$0c,$02,$c6,$8c,$12,$18
db $ff,$80,$56,$54,$52,$50,$81,$02,$80,$5e,$5a,$54,$50,$18,$14
db $10,$0a,$06,$30,$2c,$28,$02,$00,$80,$1a,$18,$16,$14,$81,$02
db $80,$02,$5e,$5a,$54,$50,$18,$14,$10,$0a,$06,$30,$2c,$28,$82
db $1c,$02,$1c,$02,$02,$1c,$1c,$00,$81,$10,$0a,$32,$28,$80,$32
db $02,$32,$02,$82,$32,$81,$06,$02,$06,$02,$82,$32,$81,$54,$1a
db $10,$0a,$80,$10,$02,$10,$02,$82,$10,$81,$16,$02,$16,$02,$82
db $0a,$83,$03,$0c,$82,$03,$0c,$0c,$c2,$88,$1c,$1c,$1c,$1c,$1c
db $1c,$83,$1c,$80,$04,$04,$2a,$02,$82,$1c,$ff,$81,$4c,$02,$4c
db $02,$2a,$02,$4c,$1c,$81,$4c,$02,$4c,$02,$4c,$00,$88,$2e,$2e
db $2e,$30,$04,$30,$c4,$80,$2e,$04,$ff,$83,$02,$88,$2e,$2e,$2e
db $30,$04,$30,$c4,$80,$2e,$04,$ff,$83,$02,$84,$02,$02,$c2,$88
db $3e,$3e,$3e,$42,$46,$42,$84,$3e,$ff,$85,$3e,$81,$3e,$88,$1c
db $46,$1c,$81,$02,$3e,$3e,$3e,$82,$34,$02,$c2,$88,$06,$06,$06
db $06,$06,$06,$82,$06,$06,$06,$06,$ff,$c2,$81,$06,$06,$80,$06
db $06,$81,$06,$06,$06,$06,$80,$06,$06,$ff,$09,$80,$10,$02,$10
db $02,$10,$02,$0c,$0c,$0c,$02,$0c,$02,$14,$14,$14,$02,$14,$02
db $85,$10,$00,$80,$32,$02,$32,$02,$32,$02,$c2,$32,$32,$32,$02
db $32,$02,$ff,$85,$32,$80,$54,$02,$54,$02,$54,$02,$50,$50,$50
db $02,$50,$02,$56,$56,$56,$02,$56,$02,$85,$54,$c4,$85,$0c,$ff
db $ff,$ff,$ff,$ff
lfff7_audiomain:
	jmp lf758

dw lc094_nmi	//NMI
dw lc000_reset	//RESET
dw lc0f7_brk	//IRQ/BRK
