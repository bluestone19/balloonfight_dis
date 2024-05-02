.macro tpa addr, var
	lda #<addr
	sta	var
	lda #>addr
	sta	var+1
	.endmacro

.macro stppuaddr addr
	lda #>addr
	sta PPUADDR
	lda #<addr
	sta PPUADDR
	.endmacro

.macro styappuaddr
	sty PPUADDR
	sta PPUADDR
	.endmacro

.macro lday addr
	lda #<addr
	ldy #>addr
	.endmacro

.macro ldya addr
	ldy #>addr
	lda #<addr
	.endmacro

.macro ldxy addr
	ldx #<addr
	ldy #>addr
	.endmacro

.macro cadc var
	clc
	adc	var
	.endmacro

.macro cadcx var
	clc
	adc	var, x
	.endmacro

.macro cadcy var
	clc
	adc	var, y
	.endmacro

.macro incr rep, var
	.repeat rep
		inc	var
		.endrep
	.endmacro

.macro inxr rep
	.repeat rep
		inx
		.endrep
	.endmacro

.macro inyr rep
	.repeat rep
		iny
		.endrep
	.endmacro

.macro decr rep, var
	.repeat rep
		dec var
		.endrep
	.endmacro

.macro dexr rep
	.repeat rep
		dex
		.endrep
	.endmacro

.macro deyr rep
	.repeat rep
		dey
		.endrep
	.endmacro

.macro aslr rep, var
	.ifblank	var
		.repeat rep
			asl
			.endrep
		.endif
	.ifnblank	var
		.repeat rep
			asl var
			.endrep
		.endif
	.endmacro

.macro rolr rep
	.repeat rep
		rol
		.endrep
	.endmacro

.macro lsrr rep
	.repeat rep
		lsr
		.endrep
	.endmacro

.macro phx
	txa
	pha
	.endmacro

.macro phy
	tya
	pha
	.endmacro

.macro plx
	pla
	tax
	.endmacro

.macro ply
	pla
	tay
	.endmacro

.macro aslx
	txa
	asl
	tax
	.endmacro

.macro lsrx
	txa
	lsr
	tax
	.endmacro