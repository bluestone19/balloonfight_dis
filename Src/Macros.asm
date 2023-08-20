.macro tpa addr, var
	lda #<addr
	sta	var
	lda #>addr
	sta	var+1
.endmacro

.macro lday addr
	lda #<addr
	ldy #>addr
.endmacro

.macro ldxy addr
	ldx #<addr
	ldy #>addr
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