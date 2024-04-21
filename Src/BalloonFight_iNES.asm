;Balloon Fight (JP/USA/EU) Disassembly - iNES ROM
;-----------------------

.P02

;Assemble PRG ROM code
.include "BalloonFight_PRG.asm"

.SEGMENT "HEADER"
;iNES header
.BYTE "NES", $1A	;iNES header string. "NES" + EOF
.BYTE $01			;PRG-ROM Size (1 * 16 KiB)
.BYTE $01			;CHR-ROM Size (1 * 8 KiB)
.BYTE $00			;Horizontal Mirroring, Mapper 0
.BYTE $08			;NES/Famicom Game, iNES 2.0
.BYTE $00, $00		;Mapper/ROM Sizes < 256, No Submapper
.BYTE $00, $00		;No extra RAM
.IF REGION <= 1		;CPU/PPU Timing
	.BYTE $00		;NTSC
.ELSE
	.BYTE $01		;PAL
.ENDIF
.BYTE $00, $00		;Not Vs. System, No Misc Roms
.BYTE $01			;Default device = Controller

;Insert CHR ROM data
.SEGMENT "TILES"
.incbin "BalloonFight.chr"
