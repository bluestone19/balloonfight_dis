;Balloon Fight (USA) Disassembly - iNES ROM
;-----------------------

.PC02

.SEGMENT "HEADER"
;iNES header
.BYTE $4E, $45, $53, $1A, $01, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

;Assemble PRG ROM code
.include "BalloonFight_PRG.asm"

.SEGMENT "TILES"
;Insert CHR ROM data
.incbin "BalloonFight.chr"
