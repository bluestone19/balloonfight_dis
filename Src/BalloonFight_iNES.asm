;Balloon Fight (USA) Disassembly - iNES ROM
;-----------------------

.P02

.SEGMENT "HEADER"
;iNES header
.BYTE "NES", $1A
.BYTE $01, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

;Assemble PRG ROM code
.include "BalloonFight_PRG.asm"

;Insert CHR ROM data
.SEGMENT "TILES"
.incbin "BalloonFight.chr"
