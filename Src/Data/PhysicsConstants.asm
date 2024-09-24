;These are all physical constant data for the 12 object types
ObjectGravityData:		;Fractional
	.BYTE $04,$04,$05,$06,$03,$03,$03,$06,$0a,$0a,$0a,$0a
ObjectFlapAccelData:	;Fractional
	.BYTE $28,$32,$46,$78,$00,$00,$00,$64,$00,$00,$00,$00

ObjectAirXAccelData:	;Fractional
	.BYTE $0a,$1e,$32,$70,$00,$00,$00,$70,$00,$00,$00,$00
ObjectGroundXAccelData:	;Fractional
	.BYTE $14,$3c,$64,$a0,$00,$00,$00,$a0,$00,$00,$00,$00

ObjectMaxXVelDataFrac:
	.BYTE $70,$b0,$e0,$40,$80,$80,$80,$40,$00,$00,$00,$00
ObjectMaxXVelDataInt:
	.BYTE $00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$00

ObjectMinXVelDataFrac:
	.BYTE $90,$50,$20,$c0,$80,$80,$80,$c0,$00,$00,$00,$00
ObjectMinXVelDataInt:
	.BYTE $ff,$ff,$ff,$fe,$ff,$ff,$ff,$fe,$00,$00,$00,$00

ObjectMaxYVelDataFrac:
	.BYTE $50,$90,$c0,$40,$40,$40,$40,$40,$00,$00,$00,$00
ObjectMaxYVelDataInt:
	.BYTE $00,$00,$00,$01,$00,$00,$00,$01,$02,$02,$02,$02

ObjectMinYVelDataFrac:
	.BYTE $b0,$70,$40,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
ObjectMinYVelDataInt:
	.BYTE $ff,$ff,$ff,$fe,$ff,$ff,$ff,$fe,$fe,$01,$fe,$fe
