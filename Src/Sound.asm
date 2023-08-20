;----------------------
; Sound Code:
;----------------------

.SEGMENT "SOUND"
EndMusic:
	jsr CheckMusic
:	rts
;-----------------------

ContinuePlayingMusic:
	lda #0
	tax
	sta $fd
	beq NextContinueMusic
NextChannelWLSRX:
	txa
	lsr
	tax
NextChannel:
	inx
	txa
	cmp #4
	beq :-	;RTS
	lda $fd
	clc
	adc #4
	sta $fd
NextContinueMusic:
	txa
	asl
	tax
	lda Sq1TrackPointer,x
	sta $fe
	lda $e1,x
	sta $ff
	lda $e1,x
	beq NextChannelWLSRX
	txa
	lsr
	tax
	dec $d0,x
	bne NextChannel
LoadTrackDataLoop:
	ldy Sq1TrackOffset,x
	inc Sq1TrackOffset,x
	lda (CurTrackPointer),y
	beq EndMusic
	tay
	cmp #$ff
	beq SubTrackEnd
	and #$c0
	cmp #$c0
	beq SetRemainingLoops
	jmp ContinueNoteDataCheck
SubTrackEnd:
	lda $d8,x
	beq GetNextTrackData
	dec $d8,x
	lda $ec,x
	sta $e8,x
	bne GetNextTrackData
SetRemainingLoops:
	tya
	and #$3f
	sta $d8,x
	dec $d8,x
	lda $e8,x
	sta $ec,x
GetNextTrackData:
	jmp LoadTrackDataLoop
ContinueNoteDataCheck:
	tya
	bpl NoteDataHas80Set
	and #$0f
	clc
	adc TrackTempo
	tay
	lda NoteLengthOptions,y
	sta $d4,x
	tay
	txa
	cmp #2
	beq lf5c4
lf575:
	ldy $e8,x
	inc $e8,x
	lda ($fe),y
NoteDataHas80Set:
	tay
	txa
	cmp #3
	beq lf5e1
	pha
	tax
	cmp #1
	beq Sq2BusyCheck
ChannelIsFree:
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
	bne SetChannelVolumeToDC
	ldy #0
	txa
	cmp #2
	beq SetChannelVolumeContinue
	ldy #$10
	bne SetChannelVolumeContinue
SetChannelVolumeToDC:
	ldy $dc,x
SetChannelVolumeContinue:
	tya
	ldy $fd
	sta SQ1_VOL,y
SetCountdownThenContinue:
	lda $d4,x
	sta $d0,x
	jmp NextChannel
Sq2BusyCheck:
	lda SFX2Cur
	and #2
	beq ChannelIsFree
	pla
	tax
	jmp SetCountdownThenContinue
lf5c4:
	tya
	ldy UnknownSoundFlag
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
	sta TRI_LINEAR
	sta $de
	jmp lf575
lf5e1:
	lda SFX1Cur
	cmp #2
	beq lf5f9
	lda lf700,y
	sta NOISE_VOL
	lda lf700+1,y
	sta NOISE_LO
	lda lf700+2,y
	sta NOISE_HI
lf5f9:
	jmp SetCountdownThenContinue
BubbleRiseSFXSq1:
.BYTE $16,$ff,$10,$c5
lf600:
.BYTE $07
lf601:
    ;95 bytes
.BYTE $f0,$00,$00,$00,$d4,$00,$c8,$00,$bd,$00,$b2,$00,$a8,$00,$9f
.BYTE $00,$96,$00,$8d,$00,$85,$00,$7e,$00,$76,$00,$70,$01,$ab,$01
.BYTE $93,$01,$7c,$01,$67,$01,$52,$01,$3f,$01,$2d,$01,$1c,$01,$0c
.BYTE $00,$fd,$00,$ee,$00,$e1,$03,$57,$03,$27,$02,$f9,$02,$cf,$02
.BYTE $a6,$02,$80,$02,$5c,$02,$3a,$02,$1a,$01,$fc,$01,$df,$01,$c4
.BYTE $03,$f8,$00,$69,$00,$63,$00,$5e,$00,$58,$00,$53,$00,$4f,$00
.BYTE $4a,$00,$46,$00,$42
NoteLengthOptions:
    ;35 bytes
.BYTE $03,$06,$0c,$18,$30,$12,$24,$09,$08,$04,$07,$01,$04,$08,$10
.BYTE $20,$40,$18,$30,$0c,$01,$06,$0c,$18,$30,$60,$24,$48,$12,$10
.BYTE $08,$0e,$02,$03,$04
WriteSq1XY:
	lda #0
	beq WriteChannelDataA
WriteTriXY:
	lda #8
	bne WriteChannelDataA
WriteNoiseXY:
	lda #$0c
	bne WriteChannelDataA
WriteSq2:
	lda #4
WriteChannelDataA:
	sta $f9
	lda #$40
	sta $fa
	stx $fb
	sty $fc
	ldy #0
ChannelWriteLoop:
	lda ($fb),y
	sta ($f9),y
	iny
	tya
	cmp #4
	bne ChannelWriteLoop
	rts
;-----------------------

LoadSoundSequence:
	tax				; \
	jsr InitNeededChannels		; | Initialize Sound Channels
	stx MusicCur	; / and Sound Variables
	lda PopParachuteReq				; \
	beq LoadMusicTrackData	; | Check [PopParachuteReq] == $00 or $02
	cmp #$02				; |
	bne LoadMusicTrackData	; /
	sta SFX1Req		; [$00F0] = [PopParachuteReq] (!= 00 or 02)
	lda #0		; \
	sta PopParachuteReq	; / Clear [PopParachuteReq]
LoadMusicTrackData:
	lda lfbca,y	; \ Load Sound Sequence Pointer to Y
	tay			; /
	ldx #0						; \
LoadMusicTempoTrackLoop:
	lda lfbca,y					; |
	sta TrackTempo,x			; | Load Sound Sequence Header
	iny							; | (9 bytes)
	inx							; |
	txa							; |
	cmp #9						; |
	bne LoadMusicTempoTrackLoop	; /

	lda #$01				; \
	sta $d0					; |
	sta $d1					; |
	sta $d2					; | Initialize Sequence stuff
	sta $d3					; |
	lda #0					; |
	sta Sq1TrackOffset		; |
	sta Sq2TrackOffset		; |
	sta TriTrackOffset		; |
	sta NoiseTrackOffset	; /
	rts
;-----------------------

    ;27 bytes
FlapSFX:
.BYTE $94,$ab,$fd,$58
FootstepNoise:
.BYTE $00,$7f,$04,$18
.BYTE $3f,$7f,$00,$00
.BYTE $06,$7f,$0a,$c0
.BYTE $08,$7f,$05,$c0
.BYTE $c1,$89,$02,$0f
.BYTE $ff,$ff,$ff
lf700:
    ;18 bytes
.BYTE $10,$00,$18,$10,$01,$18,$00,$01,$88,$02,$02,$40,$03,$05,$40
.BYTE $04,$07,$40
ClearSquareSweeps:
	lda #$7f		; \ Set Pulse Channels:
	sta SQ1_SWEEP	; | No Sweep
	sta SQ2_SWEEP	; /
StoreSoundXY:
	stx $dc
	sty $dd
	rts
;-----------------------

PlayFlapSFX:
	ldxy FlapSFX
	bne lf745
lf725:
	lda SFX3Req
	lsr
	bcs lf736
	lda SFX3Cur
	lsr
	bcs lf749
	lda SFX1Req
	and #$10
	bne PlayFlapSFX
	rts
;-----------------------

lf736:
	lda SFX3Cur
	ora #1
	sta SFX3Cur
	lda #0
	sta BubbleRiseSFXTimer
	ldxy BubbleRiseSFXSq1
lf745:
	jsr WriteSq1XY
	rts
;-----------------------

lf749:
	inc BubbleRiseSFXTimer
	lda BubbleRiseSFXTimer
	cmp #$58
	bne :+	;RTS
	lda #0
	sta SFX3Cur
	rts
;-----------------------

AudioMain:
	lda #$c0		; \ Set Frame Counter
	sta APU_FRAME	; / to 4-step sequence, clear frame interrupt flag
	jsr lfb25_music
	jsr lf90a
	jsr lfa38
	jsr CheckSFX2Change
	jsr CheckSFX1Change
	lda SFX2Req
	sta LastSFX2Req
	lda #0			; \
	sta SFX1Req		; |
	sta SFX2Req		; | Clear Music/SFX Flags
	sta MusicReq	; |
	sta SFX3Req		; /
:	rts
;-----------------------

TryFootstepNoise:
	lda SFX1Cur
	and #6
	bne :-	;RTS
	lda SFX1Cur
	and #$f0
	sta SFX1Cur
	ldxy FootstepNoise
	jmp WriteNoiseRTS
CheckMusic:
	lda MusicCur
	cmp #$20
	bne CheckSFX
	inc TripMusicFlag
InitNeededChannels:
	and #$0f	; \ Initialize Sound Channels
	cmp #$0f	; | differently depending on
	bne InitAllSoundMemory	; / Music/Jingle needs
	txa
CheckSFX:
	lda SFX1Cur
	and #$20
	bne InitSquare2Noise
InitAllSoundMemory:
	lda #$10		; \ Constant volume on:
	sta NOISE_VOL	; | - Noise Channel
	sta SQ1_VOL		; | - Pulse 1 Channel
	sta SQ2_VOL		; / - Pulse 2 Channel
	lda #$00
	sta SFX1Cur
ResetCurrentSounds:
	sta SFX2Cur
	sta MusicCur	; Clear Current Music/Jingle
	sta SplashSFXPhase
	sta SFX3Cur
	sta TRI_LINEAR	; Clear Triangle Channel Linear Counter
	sta DMC_RAW	; Clear DMC Channel Load Counter
	sta UnknownSoundFlag
	rts
;-----------------------

InitSquare2Noise:
	lda #$10	; \ Constant volume on:
	sta SQ2_VOL	; | - Pulse 2 Channel
	sta NOISE_VOL	; / - Noise Channel
	lda #0
	beq ResetCurrentSounds
ResetSplashSFXPhase:
	lda #0
	sta SplashSFXPhase
	rts
;-----------------------

PlaySplashNoise2:
	ldxy $f6f5
	jmp WriteNoiseRTS
CheckSplashCountdown:
	inc SplashSFXTimer
	lda SplashSFXTimer
	cmp #$10
	beq PlaySplashNoise2
	cmp #$20
	beq ResetSplashSFXPhase
	rts
;-----------------------

PlaySplashNoise:
	lda #0
	sta SplashSFXTimer
	lda #$f0
	sta SplashSFXPhase
	ldxy $f6f1
	jmp WriteNoiseRTS
PlayPopNoise:
	lda SFX1Cur
	and #$f0
	ora #2
	sta SFX1Cur
	lda #0
	sta PopSFXCountdown
	ldxy $f6f1
	jmp WriteNoiseRTS
CheckPopCountdown:
	inc PopSFXCountdown
	lda PopSFXCountdown
	cmp #$10
	bne :+
	jmp ClearSFX1Lower
ClearAllSoundEffects:
	jmp InitAllSoundMemory
CheckSFX1Change:
	lda SFX1Req
	lsr
	bcs ClearAllSoundEffects
	lda MusicCur
	cmp #$df
	beq ContinueSFX1ChangeCheck
	cmp #$7f
	beq ContinueSFX1ChangeCheck
	cmp #$20
	beq ContinueSFX1ChangeCheck
	lda MusicCur
	bne :+
ContinueSFX1ChangeCheck:
	lda SplashSFXPhase
	cmp #$0f
	beq PlaySplashNoise
	cmp #$f0
	beq CheckSplashCountdown
	lda SFX1Req
	lsr
	lsr
	bcs PlayPopNoise
	lsr
	bcs TryLightningStrikeSFX
	lsr
	bcs GotoTryFootstepNoise
	lda SFX1Cur
	lsr
	lsr
	bcs CheckPopCountdown
	lsr
	bcs CheckLightningSFXCountdown
:	rts
;-----------------------

GotoTryFootstepNoise:
	jmp TryFootstepNoise
TryLightningStrikeSFX:
	lda SFX1Cur
	and #$80
	bne :-
	lda SFX1Cur
	and #$f0
	ora #4
	sta SFX1Cur
	lda #0
	sta LightningSFXTimer
	sta LightningSFXPitch
	ldxy $f6ed
WriteNoiseRTS:
	jsr WriteNoiseXY
	rts
;-----------------------

CheckLightningSFXCountdown:
	inc LightningSFXTimer
	lda LightningSFXTimer
	cmp #3
	bne :+
	lda #0
	sta LightningSFXTimer
	inc LightningSFXPitch
	lda LightningSFXPitch
	cmp #$10
	bne WriteNoisePitch
	lda #$10
	sta NOISE_VOL
ClearSFX1Lower:
	lda SFX1Cur
	and #$f0
	sta SFX1Cur
:	rts
;-----------------------

WriteNoisePitch:
	sta NOISE_LO
	rts
;-----------------------

PlaySparkBounceSFX:
	lda #0
	sta SparkSFXTimer
	clc
	lda RNGOutput
	and #7
	adc #2
	sta SparkSFXLength
	lda SFX3Cur
	and #0
	ora #$80
	sta SFX3Cur
	bne lf8e8
lf8bf:
	inc SparkSFXTimer
	lda SparkSFXTimer
	cmp SparkSFXLength
	bne lf8e8
lf8ca:
	lda #$10
	sta SQ1_VOL
	sta SQ2_VOL
	lda #0
	sta SFX3Cur
	lda SFX1Cur
	and #$0f
	sta SFX1Cur
	rts
;-----------------------

lf8dd:
	jsr InitAllSoundMemory
	lda #$80
	sta SFX1Cur
	lda #2
	sta SFX1Req
lf8e8:
	ldx #$f9
	ldy #$f6
	jsr WriteSq1XY
	lda $1b
	and #$0f
	sta $4002
	ldx #$f9
	ldy #$f6
	jsr WriteSq2
	lda $1b
	lsr
	lsr
	and #$0f
	sta $4006
	rts
;-----------------------

lf907:
	jmp PlaySparkBounceSFX

lf90a:
	lda MusicCur		; \ Check if music is not playing
	beq lf91b	; / If not playing then continue as normal
	cmp #$df	; \ Songs #$DF?
	beq lf91b	; / Wouldn't that be redundant?
	lda SFX1Req		; \
	and #$e0	; | Check for sound effects that stops the music
	beq :+	; / if found, then return
	jsr InitAllSoundMemory
lf91b:
	lda SFX1Req
	asl
	bcs lf8dd
	asl
	bcs lf952
	asl
	bcs lf965
	lda SFX1Cur
	asl
	bcs lf8e8
	lda SFX1Cur
	and #$e0
	bne :+
	lda MusicCur
	cmp #$df
	beq lf94b
	lda MusicCur
	bne :+
	lda SFX3Req
	asl
	bcs lf907
	asl
	bcs lf977
	lda SFX3Cur
	asl
	bcs lf94f
	asl
	bcs lf993
lf94b:
	jsr lf725
:	rts
;-----------------------

lf94f:
	jmp lf8bf
lf952:
	lda #$0f
	sta SplashSFXPhase
	lda SFX1Cur
	and #$0f
	ora #$40
	sta SFX1Cur
	ldx #$d1
	ldy #$f9
	bne lf98f
lf965:
	lda #2
	sta SFX1Req
	lda SFX1Cur
	and #$0f
	ora #$20
	sta SFX1Cur
	ldxy $f9cd
	bne lf98f
lf977:
	lda #0
	sta ChompSFXTimer
	lda SFX3Cur
	and #0
	ora #$40
	sta SFX3Cur
	ldxy $f9d5
	jsr WriteSq2
	ldxy $f9d9
lf98f:
	jsr WriteSq1XY
	rts
;-----------------------

lf993:
	inc ChompSFXTimer
	lda ChompSFXTimer
	cmp #$12
	beq lf9ca
	cmp #6
	bcc lf9b1
	lda $1b
	ora #$10
	and #$7f
	sta FishChompPitchSq1
	rol
	sta FishChompPitchSq2
	jmp lf9bd
lf9b1:
	inc FishChompPitchSq2
	inc FishChompPitchSq2
	inc FishChompPitchSq1
	inc FishChompPitchSq1
lf9bd:
	lda FishChompPitchSq2
	sta $4006
	lda FishChompPitchSq1
	sta $4002
	rts
;-----------------------

lf9ca:
	jmp lf8ca
    ;28 bytes
.BYTE $b8,$d5,$20,$00,$9f,$93,$80,$22,$3f,$ba,$e0,$06,$3f,$bb,$ce
.BYTE $06,$b8,$93,$50,$02,$80,$7f,$60,$68,$80,$7f,$62,$68
lf9e9:
	lda SFX2Cur
	and #2
	bne :+
	lda MusicCur
	cmp #$df
	beq lf9f9
	lda MusicCur
	bne :+
lf9f9:
	lda #0
	sta BumpSFXTimer
	lda SFX2Cur
	and #$e0
	ora #2
	sta SFX2Cur
	ldx #$dd
	ldy #$f9
	bne lfa7f
lfa0c:
	inc BumpSFXTimer
	lda BumpSFXTimer
	cmp #7
	bne :+
	lda #$7f
	sta $4005
	lda #$10
	sta SQ2_VOL
	lda SFX2Cur
	and #$e0
	sta SFX2Cur
:	rts
;-----------------------

lfa27:
	jsr InitAllSoundMemory
	ldx #$e1
	ldy #$f9
	jsr WriteSq1XY
	ldx #$e5
	ldy #$f9
	jmp lfa7f
lfa38:
	lda MusicCur
	beq lfa42
	and #$0f
	cmp #$0f
	bne :+
lfa42:
	lda SFX1Cur
	and #$80
	bne :+
	lda SFX3Cur
	and #$c0
	bne :+
	lda SFX2Req
	lsr
	bcs lfa27
	lsr
	bcs lf9e9
	lsr
	bcs lfa83
	lsr
	lsr
	bcs lfa64
	lda SFX2Cur
	lsr
	lsr
	bcs lfa0c
:	rts
;-----------------------

lfa64:
	lda MusicCur
	bne :-
	lda SFX2Cur
	and #2
	bne :-
	ldx #$8a
	ldy #$fa
	jsr WriteSq2
	lda $1b
	and #$3f
	ora #$10
	sta $4006
	rts
;-----------------------

lfa7f:
	jsr WriteSq2
	rts
;-----------------------

lfa83:
	ldy #$0a
	lda #$ef
	jmp lfba5

TweetSFXBase:
.BYTE $d9,$86,$a8,$48
EnemyLandTri1:
.BYTE $08,$7f,$40,$28
EnemyLandTri2:
.BYTE $08,$7f,$45,$28

lfa96:
	inc EnemyLandSFXTimer
	lda EnemyLandSFXTimer
	cmp #4
	bne :+
	lda SFX2Cur
	and #$1f
	sta SFX2Cur
	ldxy EnemyLandTri2
	bne WriteTriRTS
CheckSFX2Change:
	lda MusicCur
	beq ContinueSFX2Check
	cmp #8
	beq ContinueSFX2Check
	and #$0f
	cmp #$0f
	bne :+
ContinueSFX2Check:
	lda SFX1Cur
	and #$80
	bne :+
	lda SFX2Req
	asl
	bcs lfb17
	asl
	bcs EnemyLandingSFX
	lda SFX2Cur
	asl
	asl
	bcs lfa96
	lda SFX2Req
	and #$20
	beq lfad9
	lda MusicCur
	beq lfb04
:	rts
;-----------------------

lfad9:
	lda MusicCur
	cmp #$df
	bne :-
	jmp CheckSFX
EnemyLandingSFX:
	lda SFX2Cur
	and #$1f
	ora #$40
	sta SFX2Cur
	lda #0
	sta TRI_LINEAR
	sta MusicCur
	sta EnemyLandSFXTimer
	lda #$10
	sta SQ2_VOL
	sta NOISE_VOL
	ldxy $fa8e
WriteTriRTS:
	jsr WriteTriXY
	rts
;-----------------------

lfb04:
	lda LastSFX2Req
	and #$20
	bne lfb10
	lda #2
	sta PopParachuteReq
lfb10:
	ldy #$08
	lda #$df
	jmp lfba5

lfb17:
	ldy #$04
	lda #$7f
	jmp lfba5

lfb1e:		; Music/Jingle: Stage Clear
	ldy #$00
	lda #$02
	jmp lfbc1

lfb25_music:
	lda TripMusicFlag	; \ Play Balloon Trip Music
	bne lfb5e			; /
	lda MusicReq		; \ Play Music/Jingle:
	lsr					; |
	bcs lfb82			; | #$01 = Game Over
	lsr					; |
	bcs lfb1e			; | #$02 = Stage Clear
	lsr					; |
	bcs lfb4c			; | #$04 = Pause
	lsr					; |
	bcs lfb7c			; | #$08 = Stage Start
	lsr					; |
	bcs lfb69			; | #$10 = Bonus Phase Perfect
	lsr					; |
	bcs lfb5e			; | #$20 = Balloon Trip / Bonus Phase Music
	lsr					; |
	bcs lfb58			; | #$40 = Fish
	lsr					; |
	bcs lfb52			; / #$80 = Respawn
	lda MusicCur	; \
	bne lfb49		; / Current Music/Jingle
	rts
;-----------------------

lfb49:
	jmp ContinuePlayingMusic

lfb4c:		; Music/Jingle: Pause
	ldy #$02
	lda #$04
	bne lfba5
lfb52:		; Music/Jingle: Respawn
	ldy #$09
	lda #$80
	bne lfb6d
lfb58:		; Music/Jingle: Fish
	ldy #$07
	lda #$40
	bne lfb6d
lfb5e:		; Music/Jingle: Balloon Trip / Bonus Game
	lda #$00
	sta TripMusicFlag
	ldy #$06
	lda #$20
	bne lfbc1
lfb69:		; Music/Jingle: Bonus Game Perfect
	ldy #$05
	lda #$10
lfb6d:
	jsr LoadSoundSequence
	ldx #$fc
	ldy #$fc
	jsr ClearSquareSweeps
	inc UnknownSoundFlag
	bne lfb49
lfb7c:		; Music/Jingle: Stage Start
	ldy #$03
	lda #$08
	bne lfb86
lfb82:		; Music/Jingle: Game Over
	ldy #$01
	lda #$01
lfb86:
	jsr LoadSoundSequence
	ldx #$80
	ldy #$80
lfb8d:
	jsr StoreSoundXY
	lda #$83		; \ Pulse 1 Channel:
	sta SQ1_SWEEP	; / Sweep, Shift = 3
	lda #$7f		; \ Pulse 2 Channel:
	sta SQ2_SWEEP	; / No Sweep
	bne lfbaf
	jsr LoadSoundSequence
	ldx #$04
	ldy #$04
	bne lfbac
lfba5:
	jsr LoadSoundSequence
	ldx #$80
	ldy #$80
lfbac:
	jsr ClearSquareSweeps
lfbaf:
	lda #0
	sta UnknownSoundFlag
	lda SFX1Cur
	and #$20
	beq lfb49
	lda #$d5
	sta SQ1_SWEEP
	bne lfb49
lfbc1:
	jsr LoadSoundSequence
	ldx #$80
	ldy #$ba
	bne lfb8d

;----------------------
; Music Data:
;----------------------

lfbca: ;Music Track Init Data
.BYTE $0b,$14,$1d,$26,$2f,$38,$41,$4a,$53,$5c,$65
lfbd5: ;Phase Clear Music Init Data
.BYTE $0c
.WORD lff02,lff0b,lff1e,lff31
lfbde: ;Game Over Music Init Data
.BYTE $15
.WORD lfe18,lfe2a,lfe65,lfe86
lfbe7: ;Pause Music Init Data
.BYTE $0c
.WORD lfe0d,$0000,lfe13,$0000
lfbf0: ;New Start Music Init Data
.BYTE $15
.WORD lff38,lff5a,lff79,lff94
lfbf9: ;Enemy Down Music Init Data
.BYTE $00
.WORD $0000,lfed7,lfeed,$0000
lfc02: ;Super Bonus Music Init Data
.BYTE $00
.WORD lffb3,lffc9,lffda,lffef
lfc0b: ;Bonus Trip Music Init Data
.BYTE $15
.WORD lfca5,lfd0a,lfd98,lfde0
lfc14: ;Eaten By Fish Music Init Data
.BYTE $15
.WORD lfeb2,$0000,lfec5,$0000
lfc1d: ;Parachute Music Init Data
.BYTE $15
.WORD $0000,lfe92,lfea1,$0000
lfc26: ;Respawn Music Init Data
.BYTE $0c
.WORD lfc59,lfc72,lfc8c,$0000
lfc2f: ;Bubble Collect Music Init Data
.BYTE $00
.WORD $0000,lfc38,lfc49,$0000

lfc38:
.BYTE $82,$02,$8b,$02
.BYTE $80,$08,$02,$10
.BYTE $02,$16,$02,$52
.BYTE $02,$02,$02,$1a
.BYTE $00
lfc49:
.BYTE $82,$02,$80,$10
.BYTE $02,$16,$02,$52
.BYTE $02,$5a,$02,$02
.BYTE $02,$56,$81,$02
lfc59:
.BYTE $80,$12,$02,$0c
.BYTE $02,$04,$02,$0c
.BYTE $02,$04,$02,$2a
.BYTE $02,$81,$04,$02
.BYTE $80,$04,$02,$81
.BYTE $04,$88,$02,$02
.BYTE $00
lfc72:
.BYTE $88,$02,$02,$80
.BYTE $04,$02,$2a,$02
.BYTE $24,$02,$2a,$02
.BYTE $24,$02,$1c,$02
.BYTE $81,$22,$02,$80
.BYTE $22,$02,$81,$24
.BYTE $88,$02
lfc8c:
.BYTE $88,$02,$80,$56
.BYTE $02,$4e,$02,$12
.BYTE $02,$4e,$02,$12
.BYTE $02,$0c,$02,$81

.BYTE $10,$02,$80,$10
.BYTE $02,$81,$12,$88
.BYTE $02
lfca5:
.BYTE $c3,$81,$02,$02
.BYTE $1c,$02,$02,$02
.BYTE $1c,$1c,$ff,$c6
.BYTE $88,$1c,$ff,$c7
.BYTE $82,$4c,$4c,$2a
.BYTE $4c,$ff,$c6,$88
.BYTE $1c,$ff,$c4,$81
.BYTE $46,$02,$46,$02
.BYTE $32,$02,$46,$80
.BYTE $2e,$2e,$ff,$c3
.BYTE $82,$46,$46,$81
.BYTE $32,$32,$46,$2e
.BYTE $ff,$80,$0c,$0c
.BYTE $81,$46,$46,$46
.BYTE $80,$04,$04,$81
.BYTE $46,$46,$02,$c8
.BYTE $82,$4c,$4c,$2a
.BYTE $4c,$ff,$c2,$81
.BYTE $46,$80,$32,$32
.BYTE $82,$46,$04,$81
.BYTE $46,$2a,$ff,$c2
.BYTE $81,$0c,$0c,$80
.BYTE $04,$04,$81,$04
.BYTE $80,$2e,$2e,$81
.BYTE $2e,$82,$24,$ff
.BYTE $00
lfd0a:
.BYTE $81,$32,$02,$02
.BYTE $06,$0c,$32,$02
.BYTE $02,$8a,$2e,$8b
.BYTE $02,$8a,$2e,$8b
.BYTE $02,$8a,$2e,$8b
.BYTE $02,$88,$2e,$32
.BYTE $2e,$d0,$8c,$2c
.BYTE $24,$ff,$d0,$2e
.BYTE $20,$ff,$c3,$80
.BYTE $28,$02,$82,$02
.BYTE $80,$2c,$02,$32
.BYTE $02,$24,$02,$82
.BYTE $02,$81,$02,$80
.BYTE $28,$02,$06,$02
.BYTE $28,$02,$81,$02
.BYTE $80,$24,$02,$32
.BYTE $02,$24,$02,$ff
.BYTE $80,$28,$02,$82
.BYTE $02,$80,$2c,$02
.BYTE $32,$02,$24,$02
.BYTE $82,$02,$89,$0c
.BYTE $0a,$08,$06,$32
.BYTE $30,$2e,$2c,$2a
.BYTE $28,$26,$24,$02
.BYTE $02,$02,$86,$02
.BYTE $c7,$84,$02,$ff
.BYTE $c4,$80,$28,$02
.BYTE $82,$02,$80,$2c
.BYTE $02,$32,$02,$24
.BYTE $02,$82,$02,$81
.BYTE $02,$80,$28,$02
.BYTE $06,$02,$28,$02
.BYTE $81,$02,$80,$24
.BYTE $02,$32,$02,$24
.BYTE $02,$ff,$c8,$84
.BYTE $02,$ff
lfd98:
.BYTE $81,$14,$02,$02
.BYTE $14,$1a,$14,$02
.BYTE $02,$88,$10,$10
.BYTE $10,$10,$14,$10
.BYTE $85,$3c,$81,$44
.BYTE $85,$4a,$81,$44
.BYTE $88,$28,$24,$20
.BYTE $46,$42,$40,$c6
.BYTE $81,$3c,$02,$02
.BYTE $44,$02,$02,$02
.BYTE $4a,$02,$46,$36
.BYTE $36,$38,$38,$02
.BYTE $3a,$02,$80,$3c
.BYTE $3c,$81,$02,$24
.BYTE $02,$02,$2c,$24
.BYTE $88,$24,$1e,$46
.BYTE $36,$38,$3a,$ff
.BYTE $c4,$84,$02,$ff
lfde0:
.BYTE $d8,$81,$06,$ff
.BYTE $c6,$88,$06,$ff
.BYTE $c7,$81,$06,$06
.BYTE $80,$06,$06,$81
.BYTE $06,$06,$80,$06
.BYTE $06,$81,$06,$06
.BYTE $ff,$c6,$88,$06
.BYTE $ff,$e0,$81,$06
.BYTE $06,$ff,$82,$0f
.BYTE $81,$06,$06,$ea
.BYTE $06,$06,$06,$06
.BYTE $ff
lfe0d:
.BYTE $c5,$80,$0e,$58,$ff,$00
lfe13:
.BYTE $c5,$80,$0e,$58,$ff
lfe18:
.BYTE $82,$1c,$1c,$c3
.BYTE $82,$1c,$1c,$81
.BYTE $1c,$1c,$1c,$02
.BYTE $ff,$c7,$88,$1c
.BYTE $ff,$00
lfe2a:
.BYTE $83,$02,$80,$0e
.BYTE $02,$0e,$02,$0c
.BYTE $02,$0e,$02,$4e
.BYTE $02,$02,$02,$0e
.BYTE $02,$0c,$02,$02
.BYTE $02,$0e,$02,$0c
.BYTE $02,$0e,$02,$4e
.BYTE $02,$02,$02,$0e
.BYTE $02,$0c,$02,$0e
.BYTE $02,$0e,$02,$0c
.BYTE $02,$0e,$02,$4e
.BYTE $02,$02,$02,$0e
.BYTE $02,$0c,$02,$88
.BYTE $4e,$18,$16,$12
.BYTE $0e,$0c,$0e
lfe65:
.BYTE $83,$02,$81,$3e
.BYTE $3e,$82,$46,$1c
.BYTE $46,$81,$02,$38
.BYTE $3e,$02,$82,$46
.BYTE $1c,$82,$48,$48
.BYTE $81,$3e,$3e,$82
.BYTE $38,$88,$24,$20
.BYTE $1c,$48,$46,$42
.BYTE $3e
lfe86:
.BYTE $82,$09,$09
.BYTE $c6,$82,$03,$0c
.BYTE $ff,$c6,$88,$06
.BYTE $ff
lfe92:
.BYTE $ed,$89,$2a,$02
.BYTE $04,$0c,$02,$04
.BYTE $08,$02,$30,$26
.BYTE $02,$30,$ff
lfea1:
.BYTE $80,$02,$ed,$89
.BYTE $0c,$02,$12,$4e
.BYTE $02,$12,$18,$02
.BYTE $0e,$08,$02,$0e
.BYTE $ff
lfeb2:
.BYTE $80,$42,$02,$48
.BYTE $02,$1e,$02,$24
.BYTE $02,$02,$02,$2a
.BYTE $02,$c6,$8c,$30
.BYTE $2a,$ff,$00
lfec5:
.BYTE $80,$24,$02,$2a
.BYTE $02,$30,$02,$06
.BYTE $02,$02,$02,$0c
.BYTE $02,$c6,$8c,$12
.BYTE $18,$ff
lfed7:
.BYTE $80,$56,$54,$52
.BYTE $50,$81,$02,$80
.BYTE $5e,$5a,$54,$50
.BYTE $18,$14,$10,$0a
.BYTE $06,$30,$2c,$28
.BYTE $02,$00
lfeed:
.BYTE $80,$1a,$18,$16
.BYTE $14,$81,$02,$80
.BYTE $02,$5e,$5a,$54
.BYTE $50,$18,$14,$10
.BYTE $0a,$06,$30,$2c
.BYTE $28
lff02:
.BYTE $82,$1c,$02,$1c
.BYTE $02,$02,$1c,$1c,$00
lff0b:
.BYTE $81,$10,$0a,$32
.BYTE $28,$80,$32,$02
.BYTE $32,$02,$82,$32
.BYTE $81,$06,$02,$06
.BYTE $02,$82,$32
lff1e:
.BYTE $81,$54,$1a,$10
.BYTE $0a,$80,$10,$02
.BYTE $10,$02,$82,$10
.BYTE $81,$16,$02,$16
.BYTE $02,$82,$0a
lff31:
.BYTE $83,$03,$0c,$82
.BYTE $03,$0c,$0c
lff38:
.BYTE $c2,$88,$1c,$1c
.BYTE $1c,$1c,$1c,$1c
.BYTE $83,$1c,$80,$04
.BYTE $04,$2a,$02,$82
.BYTE $1c,$ff,$81,$4c
.BYTE $02,$4c,$02,$2a
.BYTE $02,$4c,$1c,$81
.BYTE $4c,$02,$4c,$02
.BYTE $4c,$00
lff5a:
.BYTE $88,$2e,$2e,$2e
.BYTE $30,$04,$30,$c4
.BYTE $80,$2e,$04,$ff
.BYTE $83,$02,$88,$2e
.BYTE $2e,$2e,$30,$04
.BYTE $30,$c4,$80,$2e
.BYTE $04,$ff,$83,$02
.BYTE $84,$02,$02
lff79:
.BYTE $c2,$88,$3e,$3e
.BYTE $3e,$42,$46,$42
.BYTE $84,$3e,$ff,$85
.BYTE $3e,$81,$3e,$88
.BYTE $1c,$46,$1c,$81
.BYTE $02,$3e,$3e,$3e
.BYTE $82,$34,$02
lff94:
.BYTE $c2,$88,$06,$06
.BYTE $06,$06,$06,$06
.BYTE $82,$06,$06,$06
.BYTE $06,$ff,$c2,$81
.BYTE $06,$06,$80,$06
.BYTE $06,$81,$06,$06
.BYTE $06,$06,$80,$06
.BYTE $06,$ff,$09
lffb3:
.BYTE $80,$10,$02,$10
.BYTE $02,$10,$02,$0c
.BYTE $0c,$0c,$02,$0c
.BYTE $02,$14,$14,$14
.BYTE $02,$14,$02,$85
.BYTE $10,$00
lffc9:
.BYTE $80,$32,$02,$32
.BYTE $02,$32,$02,$c2
.BYTE $32,$32,$32,$02
.BYTE $32,$02,$ff,$85
.BYTE $32
lffda:
.BYTE $80,$54,$02,$54
.BYTE $02,$54,$02,$50
.BYTE $50,$50,$02,$50
.BYTE $02,$56,$56,$56
.BYTE $02,$56,$02,$85
.BYTE $54
lffef:
.BYTE $c4,$85,$0c,$ff
.BYTE $ff,$ff,$ff,$ff
GotoAudioMain:
	jmp AudioMain