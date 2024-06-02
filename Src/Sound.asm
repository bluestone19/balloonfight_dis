;----------------------
; Sound Code:
;----------------------

.SEGMENT "SOUND"
EndMusic:
	jsr CheckMusic
	:rts

ContinuePlayingMusic:
	lda #0				;\
	tax					;| Clear A, X, and SoundRegOffset
	sta SoundRegOffset	;/
	beq NextContinueMusic	;Always taken

NextChannelWLSRX:
	lsrx	; Halve X, since it was temporarily doubled during pointer check
NextChannel:
	inx	; Increment X to choose next channel
	txa		; \
	cmp #4	; | Return if X == 4 (Finished all channels)
	beq :-	; /
	lda SoundRegOffset	; \
	cadc #4				; | SoundRegOffset += 4
	sta SoundRegOffset	; /
NextContinueMusic:
	aslx	; X *= 2, since pointers are 2 bytes long
	lda Sq1TrackPointerLo,x	; \
	sta CurTrackPointerLo	; | Copy Channel X's track pointer to CurTrackPointer
	lda Sq1TrackPointerHi,x	; |
	sta CurTrackPointerHi	; /
	lda Sq1TrackPointerHi,x	; \ If pointer was $0000, this track is unused.
	beq NextChannelWLSRX	; / For unused tracks, go to next channel after undoing X being doubled
	lsrx	; X /= 2, since no longer working with pointers
	dec Sq1Countdown,x	; \ Decrement countdown, and unless it hits 0 don't update channel
	bne NextChannel		; /
LoadTrackDataLoop:
	ldy Sq1TrackOffset,x	; Y = Channel X's Track Offset
	inc Sq1TrackOffset,x	; Increment offset for next pass
	lda (CurTrackPointer),y	; \ If next byte in track data is 0, end music
	beq EndMusic			; /
	tay	; Y = New byte from track data
	cmp #$ff		; \ If byte was $FF, end sub track
	beq SubTrackEnd	; /
	and #%11000000		; \
	cmp #%11000000		; | If uppermost two bits are set,
	beq StartSubTrack	; / Then start a new sub track
	jmp ContinueNoteDataCheck	; Otherwise continue checking byte

SubTrackEnd:
	lda Sq1SubTrackLoops,x	; \ If no loops remaining, then load next byte insteaad of looping
	beq GetNextTrackData	; /
	dec Sq1SubTrackLoops,x	; Decrement remaining loops
	lda Sq1SubTrackStart,x	; \ Go back to start of sub track
	sta Sq1TrackOffset,x	; /
	bne GetNextTrackData	; Load next byte from start of sub track
StartSubTrack:
	tya						; \
	and #$3f				; | (Lower 6 bits of data - 1) byte becomes repeat count
	sta Sq1SubTrackLoops,x	; |
	dec Sq1SubTrackLoops,x	; /
	lda Sq1TrackOffset,x	; \ Set start point of sub track
	sta Sq1SubTrackStart,x	; /
GetNextTrackData:
	jmp LoadTrackDataLoop	; Load next byte from data and parse it

ContinueNoteDataCheck:
	tya	; Load data byte again
	bpl ByteIsNoteData	; If uppermost bit isn't set, it's a note
	and #$0f		; \
	cadc TrackTempo	; | Y = Tempo + Lower 4 bits of data byte
	tay				; /
	lda NoteLengthOptions,y	; \ Change next note length
	sta Sq1NoteLength,x		; /
	tay	; Y = New note length
	txa							; \
	cmp #2						; | If current channel is Triangle ???
	beq TriangleLengthChange	; /
LoadByteAfterLengthUpd:
	ldy Sq1TrackOffset,x	; \
	inc Sq1TrackOffset,x	; | Increment offset and load next byte
	lda (CurTrackPointer),y	; / Then immediately parse that note too
ByteIsNoteData:
	tay	; Y = Note data
	txa					; \
	cmp #3				; | If current channel is Noise
	beq PlayNoiseNote	; /
	pha	; Push current channel to stack
	tax					; \
	cmp #1				; | If current channel is Square 2, check if it's free
	beq Sq2BusyCheck	; /
ChannelIsFree:
	ldx SoundRegOffset
	lda NotePitchData+1,y	; Load lower byte of pitch
	beq @SkipPitchSet	; If 00, 
	sta SQ1_LO,x	; Write lower byte of pitch
	lda NotePitchData,y	; \
	ora #8				; | Write upper byte of pitch with L = 1 (Infinite length)
	sta SQ1_HI,x		; /
	@SkipPitchSet:
	tay	; \ Preserve upper pitch
	plx	; |	X = current channel
	tya	; /
	bne SetChannelVolumeToDC
	ldy #0
	txa
	cmp #2
	beq SetChannelVolumeContinue
	ldy #16
	bne SetChannelVolumeContinue
SetChannelVolumeToDC:
	ldy $dc,x
SetChannelVolumeContinue:
	tya
	ldy SoundRegOffset
	sta SQ1_VOL,y
SetCountdownThenContinue:
	lda Sq1NoteLength,x	; \ Set new note's countdown to defined length
	sta Sq1Countdown,x	; /
	jmp NextChannel

Sq2BusyCheck:
	lda SFX2Cur
	and #2
	beq ChannelIsFree
	plx
	jmp SetCountdownThenContinue

TriangleLengthChange:
	tya
	ldy UnknownSoundFlag
	beq @lf5ce
	lda #$ff
	bne @SetLinear
	@lf5ce:
	cadc #<-2
	aslr 2
	cmp #$3C		; \
	bcc @SetLinear	; | Cap at $3C
	lda #$3C		; /
	@SetLinear:
	sta TRI_LINEAR
	sta $de
	jmp LoadByteAfterLengthUpd

PlayNoiseNote:
	lda SFX1Cur	; \
	cmp #2		; | Don't modify if Pop SFX is playing
	beq @Skip	; /
	lda NoiseNoteSettings,y		; \
	sta NOISE_VOL				; |
	lda NoiseNoteSettings+1,y	; |
	sta NOISE_LO				; |
	lda NoiseNoteSettings+2,y	; |
	sta NOISE_HI				; /
	@Skip:
		jmp SetCountdownThenContinue

BubbleRiseSFXSq1:
	.BYTE $16,$ff,$10,$c5

NotePitchData:	; HI, LO
	.BYTE $07,$f0	; 00: A  1
	.BYTE $00,$00	; 02: Silent

	.BYTE $00,$d4	; 04: C  5
	.BYTE $00,$c8	; 06: C# 5
	.BYTE $00,$bd	; 08: D  5
	.BYTE $00,$b2	; 0A: D# 5
	.BYTE $00,$a8	; 0C: E  5
	.BYTE $00,$9f	; 0E: F  5
	.BYTE $00,$96	; 10: F# 5
	.BYTE $00,$8d	; 12: G  5
	.BYTE $00,$85	; 14: G# 5
	.BYTE $00,$7e	; 16: A  5
	.BYTE $00,$76	; 18: A# 5
	.BYTE $00,$70	; 1A: B  5

	.BYTE $01,$ab	; 1C: C  4
	.BYTE $01,$93	; 1E: C# 4
	.BYTE $01,$7c	; 20: D  4
	.BYTE $01,$67	; 22: D# 4
	.BYTE $01,$52	; 24: E  4
	.BYTE $01,$3f	; 26: F  4
	.BYTE $01,$2d	; 28: F# 4
	.BYTE $01,$1c	; 2A: G  4
	.BYTE $01,$0c	; 2C: G# 4
	.BYTE $00,$fd	; 2E: A  4
	.BYTE $00,$ee	; 30: A# 4
	.BYTE $00,$e1	; 32: B  4

	.BYTE $03,$57	; 34: C  3
	.BYTE $03,$27	; 36: C# 3
	.BYTE $02,$f9	; 38: D  3
	.BYTE $02,$cf	; 3A: D# 3
	.BYTE $02,$a6	; 3C: E  3
	.BYTE $02,$80	; 3E: F  3
	.BYTE $02,$5c	; 40: F# 3
	.BYTE $02,$3a	; 42: G  3
	.BYTE $02,$1a	; 44: G# 3
	.BYTE $01,$fc	; 46: A  3
	.BYTE $01,$df	; 48: A# 3
	.BYTE $01,$c4	; 4A: B  3

	.BYTE $03,$f8	; 4C: A  2

	.BYTE $00,$69	; 4E: C  6
	.BYTE $00,$63	; 50: C# 6
	.BYTE $00,$5e	; 52: D  6
	.BYTE $00,$58	; 54: D# 6
	.BYTE $00,$53	; 56: E  6
	.BYTE $00,$4f	; 58: F  6
	.BYTE $00,$4a	; 5A: F# 6
	.BYTE $00,$46	; 5C: G  6
	.BYTE $00,$42	; 5E: G# 6

NoteLengthOptions:
	; Tempo 0
		.BYTE 3,6,12,24,48		; 0 - 4 
		.BYTE 18,36,9,8,4,7,1	; 5 - 11
	; Tempo 12
		.BYTE 4,8,16,32,64	; 0 - 4
		.BYTE 24,48,12,1	; 5 - 8
	; Tempo 21
		.BYTE 6,12,24,48,96				; 0 - 4
		.BYTE 36,72,18,16,8,14,2,3,4	; 5 - 13

WriteSq1XY:					; \
	lda #<SQ1_VOL			; |
	beq WriteChannelDataA	; |
WriteTriXY:					; | Different entry points for each channel
	lda #<TRI_LINEAR		; | 
	bne WriteChannelDataA	; |
WriteNoiseXY:				; | Determines lower byte of SndDataTargetPtr
	lda #<NOISE_VOL			; |
	bne WriteChannelDataA	; |
WriteSq2:					; |
	lda #<SQ2_VOL			; /
WriteChannelDataA:
	sta SndDataTargetPtrLo	; Write lower byte of pointer
	lda #>SQ1_VOL			; \ Write upper byte of pointer
	sta SndDataTargetPtrHi	; / All $40
	stx SndDataSourcePtrLo	; \ Get data from address in YX
	sty SndDataSourcePtrHi	; /
	ldy #0
	@WriteLoop:
		lda (SndDataSourcePtr),y	; \
		sta (SndDataTargetPtr),y	; |
		iny							; | Load 4 bytes from Source Pointer to Target Pointer
		tya							; |
		cmp #4						; /
		bne @WriteLoop
	rts

LoadSoundSequence:
	tax						; \
	jsr InitNeededChannels	; | Initialize Sound Channels
	stx MusicCur			; / and Sound Variables
	lda PopParachuteReq	; \
	beq @LoadData		; | Check PopParachuteReq == $00 or $02
	cmp #$02			; |
	bne @LoadData		; /
	sta SFX1Req		; SFX1Req = [PopParachuteReq] (!= 00 or 02)
	lda #0				; \
	sta PopParachuteReq	; / Clear [PopParachuteReq]
	@LoadData:
	lda MusicTrackInitData,y	; \ Load Sound Sequence Pointer to Y
	tay							; /
	ldx #0							; \
	@MusicInitLoop:
		lda MusicTrackInitData,y	; |
		sta TrackTempo,x			; | Load Sound Sequence Header
		iny							; | (9 bytes)
		inx							; | Track Tempo, then pointers for Sq1-Noise Tracks
		txa							; |
		cmp #9						; |
		bne @MusicInitLoop			; /

	lda #1					; \
	sta Sq1Countdown		; |
	sta Sq2Countdown		; |
	sta TriCountdown		; | Initialize Sequence stuff
	sta NoiseCountdown		; |
	lda #0					; |
	sta Sq1TrackOffset		; |
	sta Sq2TrackOffset		; |
	sta TriTrackOffset		; |
	sta NoiseTrackOffset	; /
	rts

FlapSq1:
	.BYTE $94,$ab,$fd,$58
FootstepNoise:
	.BYTE $00,$7f,$04,$18
LightningStrikeNoise:
	.BYTE $3f,$7f,$00,$00
SplashPopNoise:
	.BYTE $06,$7f,$0a,$c0
SplashNoise2:
	.BYTE $08,$7f,$05,$c0
ShockedSqBase:
	.BYTE $c1,$89,$02,$0f
lf6fc:
	.BYTE $ff,$ff,$ff	; Unused
NoiseNoteSettings:	; Vol, Lo, Hi
	.BYTE $10,$00,$18	; 0
	.BYTE $10,$01,$18	; 3
	.BYTE $00,$01,$88	; 6: Soft snare
	.BYTE $02,$02,$40	; 9
	.BYTE $03,$05,$40	; C
	.BYTE $04,$07,$40	; F: Hard hit

ClearSquareSweeps:
	lda #$7f		; \ Set Pulse Channels:
	sta SQ1_SWEEP	; | No Sweep
	sta SQ2_SWEEP	; /
StoreSoundXY:
	stx $dc
	sty $dd
	rts

PlayFlapSFX:
	ldxy FlapSq1
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

lf749:
	inc BubbleRiseSFXTimer
	lda BubbleRiseSFXTimer
	cmp #$58
	bne :+	;RTS
	lda #0
	sta SFX3Cur
	rts

AudioMain:
	lda #$c0		; \ Set Frame Counter
	sta APU_FRAME	; / to 4-step sequence, clear frame interrupt flag
	jsr ManageMusic
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
	:rts

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
	and #$0f				; \ Initialize Sound Channels
	cmp #$0f				; | differently depending on
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

InitSquare2Noise:
	lda #$10		; \ Constant volume on:
	sta SQ2_VOL		; | - Pulse 2 Channel
	sta NOISE_VOL	; / - Noise Channel
	lda #0
	beq ResetCurrentSounds
ResetSplashSFXPhase:
	lda #0
	sta SplashSFXPhase
	rts

PlaySplashNoise2:
	ldxy SplashNoise2
	jmp WriteNoiseRTS
CheckSplashCountdown:
	inc SplashSFXTimer
	lda SplashSFXTimer
	cmp #$10
	beq PlaySplashNoise2
	cmp #$20
	beq ResetSplashSFXPhase
	rts

PlaySplashNoise:
	lda #0
	sta SplashSFXTimer
	lda #$f0
	sta SplashSFXPhase
	ldxy SplashPopNoise
	jmp WriteNoiseRTS
PlayPopNoise:
	lda SFX1Cur
	and #$f0
	ora #2
	sta SFX1Cur
	lda #0
	sta PopSFXCountdown
	ldxy SplashPopNoise
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
	lsrr 2
	bcs PlayPopNoise
	lsr
	bcs TryLightningStrikeSFX
	lsr
	bcs GotoTryFootstepNoise
	lda SFX1Cur
	lsrr 2
	bcs CheckPopCountdown
	lsr
	bcs CheckLightningSFXCountdown
	:rts

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
	ldxy LightningStrikeNoise
WriteNoiseRTS:
	jsr WriteNoiseXY
	rts

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
	:rts

WriteNoisePitch:
	sta NOISE_LO
	rts

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

PlayShocked:
	jsr InitAllSoundMemory
	lda #$80
	sta SFX1Cur
	lda #2
	sta SFX1Req
lf8e8:
	ldxy ShockedSqBase
	jsr WriteSq1XY
	lda RNGOutput
	and #$0f
	sta SQ1_LO
	ldxy ShockedSqBase
	jsr WriteSq2
	lda RNGOutput
	lsrr 2
	and #$0f
	sta SQ2_LO
	rts

lf907:
	jmp PlaySparkBounceSFX

lf90a:
	lda MusicCur	; \ Check if music is not playing
	beq lf91b		; / If not playing then continue as normal
	cmp #$df	; \ Songs #$DF?
	beq lf91b	; / Wouldn't that be redundant?
	lda SFX1Req	; \
	and #$e0	; | Check for sound effects that stops the music
	beq :+		; / if found, then return
	jsr InitAllSoundMemory
lf91b:
	lda SFX1Req
	asl
	bcs PlayShocked
	asl
	bcs PlaySplash
	asl
	bcs PlayFalling
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
	:rts

lf94f:
	jmp lf8bf
PlaySplash:
	lda #$0f
	sta SplashSFXPhase
	lda SFX1Cur
	and #$0f
	ora #$40
	sta SFX1Cur
	ldxy lf9d1
	bne lf98f

PlayFalling:
	lda #2		; \ Also request pop sfx
	sta SFX1Req	; /
	lda SFX1Cur	; \
	and #$0f	; | Preserve End SFX, Pop, Lightning Strike, and Player Footstep
	ora #$20	; | Add in Falling to current SFX1
	sta SFX1Cur	; /
	ldxy FallingSq1
	bne lf98f
lf977:
	lda #0
	sta ChompSFXTimer
	lda SFX3Cur
	and #0
	ora #$40
	sta SFX3Cur
	ldxy lf9d5
	jsr WriteSq2
	ldxy lf9d9
lf98f:
	jsr WriteSq1XY
	rts

lf993:
	inc ChompSFXTimer
	lda ChompSFXTimer
	cmp #$12
	beq lf9ca
	cmp #6
	bcc lf9b1
	lda RNGOutput
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
	sta SQ2_LO
	lda FishChompPitchSq1
	sta SQ1_LO
	rts

lf9ca:
	jmp lf8ca

FallingSq1:
	.BYTE $b8,$d5,$20,$00
lf9d1:
	.BYTE $9f,$93,$80,$22
lf9d5:
	.BYTE $3f,$ba,$e0,$06
lf9d9:
	.BYTE $3f,$bb,$ce,$06
lf9dd:
	.BYTE $b8,$93,$50,$02
lf9e1:
	.BYTE $80,$7f,$60,$68
lf9e5:
	.BYTE $80,$7f,$62,$68

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
	ldxy lf9dd
	bne lfa7f
lfa0c:
	inc BumpSFXTimer
	lda BumpSFXTimer
	cmp #7
	bne :+
	lda #$7f
	sta SQ2_SWEEP
	lda #$10
	sta SQ2_VOL
	lda SFX2Cur
	and #$e0
	sta SFX2Cur
	:rts

lfa27:
	jsr InitAllSoundMemory
	ldxy lf9e1
	jsr WriteSq1XY
	ldxy lf9e5
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
	bcs PlayBubbleCollect
	lsrr 2
	bcs lfa64
	lda SFX2Cur
	lsrr 2
	bcs lfa0c
	:rts

lfa64:
	lda MusicCur
	bne :-
	lda SFX2Cur
	and #2
	bne :-
	ldxy TweetSq2Base
	jsr WriteSq2
	lda RNGOutput
	and #$3f
	ora #$10
	sta SQ2_LO
	rts

lfa7f:
	jsr WriteSq2
	rts

PlayBubbleCollect:
	ldy #10
	lda #$ef
	jmp lfba5

TweetSq2Base:
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
	bcs PlayEnemyDown
	asl
	bcs EnemyLandingSFX
	lda SFX2Cur
	aslr 2
	bcs lfa96
	lda SFX2Req
	and #$20
	beq lfad9
	lda MusicCur
	beq PlayParachuting
	:rts

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
	ldxy EnemyLandTri1
WriteTriRTS:
	jsr WriteTriXY
	rts

PlayParachuting:	; Sequenced SFX: Parachuting
	lda LastSFX2Req
	and #$20
	bne @SkipPopReq
	lda #2
	sta PopParachuteReq
	@SkipPopReq:
	ldy #$08
	lda #$df
	jmp lfba5

PlayEnemyDown:	; Sequenced SFX: Enemy Down
	ldy #4
	lda #$7f
	jmp lfba5

PlayPhaseClear:		; Music/Jingle: Stage Clear
	ldy #0
	lda #$02
	jmp lfbc1

ManageMusic:
	lda TripMusicFlag	; \ Play Balloon Trip Music
	bne PlayBonusTrip	; /
	lda MusicReq		; \ Play Music/Jingle:
	lsr					; |
	bcs PlayGameOver	; | #$01 = Game Over
	lsr					; |
	bcs PlayPhaseClear	; | #$02 = Stage Clear
	lsr					; |
	bcs PlayPause		; | #$04 = Pause
	lsr					; |
	bcs PlayNewStart	; | #$08 = Stage Start
	lsr					; |
	bcs PlaySuperBonus	; | #$10 = Bonus Phase Perfect
	lsr					; |
	bcs PlayBonusTrip	; | #$20 = Balloon Trip / Bonus Phase Music
	lsr					; |
	bcs PlayEatenByFish	; | #$40 = Fish
	lsr					; |
	bcs PlayRespawn		; / #$80 = Respawn
	lda MusicCur			; \ If no new requests, and music is playing,
	bne ContinueMusicUpdate	; / Continue current Music/Jingle
	rts	; Return if no music was requested & none is playing
ContinueMusicUpdate:
	jmp ContinuePlayingMusic

PlayPause:
	ldy #2
	lda #$04
	bne lfba5
PlayRespawn:
	ldy #9
	lda #$80
	bne lfb6d
PlayEatenByFish:
	ldy #7
	lda #$40
	bne lfb6d
PlayBonusTrip:	; Balloon Trip / Bonus Phase
	lda #0
	sta TripMusicFlag
	ldy #6
	lda #$20
	bne lfbc1
PlaySuperBonus:		; Music/Jingle: Bonus Game Perfect
	ldy #5
	lda #$10
lfb6d:
	jsr LoadSoundSequence
	ldx #$fc
	ldy #$fc
	jsr ClearSquareSweeps
	inc UnknownSoundFlag
	bne ContinueMusicUpdate
PlayNewStart:
	ldy #3
	lda #$08
	bne lfb86
	
PlayGameOver:
	ldy #1
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
	beq ContinueMusicUpdate
	lda #$d5
	sta SQ1_SWEEP
	bne ContinueMusicUpdate
lfbc1:
	jsr LoadSoundSequence
	ldx #$80
	ldy #$ba
	bne lfb8d

;----------------------
; Music Data:
;----------------------

MusicTrackInitData: ;Music Track Init Data
	; Offsets to each track's init data
		.BYTE PhaseClearInitData - MusicTrackInitData
		.BYTE GameOverInitData - MusicTrackInitData
		.BYTE PauseInitData - MusicTrackInitData
		.BYTE NewStartInitData - MusicTrackInitData
		.BYTE EnemyDownInitData - MusicTrackInitData
		.BYTE SuperBonusInitData - MusicTrackInitData
		.BYTE BonusTripInitData - MusicTrackInitData
		.BYTE EatenByFishInitData - MusicTrackInitData
		.BYTE ParachuteInitData - MusicTrackInitData
		.BYTE RespawnInitData - MusicTrackInitData
		.BYTE BubbleCollectInitData - MusicTrackInitData

	PhaseClearInitData: ;Phase Clear Music Init Data
		.BYTE 12
		.WORD PhaseClearSq1,PhaseClearSq2,PhaseClearTri,PhaseClearNoise
	GameOverInitData: ;Game Over Music Init Data
		.BYTE 21
		.WORD GameOverSq1,GameOverSq2,GameOverTri,GameOverNoise
	PauseInitData: ;Pause Music Init Data
		.BYTE 12
		.WORD PauseSq1,$0000,PauseTri,$0000
	NewStartInitData: ;New Start Music Init Data
		.BYTE 21
		.WORD NewStartSq1,NewStartSq2,NewStartTri,NewStartNoise
	EnemyDownInitData: ;Enemy Down Music Init Data
		.BYTE 0
		.WORD $0000,EnemyDownSq2,EnemyDownTri,$0000
	SuperBonusInitData: ;Super Bonus Music Init Data
		.BYTE 0
		.WORD SuperBonusSq1,SuperBonusSq2,SuperBonusTri,SuperBonusNoise
	BonusTripInitData: ;Bonus Trip Music Init Data
		.BYTE 21
		.WORD BonusTripSq1,BonusTripSq2,BonusTripTri,BonusTripNoise
	EatenByFishInitData: ;Eaten By Fish Music Init Data
		.BYTE 21
		.WORD EatenByFishSq1,$0000,EatenByFishTri,$0000
	ParachuteInitData: ;Parachute Music Init Data
		.BYTE 21
		.WORD $0000,ParachuteSq2,ParachuteTri,$0000
	RespawnInitData: ;Respawn Music Init Data
		.BYTE 12
		.WORD RespawnSq1,RespawnSq2,RespawnTri,$0000
	BubbleCollectInitData: ;Bubble Collect Music Init Data
		.BYTE 0
		.WORD $0000,BubbleCollectSq2,BubbleCollectTri,$0000

BubbleCollectSq2:
	.BYTE $82,$02
	.BYTE $8b,$02
	.BYTE $80,$08,$02,$10,$02,$16,$02,$52,$02,$02,$02,$1a
	.BYTE $00
BubbleCollectTri:
	.BYTE $82,$02
	.BYTE $80,$10,$02,$16,$02,$52,$02,$5a,$02,$02,$02,$56,$81,$02

RespawnSq1:
	.BYTE $80,$12,$02,$0c,$02,$04,$02,$0c,$02,$04,$02,$2a,$02
	.BYTE $81,$04,$02
	.BYTE $80,$04,$02
	.BYTE $81,$04
	.BYTE $88,$02,$02
	.BYTE $00
RespawnSq2:
	.BYTE $88,$02,$02
	.BYTE $80,$04,$02,$2a,$02,$24,$02,$2a,$02,$24,$02,$1c,$02
	.BYTE $81,$22,$02
	.BYTE $80,$22,$02
	.BYTE $81,$24
	.BYTE $88,$02
RespawnTri:
	.BYTE $88,$02
	.BYTE $80,$56,$02,$4e,$02,$12,$02,$4e,$02,$12,$02,$0c,$02
	.BYTE $81,$10,$02
	.BYTE $80,$10,$02
	.BYTE $81,$12
	.BYTE $88,$02

BonusTripSq1:
	.BYTE $c3	; Repeat 3 times
		.BYTE $81,$02
			.BYTE $02
			.BYTE $1c	; C 4
			.BYTE $02
			.BYTE $02
			.BYTE $02
			.BYTE $1c	; C 4
			.BYTE $1c	; C 4
		.BYTE $ff
	.BYTE $c6	; Repeat 6 times
		.BYTE $88,$1c	; C 4
		.BYTE $ff
	.BYTE $c7	; Repeat 7 times
		.BYTE $82,$4c	; A 2
			.BYTE $4c	; A 2
			.BYTE $2a	; G 4
			.BYTE $4c	; A 2
		.BYTE $ff
	.BYTE $c6	; Repeat 6 times
		.BYTE $88,$1c	; C 4
		.BYTE $ff
	.BYTE $c4	; Repeat 4 times
		.BYTE $81,$46	; A 3
			.BYTE $02
			.BYTE $46	; A 3
			.BYTE $02
			.BYTE $32	; B 4
			.BYTE $02
			.BYTE $46	; A 3
		.BYTE $80,$2e	; A 4
			.BYTE $2e	; A 4
		.BYTE $ff
	.BYTE $c3	; Repeat 3 times
		.BYTE $82,$46	; A 3
			.BYTE $46	; A 3
		.BYTE $81,$32	; B 4
			.BYTE $32	; B 4
			.BYTE $46	; A 3
			.BYTE $2e	; A 4
		.BYTE $ff
	.BYTE $80,$0c	; E 5
		.BYTE $0c	; E 5
	.BYTE $81,$46	; A 3
		.BYTE $46	; A 3
		.BYTE $46	; A 3
	.BYTE $80,$04	; C 5
		.BYTE $04	; C 5
	.BYTE $81,$46	; A 3
		.BYTE $46	; A 3
		.BYTE $02
	.BYTE $c8	; Repeat 8 times
		.BYTE $82,$4c	; A 2
			.BYTE $4c	; A 2
			.BYTE $2a	; G 4
			.BYTE $4c	; A 2
		.BYTE $ff
	.BYTE $c2	; Repeat 2 times
		.BYTE $81,$46	; A 3
		.BYTE $80,$32	; B 4
			.BYTE $32	; B 4
		.BYTE $82,$46	; A 3
			.BYTE $04	; C 5
		.BYTE $81,$46	; A 3
			.BYTE $2a	; G 4
		.BYTE $ff
	.BYTE $c2	; Repeat 2 times
		.BYTE $81,$0c	; E 5
			.BYTE $0c	; E 5
		.BYTE $80,$04	; C 5
			.BYTE $04	; C 5
		.BYTE $81,$04	; C 5
		.BYTE $80,$2e	; A 4
			.BYTE $2e	; A 4
		.BYTE $81,$2e	; A 4
		.BYTE $82,$24	; E 4
		.BYTE $ff
	.BYTE $00
BonusTripSq2:
	.BYTE $81,$32	; B 4
		.BYTE $02
		.BYTE $02
		.BYTE $06	; C#5
		.BYTE $0c	; E 5
		.BYTE $32	; B 4
		.BYTE $02
		.BYTE $02
	.BYTE $8a,$2e	; A 4
	.BYTE $8b,$02
	.BYTE $8a,$2e	; A 4
	.BYTE $8b,$02
	.BYTE $8a,$2e	; A 4
	.BYTE $8b,$02
	.BYTE $88,$2e	; A 4
		.BYTE $32	; B 4
		.BYTE $2e	; A 4
	.BYTE $d0	; Repeat 16 times
		.BYTE $8c,$2c	; G#4
			.BYTE $24	; E 4
		.BYTE $ff
	.BYTE $d0	; Repeat 16 times
		.BYTE $2e	; A 4
		.BYTE $20	; D 4
		.BYTE $ff
	.BYTE $c3	; Repeat 3 times
		.BYTE $80,$28	; F#4
			.BYTE $02
		.BYTE $82,$02
		.BYTE $80,$2c	; G#4
			.BYTE $02
			.BYTE $32	; B 4
			.BYTE $02
			.BYTE $24	; E 4
			.BYTE $02
		.BYTE $82,$02
		.BYTE $81,$02
		.BYTE $80,$28	; F#4
			.BYTE $02
			.BYTE $06	; C#5
			.BYTE $02
			.BYTE $28	; F#4
			.BYTE $02
		.BYTE $81,$02
		.BYTE $80,$24	; E 4
			.BYTE $02
			.BYTE $32	; B 4
			.BYTE $02
			.BYTE $24	; E 4
			.BYTE $02
		.BYTE $ff
	.BYTE $80,$28	; F#4
		.BYTE $02
	.BYTE $82,$02
	.BYTE $80,$2c
		.BYTE $02
		.BYTE $32	; B 4
		.BYTE $02
		.BYTE $24	; E 4
		.BYTE $02
	.BYTE $82,$02
	.BYTE $89,$0c
		.BYTE $0a
		.BYTE $08
		.BYTE $06
		.BYTE $32	; B 4
		.BYTE $30
		.BYTE $2e
		.BYTE $2c
		.BYTE $2a
		.BYTE $28	; F#4
		.BYTE $26
		.BYTE $24	; E 4
		.BYTE $02
		.BYTE $02
		.BYTE $02
	.BYTE $86,$02
	.BYTE $c7
		.BYTE $84,$02
		.BYTE $ff
	.BYTE $c4	; Repeat 4 times
		.BYTE $80,$28	; F#4
			.BYTE $02
		.BYTE $82,$02
		.BYTE $80,$2c	; G#4
			.BYTE $02
			.BYTE $32	; B 4
			.BYTE $02
			.BYTE $24	; E 4
			.BYTE $02
		.BYTE $82,$02
		.BYTE $81,$02
		.BYTE $80,$28	; F#4
			.BYTE $02
			.BYTE $06	; C#5
			.BYTE $02
			.BYTE $28	; F#4
			.BYTE $02
		.BYTE $81,$02
		.BYTE $80,$24	; E 4
			.BYTE $02
			.BYTE $32	; B 4
			.BYTE $02
			.BYTE $24	; E 4
			.BYTE $02
		.BYTE $ff
	.BYTE $c8
		.BYTE $84,$02
		.BYTE $ff
BonusTripTri:
	.BYTE $81,$14,$02,$02,$14,$1a,$14,$02,$02
	.BYTE $88,$10,$10,$10,$10,$14,$10
	.BYTE $85,$3c
	.BYTE $81,$44
	.BYTE $85,$4a
	.BYTE $81,$44
	.BYTE $88,$28,$24,$20,$46,$42,$40
	.BYTE $c6
		.BYTE $81,$3c,$02,$02,$44,$02,$02,$02,$4a,$02,$46,$36,$36,$38,$38,$02,$3a,$02
		.BYTE $80,$3c,$3c
		.BYTE $81,$02,$24,$02,$02,$2c,$24
		.BYTE $88,$24,$1e,$46,$36,$38,$3a
		.BYTE $ff
	.BYTE $c4
		.BYTE $84,$02
		.BYTE $ff
BonusTripNoise:
	.BYTE $d8
		.BYTE $81,$06
		.BYTE $ff
	.BYTE $c6
		.BYTE $88,$06
		.BYTE $ff
	.BYTE $c7
		.BYTE $81,$06
			.BYTE $06
		.BYTE $80,$06
			.BYTE $06
		.BYTE $81,$06
			.BYTE $06
		.BYTE $80,$06
			.BYTE $06
		.BYTE $81,$06
			.BYTE $06
		.BYTE $ff
	.BYTE $c6
		.BYTE $88,$06
		.BYTE $ff
	.BYTE $e0
		.BYTE $81,$06
			.BYTE $06
		.BYTE $ff
	.BYTE $82,$0f
	.BYTE $81,$06
		.BYTE $06
	.BYTE $ea
		.BYTE $06,$06
			.BYTE $06
			.BYTE $06
		.BYTE $ff
		
PauseSq1:
	.BYTE $c5	; Repeat 5 times
		.BYTE $80,$0e	; F 5
			.BYTE $58	; F 6
		.BYTE $ff
	.BYTE $00
PauseTri:
	.BYTE $c5	; Repeat 5 times
		.BYTE $80,$0e	; F 4
			.BYTE $58	; F 5
		.BYTE $ff

GameOverSq1:
	.BYTE $82,$1c	; C 4
		.BYTE $1c	; C 4
	.BYTE $c3
		.BYTE $82,$1c	; C 4
			.BYTE $1c	; C 4
		.BYTE $81,$1c	; C 4
			.BYTE $1c	; C 4
			.BYTE $1c	; C 4
			.BYTE $02
		.BYTE $ff
	.BYTE $c7
		.BYTE $88,$1c	; C 4
		.BYTE $ff
	.BYTE $00
GameOverSq2:
	.BYTE $83,$02
	.BYTE $80,$0e	; F 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $4e	; C 6
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $4e	; C 6
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $4e	; C 6
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $0e	; F 5
		.BYTE $02
		.BYTE $0c	; E 5
		.BYTE $02
	.BYTE $88,$4e	; C 6
		.BYTE $18	; A#5
		.BYTE $16	; A 5
		.BYTE $12	; G 5
		.BYTE $0e	; F 5
		.BYTE $0c	; E 5
		.BYTE $0e	; F 5
GameOverTri:
	.BYTE $83,$02
	.BYTE $81,$3e
		.BYTE $3e
	.BYTE $82,$46
		.BYTE $1c
		.BYTE $46
	.BYTE $81,$02
		.BYTE $38
		.BYTE $3e
		.BYTE $02
	.BYTE $82,$46
		.BYTE $1c
	.BYTE $82,$48
		.BYTE $48
	.BYTE $81,$3e
		.BYTE $3e
	.BYTE $82,$38
	.BYTE $88,$24
		.BYTE $20
		.BYTE $1c
		.BYTE $48
		.BYTE $46
		.BYTE $42
		.BYTE $3e
GameOverNoise:
	.BYTE $82,$09
		.BYTE $09
	.BYTE $c6
		.BYTE $82,$03
			.BYTE $0c
		.BYTE $ff
	.BYTE $c6
		.BYTE $88,$06
		.BYTE $ff

ParachuteSq2:
	.BYTE $ed
		.BYTE $89,$2a
			.BYTE $02
			.BYTE $04
			.BYTE $0c
			.BYTE $02
			.BYTE $04
			.BYTE $08
			.BYTE $02
			.BYTE $30
			.BYTE $26
			.BYTE $02
			.BYTE $30
		.BYTE $ff
ParachuteTri:
	.BYTE $80,$02
	.BYTE $ed
		.BYTE $89,$0c
			.BYTE $02
			.BYTE $12
			.BYTE $4e
			.BYTE $02
			.BYTE $12
			.BYTE $18
			.BYTE $02
			.BYTE $0e
			.BYTE $08
			.BYTE $02
			.BYTE $0e
		.BYTE $ff

EatenByFishSq1:
	.BYTE $80,$42
		.BYTE $02
		.BYTE $48
		.BYTE $02
		.BYTE $1e
		.BYTE $02
		.BYTE $24
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $2a
		.BYTE $02
	.BYTE $c6
		.BYTE $8c,$30
			.BYTE $2a
		.BYTE $ff
	.BYTE $00
EatenByFishTri:
	.BYTE $80,$24
		.BYTE $02
		.BYTE $2a
		.BYTE $02
		.BYTE $30
		.BYTE $02
		.BYTE $06
		.BYTE $02
		.BYTE $02
		.BYTE $02
		.BYTE $0c
		.BYTE $02
	.BYTE $c6
		.BYTE $8c,$12
			.BYTE $18
		.BYTE $ff

EnemyDownSq2:
	.BYTE $80,$56
		.BYTE $54
		.BYTE $52
		.BYTE $50
	.BYTE $81,$02
	.BYTE $80,$5e
		.BYTE $5a
		.BYTE $54
		.BYTE $50
		.BYTE $18
		.BYTE $14
		.BYTE $10
		.BYTE $0a
		.BYTE $06
		.BYTE $30
		.BYTE $2c
		.BYTE $28
		.BYTE $02
	.BYTE $00
EnemyDownTri:
	.BYTE $80,$1a
		.BYTE $18
		.BYTE $16
		.BYTE $14
	.BYTE $81,$02
	.BYTE $80,$02
		.BYTE $5e
		.BYTE $5a
		.BYTE $54
		.BYTE $50
		.BYTE $18
		.BYTE $14
		.BYTE $10
		.BYTE $0a
		.BYTE $06
		.BYTE $30
		.BYTE $2c
		.BYTE $28

PhaseClearSq1:
	.BYTE $82,$1c
		.BYTE $02
		.BYTE $1c
		.BYTE $02
		.BYTE $02
		.BYTE $1c
		.BYTE $1c
	.BYTE $00
PhaseClearSq2:
	.BYTE $81,$10
		.BYTE $0a
		.BYTE $32
		.BYTE $28
	.BYTE $80,$32
		.BYTE $02
		.BYTE $32
		.BYTE $02
	.BYTE $82,$32
	.BYTE $81,$06
		.BYTE $02
		.BYTE $06
		.BYTE $02
	.BYTE $82,$32
PhaseClearTri:
	.BYTE $81,$54
		.BYTE $1a
		.BYTE $10
		.BYTE $0a
	.BYTE $80,$10
		.BYTE $02
		.BYTE $10
		.BYTE $02
	.BYTE $82,$10
	.BYTE $81,$16
		.BYTE $02
		.BYTE $16
		.BYTE $02
	.BYTE $82,$0a
PhaseClearNoise:
	.BYTE $83,$03
		.BYTE $0c
	.BYTE $82,$03
		.BYTE $0c
		.BYTE $0c

NewStartSq1:
	.BYTE $c2
		.BYTE $88,$1c,$1c,$1c,$1c,$1c,$1c
		.BYTE $83,$1c
		.BYTE $80,$04,$04,$2a,$02
		.BYTE $82,$1c
		.BYTE $ff
	.BYTE $81,$4c,$02,$4c,$02,$2a,$02,$4c,$1c
	.BYTE $81,$4c,$02,$4c,$02,$4c
	.BYTE $00
NewStartSq2:
	.BYTE $88,$2e,$2e,$2e,$30,$04,$30
	.BYTE $c4
		.BYTE $80,$2e,$04
		.BYTE $ff
	.BYTE $83,$02
	.BYTE $88,$2e,$2e,$2e,$30,$04,$30
	.BYTE $c4
		.BYTE $80,$2e,$04
		.BYTE $ff
	.BYTE $83,$02
	.BYTE $84,$02,$02
NewStartTri:
	.BYTE $c2
		.BYTE $88,$3e,$3e,$3e,$42,$46,$42
		.BYTE $84,$3e
		.BYTE $ff
	.BYTE $85,$3e
	.BYTE $81,$3e
	.BYTE $88,$1c,$46,$1c
	.BYTE $81,$02,$3e,$3e,$3e
	.BYTE $82,$34,$02
NewStartNoise:
	.BYTE $c2
		.BYTE $88,$06,$06,$06,$06,$06,$06
		.BYTE $82,$06,$06,$06,$06
		.BYTE $ff
	.BYTE $c2
		.BYTE $81,$06,$06
		.BYTE $80,$06,$06
		.BYTE $81,$06,$06,$06,$06
		.BYTE $80,$06,$06
		.BYTE $ff
	.BYTE $09

SuperBonusSq1:
	.BYTE $80,$10,$02,$10,$02,$10,$02,$0c,$0c,$0c,$02,$0c,$02,$14,$14,$14,$02,$14,$02
	.BYTE $85,$10,$00
SuperBonusSq2:
	.BYTE $80,$32,$02,$32,$02,$32,$02
	.BYTE $c2
		.BYTE $32,$32,$32,$02,$32,$02
		.BYTE $ff
	.BYTE $85,$32
SuperBonusTri:
	.BYTE $80,$54,$02,$54,$02,$54,$02,$50,$50,$50,$02,$50,$02,$56,$56,$56,$02,$56,$02
	.BYTE $85,$54
SuperBonusNoise:
	.BYTE $c4
		.BYTE $85,$0c
		.BYTE $ff

.BYTE $ff,$ff,$ff,$ff	; Unused

GotoAudioMain:
	jmp AudioMain