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
	ldy NewSq1Vol,x
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
	sta NOISE_VOL				; | Write noise note parameters based on index
	lda NoiseNoteSettings+1,y	; |
	sta NOISE_LO				; |
	lda NoiseNoteSettings+2,y	; |
	sta NOISE_HI				; /
	@Skip:
		jmp SetCountdownThenContinue

BubbleRiseSFXSq1:
	.BYTE $16,$ff,$10,$c5

NotePitchData:	; HI, LO
	.BYTE $07,$f0	; 00: A  1 (Cannot be used)
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
	jsr InitNeededChannels	; | Initialize Sound Channels and Sound Variables
	stx MusicCur			; / MusicCur = A
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

.BYTE $ff,$ff,$ff	; Unused

NoiseNoteSettings:	; Vol, Lo, Hi
	.BYTE $10,$00,$18	; 0
	.BYTE $10,$01,$18	; 3
	.BYTE $00,$01,$88	; 6: Soft snare
	.BYTE $02,$02,$40	; 9
	.BYTE $03,$05,$40	; C
	.BYTE $04,$07,$40	; F: Hard hit

UpdatePulseSettings:
	lda #$7f		; \ Set Pulse Channels:
	sta SQ1_SWEEP	; | No Sweep
	sta SQ2_SWEEP	; /
StoreNewSqVol:
	stx NewSq1Vol
	sty NewSq2Vol
	rts

PlayFlapSFX:
	ldxy FlapSq1	; Load & play Pulse 1 data
	bne FlapAndBubbleWrite	; Always taken

FlapAndBubbleCheck:
	lda SFX3Req				; \
	lsr						; | If Bubble Rise SFX is requested, start it
	bcs PlayBubbleRiseSFX	; /
	lda SFX3Cur				; \
	lsr						; | If Bubble Rise SFX is currently playing, manage it
	bcs ManageBubbleRiseSFX	; /
	lda SFX1Req		; \
	and #$10		; | Check if Player Flap SFX was requested
	bne PlayFlapSFX	; / If so play it
	rts

PlayBubbleRiseSFX:
	lda SFX3Cur	; \
	ora #1		; | Set Bubble Rise SFX to currently playing
	sta SFX3Cur	; /
	lda #0					; \ Initialize Bubble Rise SFX Timer
	sta BubbleRiseSFXTimer	; /
	ldxy BubbleRiseSFXSq1
FlapAndBubbleWrite:
	jsr WriteSq1XY	; \ Can be simplified to a jmp instruction
	rts				; /

ManageBubbleRiseSFX:
	inc BubbleRiseSFXTimer	; Increment timer
	lda BubbleRiseSFXTimer	; \
	cmp #88					; | Return unless timer == 88 frames
	bne :+	;RTS			; /
	lda #0		; \
	sta SFX3Cur	; | At 88 frames, finish sound effect
	rts			; /

AudioMain:
	lda #$c0		; \ Set Frame Counter
	sta APU_FRAME	; / to 4-step sequence, clear frame interrupt flag
	jsr ManageMusic
	jsr lf90a
	jsr lfa38
	jsr CheckSFX2Change
	jsr CheckSFX1Change
	lda SFX2Req		; \ Keep a copy of previous SFX2 requests
	sta LastSFX2Req	; /
	lda #0			; \
	sta SFX1Req		; |
	sta SFX2Req		; | Clear Music/SFX request Flags
	sta MusicReq	; |
	sta SFX3Req		; /
	:rts

TryFootstepNoise:
	lda SFX1Cur		; \
	and #%00000110	; | Return if Pop or Lightning Strike sound effects are playing
	bne :-			; /
	lda SFX1Cur	; \
	and #$f0	; |
	sta SFX1Cur	; /
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
	lda #$00	; \ Clear current SFX1 sounds
	sta SFX1Cur	; /
ResetCurrentSounds:
	sta SFX2Cur	; Clear Current SFX2 sounds
	sta MusicCur	; Clear Current Music/Jingle
	sta SplashSFXPhase	; Reset Splash SFX phase
	sta SFX3Cur	; Clear Current SFX3 sounds
	sta TRI_LINEAR	; Clear Triangle Channel Linear Counter
	sta DMC_RAW	; Clear DMC Channel Load Counter
	sta UnknownSoundFlag
	rts

InitSquare2Noise:
	lda #$10		; \ Constant volume on:
	sta SQ2_VOL		; | - Pulse 2 Channel
	sta NOISE_VOL	; / - Noise Channel
	lda #0					; \ Move on to reset more sound variables
	beq ResetCurrentSounds	; / Always taken

ResetSplashSFXPhase:
	lda #0				; \ Reset SplashSFXPhase to 0
	sta SplashSFXPhase	; /
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
	lda SFX1Req					; \
	lsr							; | If bit 0 of SFX1Req is set, clear all sound effects
	bcs ClearAllSoundEffects	; /
	lda MusicCur	; \
	cmp #$df		; | Continue if currently playing Parachute music
	beq @Continue	; /
	cmp #$7f		; \ Continue if currently playing Enemy Down
	beq @Continue	; /
	cmp #$20		; \ Continue if currently playing Bonus / Trip music
	beq @Continue	; /
	lda MusicCur	; \ Return if playing anything else
	bne :+			; /
	@Continue:
	lda SplashSFXPhase
	cmp #$0f
	beq PlaySplashNoise
	cmp #$f0
	beq CheckSplashCountdown
	lda SFX1Req			; \
	lsrr 2				; | If SFX1Req bit 1 set, play Pop SFX
	bcs PlayPopNoise	; /
	lsr							; \ Bit 2 is Lightning Strike
	bcs TryLightningStrikeSFX	; /
	lsr							; \ Bit 3 is Player Footsteps
	bcs GotoTryFootstepNoise	; /
	lda SFX1Cur				; \
	lsrr 2					; | If SFX1Cur bit 1 set, manage Pop SFX
	bcs CheckPopCountdown	; /
	lsr								; \ If SFX1Cur bit 2 set, manage Lightning Strike
	bcs ManageLightningStrikeSFX	; /
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

ManageLightningStrikeSFX:
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

PlaySparkBounceSFX2:
	lda #0				; \ Initialize Spark SFX Timer
	sta SparkSFXTimer	; /
	clc					; \
	lda RNGOutput		; |
	and #7				; | SparkSFXLength = 2-9 frames
	adc #2				; |
	sta SparkSFXLength	; /
	lda SFX3Cur	; \
	and #0		; | Clear SFX3 (The lda/and instructions are actually redundant)
	ora #$80	; | Set Current SFX3 to SparkBounce
	sta SFX3Cur	; /
	bne ContinueShocked

ContinueSparkBounceSFX2:
	inc SparkSFXTimer	; \ Increment Spark SFX Timer
	lda SparkSFXTimer	; /
	cmp SparkSFXLength	; \ If Spark SFX Timer != Spark SFX Length, continue to play shocked sfx
	bne ContinueShocked	; / Otherwise if equal, end it
EndSFX3:
	lda #$10	; \
	sta SQ1_VOL	; | Set Pulse 1 & 2 Volume, Duty cycle, etc.
	sta SQ2_VOL	; /
	lda #0		; \ Clear Currently playing SFX3
	sta SFX3Cur	; /
	lda SFX1Cur	; \
	and #$0f	; | Clear Player Flap, Falling, Splash, and Shocked SFX
	sta SFX1Cur	; /
	rts

PlayShocked:
	jsr InitAllSoundMemory	; Clear all other sounds
	lda #$80	; \ Set current SFX1 to only Shocked
	sta SFX1Cur	; /
	lda #2		; \ Also request Pop SFX
	sta SFX1Req	; /
ContinueShocked:
	ldxy ShockedSqBase	; \ Write base SFX data to Pulse 1
	jsr WriteSq1XY		; /
	lda RNGOutput	; \
	and #$0f		; | Alter lower pitch byte to be 0-15 randomly
	sta SQ1_LO		; /
	ldxy ShockedSqBase	; \ Write base SFX data to Pulse 2
	jsr WriteSq2		; /
	lda RNGOutput	; \
	lsrr 2			; | Alter lower pitch byte of Pulse 2 to the same random value but shifted 2 bits right
	and #$0f		; | Also ultimately 0-15
	sta SQ2_LO		; /
	rts

PlaySparkBounceSFX:	; Locally branchable jump point
	jmp PlaySparkBounceSFX2

lf90a:
	lda MusicCur	; \ Check if music is not playing
	beq @CanPlay	; / If not playing then continue as normal
	cmp #$df		; \ If playing Enemy Parachute theme, it can play alongside
	beq @CanPlay	; /
	lda SFX1Req		; \ Check for sound effects that stop the music
	and #%11100000	; | (Shocked, Splash, or Falling)
	beq :+			; / if none found, then return
	jsr InitAllSoundMemory
	@CanPlay:
	lda SFX1Req		; \ Check SFX1 requests
	asl				; |
	bcs PlayShocked	; |	Bit 7: Shocked
	asl				; |
	bcs PlaySplash	; | Bit 6: Splash
	asl				; |
	bcs PlayFalling	; / Bit 5: Falling
	lda SFX1Cur			; \
	asl					; | Check if Shocked SFX is playing
	bcs ContinueShocked	; /
	lda SFX1Cur		; \
	and #%11100000	; | Return if Shocked, Splash, or Falling is playing
	bne :+			; /
	lda MusicCur	; \
	cmp #$df		; | If currently playing Enemy Parachuting theme
	beq @Skip		; / then skip checking upper SFX3
	lda MusicCur	; \	Return if anything else is playing
	bne :+			; /
	lda SFX3Req				; \ Check SFX3 requests
	asl						; |
	bcs PlaySparkBounceSFX	; | Bit 7: Spark Bounce
	asl						; |
	bcs PlayFishChompSFX	; / Bit 6: Fish Chomp
	lda SFX3Cur					; \ Check current SFX3 playing
	asl							; |
	bcs ContinueSparkBounceSFX	; | Bit 7: Spark Bounce
	asl							; |
	bcs ContinueFishChompSFX	; / Bit 6: Fish Chomp
	@Skip:
	jsr FlapAndBubbleCheck	; \ Can be simplified to a jmp
	:rts					; /

ContinueSparkBounceSFX:
	jmp ContinueSparkBounceSFX2

PlaySplash:
	lda #$0f			; \ Initialize SplashSFXPhase
	sta SplashSFXPhase	; /
	lda SFX1Cur	; \
	and #$0f	; | Preserve End SFX, Pop, Lightning Strike, and Player Footstep
	ora #$40	; | Add in Splash to current SFX1
	sta SFX1Cur	; /
	ldxy SplashSq1	; Write Pulse 1 data
	bne WriteSplashFallChompSq1	; Always taken

PlayFalling:
	lda #2		; \ Also request pop SFX
	sta SFX1Req	; /
	lda SFX1Cur	; \
	and #$0f	; | Preserve End SFX, Pop, Lightning Strike, and Player Footstep
	ora #$20	; | Add in Falling to current SFX1
	sta SFX1Cur	; /
	ldxy FallingSq1	; Write Pulse 1 data
	bne WriteSplashFallChompSq1	; Always taken

PlayFishChompSFX:
	lda #0				; \ Initialize timer to 0
	sta ChompSFXTimer	; /
	lda SFX3Cur	; \
	and #0		; | Set current SFX3 to Fish Chomp
	ora #$40	; | (the lda/and/ora could have been simplified to just an lda #$40...)
	sta SFX3Cur	; /
	ldxy FishChompSq2	; \ Play Pulse 2 channel of chomp SFX
	jsr WriteSq2		; /
	ldxy FishChompSq1	; Play Pulse 1 channel of chomp SFX
WriteSplashFallChompSq1:
	jsr WriteSq1XY	; \ Could have been a single JMP instruction
	rts				; /

ContinueFishChompSFX:
	inc ChompSFXTimer	; Increment timer
	lda ChompSFXTimer	; \
	cmp #18				; | If timer == 18 then end sound effect
	beq @EndChomp		; /
	cmp #6			; \ If timer < 6 then increment pitch
	bcc @IncPitch	; /
	lda RNGOutput			; \ If timer is 6-18 then
	ora #$10				; | Randomize pulse 1 pitch
	and #$7f				; | 16-127
	sta FishChompPitchSq1	; /
	rol						; \ Pulse 2 pitch is double of pulse 1
	sta FishChompPitchSq2	; /
	jmp @WritePitch

	@IncPitch:
		inc FishChompPitchSq2	; \
		inc FishChompPitchSq2	; | Increase both pitches by 2
		inc FishChompPitchSq1	; |
		inc FishChompPitchSq1	; /
	@WritePitch:
		lda FishChompPitchSq2	; \
		sta SQ2_LO				; | Set both low byte of respective channels to pitches
		lda FishChompPitchSq1	; |
		sta SQ1_LO				; /
		rts

	@EndChomp:
		jmp EndSFX3

FallingSq1:
	.BYTE $b8,$d5,$20,$00
SplashSq1:
	.BYTE $9f,$93,$80,$22
FishChompSq2:
	.BYTE $3f,$ba,$e0,$06
FishChompSq1:
	.BYTE $3f,$bb,$ce,$06
BumpSq2:
	.BYTE $b8,$93,$50,$02
PointCountSq1:
	.BYTE $80,$7f,$60,$68
PointCountSq2:
	.BYTE $80,$7f,$62,$68

PlayBumpSFX:
	lda SFX2Cur	; \
	and #2		; | If already playing Bump SFX, return
	bne :+		; /
	lda MusicCur	; \
	cmp #$df		; | Allow Bump SFX to play during parachuting
	beq @Continue	; /
	lda MusicCur	; \ Do not play if any other music is playing
	bne :+			; /
	@Continue:
	lda #0				; \ Initialize bump timer
	sta BumpSFXTimer	; /
	lda SFX2Cur	; \
	and #$e0	; | Clear all SFX2 except for Enemy Parachuting, Landing, and Down
	ora #2		; | Add in Bump SFX
	sta SFX2Cur	; /
	ldxy BumpSq2				; \ Write to Pulse 2
	bne PointCountBumpWriteSq2	; /

ManageBumpSFX:
	inc BumpSFXTimer	; Increment Bump timer
	lda BumpSFXTimer	; \
	cmp #7				; | Return if Timer != 7
	bne :+				; /
	lda #$7f		; \
	sta SQ2_SWEEP	; /
	lda #$10	; \
	sta SQ2_VOL	; /
	lda SFX2Cur	; \
	and #$e0	; |
	sta SFX2Cur	; /
	:rts

PlayPointCountSFX:
	jsr InitAllSoundMemory	; Clear any other sounds
	ldxy PointCountSq1			; \
	jsr WriteSq1XY				; | Write sound effect to Pulse channels & return
	ldxy PointCountSq2			; |
	jmp PointCountBumpWriteSq2	; /

lfa38:
	lda MusicCur	; \
	beq @Continue	; / If no music is playing, continue with check
	and #$0f	; \
	cmp #$0f	; | Return if not playing Enemy Down, Parachuting, or Bubble Collect jingles
	bne :+		; /
	@Continue:
	lda SFX1Cur	; \
	and #$80	; | Return if Shocked is playing
	bne :+		; /
	lda SFX3Cur	; \
	and #$c0	; | Return if Fish Chomp or Spark Bounce is playing
	bne :+		; /
	lda SFX2Req				; \ Check SFX2 Requests
	lsr						; |
	bcs PlayPointCountSFX	; | Bit 0: Point Count
	lsr						; |
	bcs PlayBumpSFX			; | Bit 1: Bump
	lsr						; |
	bcs PlayBubbleCollect	; | Bit 2: Bubble Collect
	lsrr 2					; |
	bcs PlayBirdTweetSFX	; / Bit 4: Bird Tweet/Flap
	lda SFX2Cur			; \
	lsrr 2				; | If was currently playing Bump SFX, manage it
	bcs ManageBumpSFX	; /
	:rts

PlayBirdTweetSFX:
	lda MusicCur	; \ Do not play if music is playing
	bne :-			; /
	lda SFX2Cur	; \
	and #2		; | Do not play if Bump SFX is playing
	bne :-		; /
	ldxy TweetSq2Base	; \
	jsr WriteSq2		; / Write the base value of the tweet sound effect to Pulse 2
	lda RNGOutput	; \
	and #$3f		; | Randomize then re-write Pulse 2's Low byte
	ora #$10		; | Low pitch = 16-63
	sta SQ2_LO		; /
	rts

PointCountBumpWriteSq2:
	jsr WriteSq2	; \ Could be simplified to single jmp instruction
	rts				; /

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

ManageEnemyLandSFX:
	inc EnemyLandSFXTimer	;
	lda EnemyLandSFXTimer	; \
	cmp #4					; |
	bne :+					; /
	lda SFX2Cur	; \
	and #$1f	; |
	sta SFX2Cur	; /
	ldxy EnemyLandTri2	; \
	bne WriteTriRTS		; /

CheckSFX2Change:
	lda MusicCur
	beq @Continue
	cmp #8
	beq @Continue
	and #$0f
	cmp #$0f
	bne :+
	@Continue:
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
	bcs ManageEnemyLandSFX
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
	ldx #$fc	; \ Settings for pulse channels:
	ldy #$fc
	jsr UpdatePulseSettings
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
	ldx #$80	; \ Settings for pulse channels:
	ldy #$80	; / Duty = 50%, + ???
lfb8d:
	jsr StoreNewSqVol
	lda #$83		; \ Pulse 1 Channel:
	sta SQ1_SWEEP	; / Sweep, Shift = 3
	lda #$7f		; \ Pulse 2 Channel:
	sta SQ2_SWEEP	; / No Sweep
	bne lfbaf

	jsr LoadSoundSequence
	ldx #$04	; \ Settings for pulse channels:
	ldy #$04
	bne lfbac
lfba5:
	jsr LoadSoundSequence
	ldx #$80	; \ Settings for pulse channels:
	ldy #$80	; / Duty = 50%, + ???
lfbac:
	jsr UpdatePulseSettings
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
	ldx #$80	; \ Settings for pulse channels:
	ldy #$ba
	bne lfb8d

.include "Data/MusicData.asm"

.BYTE $ff,$ff,$ff,$ff	; Unused

GotoAudioMain:
	jmp AudioMain