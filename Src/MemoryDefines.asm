;----------------------
; RAM Map:
;----------------------

PPUCTRLShadow		.set $00
PPUMASKShadow		.set $01
BTXScroll			.set $17
PPUCTRLShadowBT		.set $18

P1Score				.set $03	;$03-$07
P1Score0			.set $03
P1Score1			.set $04
P1Score2			.set $05
P1Score3			.set $06
P1Score4			.set $07

P2Score				.set $08	;$08-$0C
P2Score0			.set $08
P2Score1			.set $09
P2Score2			.set $0A
P2Score3			.set $0B
P2Score4			.set $0C

TopScore			.set $0D	;$0D-$11
TopScore0			.set $0D
TopScore1			.set $0E
TopScore2			.set $0F
TopScore3			.set $10
TopScore4			.set $11

Temp12				.set $12
Temp13				.set $13
Temp14				.set $14
Temp15				.set $15

FrameProcessFlag	.set $02
FrameCounter		.set $19

GameMode			.set $16	;0 = Balloon Fight (A/B), 1 = Balloon Trip (C)
DemoFlag			.set $3A
TwoPlayerFlag		.set $40

CurrentPhaseHeader	.set $3B
CurrentPhaseNum		.set $3C
PhaseDisplayTimer	.set $3D
P1Lives				.set $41
P2Lives				.set $42
StatusUpdateFlag	.set $46

MainMenuCursor		.set $3F

TargetUpdateScore	.set $3E
ScoreDigits			.set $43	;$43-$48
ScoreDigit1			.set $43
ScoreDigit2			.set $44
ScoreDigit3			.set $45
ScoreDigit4			.set $47
ScoreDigit5			.set $48

DivisionRemainder	.set $43	;Division Dividend & Modulo Result

BTRankLo			.set $49
BTRankHi			.set $4A
CollisionUnknown	.set $4B
; $004B = ?

CloudCount			.set $A3	; Amount of Clouds (zero-based) (-1 if none)
; $00A4 = Selected Cloud ID? (Blink?)
; $00A5 = Selected Cloud ID?? (Lightning?)
; $00A6-$00A8 = Cloud 16x16 Tile Attribute $23xx (Top?)
; $00A9-$00AB = Cloud 16x16 Tile Attribute $23xx
; $00AC-$00AE = Cloud 16x16 Tile Attribute $23xx
; $00AF-$00B1 = Cloud 16x16 Tile Attribute $23xx
; $00B2-$00B4 = Cloud related
; $00B5-$00B7 = Cloud related
SparkCountdown		.set $B8
; $00B9 = Unused?
SparkIntensity		.set $BA	;Speed of sparks
SplashAnim			.set $BB	;Water Splash Animation Frame
SplashXOffset		.set $BC	;The X position of the splash sprite

ScrollLockTimer		.set $C5	;Balloon Trip Scroll Lock Timer
EnemyStartDelay		.set $C6	;Frames before enemies start to inflate their balloons
EnemyInflateSpeed	.set $C7	;Higher=slower, determines how long it takes for enemy to inflate balloon
PhaseType			.set $C8	;0 = Regular, 1 = Bonus
TileScrollCount		.set $C9	;For Balloon Trip
ScreenScrollCount	.set $CA	;For Balloon Trip
; $00CB = ???
CollisionFlags		.set $CC
PlatformCount		.set $CD
; $00CE = Unused
; $00CF = Unused

TempDriftVel		.set $12
TempDriftVelLo		.set TempDriftVel
TempDriftVelHi		.set $13

RNGOutput			.set $1B
RNGLower			.set RNGOutput
RNGUpper			.set $1C

LoadPointer			.set $1D
LoadPointerLo		.set LoadPointer
LoadPointerHi		.set $1E

DataPointer			.set $1F
DataPointerLo		.set DataPointer
DataPointerHi		.set $20

OAMPointer			.set $1F
OAMPointerLo		.set OAMPointer
OAMPointerHi		.set $20

ScorePointer		.set $21
ScorePointerLo		.set ScorePointer
ScorePointerHi		.set $22

LeftPointer			.set $23
LeftPointerLo		.set LeftPointer
LeftPointerHi		.set $24

RightPointer		.set $25
RightPointerLo		.set RightPointer
RightPointerHi		.set $26

TopPointer			.set $27
TopPointerLo		.set TopPointer
TopPointerHi		.set $28

BottomPointer		.set $29
BottomPointerLo		.set BottomPointer
BottomPointerHi		.set $2A

TempWord			.set $2B
TempWordLo			.set TempWord
TempWordHi			.set $2C

TempVel				.set $2B
TempVelFrac			.set TempWord
TempVelInt			.set $2C

VelMult				.set $2D

PreciseVel			.set $2E
PreciseVelSub		.set PreciseVel
PreciseVelFrac		.set $2F
PreciseVelInt		.set $30

; Sound:
; $00D0 = ?
; $00D1 = ?
; $00D2 = ?
; $00D3 = ?

; $00DC = ?
; $00DD = ?

TrackTempo			.set $DF

; Channel Track Pointers
Sq1TrackPointer		.set $E0
Sq1TrackPointerLo	.set $E0
Sq1TrackPointerHi	.set $E1

Sq2TrackPointer		.set $E2
Sq2TrackPointerLo	.set $E2
Sq2TrackPointerHi	.set $E3

TriTrackPointer		.set $E4
TriTrackPointerLo	.set $E4
TriTrackPointerHi	.set $E5

NoiseTrackPointer	.set $E6
NoiseTrackPointerLo	.set $E6
NoiseTrackPointerHi	.set $E7

; Track Offsets
Sq1TrackOffset		.set $E8
Sq2TrackOffset		.set $E9
TriTrackOffset		.set $EA
NoiseTrackOffset	.set $EB

; Sound Requests
SFX1Req				.set $F0
SFX2Req				.set $F1
MusicReq			.set $F2
SFX3Req				.set $F3

; Current Sounds
SFX1Cur				.set $F4
SFX2Cur				.set $F5
MusicCur			.set $F6
SFX3Cur				.set $F7

; $00F8 = Unused?
; $00F9-$00FC = Written but not read?

SndDataTargetPtr	.set $F9
SndDataTargetPtrLo	.set $F9
SndDataTargetPtrHi	.set $FA

SndDataSourcePtr	.set $FB
SndDataSourcePtrLo	.set $FB
SndDataSourcePtrHi	.set $FC

SoundAttrOffset		.set $FD
CurTrackPointer		.set $FE
CurTrackPointerLo	.set $FE
CurTrackPointerHi	.set $FF

; $0100-$01FF = Stack

OAM					.set $0200

; PPU Buffer
PPUBuffer			.set $0300
; $0300-$03FF = PPU Upload Buffer Blocks
;					(16-bit PPU Address,
;					 8-bit Size,
;					 Data, and repeat)

; PPU Upload Buffer:
PPUAddressHi		.set $50
PPUAddressLo		.set $51

PPUBufferPosition	.set $52
PPUBufferSize		.set $53

PPUBlockAddrLo		.set $54
PPUBlockAddrHi		.set $55

UploadBlockSize		.set $56
PPUTempBlock		.set $57

; $005A-$0079 = Palette
; $007A-$007E = Unused?


; Star Animation:
StarUpdateFlag		.set $4C
; $004D-$004E = Unused
; $004F = Star Animation - Star ID
; $0050 = PPU Address Low
; $0051 = PPU Address High


; - Object RAM:
; Note: One byte per object (Player, Enemy...)
; +0 = Player 1
; +1 = Player 2 (Bubble in Balloon Trip)
; +2 to +7 = Enemies
; +8 = Fish (Mostly Unused)

ObjectAction		.set $31	;$0031-$0039

ObjectType			.set $0451	;$0451-$0459
ObjectStatus		.set $7F	;$007F-$0087
ObjectBalloons		.set $88	;$0088-$0090

;Appearance
ObjectDirection		.set $0448	;$0448-$0450 (0 = Left, 1 = Right)
ObjectAnimFrame		.set $0436	;$0436-$043E
ObjectAnimTimer		.set $043F	;$043F-$0447

;Position
ObjectXPosInt		.set $91	;$0091-$0099
ObjectYPosInt		.set $9A	;$009A-$00A2
ObjectXPosFrac		.set $0400	;$0400-$0408
ObjectYPosFrac		.set $0409	;$0409-$0411

;Velocity
ObjectYVelFrac		.set $0412	;$0412-$041A
ObjectYVelInt		.set $041B	;$041B-$0423
ObjectXVelFrac		.set $0424	;$0424-$042C
ObjectXVelInt		.set $042D	;$042D-$0435

;Extra Attributes
ObjectCountdown		.set $045A	;$045A-$0462 Used for Auto-Input and spark death delay before falling
ObjectDriftXVelFrac	.set $0463	;$0463-$046B
ObjectDriftXVelInt	.set $046C	;$046C-$0474
ObjectHitCooldown	.set $0475	;$0475-$047D
ObjectUnknown5		.set $047E	;$047E-$0486

PlayerInvincible	.set $BD	;$00BD-$00BE
PlayerInvTimer		.set $BF	;$00BF-$00C0
PlayerFreeze		.set $C1	;$00C1-$00C2
PlayerSpawnDelay	.set $C3	;$00C3-$00C4

ColScoreOffset		.set $0487
BTPlatformX			.set $0488	;Balloon Trip Starting Platform X Position

;Fish
FishYDirection		.set $0489	;0 = Up, 1 = Down
FishAnimation		.set $048A
FishTargetID		.set $048B	;Matches target object ID
FishTargetEaten		.set $048C
FishFrameTime		.set $048D
FishUnused1			.set $048E
FishUnused2			.set $048F

;Sparks
SparkXPosInt		.set $0490	;$0490-$04A3
SparkYPosInt		.set $04A4	;$04A4-$04B7
SparkXPosFrac		.set $04B8	;$04B8-$04CB
SparkYPosFrac		.set $04CC	;$04CC-$04DF
SparkXVelInt		.set $04E0	;$04E0-$04F3
SparkYVelInt		.set $04F4	;$04F4-$0507
SparkXVelFrac		.set $0508	;$0508-$051B
SparkYVelFrac		.set $051C	;$051C-$052F
SparkAnim			.set $0530	;$0530-$0543
SparkLightning		.set $0544	;$0544-$0557

BonusPhaseIntensity	.set $0558
BalloonPts			.set $0559	; In hundreds of points
BalloonRiseSpeed	.set $055A
SuperBonusPtsUpper	.set $055B
SuperBonusPtsLower	.set $055C

BalloonStatus		.set $055D	; $055D-$0566 = Balloon GFX (Type? Status?)
BalloonXPosInt		.set $0567	; $0567-$0570
BalloonXPosFrac		.set $0571	; $0571-$057A
BalloonYPosInt		.set $057B	; $057B-$0584
BalloonYPosFrac		.set $0585	; $0585-$058E
BalloonXVelInt		.set $058F	; $058F-$0598
BalloonXVelFrac		.set $0599	; $0599-$05A2

BalloonAccelFrac	.set $05B7	; $05B7-$05C0
BalloonAccelInt		.set $05C1	; $05C1-$05CA

BonusBalloonStock	.set $05CB
BonusBalloonDelay	.set $05CC
P1BonusBalloons		.set $05CD
P2BonusBalloons		.set $05CE
P1TripBalloons		.set $05CE

PropellerCount		.set $05D1
PropellerXPos		.set $05D2	; $05D2-$05DB
PropellerYPos		.set $05DC	; $05DC-$05E5
PropellerAddrLo		.set $05E6	; $05E6-$05EF
PropellerAddrHi		.set $05F0	; $05F0-$05F9
PropellerType		.set $05FA	; $05FA-$0603
PropellerState		.set $0604	; $0604-$060D
PropellerCountdown	.set $060E	; $060E-$0617

PopupCountdown		.set $0618	; $0618-$0619
PopupState			.set $061A	; $061A-$061B

; $0620-$0628 = ???
ABtnCooldown		.set $0620
GameATopScore		.set $0629	;1-Player Game Top Score
GameBTopScore		.set $062E	;2-Player Game Top Score
GameCTopScore		.set $0633	;Balloon Trip Top Score

; $0700-$07F9 = Balloon Trip Rank 01 to 50 Scores (5 bytes each)
;				 Rank 47 = Score 000000

; $07E8 = Balloon Trip Music Flag
; $07F0 = Audio related?
; $07F5 = $F0 SFX Flags for Balloon Trip?

HALStringMem		.set $07FA


.IF REGION >= 1
	SoundPage	.set $0700
.ELSE
	SoundPage	.set $0600
.ENDIF

SparkSFXTimer		.set $00E0 + SoundPage
SparkSFXLength		.set $00E1 + SoundPage
BubbleRiseSFXTimer	.set $00E4 + SoundPage
TripMusicFlag		.set $00E8 + SoundPage
LastSFX2Req			.set $00E9 + SoundPage
UnknownSoundFlag	.set $00F0 + SoundPage	;TODO: Figure this out
LightningSFXPitch	.set $00F1 + SoundPage
LightningSFXTimer	.set $00F3 + SoundPage
PopParachuteReq		.set $00F5 + SoundPage
EnemyLandSFXTimer	.set $00F6 + SoundPage
PopSFXCountdown		.set $00F7 + SoundPage
BumpSFXTimer		.set $00F9 + SoundPage
SplashSFXPhase		.set $00FA + SoundPage
SplashSFXTimer		.set $00FB + SoundPage
ChompSFXTimer		.set $00FC + SoundPage
FishChompPitchSq2	.set $00FD + SoundPage
FishChompPitchSq1	.set $00FE + SoundPage

;----------------------
; Registers:
;----------------------

; PPU Registers
PPUCTRL		.set $2000
PPUMASK		.set $2001
PPUSTATUS	.set $2002
OAMADDR     .set $2003
OAMDATA		.set $2004
PPUSCROLL	.set $2005
PPUADDR		.set $2006
PPUDATA		.set $2007
OAMDMA      .set $4014

; APU Registers
SQ1_VOL		.set $4000
SQ1_SWEEP	.set $4001
SQ1_LO		.set $4002
SQ1_HI		.set $4003

SQ2_VOL		.set $4004
SQ2_SWEEP	.set $4005
SQ2_LO		.set $4006
SQ2_HI		.set $4007

TRI_LINEAR	.set $4008
TRI_LO		.set $400A
TRI_HI		.set $400B

NOISE_VOL	.set $400C
NOISE_LO	.set $400E
NOISE_HI	.set $400F

DMC_FREQ	.set $4010
DMC_RAW		.set $4011
DMC_START	.set $4012
DMC_LEN		.set $4013

SND_CHN		.set $4015
APU_FRAME	.set $4017

;Controllers
JOY1		.set $4016
JOY2		.set $4017

Joy1Press	.set $061C
Joy2Press	.set $061D
Joy1Hold	.set $061E
Joy2Hold	.set $061F

ABtn		.set %10000000
BBtn		.set %01000000
SelectBtn	.set %00100000
StartBtn	.set %00010000
UpDPad		.set %00001000
DownDPad	.set %00000100
LeftDPad	.set %00000010
RightDPad	.set %00000001