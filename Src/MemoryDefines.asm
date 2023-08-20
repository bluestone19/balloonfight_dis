;----------------------
; RAM Map:
;----------------------

.SEGMENT "ZEROPAGE"
PPUCTRLShadow		.set $00
PPUMASKShadow		.set $01
FrameProcessFlag	.set $02
P1Score				.set $03	;$03-$07
P2Score				.set $08	;$08-$0C
TopScore			.set $0D	;$0D-$11
Temp12				.set $12
Temp13				.set $13
Temp14				.set $14
Temp15				.set $15
GameMode			.set $16	;0 = Balloon Fight (A/B), 1 = Balloon Trip (C)
BTXScroll			.set $17
PPUCTRLShadowBT		.set $18
FrameCounter		.set $19

RNGOutput			.set $1B
RNGLower			.set RNGOutput
RNGUpper			.set $1C

LoadPointer			.set $1D
LoadPointerLo		.set LoadPointer
LoadPointerHi		.set $1E

DataPointer			.set $1F
DataPointerLo		.set DataPointer
DataPointerHi		.set $20

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

ObjectAction		.set $31	;$31-$39

DemoFlag			.set $3A
CurrentPhaseHeader	.set $3B
CurrentPhaseNum		.set $3C
; $003D = Phase Number Display Time
; $003E = Score ID to Update (0 = Player 1, 1 = Player 2, 2 = Top Score)
; $003F = Main Menu Cursor
; $0040 = 2 Player Flag
; $0041 = Player 1 Lives
; $0042 = Player 2 Lives
; $0043 = Division Dividend & Modulo Result
;		 & 1st Digit score to add
; $0044 = 2nd Digit score to add
; $0045 = 3rd Digit score to add
; $0046 = Status Bar Update Flag
; $0047 = 4th Digit score to add
; $0048 = 5th Digit score to add
; $0049 = Balloon Trip Rank 0x
; $004A = Balloon Trip Rank x0
; $004B = ?

; Star Animation:
; $004C = Star Update?
; $004D-$004E = Unused
; $004F = Star Animation - Star ID
; $0050 = PPU Address Low
; $0051 = PPU Address High

; PPU Upload Buffer:
; $0050 = PPU Address High
; $0051 = ???
; $0052 = PPU Upload Buffer Position
; $0053 = PPU Upload Buffer Size

; $0054 = (Temp) Cloud/Flipper X coordinate used for rendering
; $0055 = (Temp) Cloud/Flipper Y coordinate used for rendering

; $0056 = Size of upload to PPU Buffer
; $0057-$007E? = Data to upload to PPU Buffer Blocks

; $005A-$0079 = Palette
; $007A-$007E = Unused?

; See Object RAM notes
; $007F-$0087 = Object Status
; $0088-$0090 = Object Balloons
; $0091-$0099 = Object X Positions (Int)
; $009A-$00A2 = Object Y Positions (Int)

; $00A3 = Amount of Clouds (zero-based) (-1 if none)
; $00A4 = Selected Cloud ID? (Blink?)
; $00A5 = Selected Cloud ID?? (Lightning?)
; $00A6-$00A8 = Cloud 16x16 Tile Attribute $23xx (Top?)
; $00A9-$00AB = Cloud 16x16 Tile Attribute $23xx
; $00AC-$00AE = Cloud 16x16 Tile Attribute $23xx
; $00AF-$00B1 = Cloud 16x16 Tile Attribute $23xx
; $00B2-$00B4 = Cloud related
; $00B5-$00B7 = Cloud related
; $00B8 = Lightning Bolt Countdown
; $00B9 = Unused?
; $00BA = Lightning Bolt Intensity (Speed)
; $00BB = Water Plonk Animation Frame
; $00BC = ?
; $00BD-$00BE = Player 1/2 Invincibility Flag
; $00BF-$00C0 = Player 1/2 Invincibility Time
; $00C1-$00C2 = Player 1/2 Freeze Flag
; $00C3-$00C4 = Player 1/2 Respawn Delay
; $00C5 = Lock Scrolling Time (Balloon Trip)
; $00C6 = ???
; $00C7 = ???
; $00C8 = Phase Type (00 = Regular, 01 = Bonus)
; $00C9 = Tile Scroll Counter (Balloon Trip)
; $00CA = Screen Scroll Counter (Balloon Trip)
; $00CB = ???
; $00CC = Collision related
; $00CD = Amount of Platforms
; $00CE = Unused
; $00CF = Unused

; Sound:
; $00D0 = ?
; $00D1 = ?
; $00D2 = ?
; $00D3 = ?

; $00DC = ?
; $00DD = ?

TrackTempo			.set $DF
Sq1TrackPointer		.set $E0
Sq2TrackPointer		.set $E2
TriTrackPointer		.set $E4
NoiseTrackPointer	.set $E6

Sq1TrackOffset		.set $E8
Sq2TrackOffset		.set $E9
TriTrackOffset		.set $EA
NoiseTrackOffset	.set $EB

SFX1Req				.set $F0
SFX2Req				.set $F1
MusicReq			.set $F2
SFX3Req				.set $F3

SFX1Cur				.set $F4
SFX2Cur				.set $F5
MusicCur			.set $F6
SFX3Cur				.set $F7

; $00F8 = Unused?
; $00F9-$00FC = Written but not read?
; $00FD = ?
CurTrackPointer		.set $FE

; $0100-$01FF = Stack

.SEGMENT "OAM"
OAM:		.res $FF

.SEGMENT "PPUBUFFER"
PPUBuffer:	.res $FF
; $0300-$03FF = PPU Upload Buffer Blocks
;					(16-bit PPU Address,
;					 8-bit Size,
;					 Data, and repeat)

; - Object RAM:
; Note: One byte per object (Player, Enemy...)
; +0 = Player 1
; +1 = Player 2
; +2 to +7 = Enemies
; +8 = Fish (Mostly Unused)

; $0400-$0408 = X Positions (Frac)
; $0409-$0411 = Y Positions (Frac)
; $0412-$041A = Y Velocity (Frac)
; $041B-$0423 = Y Velocity (Int)
; $0424-$042C = X Velocity (Frac)
; $042D-$0435 = X Velocity (Int)
; $0436-$043E = Animation Frame
; $043F-$0447 = ?
; $0448-$0450 = Direction (0 = Left, 1 = Right)
; $0451-$0459 = Object Type
; $045A-$0462 = ?
; $0463-$046B = ?
; $046C-$0474 = ?
; $0475-$047D = ?
; $047E-$0486 = ?

; $0487 = ???
; $0488 = Balloon Trip Starting Platform X Position
; $0489 = Fish Y Direction (0 = Up, 1 = Down)
; $048A = Fish Animation?
; $048B = Fish Target ID (Object ID)
; $048C = Fish Target Eaten Flag
; $048D = Fish Frame Time?
; $048E = Fish? Unused?
; $048F = Fish? Unused?

; $0490-$04A3 = Lightning Bolt X Position (Int)
; $04A4-$04B7 = Lightning Bolt Y Position (Int)
; $04B8-$04CB = Lightning Bolt X Position (Frac)
; $04CC-$04DF = Lightning Bolt Y Position (Frac)
; $04E0-$04F3 = Lightning Bolt X Velocity (Int)
; $04F4-$0507 = Lightning Bolt Y Velocity (Int)
; $0508-$051B = Lightning Bolt X Velocity (Frac)
; $051C-$052F = Lightning Bolt Y Velocity (Frac)
; $0530-$0543 = Lightning Bolt Animation Frame?
; $0544-$0557 = Lightning Bolt?

; $0558 = Bonus Phase Intensity Level
; $0559 = Bonus Phase / Balloon Trip x00 points per balloon
; $055A = Balloon Rising Speed
; $055B = Bonus Phase Super Bonus x0000 points
; $055C = Bonus Phase Super Bonus 0x000 points
; $055D-$0566 = Balloon GFX (Type? Status?)
; $0567-$0570 = Balloon X positions
; $0571-$057A = Balloon?
; $057B-$0584 = Balloon Y positions

; $05CB = ?
; $05CC = ?
; $05CD-$05CE = Player 1/2 Touched Balloons Counter
; $05CE = Balloon Trip Balloon Counter

; $05D1 = Amount of Flippers
; $05D2-$05DB = Flipper X positions
; $05DC-$05E5 = Flipper Y positions
; $05E6-$05EF = ?
; $05F0-$05F9 = ?
; $05FA-$0603 = Flippers Type


; $0618-$0619 = ?
; $061A-$061B = ?

; $061C-$061D = Controller 1/2 Pressed Buttons
; $061E-$061F = Controller 1/2 Held Buttons
; $0620-$0628 = ???
GameATopScore		.set $0629	;1-Player Game Top Score
GameATopScore		.set $062E	;2-Player Game Top Score
GameATopScore		.set $0633	;Balloon Trip Top Score

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

.SEGMENT "PPUMEM"
Nametable0:			.res $400
Nametable1:			.res $400
Nametable2:			.res $400
Nametable3:			.res $400
NametableMirror:	.res $F00
Palette:			.res $20
PaletteMirror:		.res $E0

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