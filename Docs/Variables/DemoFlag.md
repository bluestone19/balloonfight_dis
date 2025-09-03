# Demo Flag Details
The demo flag at 0x3A is used for the "attract mode" on the title screen. For more details on how demo mode is started, see the Frame Counter documentation. The following document will explain the effects of the flag specifically.

## Player Control
When the demo flag is set, neither balloon fighter can be controlled by a player. 

## Scoring
While in demo mode, none of the scores are updated. The UpdateScore/AddScore subroutine has a check at the start to prevent this. Scores can only be manipulated if the Demo Flag is equal to zero.

## HUD
One minor difference in the HUD is the lack of the phase number text. At the start of a phase, right after the request to play the phase start jingle, it will check if the demo flag is non-zero. If the flag contains anything other than zero at this point, the flashing phase text is skipped.
Interestingly, this is only strictly true for phases where the jingle would play. Because of this, if the demo theoretically could progress beyond phase one, the text could actually appear in the demo. However, due to the fact the demo never checks for completion of a phase, this isn't possible without tampering.

## Sounds
No sound is played in demo mode. This is not specifically linked to the demo flag, however. The sound is disabled during the StartDemoGame branch. Theoretically, the sound engine is acting completely normally and has no idea whether the game is in demo mode or not.

## Game Loop
Two changes are made to the game loop logic if the flag is set. First, if either player dies, the demo game ends immediately by returning from the subroutine. Normally, this sends you to the title screen. See "Stack Tampering" for more details.
The second change is for the end-of-phase check. Before checking if all the enemies are gone, it will first check the demo flag. If the demo flag is zero, it will continue on with the normal game loop. If the demo flag is not zero, it will poll he controller to see if you've pressed start or select yet, then just continue the loop without ever giving the chance to move on to the next phase. So no, unfortunately even if the automatic players manage to defeat all the enemies, they will never move on to phase two.

## Flag Alteration
The flag is only ever changed intentionally in the game at two points, exactly where you would expect. In the StartDemo branch, right at the start the demo flag is incremented. Then, whenever the StartDemoGame routine comes back here, the flag is set to 0 before returning to the title screen.
Of course, the flag is also reset to zero at every system reset. This happens regardless of the HAL check, because the zero page is cleared before that. So no, you cannot trick the game into demo mode by resetting during it.

## Flag Tampering
Some mildly amusing things can happen if you decide to manipulate the flag, either through an emulator's RAM viewer or a cheat device like a game genie. Most apparent are the effects on the player characters.
If the demo flag is forced to be 0, then you will be able to control both players in the "demo mode." The only real difference from the normal game will be the lack of audio and starting with zero lives. Getting a game over in this state will send you back to the title screen, seemingly like normal, but if you look at the system's memory you'll see you've left behind four extra bytes in the stack. See "Stack Tampering" for more details.

## Stack Tampering
When the game enters demo mode normally, the demo game is treated as a subroutine of the title screen. On the stack, this is a pointer to return from demo mode, followed by a pointer to start the game, because the title screen itself is actually just a subroutine in the process starting the game.
However, when you start messing with the flag, things can get a little weird. If you turn off the flag during a demo, then the game will basically forget about those pointers it left behind. The end of a non-demo game doesn't return using the stack, it just jumps back to the top of StartGame. Of course, that means it now creates a new title screen at what it assumes is the bottom of the stack. This leaves behind basically four junk bytes in the stack and moves the resting point of the stack up.
If you keep doing this, eventually you'll overflow the stack. However, that in itself doesn't actually do anything, because we just start overwriting the oldest junk and nothing interesting happens. No crash, no glitch, nothing.
Inversely, if the force the flag to be on when it shouldn't, then returning from a game will crash due to a stack underflow. Assuming you do this fresh from power on, you will jump to address 0x0000, which is at the start of RAM. That may sound like the start of some kind of ACE exploit, but keep in mind we already went off the map because there's no way to mess with the demo flag without something like a game genie or memory editor. Not to mention the first three bytes of RAM are very much out of player control, being PPUCTRLShadow, then PPUMASKShadow, then FrameProcessFlag. Doing anything useful with this seems like a dead end.