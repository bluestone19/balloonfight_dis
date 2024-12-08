# Frame Counter Details

The Frame Counter is located at 0x19. It's used all throughout the game for referencing time. It is incremented by one for every frame drawn no matter what state the game is in, as it is updated during the NMI. There are only two other things that can actually modify the Frame Counter. The first is GotoTitleScreen, which resets the count to 0 whenever the player returns to the title. The second is also on the title screen, where the count is reset to 0 after the player changes the highlighted mode by pressing select.

## Title Screen Demo

While on the title screen, waiting for a while will start a demo/attract mode. This waiting is governed by the Frame Counter, specifically by triggering the demo when the counter overflows from 255 (0xFF) to 0. This is also the reason for the two ways of resetting the counter. The count is reset after a game over to prevent the game from quickly flashing from game over, to title screen, back to apparent gameplay. The reset when pressing select is also there so you aren't interrupted while actively navigating the menu.

## Character Automated Inputs

In ObjectUpdateAction, the demo players' automated inputs are randomized once every 16 frames. Specifically, whenever the frame counter's last four bits hit 0.

The frame counter is also used to change which player the character targets. Every four frames, the target switches between player 1 or 2, and the opposite player is selected for every other object based on their object slot index. 

## Animations
In ManageObjectVelocity, the animation for getting zapped uses the Frame Counter. It increments the animation frame every other video frame.

All the other animations that are managed by ObjectUpdateAnim use the Frame Counter as well. "Fast" animations move to the next frame of animation when the last two bits of the counter are all zero. Because of this, these animations update every 4 video frames. Similarly, "Slow" animations update every 8 video frames, or when the last three bits of the counter all hit zero.