# Objects

Players and enemies share a significant amount of functionality. In order to ensure compatibility and simplify the code, they were abstracted into the what we might generally call a "game object" if we were working with a modern game engine. Throughout the comments and documentation you'll see reference to "objects," and in most cases that refers to one of these "game objects." Each of these objects shares several parallel arrays containing attributes such as position and velocity.

I'm very sorry about any confusion with the NES sprites, also often called "objects." To be clear, the OAM (Object Attribute Memory) is actually not referring to "game objects" but instead to NES sprites. See the OAM doc for more details on that. Although each game object is represented on screen using NES sprite objects, an NES sprite object is not the same thing as a game object. Each game object has 6 NES sprite objects which it controls.

From here on out, when this document says "object," it does mean:
* Players
* Balloon Birds
* Fish
* Bubbles (Which are technically just Balloon Birds)
But it does not mean:
* Individual 8x8 sprites
* Sparks
* Balloons
* Propellers (These don't even use sprites, they exist as background tiles)

## Variables

All objects have allocated space for each of these variables.
-Action
-Type
-Status
-Balloons
-Direction
-Animation Frame
-Animation Timer
-Position (X & Y, Fractional)
-Velocity (X & Y, Fractional)
-Countdown
-Drift Velocity (Fractional, only used by Balloon Birds)
-Hit Cooldown
-Upgrade Flag (Only used by Balloon Birds)
