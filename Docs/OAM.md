# OAM (Sprite) Usage

The OAM (Object Attribute Memory) is the NES' way of defining how sprites (technically known as Objects) are rendered to the screen. I hope it isn't too confusing, as throughout this documentation we also refer to things like players and enemies as "objects" or "sprites," but in this case we are referring to the literal building blocks of how those are drawn. Each individual Object is a single 8px by 8px graphic tile which has four bytes. These bytes are the Y position, Index, Attributes, and X position, in that order.

Balloon Fight largely uses hardcoded definitions for how to use each of the 64 individual Objects. For example, The first Object is reserved for the first spark in Game A & B. This first Object slot is never used for anything else. There are some exceptions though, for example since the Balloon Birds never appear in Balloon Trip, all of their Objects are reallocated to balloons and sparks. The only Objects that are allocated for multiple uses at once are 56-59, which are used for both the lightning strikes and for the water splash effect. However, the lightning strikes are considered lower priority, so if a splash happens while a lighting strike is happening, the splash will simply overwrite those Objects, making the lightning seem to disappear.

##Game A/B Objects:
-0: Spark 1
-1: Spark 2
-2-7: Fish
-8-13: Player 1
-14-19: Player 2
-20-25: Enemy 1
-26-31: Enemy 2
-32-37: Enemy 3
-38-43: Enemy 4
-44-49: Enemy 5
-50-55: Enemy 6
-56-58: Lightning Strike
-56-59: Splash (Overrides Lightning if overlap)
-60-61: Score Popup 1
-62-63: Score Popup 2