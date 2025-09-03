# Object Status

The 9 game object slots each have a corresponding status variable, which is used to indicate how the object should be animated and rendered. Basically, the status number indicates what the object is doing. Note that ObjectAction is an entirely separate concept, as that represents the "controller inputs" for that game object.

Status IDs have different meanings based on what object is in that slot. For example, status 3 means walking for a player, idle on the ground for a Balloon Bird, and the first stage of leaping out of the water for the fish.

## Player States
* 0: Flapping arms (Either flying or falling off the stage)
* 1: Air idle
* 2: Walking
* 3: Ground idle
* 4: Forward Skid (Player stopped walking and is sliding)
* 5: Turn Skid (Player turned around while walking)
* 6: Air Hit
* 7: Walking Hit
* 8: Ground Hit
* 9: Forward Skid Hit
* 10: Turn Skid Hit

## Enemy States
* -1: Empty slot
* 0: Flapping arms (Either flying, falling off the stage, or bubble)
* 1: Air idle & Parachuting
* 2: Ground idle (waiting before starting to pump)
* 3: Starting to pump (No balloon yet)
* 4: Pumping, small balloon
* 5: Pumping, medium balloon
* 6: Pumping, large balloon

## Fish States
* -1: Underwater, swimming
* 1: Rising stage 1
* 2: Rising stage 2
* 3: Rising stage 3
* 4: Falling stage 1
* 5: Falling stage 2
* 6: Falling stage 3