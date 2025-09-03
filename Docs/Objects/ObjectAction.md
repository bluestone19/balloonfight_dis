# Object Action

Each object has a virtual controller which we call its Action. For players, this is pretty straightforward. For balloon birds, it may be interesting to know that internally the game has them "using a controller" just like the player. In fact, with some simple hacking you can actually re-route your controls into an enemy and play as them.

## Button Mask

Since the Action is pretty much mirroring how NES gamepad input normally works, if you're already familiar this will be nothing new. The standard NES controller has 8 buttons: four directions, Select, Start, A, B. You might not normally think of the D-Pad as a set of four buttons, but that's all it actually is. Of course, the cross-shaped plastic bit has a rocking pivot which prevents opposite directions from being put in at the same time.
The data for the controller's state can be neatly fit into a single byte. 8 bits, 8 on/off states, 8 buttons. Skipping over how it gets there, the final input byte we get will have the bits/buttons in this order:
<details>
	<summary>Table of Object Addresses</summary>
 
|Button|Bit|Hex Value|Decimal
|:---|---:|---:|---:|
|Right|`0`|`0x01`|`1`|
|Left|`1`|`0x02`|`2`|
|Down|`2`|`0x04`|`4`|
|Up|`3`|`0x08`|`8`|
|Start|`4`|`0x10`|`16`|
|Select|`5`|`0x20`|`32`|
|B|`6`|`0x40`|`64`|
|A|`7`|`0x80`|`128`|

</details>
In the disassembly, you will see that any checks against input/actions will have been replaced with more easily readable names. For example, something like "AND #$40" will be replaced with "AND BBtn." These have been assigned their true numerical value in MemoryDefines.asm at the bottom.
Since these are bitmasks, to make a check that allows for one of several different buttons to work for the same action, the values can be simply added together. "AND ABtn + BBtn" will return a non-zero value if either the A or B button is pressed, separately or together.