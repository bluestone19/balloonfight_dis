# OAM (Sprite) Usage

The OAM (Object Attribute Memory) is the NES' way of defining how sprites (technically known as Objects) are rendered to the screen. I hope it isn't too confusing, as throughout this documentation we also refer to things like players and enemies as "objects" or "sprites," but in this case we are referring to the literal building blocks of how those are drawn. Each individual Object is a single 8px by 8px graphic tile which has four bytes. These bytes are the Y position, Index, Attributes, and X position, in that order. The position of each sprite is based on the upper left corner of the Object. The X and Y positions also go from 0,0 at the upper left corner of the screen down to the bottom right of the screen at 255, 239.

Balloon Fight largely uses hardcoded definitions for how to use each of the 64 individual Objects. For example, The first Object is reserved for the first spark in Game A & B. This first Object slot is never used for anything else. There are some exceptions though, for example since the Balloon Birds never appear in Balloon Trip, all of their Objects are reallocated to balloons and sparks. The only Objects that are allocated for multiple uses at once are 56-59, which are used for both the lightning strikes and for the water splash effect. However, the lightning strikes are considered lower priority, so if a splash happens while a lighting strike is happening, the splash will simply overwrite those Objects, making the lightning seem to disappear.

## Object Memory Addresses

OAM is updated each frame by initiating the NES' built-in function to copy a page from the system's RAM. In the case of Balloon Fight, the editable mirror of OAM is kept at 0x0200-0x02FF.

<details>
	<summary>Table of Object Addresses</summary>
 
|Object|Y Pos|Tile Index|Attribute|X Pos|
|:---|---:|---:|---:|---:|
|Object 0|`0x0200`|`0x0201`|`0x0202`|`0x0203`|
|Object 1|`0x0204`|`0x0205`|`0x0206`|`0x0207`|
|Object 2|`0x0208`|`0x0209`|`0x020A`|`0x020B`|
|Object 3|`0x020C`|`0x020D`|`0x020E`|`0x020F`|
|Object 4|`0x0210`|`0x0211`|`0x0212`|`0x0213`|
|Object 5|`0x0214`|`0x0215`|`0x0216`|`0x0217`|
|Object 6|`0x0218`|`0x0219`|`0x021A`|`0x021B`|
|Object 7|`0x021C`|`0x021D`|`0x021E`|`0x021F`|
|Object 8|`0x0220`|`0x0221`|`0x0222`|`0x0223`|
|Object 9|`0x0224`|`0x0225`|`0x0226`|`0x0227`|
|Object 10|`0x0228`|`0x0229`|`0x022A`|`0x022B`|
|Object 11|`0x022C`|`0x022D`|`0x022E`|`0x022F`|
|Object 12|`0x0230`|`0x0231`|`0x0232`|`0x0233`|
|Object 13|`0x0234`|`0x0235`|`0x0236`|`0x0237`|
|Object 14|`0x0238`|`0x0239`|`0x023A`|`0x023B`|
|Object 15|`0x023C`|`0x023D`|`0x023E`|`0x023F`|
|Object 16|`0x0240`|`0x0241`|`0x0242`|`0x0243`|
|Object 17|`0x0244`|`0x0245`|`0x0246`|`0x0247`|
|Object 18|`0x0248`|`0x0249`|`0x024A`|`0x024B`|
|Object 19|`0x024C`|`0x024D`|`0x024E`|`0x024F`|
|Object 20|`0x0250`|`0x0251`|`0x0252`|`0x0253`|
|Object 21|`0x0254`|`0x0255`|`0x0256`|`0x0257`|
|Object 22|`0x0258`|`0x0259`|`0x025A`|`0x025B`|
|Object 23|`0x025C`|`0x025D`|`0x025E`|`0x025F`|
|Object 24|`0x0260`|`0x0261`|`0x0262`|`0x0263`|
|Object 25|`0x0264`|`0x0265`|`0x0266`|`0x0267`|
|Object 26|`0x0268`|`0x0269`|`0x026A`|`0x026B`|
|Object 27|`0x026C`|`0x026D`|`0x026E`|`0x026F`|
|Object 28|`0x0270`|`0x0271`|`0x0272`|`0x0273`|
|Object 29|`0x0274`|`0x0275`|`0x0276`|`0x0277`|
|Object 30|`0x0278`|`0x0279`|`0x027A`|`0x027B`|
|Object 31|`0x027C`|`0x027D`|`0x027E`|`0x027F`|
|Object 32|`0x0280`|`0x0281`|`0x0282`|`0x0283`|
|Object 33|`0x0284`|`0x0285`|`0x0286`|`0x0287`|
|Object 34|`0x0288`|`0x0289`|`0x028A`|`0x028B`|
|Object 35|`0x028C`|`0x028D`|`0x028E`|`0x028F`|
|Object 36|`0x0290`|`0x0291`|`0x0292`|`0x0293`|
|Object 37|`0x0294`|`0x0295`|`0x0296`|`0x0297`|
|Object 38|`0x0298`|`0x0299`|`0x029A`|`0x029B`|
|Object 39|`0x029C`|`0x029D`|`0x029E`|`0x029F`|
|Object 40|`0x02A0`|`0x02A1`|`0x02A2`|`0x02A3`|
|Object 41|`0x02A4`|`0x02A5`|`0x02A6`|`0x02A7`|
|Object 42|`0x02A8`|`0x02A9`|`0x02AA`|`0x02AB`|
|Object 43|`0x02AC`|`0x02AD`|`0x02AE`|`0x02AF`|
|Object 44|`0x02B0`|`0x02B1`|`0x02B2`|`0x02B3`|
|Object 45|`0x02B4`|`0x02B5`|`0x02B6`|`0x02B7`|
|Object 46|`0x02B8`|`0x02B9`|`0x02BA`|`0x02BB`|
|Object 47|`0x02BC`|`0x02BD`|`0x02BE`|`0x02BF`|
|Object 48|`0x02C0`|`0x02C1`|`0x02C2`|`0x02C3`|
|Object 49|`0x02C4`|`0x02C5`|`0x02C6`|`0x02C7`|
|Object 50|`0x02C8`|`0x02C9`|`0x02CA`|`0x02CB`|
|Object 51|`0x02CC`|`0x02CD`|`0x02CE`|`0x02CF`|
|Object 52|`0x02D0`|`0x02D1`|`0x02D2`|`0x02D3`|
|Object 53|`0x02D4`|`0x02D5`|`0x02D6`|`0x02D7`|
|Object 54|`0x02D8`|`0x02D9`|`0x02DA`|`0x02DB`|
|Object 55|`0x02DC`|`0x02DD`|`0x02DE`|`0x02DF`|
|Object 56|`0x02E0`|`0x02E1`|`0x02E2`|`0x02E3`|
|Object 57|`0x02E4`|`0x02E5`|`0x02E6`|`0x02E7`|
|Object 58|`0x02E8`|`0x02E9`|`0x02EA`|`0x02EB`|
|Object 59|`0x02EC`|`0x02ED`|`0x02EE`|`0x02EF`|
|Object 60|`0x02F0`|`0x02F1`|`0x02F2`|`0x02F3`|
|Object 61|`0x02F4`|`0x02F5`|`0x02F6`|`0x02F7`|
|Object 62|`0x02F8`|`0x02F9`|`0x02FA`|`0x02FB`|
|Object 63|`0x02FC`|`0x02FD`|`0x02FE`|`0x02FF`|

</details>

## Game A/B Objects:
* 0: Spark 1
* 1: Spark 2
* 2-7: Fish
* 8-13: Player 1
* 14-19: Player 2
* 20-25: Enemy 1
* 26-31: Enemy 2
* 32-37: Enemy 3
* 38-43: Enemy 4
* 44-49: Enemy 5
* 50-55: Enemy 6
* 56-58: Lightning Strike
* 56-59: Splash (Overrides Lightning if overlap)
* 60-61: Score Popup 1
* 62-63: Score Popup 2