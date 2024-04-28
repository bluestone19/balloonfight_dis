# Balloon Fight NES Disassembly

This is based off of the initial work of LuigiBlood, expanding it out and trying to put easy to understand labels and comments on everything. The end goal is to have a thorough and complete understanding of everything in this game. Some of the key sub-goals include:
-Search for glitches, and understand the causes of known glitches
-See if any information about the game's development can be deduced by analyzing the structure of the code
-Outline the small handful of differences between the Japanese, European, and North American versions of the game.
-Document the structure of the game and its data to allow players to create patches to add features, fix glitches, and so on.

The reason for the change from LuigiBlood's original choice of bass as an assembler to CC65 was mostly arbitrary. However, I'm trying to make use of CC65's features to hopefully make the code easier to understand and read, with things like macros and defining names for memory addresses. Maybe in the future when the assembly version is completely documented, I might want to try converting it to C, since CC65 can do that too. We'll see!

## Build ROM

This project is not intended for being built into a ROM to play or distribute. For this reason, it doesn't include the game's graphics. However, the codebase is written in a format compatible with CC65. Specifically, it uses CA65 & LD65.

https://cc65.github.io/

### Checksums

The original CRC32 checksums for the PRG ROM are:
```
- JP: 575ED2FE
- US: BD2E9025
- EU: 5E137C5B
```

## Acknowledgements

This is a fork of LuigiBlood's disassembly of Balloon Fight, so a big thanks for that!
https://github.com/LuigiBlood/balloonfight_dis

Another big help in labelling things was Quick Curly's Balloon Fight Level Editing & Hacking document. Thanks to him and the other contributors!
https://www.romhacking.net/documents/698/