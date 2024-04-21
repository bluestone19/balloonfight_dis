# Balloon Fight NES Disassembly

Requires CC65 to assemble: https://cc65.github.io/
Specifically, uses CA65 & LD65

## Build ROM

- Include CHR ROM file of Balloon Fight as "BalloonFight.chr" in Src
- Run build.bat, it will output the PRG ROM, as well as the iNES ROM if the CHR ROM is present

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