# Background Stars

The background stars are purely decorative. In Balloon Trip mode, they're completely static, but in Game A and B they flicker and move.

## Placement

The star positions seem arbitrary, but they're not randomized. At the start of every phase, the stars begin in the same state. The table StarPositions contains 64 PPU addresses for the tiles the stars are drawn on. The first 32 are animated, the last 32 are static. There are no duplicates.