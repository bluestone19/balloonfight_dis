# Random Number Generator

Pseudo-randomness in the game is accomplished using a linear feedback shift register. The random value is stored across two bytes at 0x1B and 0x1C. 0x1C is the upper half, and 0x1B is the lower half. Only the lower half is used by the rest of the game, but both values are treated as one big 16 bit number for the purpose of the LFSR.