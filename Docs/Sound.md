# Sound/Music Routines

All of the sound-related code exists in one big chunk at the end of the PRG ROM. It's separated from the rest with a chunk of bytes set to 0xFF, and is only called from the NMI routine. My personal theory is that this portion of the code wasn't written by Satoru Iwata, since the style of the code seems different. Entirely possible I'm wrong though.

## Glitches

The main difference between the US and Japanese versions of the game was a bug fix in the sound routines. It seems that many sound-related variables were accidentally put in the last page of the NES memory, which is also used to store a leaderboard of scores for Balloon Trip mode. Because of this, the last few scores in this list are constantly corrupted, causing the player to always start at Rank 47 instead of 50. Strangely, it's the US and European versions that have the glitch, and the Japanese version which has it fixed. I'm really not sure how this happened, given that the Japanese version released first.