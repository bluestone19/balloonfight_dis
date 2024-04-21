@echo off
if not exist "Build" mkdir Build
echo [36mAssembling PRG ROM[0m
ca65 -t nes Src/BalloonFight_PRG.asm -o BalloonFightPRG.o
ld65 -C Src/Config/BalloonFightPRG.cfg BalloonFightPRG.o -o Build/BalloonFightBuild.prg
echo [36mPRG ROM build complete[0m
rm BalloonFightPRG.o
if not exist "Src/BalloonFight.chr" echo [31mBalloonFight.chr was not found, cannot build iNES ROM[0m
if not exist "Src/BalloonFight.chr" Pause
if not exist "Src/BalloonFight.chr" Exit
echo [36mAssembling iNES ROM[0m
ca65 -t nes Src/BalloonFight_iNES.asm -o BalloonFightNES.o
ld65 -C Src/Config/BalloonFight.cfg BalloonFightNES.o -o Build/BalloonFightBuild.nes
echo [36miNES build complete[0m
rm BalloonFightNES.o
Pause