# Phase Data Format

Thanks to Quick Curly for documenting this originally!

Each Phase is constructed according to a header, which contains some basic information about the layout and pointers to data. If you look in [PhaseData.asm](/Src/Data/PhaseData.asm), it is organized like this:
```
Graphics Data Pointer List (Terminated by $0000)
Cloud List (Terminated by $FF)
Propeller List (Terminated by $FF)
Enemy Pointer
Collision Box Count (minus one)
Collision Data Pointer
```