# ZGB
*zig game boy*

ZGB is a gameboy emulator written in zig !
This is more of a learning project than a real project, don't attempt to use it

# Features
- [x] CPU Emulation
- [x] Step debugger
- [x] Basic decompiler
- [ ] Sound (WIP)

# Supported games
- Tetris
- Dr mario

# Passed tests
## Blargg test roms
- [ ] CPU
    - [x] 01-special
    - [x] 02-interrupts
    - [x] 03-op sp,hl
    - [x] 04-op r,imm
    - [x] 05-op rp
    - [x] 06-ld r,r
    - [x] 07-jr,jp,call,ret,reti
    - [x] 08-misc instr
    - [x] 09-op r,r
    - [x] 10-bit ops
    - [x] 11-op a, (hl)
- [ ] Instr timing
- [ ] Interrupt time