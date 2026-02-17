# Advent of Code 2019

Advent of Code 2019 in modern Fortran

gfortran on RaspberryPiOS (Debian trixie)

Use gfortran -Wall filename.f90 -o filename to compile and link.

## Notes

**Day 1** I've not used modern Fortran before, so this was a nice gentle start. Part 1 consists of a do loop and a function; part 2 extends part 1 with a nested do loop. On the Pi4B I'm using, part 1 executes in 0.006s, part 2 takes the same time suggesting that file i/o is the bulk of the work.
