# Advent of Code 2019

Advent of Code 2019 in Modern Fortran

gfortran on RaspberryPiOS (Debian trixie)

Use gfortran -Wall filename.f90 -o filename to compile and link.

## Notes

**Day 1** I've not used Modern Fortran before, so this was a nice gentle start. Part 1 consists of a do loop and a function; part 2 extends part 1 with a nested do loop. On the Pi4B I'm using, part 1 executes in 0.006s, part 2 takes the same time suggesting that file i/o is the bulk of the work.

**Day 2** I'm probably going to regret starting the intcode tape array at 1 rather than 0 in a later problem, but it's simple enough for today's implementation of intcode. Part 1 runs in 0.006s, part 2 in 0.014s on the Pi4B.

**Day 3** The code for part 1 is rather long-winded but has a decent run-time of 0.025s on the Pi4B. There's an obvious way to refactor the getintersection subroutine, but I've left it as-is for now. Part 2 is straightforward when based on part 1 and having the points/lines defined as complex numbers helps. The combined solution that part 2 produces executes in 0.03s.

**Day 4** Part 1 straightforward and executes in 0.06s, part 2 is fiddly and could definitely be solved more elegantly than the spaghetti I've produced. Also takes 0.06s.

**Day 5** As predicted on day 2 I instantly regretted the design decision I'd made over not using 0 as the first index of the intcode tape array. So I solved part 1 anyway and then refactored before plunging on into part 2. The refactored part 1 here runs the air conditioning diagnostic in 0.007s. Part 2 to do.
