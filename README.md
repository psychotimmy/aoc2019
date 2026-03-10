# Advent of Code 2019

Advent of Code 2019 in Modern Fortran

gfortran on RaspberryPiOS (Debian trixie)

Use gfortran -Wall filename.f90 -o filename to compile and link.

## Notes

**Day 1** I've not used Modern Fortran before, so this was a nice gentle start. Part 1 consists of a do loop and a function; part 2 extends part 1 with a nested do loop. On the Pi4B I'm using, part 1 executes in 0.006s, part 2 takes the same time suggesting that file i/o is the bulk of the work.

**Day 2** I'm probably going to regret starting the intcode tape array at 1 rather than 0 in a later problem, but it's simple enough for today's implementation of intcode. Part 1 runs in 0.006s, part 2 in 0.014s on the Pi4B.

**Day 3** The code for part 1 is rather long-winded but has a decent run-time of 0.025s on the Pi4B. There's an obvious way to refactor the getintersection subroutine, but I've left it as-is for now. Part 2 is straightforward when based on part 1 and having the points/lines defined as complex numbers helps. The combined solution that part 2 produces executes in 0.03s.

**Day 4** Part 1 straightforward and executes in 0.06s, part 2 is fiddly and could definitely be solved more elegantly than the spaghetti I've produced. Also takes 0.06s.

**Day 5** As predicted on day 2 I instantly regretted the design decision I'd made over not using 0 as the first index of the intcode tape array. So I solved part 1 anyway and then refactored before plunging on into part 2. The refactored part 1 here runs the air conditioning diagnostic in 0.007s. Part 2 took moments to implement after the refactoring from part 1 and also executes in 0.007s.

**Day 6** A binary tree puzzle. Fun, as I get to play with modules and types for the first time, plus the trim function for strings *and* recursion. This would have been far more difficult in Fortran 77, even if I'd allowed myself to use recursion. Part 1 executes in 0.13s. Part 2 uses the same tree and works out the shortest distance between two specific nodes by finding all of their common nodes in the path from the root node, and then calculates the combined path length from all common nodes to the specific nodes. The shortest path between the specific nodes is the answer. Part 2 also answers part 1 in a combined time of 0.9s. 

**Day 7** Part 1 needed my existing intcode machine modified to accept multiple inputs, but the biggest piece of code I needed to write was to generate all of the unique 5 digit permutations from 0 1 2 3 4 to 4 3 2 1 0. A recursive subroutine does this nicely, but I can already see that part 2 will need both this generalising properly as well as require more development of the intcode input opcode. Part 2 was rather more painful than expected, but mostly due to the less than clear instructions for the puzzle. Once I'd figured out what they were trying to say it was straightforward after generalising my permutation generator and introducing the concept of an 'exit' opcode from runprogram so that I could tell when the program on the final amplifier had halted, rather than just returning an output. Part 1 runs in 0.04s, part 2 in 0.18s.

**Day 8** Part 1 is straightforward (count the zeros on each layer and mulitply the number of 1s by number of 2s for the layer with the least 0s). It runs in 0.007s. The message in part 2 was easier to read by substituting black (0) pixels for spaces and white (1) pixels for Xs. Part 2 also runs in 0.007s.

**Day 9** Implementing relative mode took a little bit of thinking before I got it right, but the diagnostics output by the puzzle data were helpful! Part 1 runs in 0.008s, part 2 (probably the simplest part 2 ever) runs in 0.05s

**Day 10** This puzzle becomes straightforward - if a little long-winded - if polar co-ordinates are used. As real numbers appear in the intermediate calculations I've used a suitable epsilon value for equality comparisons. (It's not really needed given the calculations are always from the same set of integers so produce exactly the same real numbers each time. It's a good habit though.) Part 1 runs in 0.31s as does part 2 (which also answers part 1). In part 2 the list of asteroids doesn't have to be parsed multiple times as the target asteroid (200) is already visible from the asteroid at the origin for both the sample data and puzzle data I have. If this wasn't the case then the list scanning would need tweaking.

**Day 11** Straightforward reuse of the intcode computer from Day 9 part 2, with a little bit of robot movement for good measure. Part 1 runs in 0.14s, part 2 in 0.012s.
