#!/usr/bin/env bash
#

echo "Part 2 takes several seconds to run, and may run out of memory on a"
echo "smaller machine."
echo "Solution for problem 1: $(runghc --ghc-arg="-i src" src/AC/Dec14.hs 1 data/dec14_input.txt)"
echo "Solution for problem 2: $(runghc --ghc-arg="-i src" src/AC/Dec14.hs 2 data/dec14_input.txt)"
