#!/usr/bin/env bash
#

# for debugging/exploration
#echo "Debugging the track: $(runghc --ghc-arg="-i src" src/AC/Dec13.hs 1-debug test/data/dec13_input.txt)"

echo "Solution for problem 1: $(runghc --ghc-arg="-i src" src/AC/Dec13.hs 1 data/dec13_input.txt)"
echo "Solution for problem 2: $(runghc --ghc-arg="-i src" src/AC/Dec13.hs 2 data/dec13_input.txt)"
