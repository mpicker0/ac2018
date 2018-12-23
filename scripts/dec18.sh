#!/usr/bin/env bash
#

# for debugging/exploration
#echo "Debugging the map: $(runghc --ghc-arg="-i src" src/AC/Dec18.hs 1-debug test/data/dec18_input_0.txt)"

echo "Solution for problem 1: $(runghc --ghc-arg="-i src" src/AC/Dec18.hs 1 data/dec18_input.txt)"
echo "Solution for problem 2: $(runghc --ghc-arg="-i src" src/AC/Dec18.hs 2 data/dec18_input.txt)"
