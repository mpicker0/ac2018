#!/usr/bin/env bash
#

echo "Solution for problem 1: $(runghc --ghc-arg="-i src" src/AC/Dec11.hs 1 data/dec11_input.txt)"
echo "Solution for problem 2: [takes a long time to compute; uncomment to run anyway]"
#echo "Solution for problem 2: $(runghc --ghc-arg="-i src" src/AC/Dec11.hs 2 data/dec11_input.txt)"
