#!/usr/bin/env bash
#

# answer for problem 1: 425688
echo "Solution for problem 1: $(runghc --ghc-arg="-i src" src/AC/Dec09.hs 1 data/dec09_input.txt)"
echo "Solution for problem 2: [takes a long time to compute; uncomment to run anyway]"
#echo "Solution for problem 2: $(runghc --ghc-arg="-i src" src/AC/Dec09.hs 2 data/dec09_input.txt)"
