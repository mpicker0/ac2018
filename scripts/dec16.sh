#!/usr/bin/env bash
#

echo "Solution for problem 1: $(runghc --ghc-arg="-i src" src/AC/Dec16.hs 1 data/dec16_input_1.txt data/dec16_input_2.txt)"
echo "Solution for problem 2: $(runghc --ghc-arg="-i src" src/AC/Dec16.hs 2 data/dec16_input_1.txt data/dec16_input_2.txt)"
