#!/usr/bin/env bash
#

echo "Solution for problem 1: $(runghc --ghc-arg="-i src" src/AC/Dec12.hs 1 data/dec12_input.txt)"
echo "Solution for problem 2: $(runghc --ghc-arg="-i src" src/AC/Dec12.hs 2 data/dec12_input.txt)"
