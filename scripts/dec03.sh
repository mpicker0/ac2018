#!/usr/bin/env bash
#

echo "Solution for problem 1: $(runghc --ghc-arg="-i src" src/AC/Dec03.hs 1 data/dec03_input.txt)"
echo "Solution for problem 2: $(runghc --ghc-arg="-i src" src/AC/Dec03.hs 2 data/dec03_input.txt)"
