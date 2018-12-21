#!/usr/bin/env bash
#

echo "These take a few seconds to run, don't panic."
echo "Solution for problem 1: $(runghc --ghc-arg="-i src" src/AC/Dec10.hs 1 data/dec10_input.txt)"
echo "Solution for problem 2: $(runghc --ghc-arg="-i src" src/AC/Dec10.hs 2 data/dec10_input.txt)"
