#!/usr/bin/env bash
#
# Run all tests

runhaskell -isrc -itest test/Spec.hs "$@"
