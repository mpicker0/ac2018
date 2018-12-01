# Overview

These are my solutions to [Advent of Code 2018](https://adventofcode.com/2018),
written in [Haskell](https://www.haskell.org/).  Since I'm new to the language,
expect the early exercises to be bad and/or nonidiomatic, and the later ones to
be merely questionable.

# Docker setup

Rather than installing Haskell locally, I am running everything inside a Docker
container.  Run the following from within this directory to get a `bash` shell
to run Haskell commands:

```bash
docker run --interactive --tty --name haskell-work  \
  --volume "$PWD":/root --workdir /root haskell /bin/bash
```

All command examples for the remainder of the document are assumed to be run
within the container.

If this is the first time starting this image, install
[Hspec](http://hspec.github.io/) so the tests can run.

```bash
cabal update && cabal install hspec
```    

# Running
## Running the tests

Run all tests with

```bash
runhaskell -isrc -itest test/Spec.hs
```

## Running the exercises

Run the exercise for a particular day with

```bash
scripts/dec01.sh
```
