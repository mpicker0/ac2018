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

Also, install the regular expression library since some of the solutions use
that:

```bash
cabal install regex-posix
```

# Running
## Running the tests

Run all tests with

```bash
scripts/tests.sh
```

Run a group of tests or an individual test by specifying the path in the
hierarchy, for example:

```bash
# all tests for AC.Dec01
scripts/tests.sh -m AC.Dec01

# an individual test
scripts/tests.sh -m "AC.Dec01/part2/finds the result for example 2"
```

## Running the exercises

Run the exercise for a particular day with

```bash
scripts/dec01.sh
```

# GHCi cheat sheet
Start ghci with a reference to the source directory so that scripts (and their
dependencies) can be loaded:

```bash
ghci -isrc
```

Load a source file into ghci

```
:load src/AC/Dec03.hs
```
