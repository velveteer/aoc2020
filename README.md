# Advent of Code 2020

Solutions to [https://adventofcode.com](https://adventofcode.com/2020). 

## Requirements
This repository prescribes no package management beyond Hackage dependencies.

You need `ghc` and `cabal-install` to compile and run the code. 

### Quick Install

1. [Get ghcup](https://www.haskell.org/ghcup/)
2. Install `ghc` 
```sh
ghcup install ghc 8.10
```
3. Install `cabal-install`
```sh
ghcup install cabal
```

4. (Optional) Install ghcid 
```sh
cabal install ghcid
```
5. (Optional) Install doctest 
```
cabal install doctest
```

## REPL

### Fire up GHCi

```sh
cabal repl
```

### Or fire up the GHCi daemon 

```sh
ghcid
```
This will reload the interpreter and run doctests on every file save. Requires both [ghcid](https://github.com/ndmitchell/ghcid) and [doctest](https://github.com/sol/doctest) to be on your `PATH`.

