# grfn
![GitHub CI](https://img.shields.io/github/actions/workflow/status/threeeyedgod/GRFN/ci.yml)
 [![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/threeeyedgod/GRFN/blob/main/LICENSE)
[![Stable Version](https://img.shields.io/github/v/tag/ThreeEyedGod/GRFN)](https://img.shields.io/github/v/tag/ThreeEyedGod/grfn)
[![Latest Release](https://img.shields.io/github/v/release/ThreeEyedGod/GRFN?color=%233D9970)](https://img.shields.io/github/v/release/ThreeEyedGod/grfn?color=%233D9970)


This small haskell program demonstrates a quick implementation of this paper ["Get pre-factored random numbers easily"](https://twitter.com/michael_nielsen/status/1724854680990486780?s=20). The full paper may be read [here](https://link.springer.com/content/pdf/10.1007/s00145-003-0051-5.pdf).

A reference Python implemention is [here](https://www.johndcook.com/blog/2023/11/17/factored-random-numbers/).

Synopsis
---------
### Highlights
Uses Refinement Types (LiquidHaskell), Property Testing (QuickCheck) some lazy evaluation (cf. Shortcircuit)

### Standard
hlint; github actions 

Issues
-------
### Problems
This was developed on Apple/M1. Liquidhaskell proved difficult to work with stack at this time (late 2023), so used cabal. 

### To-do
A test to check the uniform randomness of the output needs to be put in. 



