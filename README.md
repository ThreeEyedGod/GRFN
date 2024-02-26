# grfn
![GitHub CI](https://img.shields.io/github/actions/workflow/status/threeeyedgod/GRFN/ci.yml)
 [![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/threeeyedgod/GRFN/blob/main/LICENSE)
[![Stable Version](https://img.shields.io/github/v/tag/ThreeEyedGod/GRFN)](https://img.shields.io/github/v/tag/ThreeEyedGod/grfn)
[![Latest Release](https://img.shields.io/github/v/release/ThreeEyedGod/GRFN?color=%233D9970)](https://img.shields.io/github/v/release/ThreeEyedGod/grfn?color=%233D9970)


This small haskell program demonstrates a quick implementation of this paper ["Get pre-factored random numbers easily"](https://twitter.com/michael_nielsen/status/1724854680990486780?s=20). The full paper may be read [here](https://link.springer.com/content/pdf/10.1007/s00145-003-0051-5.pdf).
A synopsis is available Section 2 [here] in a different paper (https://math.dartmouth.edu/~carlp/kalai3.pdf) dealing with getting pre-factored random numbers for Gausian distributions.

A reference Python implemention is [here](https://www.johndcook.com/blog/2023/11/17/factored-random-numbers/).

The Adam Kalai algorithm itself is an easier (but less efficient) version of [Eric Bach's original algorithm](https://pages.cs.wisc.edu/~cs812-1/pfrn.pdf). 

Synopsis
---------
### Highlights
Formal Verification using Refinement Types (LiquidHaskell), Property Testing (QuickCheck), Stan for Static analysis

### Standard
hlint; github actions, IDE:Cursor+hoogle-vscode+ormolu ; Haddock ; makefile

Issues
-------
### Problems
1. This was developed on Apple/M1. Liquidhaskell proved difficult to work with stack at this time (late 2023), so used cabal. 
2. Haddock needed an older version: cabal install haddock-2.27.0 --allow-newer

### To-do
A test to check the uniform randomness of the output needs to be put in. The NIST csrc site has [literature](https://csrc.nist.gov/search?keywords=test+suite+random+number+generation&ipp=25&sortBy=relevance&showOnly=publications%2Cprojects%2Cnews%2Cevents%2Cpresentations%2Cglossary%2Ctopics&topicsMatch=ANY&status=Final%2CDraft) and [algorithms](https://csrc.nist.rip/groups/ST/toolkit/rng/documents/nissc-paper.pdf) and [test suites](https://csrc.nist.gov/pubs/itlb/2000/12/a-statistical-test-suite-for-random-and-pseudorand/final).

### Documentation
  ~/grfn/dist-newstyle/build/aarch64-osx/ghc-9.4.7/grfn-0.1.0.0/doc/html/grfn/index.html

