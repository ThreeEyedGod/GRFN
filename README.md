# grfn
![GitHub CI](https://img.shields.io/github/actions/workflow/status/threeeyedgod/GRFN/ci.yml)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/threeeyedgod/GRFN/blob/main/LICENSE)
[![Stable Version](https://img.shields.io/github/v/tag/ThreeEyedGod/GRFN)](https://img.shields.io/github/v/tag/ThreeEyedGod/grfn)
[![Latest Release](https://img.shields.io/github/v/release/ThreeEyedGod/GRFN?color=%233D9970)](https://img.shields.io/github/v/release/ThreeEyedGod/grfn?color=%233D9970)
[![Hackage](https://img.shields.io/hackage/v/grfn.svg)](https://hackage.haskell.org/package/grfn)

Synopsis
---------

Implementation of this paper ["Get pre-factored random numbers easily"](https://twitter.com/michael_nielsen/status/1724854680990486780?s=20). The full paper may be read [here](https://link.springer.com/content/pdf/10.1007/s00145-003-0051-5.pdf).  A synopsis is available in Section 2 in this other [paper](https://math.dartmouth.edu/~carlp/kalai3.pdf) dealing with getting pre-factored random numbers for Gausian distributions.  A reference Python implemention is [here](https://www.johndcook.com/blog/2023/11/17/factored-random-numbers/).

The Adam Kalai algorithm itself is an easier (but less efficient) version of [Eric Bach's original algorithm](https://pages.cs.wisc.edu/~cs812-1/pfrn.pdf). 

### Notes
**Parallelized / Concurrent** ; Property Testing (QuickCheck), [Stan](https://hackage.haskell.org/package/stan) for Static analysis
hlint; github actions, IDE:Cursor+ormolu ; Haddock ; makefile; Benchmark (tasty) ; Verification using Refinement Types (LiquidHaskell) during development ; HPC code coverage enabled; cabal/stack profiling; Kleisli Applicative

### Performance
Development on an entry level M1 ==> Ghc settings and usable cores (rtsopts as well) set to 4.
