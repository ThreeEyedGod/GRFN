{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_grfn (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "grfn"
version :: Version
version = Version [1,0,0,0] []

synopsis :: String
synopsis = "Uniformly-random pre-factored number generation as per Kalai's algorithm"
copyright :: String
copyright = "2024 ThreeEyedGod"
homepage :: String
homepage = "https://github.com/threeeyedgod/grfn#readme"
