{-# LANGUAGE NoImplicitPrelude #-}

-- | Module for accessing this math function
module Lib
  ( genARandomPreFactoredNumberLEn,
    createSeq',
  )
where

-- import Data.Bool (bool) -- after refactoring yet again, using Data.bool.HT this is no longer being used
import Data.Bool.HT (if')
import Data.Numbers.Primes (isPrime)
-- import ShortCircuit (if') -- now after refactoring using bool, this is not being used

import Data.String -- for tuple experiments
{-@ lazy genARandomPreFactoredNumberLEn @-}
-- disabling termination checking
import Data.Text (pack)
import Protolude hiding (bool, die)
import RefinementHelper
import System.Random.Stateful (globalStdGen, uniformRM)

-- | This is the Entry Function.
-- Provide an integer input and it should generate a tuple of a number less than the integer i/p and its factors
genARandomPreFactoredNumberLEn :: Int -> IO (Either Text (Int, [Int]))
genARandomPreFactoredNumberLEn x | x <= 0 = pure $ Left $ pack "Invalid"
genARandomPreFactoredNumberLEn 1 = pure $ Right (1, [1])
genARandomPreFactoredNumberLEn n | n >= 2 = do
  rndM <- fmap filterInvalid (getRndMInt (2, n))
  case rndM of
    Left _ -> pure $ Left $ pack "Invalid"
    Right upper -> if' (ps <= n) (pure $ Right rsp) (genARandomPreFactoredNumberLEn n) -- Data.bool.HT if'
      where
        rsp@(ps, sq) = (product sq, createSeq' upper) -- Haskell as-pattern @
genARandomPreFactoredNumberLEn _ = pure $ Left $ pack "Invalid"

{-@ lazy createSeq' @-}
-- disabling termination checking
{-@ createSeq' :: Pos -> PrimeFactors @-}

-- | Creates a sequence of prime ints below the input integer.
-- The input has to be positive int.
-- The output will then be a list of primes (of positive ints).
createSeq' :: Int -> [Int]
createSeq' 1 = [1]
createSeq' n | n >= 2 = 1 : [x | x <- filter isPrime [2 .. n], x > 0] -- the x > 0 is what proves to SMT solver than it's Pos within PrimeFactors
createSeq' _ = die "impossible"

-- helper functions
-- Get a random integer given a lower and upper bound

-- | Get a Random Integer with uniform probability in a range.
getRndMInt :: (Int, Int) -> IO Int
getRndMInt (l, u) = uniformRM (l, u) globalStdGen :: IO Int

data TupleList = TupleStr [(String, String)] | TupleInt [(Integer, Integer)] | TupleDouble deriving (Show, Eq)

data IntStr = Int Integer | Str String | NotSupported deriving (Show, Eq)

mapTuples :: (Show a) => (a -> a -> a) -> [(a, a)] -> [a]
mapTuples f ts = map (\t -> f (fst t) (snd t)) ts

reduceTupleList :: TupleList -> IntStr
reduceTupleList ts = case ts of
  TupleStr s -> Str (foldr (++) "" (mapTuples (++) s))
  TupleInt i -> Int (foldr (+) 0 (mapTuples (-) i))
  _ -> NotSupported

 -- Example usage: tupleList = TupleInt [(2,1), (5,1)]; reduceTupleList tupleList (output: 5)

