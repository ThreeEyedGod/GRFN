{-# LANGUAGE BlockArguments, FlexibleContexts, FlexibleInstances, PolyKinds, ScopedTypeVariables #-}
{-# Language PartialTypeSignatures #-}
{-# Language NamedWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Lib
    ( genARandomPreFactoredNumberLEn
     ,firstPrimeLE
     ,createSeq
    ) where


import Data.Numbers.Primes (isPrime)
import System.Random.Stateful (uniformRM, globalStdGen)
import Data.Either 
import RefinementHelper

{-@ lazy genARandomPreFactoredNumberLEn @-} -- disabling termination checking
genARandomPreFactoredNumberLEn :: Int -> IO (Either String (Int,[Int]))
genARandomPreFactoredNumberLEn x | x <= 0  = pure $ Left "Invalid"
genARandomPreFactoredNumberLEn 1           = pure $ Right (1, [1])
genARandomPreFactoredNumberLEn n | n >= 2  = do 
    rndN <- uniformRM (2, n) globalStdGen :: IO Int
    case filterInvalid rndN of 
        Left invalid -> pure $ Left "Invalid"
        Right upper  -> 
            let sq = createSeq upper 
                ps = product sq
            in if ps <= n
                then pure $ Right (ps, sq)
                else genARandomPreFactoredNumberLEn n
genARandomPreFactoredNumberLEn _           = pure $ Left "Invalid"


{-@ lazy createSeq @-} -- disabling termination checking
{-@ createSeq :: Pos -> PrimeFactors @-}
createSeq :: Int -> [Int]
createSeq 1                  = [1]
createSeq n | n >= 2         = case filterInvalidNonPos n of 
                                Left invalid -> createSeq 1
                                Right nGte1  -> do 
                                                    case filterInvalidNonPos (si-1) of
                                                        Left invalid -> createSeq 1 
                                                        Right okN    ->  si : createSeq okN
                                                where si = firstPrimeLE nGte1 
createSeq _                  = die "impossible"

--{-@ lazy firstPrimeLE @-} -- disabling termination checking
{-@ firstPrimeLE :: Pos -> Pos @-}
firstPrimeLE :: Int -> Int
firstPrimeLE 1             = 1
firstPrimeLE n | isPrime n = n
firstPrimeLE n | n > 0     = firstPrimeLE (n-1)
firstPrimeLE _             = die "impossible"
