module Lib
    ( genARandomPreFactoredNumberLEn
     ,firstPrimeLE
     ,createSeq
    ) where

import Data.Numbers.Primes (isPrime)
import System.Random.Stateful (uniformRM, globalStdGen)
import RefinementHelper 
import ShortCircuit (if')

{-@ lazy genARandomPreFactoredNumberLEn @-} -- disabling termination checking
genARandomPreFactoredNumberLEn :: Int -> IO (Either String (Int,[Int]))
genARandomPreFactoredNumberLEn x | x <= 0  = pure $ Left "Invalid"
genARandomPreFactoredNumberLEn 1           = pure $ Right (1, [1])
genARandomPreFactoredNumberLEn n | n >= 2  = do 
                                                rndM <- fmap filterInvalid (getRndMInt (2, n)) 
                                                case rndM of 
                                                    Left _ -> pure $ Left "Invalid"
                                                    Right upper  -> if' (ps <= n) (pure $ Right rsp) (genARandomPreFactoredNumberLEn n) --if' from shortcircuit, used here for convenience not lazy evaluation
                                                                        where rsp@(ps, sq) = (product sq, createSeq upper) -- Haskell as-pattern @
genARandomPreFactoredNumberLEn _           = pure $ Left "Invalid"

{-@ lazy createSeq @-} -- disabling termination checking
{-@ createSeq :: Pos -> PrimeFactors @-}
createSeq :: Int -> [Int]
createSeq 1                  = [1]      
createSeq n | n >= 2         = case filterInvalidNonPos n of 
                                Left _ -> createSeq 1
                                -- Right nGte1  -> if' fVal (si : createSeq rght) (createSeq 1 )
                                --                     where (si, fVal, rght) = (firstPrimeLE nGte1, filterInvalidNonPos (si-1), fromRight fVal)
                                Right nGte1  -> do 
                                                    case filterInvalidNonPos (si-1) of
                                                        Left _ -> createSeq 1 
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


-- helper functions
-- get a random integer given a lower and upper bound
getRndMInt :: (Int, Int) -> IO Int 
getRndMInt (l,u) = uniformRM (l, u) globalStdGen :: IO Int


-- kind of dicey 'unsafe function' I assume that the value will always be Right something
fromRight :: Either l r -> r
fromRight (Right v) = v
fromRight _ = error "ouch"

