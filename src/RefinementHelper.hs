{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Module for helping out refinements with LH in other modules
module RefinementHelper (die, filterInvalid) where

import Protolude hiding (die)
import Prelude (String, error)

-- for reference type Pos = {v:Int | 0 < v}

-- so that these functions can be used in refinement
{-@ measure isPrime :: Int-> Bool @-}
{-@ measure maximum :: Ord a => [a] -> a @-}
{-@ measure product :: [Int] ->Int @-}

{-@ predicate Btwn Lo N Hi = Lo <= N && N < Hi @-}
{-@ predicate BtwnBothIncl Lo N Hi = Lo <= N && N <= Hi @-}

{-@ predicate BtwnXclu Lo V Hi = (Lo < V && V < Hi) @-}
{-@ predicate Lt X Y = X < Y        @-}
{-@ predicate Ge X Y = not (Lt X Y) @-}
{-@ predicate Lte X Y = X <= Y        @-}
{-@ predicate Gt X Y = not (Lte X Y) @-}
{-@ predicate Ne X Y = X /= Y @-}


{-@ type Rng Lo Hi = {v:Pos | (Btwn Lo v Hi)} @-}
{-@ type RngPos Lo Hi = {v:Pos | (BtwnBothIncl Lo v Hi)} @-}
{-@ type RngPrimes Lo Hi = {v:Pos | (v==1) || (isPrime v) && (BtwnBothIncl Lo v Hi)} @-}
{-@ type RngPrimeFactors Lo Hi N = {v:[RngPrimes Lo Hi] | ((Lo==1) && (Hi==1)) || (product v == N)} @-}

{-@ type LstPosMaxN Lo Hi = v:[RngPos Lo Hi] @-}
{-@ type TuplePos F S = {v:(Pos, Pos) | fst v == F && snd v == S && (Ge S F)} @-}

-- Haskell Type Definitions
-- {-@ type factoredInt i [j] :: (i, [j])<{\n fl -> n == product fl}> @-}
-- {-@ factoredIntex :: (Int, [Int]) @-}
-- factoredIntex :: (Int, [Int])
-- factoredIntex = (9, [1,3,3])

mustGoDown :: [Int]
{-@ type DecrList a = [a]<{\xi xj -> xi >= xj}> @-}
{-@ mustGoDown :: DecrList Int @-}
mustGoDown = [3, 2, 1]

-- To assert that code is unreachable
{-@ die :: {v:String | false} -> a @-}

-- | die is to assert that code is unreachable
die :: String -> a
die = error

-- Domain Data
{-@ type Pos = {v:Nat | 0 < v} @-}
{-@ type PrimeFactors = {xs: [ v:Pos] | 1 <= len xs}  @-}

{-@ type IntsGte2 = {v:Int | 2 <= v} @-}

{-@ filterInvalid :: x:Int -> rv:Either String IntsGte2 @-}

-- | filterInvalid trips with a String error whenIntegerprovided is not greater than or equal to 2
filterInvalid :: Int -> Either String Int
filterInvalid = intsGte2

-- | intsGte2 trips with a String error whenIntegerprovided is not greater than or equal to 2

{-@ intsGte2 :: x:Int -> rv : (Either String {rght:IntsGte2 | rght == x })   @-}
intsGte2 :: Int -> Either String Int
intsGte2 n = if n < 2 then Left "Invalid" else Right n

-- -- | filterInvalidNonPosIntegertrips with a String error whenIntegerprovided is a Pos

-- {-@ filterInvalidNonPosInteger :: x:PosInteger -> rv:Either String PosInteger @-}
-- filterInvalidNonPosInteger :: Integer -> Either String Integer
-- filterInvalidNonPosInteger= intsPosInteger

-- -- | intsPosIntegertrips with a String error whenIntegerprovided is not a Pos

-- {-@ intsPosInteger :: x:PosInteger -> rv : (Either String {rght:PosInteger| rght == x })   @-}
-- intsPosInteger :: Integer -> Either String Integer
-- intsPosInteger n = if n < 1 then Left "Invalid" else Right n
