{-# LANGUAGE NoMonomorphismRestriction #-} -- //FIXME with protolude

{-@ LIQUID "--notermination"           @-}

-- | Module for helping out refinements with LH in other modules
module RefinementHelper (die, pow2, pow2Less1) where

import Protolude hiding (die)
import Prelude (String, error)
import Control.Monad.Loops (iterateWhile)


-- for reference type Pos = {v:Int | 0 < v}
-- Nat is a type alias type Nat = {v:Int | 0 <= v} thats in the LH prelude

{-@ assume iterateWhile :: (Monad m) => (a -> Bool) -> m a -> m a  @-}


-- so that these functions can be used in refinement
{-@ measure isPrime :: Int-> Bool @-}
{-@ measure maximum :: Ord a => [a] -> a @-}
{-@ measure product :: [Int] -> Int @-}
{-@ measure minimum :: Ord a => [a] -> a @-}

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

 {-@ type DecrList a = [a]<{\xi xj -> xi >= xj}> @-}


{-@ type LstPosMaxN Lo Hi = v:[RngPos Lo Hi] @-}
{-@ type TuplePos F S = {v:(Pos, Pos) | fst v == F && snd v == S && (Ge S F)} @-}

{-@ type TupleIntList = (Pos, [Pos]) @-}
{-@ type DecrList a = [a]<{\xi xj -> xi >= xj}> @-}
{-@ type TupleIntListDecr = (Pos, DecrList Pos) @-}

{-@ type TupleIntListFactored = {u: TupleIntListDecr | fst u == 1 || (product (snd u)) == (fst u) } @-}
{-@ type EitherTupleIntListFactors N = Either Text {rght:TupleIntListFactored | fst rght <= N } @-}

{-@ measure pow2 :: Nat -> Pos @-}
{-@ pow2 :: Nat -> Pos @-}
pow2 :: Int -> Int
pow2 0 = 1
pow2 n | n >= 1 = 2 * pow2 (n - 1)
pow2 _ = die "not possible"

{-@ measure pow2Less1 :: Nat -> Nat @-}
{-@ pow2Less1 :: Nat -> Nat @-}
pow2Less1 :: Int -> Int
pow2Less1 0 = 0
pow2Less1 n | n >= 1 = 2 * pow2 (n - 1) - 1 
pow2Less1 _ = die "not possible"


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
