-- | Module for helping out refinements with LH in other modules
module RefinementHelper (die, filterInvalid, filterInvalidNonPos, descPosList) where

import Protolude hiding (die)
import Prelude (String, error) 

-- type Pos = {v:Int | 0 < v}

-- so that isPrime can be used in refinement
{-@ measure isPrime :: Int -> Bool @-}
{-@ measure maximum :: Ord a => [a] -> a @-}

{-@ predicate Btwn Lo N Hi = Lo <= N && N < Hi @-}
{-@ predicate BtwnXclu Lo V Hi = (Lo < V && V < Hi) @-}
{-@ predicate Lt X Y = X < Y        @-}
{-@ predicate Ge X Y = not (Lt X Y) @-}
{-@ predicate Lte X Y = X <= Y        @-}
{-@ predicate Gt X Y = not (Lte X Y) @-}
{-@ predicate Ne X Y = X /= Y @-}

{-@ type Rng Lo Hi = {v:Int | (Btwn Lo v Hi)} @-}
{-@ type RngPos Lo Hi = {v:Pos | (Btwn Lo v Hi)} @-}
{-@ type TuplePos F S = {v:(Pos, Pos) | fst v == F && snd v == S && (Ge S F)} @-}

{-@ measure descPosList :: [Pos] -> Bool @-}
{-@ descPosList :: [Pos] -> Bool @-}
descPosList :: [Int] -> Bool
descPosList [] = True
descPosList x = True
descPosList (x : xs) = (x >= val) && descPosList xs
                        where 
                            val = fromMaybe 0 (head xs)

-- To assert that code is unreachable
{-@ die :: {v:String | false} -> a @-}

-- | die is to assert that code is unreachable
die :: String -> a
die = error

-- Domain Data
{-@ type Pos = {v:Nat | 0 < v} @-}
{-@ type PrimeFactors = {xs: [ v:Pos ] | 1 <= len xs}  @-}

{-@ type IntsGte2 = {v:Int | 2 <= v} @-}

{-@ filterInvalid :: x:Int -> rv:Either String IntsGte2 @-}

-- | filterInvalid trips with a String error when Int provided is not greater than or equal to 2
filterInvalid :: Int -> Either String Int
filterInvalid = intsGte2

-- | intsGte2 trips with a String error when Int provided is not greater than or equal to 2

{-@ intsGte2 :: x:Int -> rv : (Either String {rght:IntsGte2 | rght == x })   @-}
intsGte2 :: Int -> Either String Int
intsGte2 n = if n < 2 then Left "Invalid" else Right n

-- | filterInvalidNonPos trips with a String error when Int provided is a Pos

{-@ filterInvalidNonPos :: x:Int -> rv:Either String Pos @-}
filterInvalidNonPos :: Int -> Either String Int
filterInvalidNonPos = intsPos

-- | intsPos trips with a String error when Int provided is not a Pos

{-@ intsPos :: x:Int -> rv : (Either String {rght:Pos | rght == x })   @-}
intsPos :: Int -> Either String Int
intsPos n = if n < 1 then Left "Invalid" else Right n
