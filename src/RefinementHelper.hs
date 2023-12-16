module RefinementHelper (die, filterInvalid, filterInvalidNonPos) where
import Prelude (String, error)
import Protolude hiding (die)

-- To assert that code is unreachable 
{-@ die :: {v:String | false} -> a @-}
die :: String -> a
die = error

-- Domain Data
{-@ type Pos = {v:Nat | 0 < v} @-}
{-@ type PrimeFactors = {xs: [ v:Pos ] | 1 <= len xs}  @-}

{-@ type IntsGte2 = {v:Int | 2 <= v} @-}

{-@ filterInvalid :: x:Int -> rv:Either String IntsGte2 @-}
filterInvalid :: Int -> Either String Int
filterInvalid = intsGte2

{-@ intsGte2 :: x:Int -> rv : (Either String {rght:IntsGte2 | rght == x })   @-}
intsGte2 :: Int -> Either String Int
intsGte2 n = if n < 2 then Left "Invalid" else Right n

{-@ filterInvalidNonPos :: x:Int -> rv:Either String Pos @-}
filterInvalidNonPos :: Int -> Either String Int
filterInvalidNonPos = intsPos

{-@ intsPos :: x:Int -> rv : (Either String {rght:Pos | rght == x })   @-}
intsPos :: Int -> Either String Int
intsPos n = if n < 1 then Left "Invalid" else Right n    
