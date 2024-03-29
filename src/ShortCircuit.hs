{-# LANGUAGE NoImplicitPrelude #-}

{-@ LIQUID "--skip-module" @-}

-- | Module for nifty functions leveraging lazy evaluation
module ShortCircuit
  ( -- * Type classes
    HasFalse (..),
    HasTrue (..),
    Shortcircuit (..),

    -- * Short circuit tests
    isFalse,
    if',
    unless',
    (??),
    (||),
    (&&),
    firstTrueOf,
    lastFalseOf,
    firstFalseOf,

    -- * Monadic short circuits
    orM,
    andM,
    firstTrueOfM,
    lastFalseOfM,
    firstFalseOfM,
  )
where

import Control.Monad
import Data.Maybe
import Prelude hiding ((&&), (||))

-- | Types with a defined false value.
class HasFalse a where
  -- | A false value for this type.
  -- If 'Shortcircuit' @a@ holds, @isTrue false == False@ must hold.
  false :: a

-- | Types with a defined true value.
class HasTrue a where
  -- | A true value for this type.
  -- If 'Shortcircuit' @a@ holds, @isTrue true == True@ must hold.
  true :: a

-- | Types that support short circuits.
class Shortcircuit a where
  -- | Whether the value is true-like (i.e. not false-like).
  isTrue :: a -> Bool

-- | Whether the value is false-like (i.e. not true-like).
isFalse :: (Shortcircuit a) => a -> Bool
isFalse = not . isTrue

-- | @if then else@ generalised to 'Shortcircuit'.
if' :: (Shortcircuit a) => a -> b -> b -> b
if' x a b
  | isTrue x = a
  | otherwise = b

-- | The opposite of 'if''.
unless' :: (Shortcircuit a) => a -> b -> b -> b
unless' x a b
  | isFalse x = a
  | otherwise = b

infix 2 ??, ||, &&

-- | Like 'if'', but with different argument order, allowing infix use.
(??) :: (Shortcircuit a) => b -> b -> a -> b
a ?? b = \x -> if' x a b

-- | 'Prelude.||' generalised to 'Shortcircuit'.
(||) :: (Shortcircuit a) => a -> a -> a
(||) = join if'

-- | 'Prelude.&&' generalised to 'Shortcircuit'.
(&&) :: (Shortcircuit a) => a -> a -> a
(&&) = join unless'

-- | Returns the first true-ish value from a list, or 'false'.
firstTrueOf :: (Shortcircuit a, HasFalse a) => [a] -> a
firstTrueOf = foldr (||) false

-- | Returns the last false-ish value from a list, or 'true'.
lastFalseOf :: (Shortcircuit a, HasTrue a) => [a] -> a
lastFalseOf = foldr (&&) true

-- | Returns the first False-sh value of a provided list
firstFalseOf :: (Shortcircuit a, HasTrue a) => [a] -> a
firstFalseOf = foldr (flip (&&)) true -- foldl (&&) true stan reported that foldl was space-leaking hence moved to the current one

-- | Short-circuit two actions, performing the second only if the first returned a false-ish value.
orM :: (Monad m, Shortcircuit a) => m a -> m a -> m a
orM a b = a >>= \x -> (pure x ?? b) x

-- | Short-circuit two actions, performing the second only if the first returned a true-ish value.
andM :: (Monad m, Shortcircuit a) => m a -> m a -> m a
andM a b = a >>= \x -> (b ?? pure x) x

-- | Short-circuit a list of actions, performing only until a true-ish value is found, or the list exhausted.
firstTrueOfM :: (Monad m, Shortcircuit a, HasFalse a) => [m a] -> m a
firstTrueOfM = foldr orM (pure false)

-- | Short-circuit a list of actions, performing only until a false-ish value is found, or the list exhausted.
lastFalseOfM :: (Monad m, Shortcircuit a, HasTrue a) => [m a] -> m a
lastFalseOfM = foldr andM (pure true)

-- | Returns the first False-ish value from a monadic list
firstFalseOfM :: (Monad m, Shortcircuit a, HasTrue a) => [m a] -> m a
firstFalseOfM = foldr (flip andM) (pure true) -- foldl andM (pure true) stan reported that foldl was space - leaking hence moved to the current one

instance HasTrue Bool where
  true = True

instance HasFalse Bool where
  false = False

instance Shortcircuit Bool where
  isTrue = id

instance HasFalse (Maybe a) where
  false = Nothing

instance Shortcircuit (Maybe a) where
  isTrue = isJust

instance Shortcircuit (Either a b) where
  isTrue (Left _) = False
  isTrue (Right _) = True

