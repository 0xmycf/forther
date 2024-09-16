{-| Haddock {{{
Module      : Save
Description : Short description
Copyright   : (c) 0xmycf, 2024
License     : MIT
Maintainer  : mycf.mycf.mycf@gmail.com
Stability   : experimental
Portability : POSIX

Save Prelude Function alternatives

TODO this should be safe, no?

}}}-}
module Save
  ( takeS
  , dropS
  , headS
  , lastS
  -- * other functions which are not in the Prelude but quite useful
  , orElse
  , toTuple
  ) where

-- TODO
takeS :: Int -> [a] -> [a]
takeS = error "takeS not implemented"

dropS :: Int -> [a] -> [a]
dropS = error "dropS not implemented"

headS :: [a] -> Maybe a
headS []    = Nothing
headS (x:_) = Just x

lastS :: [a] -> Maybe a
lastS []     = Nothing
lastS [x]    = Just x
lastS (_:xs) = lastS xs

-- | Is strict in the second argument
--- >>> orElse (Just 1) 2
-- 1
--
-- >>> orElse Nothing 2
-- 2
orElse :: Maybe a -> a -> a
orElse Nothing  !a = a
orElse (Just !a) _ = a

toTuple :: [a] -> Maybe (a, a)
toTuple [a, b] = pure (a, b)
toTuple _      = Nothing


