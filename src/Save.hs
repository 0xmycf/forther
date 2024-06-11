{-| Haddock {{{
Module      : Save
Description : Short description
Copyright   : (c) 0xmycf, 2024
License     : MIT
Maintainer  : mycf.mycf.mycf@gmail.com
Stability   : experimental
Portability : POSIX

Save Prelude Function alternatives

}}}-}
module Save
  ( takeS
  , dropS
  , headS
  ) where

-- TODO
takeS :: Int -> [a] -> [a]
takeS = error "takeS not implemented"

dropS :: Int -> [a] -> [a]
dropS = error "dropS not implemented"

headS :: [a] -> Maybe a
headS []    = Nothing
headS (x:_) = Just x
