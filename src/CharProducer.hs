{-| Haddock {{{
Module      : CharProducer
Description : A typeclass for producing characters
Copyright   : (c) 0xmycf, 2024
License     : MIT
Maintainer  : mycf.mycf.mycf@gmail.com
Stability   : experimental
Portability : POSIX
}}}-}
module CharProducer
    ( CharProducer(..)
    ) where

class Monad m => CharProducer m where
  produceChar :: m (Maybe Char)
  peekChar :: m (Maybe Char)
