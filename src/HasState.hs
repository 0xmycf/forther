{-# LANGUAGE FunctionalDependencies #-}
module HasState where

class Monad m => HasState s m | m -> s where
  get :: m s
  put :: s -> m ()
  modify :: (s -> s) -> m ()
  modify f = do
    s <- get
    put $! f s

