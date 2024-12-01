module MonadLift 
  ( MonadLift(..)
  ) where

-- | equiv to 'MonadTrans' from mtl
class MonadLift mon where
  lift :: Monad m => m a -> mon m a

