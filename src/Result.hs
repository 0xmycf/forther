module Result
  ( Result(runResult)
  , Result.fail
  , ok
  , liftIO
  , lift
  ) where

import Prelude hiding (fail)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Fail (fail)

newtype Result e m a = Result { runResult :: m (Either e a) }

fail :: Monad m => e -> Result e m a
fail e = Result $ pure (Left e)

ok :: Monad m => a -> Result e m a
ok a = Result $ pure (Right a)

instance Monad m => Functor (Result e m) where
  fmap f res = let mon = runResult res
                in Result $ fmap (fmap f) mon

instance Monad m => Applicative (Result e m) where
  pure = ok
  fres <*> res = undefined
    let fmon = runResult fres
        mon = runResult res
    in Result do
      f <- fmon
      e <- mon
      pure $ f <*> e

instance Monad m => Monad (Result e m) where
  res >>= f =
    let mon = runResult res
    in Result $
        mon >>= (\case
          Left err -> pure $ Left err
          Right v -> runResult $ f v )

instance MonadIO m => MonadIO (Result e m) where
  liftIO io = Result do
    a <- liftIO io
    pure $ Right a

instance Monad m => MonadFail (Result String m) where
  fail = Result.fail

lift :: Monad m => m a -> Result e m a
lift mon = Result $ Right <$> mon

