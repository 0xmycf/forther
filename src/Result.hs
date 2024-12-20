{-# LANGUAGE PatternSynonyms #-}
module Result
  ( Result(runResult)
--  , runResult'
  , pattern Err
  , pattern Ok
  , Result.fail
  , ok
  , orFailWith
  , liftEither
  , liftIO
  ) where

import           Control.Monad.Fail     (fail)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           MonadLift              (MonadLift(..))
import           Prelude                hiding (fail)
import qualified Control.Monad.Fail as Fail

newtype Result e m a
  = Result { runResult :: m (Either e a) }

pattern Err :: e -> Either e a
pattern Err e = Left e

pattern Ok :: a -> Either e a
pattern Ok e = Right e
{-# COMPLETE Ok, Err #-}

fail :: Monad m => e -> Result e m a
fail e = Result $ pure (Left e)

ok :: Monad m => a -> Result e m a
ok a = Result $ pure (Right a)

orFailWith :: MonadFail f => Maybe a -> String -> f a
orFailWith e reason = case e of
  Just a  -> pure a
  Nothing -> Fail.fail reason

liftEither :: Monad m => Either e a -> Result e m a
liftEither = Result . pure

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

instance MonadLift (Result e) where
  lift = Result.lift
