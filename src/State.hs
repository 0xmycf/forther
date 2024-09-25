module State
  ( State
  -- , HasState(..)
  -- ** Running
  , runState
  , evalState
  , execState
  -- ** Functions
  , state
  -- *** Get, put, modify
  -- These are useful if you define your own instances
  , get
  , put
  , modify
  -- ** Lifting
  , liftIO
  ) where

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           MonadLift              (MonadLift(..))
import HasState (HasState (..))

{-
  I said I wanted to do this without any external dependencies...
-}

newtype State s m a
  = State { runState :: s -> m (a, s) }

evalState :: Monad m => State s m a -> s -> m a
evalState s = fmap fst . runState s

execState :: Monad m => State s m a -> s -> m s
execState s = fmap snd . runState s

state :: (s -> m (a,s)) -> State s m a
state = State

get' :: Monad m => State s m s
get' = state $ \s -> pure (s, s)

put' :: Monad m => s -> State s m ()
put' s = state $ \_ -> pure ((), s)

modify' :: Monad m => (s -> s) -> State s m ()
modify' f = do
  s <- get'
  put' $! f s

instance Monad m => Functor (State s m) where
  fmap f s =
    state $ \ns -> do
      (a, s') <- runState s ns
      pure (f a, s')

instance Monad m => Applicative (State s m) where
  pure a = state (pure . (a,))

  sf <*> s =
    state $ \ns -> do
          (f, s') <- runState sf ns
          (a, s'') <- runState s s'
          pure (f a, s'')

instance Monad m => Monad (State s m) where
  return = pure
  s >>= f = state $ \ns -> do
        (a, s') <- runState s ns
        runState (f a) s'

instance MonadIO m => MonadIO (State s m) where
  liftIO io = state \ns -> do
    a <- liftIO io
    pure (a, ns)

lift :: Monad m => m a -> State s m a
lift action = state $ \s -> do
  a <- action
  pure (a, s)

instance Monad m => HasState s (State s m) where
  get = get'
  put = put'
  modify = modify'

instance MonadLift (State s) where
  lift = State.lift
