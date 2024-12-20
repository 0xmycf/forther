{-| Haddock {{{
Module      : IOBuffer
Description : Can read from a Handle but enqueue the
              read data into an internal buffer (peek)
Copyright   : (c) 0xmycf, 2024
License     : MIT
Maintainer  : mycf.mycf.mycf@gmail.com
Stability   : experimental
Portability : POSIX

The Handle and the Buffer work on 'Char's not 'Word8's.

'peekChar' will return the head of the buffer if it is not empty.
or query the 'Handle' for more data and enqueue it into the buffer.
If 'peekChar' returns 'Nothing' then the 'Handle' is empty.

'produceChar' will return the head of the buffer if it is not empty.
or query the 'Handle' for more data.
}}}-}
module IOPeekBuffer
  ( IOPeekBuffer
  , withBuffer
  ) where

import           CharProducer      (CharProducer(..))
import           Control.Exception (SomeException)
import qualified Control.Exception as Ex
import           Data.Functor      (($>))
import           GHC.IO.Handle     (Handle, hGetChar)
import           HasState          (HasState (get, modify))
import qualified State

newtype IOPeekBuffer a
  = IOPeekBuffer { runBuffer :: State.State Buffer IO a }
  deriving newtype (Applicative, Functor, HasState Buffer, Monad)

data Buffer
  = Buffer
      { buffer :: Maybe Char
      , handle :: Handle
      }

withBuffer :: Handle -> IOPeekBuffer a -> IO a
withBuffer h buf = State.evalState (runBuffer buf) (Buffer Nothing h)

getHead :: IOPeekBuffer (Maybe Char)
getHead = buffer <$> get

removeHead :: IOPeekBuffer ()
removeHead = IOPeekBuffer $ modify \buf -> buf { buffer = Nothing }

put :: Char -> IOPeekBuffer ()
put c = modify \buf -> buf { buffer = Just c }

liftIO :: IO a -> IOPeekBuffer a
liftIO = IOPeekBuffer . State.liftIO

catch :: IO a -> (SomeException -> IO a) -> IO a
catch = Ex.catch

instance CharProducer IOPeekBuffer where
  produceChar :: IOPeekBuffer (Maybe Char)
  produceChar = do
    hd <- peekChar
    case hd of
      Nothing -> pure Nothing;
      Just c  -> removeHead $> Just c

  peekChar :: IOPeekBuffer (Maybe Char)
  peekChar = do
    may_head <- getHead
    st <- get @Buffer
    case may_head of
      Nothing -> do
        c <- liftIO $ hGetChar st.handle `catch` const (pure '\0')
        case c of {
          '\0' -> pure Nothing;
          _    -> put c $> Just c
        }
      -------------------------
      j -> pure j
