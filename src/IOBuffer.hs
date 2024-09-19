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
module IOBuffer
  ( IOBuffer
  , withBuffer
  , CharProducer(..)
  ) where

import           CharProducer      (CharProducer(..))
import           Control.Exception (SomeException)
import qualified Control.Exception as Ex
import           Data.Functor      (($>))
import           GHC.IO.Handle     (Handle, hGetChar)
import           Save              (headS)
import qualified State

newtype IOBuffer a
  = IOBuffer { runBuffer :: State.State Buffer IO a }
  deriving newtype (Applicative, Functor, Monad)

data Buffer
  = Buffer
      { buffer :: String
      , handle :: Handle
      }

withBuffer :: Handle -> IOBuffer a -> IO a
withBuffer h buf = State.evalState (runBuffer buf) (Buffer "" h)

getHead :: State.State Buffer IO (Maybe Char)
getHead = headS . buffer <$> State.get

removeHead :: IOBuffer ()
removeHead = IOBuffer $ State.modify \buf -> buf { buffer = tail buf.buffer }

put :: Char -> State.State Buffer IO ()
put c = State.modify \buf -> buf { buffer = c : buf.buffer }

liftIO :: IO a -> State.State Buffer IO a
liftIO = State.liftIO

catch :: IO a -> (SomeException -> IO a) -> IO a
catch = Ex.catch

instance CharProducer IOBuffer where
  produceChar :: IOBuffer (Maybe Char)
  produceChar = do
    hd <- peekChar
    case hd of
      Nothing -> pure Nothing;
      Just c  -> removeHead $> Just c

  peekChar :: IOBuffer (Maybe Char)
  peekChar = IOBuffer do
    may_head <- getHead
    st <- State.get
    case may_head of
      Nothing -> do
        c <- liftIO $ hGetChar st.handle `catch` const (pure '\0')
        case c of {
          '\0' -> pure Nothing;
          _    -> put c $> Just c
        }
      -------------------------
      j -> pure j
