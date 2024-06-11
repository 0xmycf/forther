module Repl
  ( repl ) where

import           Control.Monad (forM_, void, (>=>))
import           Data.List     (dropWhileEnd, isPrefixOf, stripPrefix)
import           Stack         (Stack, empty, popN, popUnsafe, push, unstack)
import qualified State
import           State         (execState, get, liftIO, modify, put)
import           System.IO     (hFlush, stdout)
import           Text.Read     (readMaybe)

data StackElement where
  Exact :: Int -> StackElement
  Inexact :: Double -> StackElement
  Boolean :: Bool -> StackElement
  List :: [a] -> StackElement
  Text :: String -> StackElement

-- TODO
instance Num StackElement where
  Exact a + Exact b = Exact (a + b)
  Exact a - Exact b = Exact (a - b)
  Exact a * Exact b = Exact (a * b)
  fromInteger = Exact . fromInteger
  abs = undefined
  signum = undefined

instance Show StackElement where
  show (Exact a) = show a

type State a = State.State (Stack StackElement) IO a

prefix :: String
prefix = "$> "

repl :: IO ()
repl = void (execState loop empty)

loop :: State ()
loop = do
  liftIO $ do { putStr prefix; hFlush stdout }
  line <- liftIO getLine
  eval . words $ line
  loop

eval :: [String] -> State ()
eval ws = newState <$ forM_ ws do translate
  where
    newState = undefined
    translate x
      | "-" `isPrefixOf` x && length x > 1 = modify $ push (Exact $ read @Int x)
      | all (`elem` ['0'..'9']) x = modify $ push (Exact $ read @Int x)
      | "-" == x = do
        foo <- get
        case popN 2 foo of
          Just ([b, a], stack) -> do
            liftIO $ print (b - a)
            put $ push (b - a) stack
          _ -> error "eval: not implemented"
      | "show" == x = get >>= liftIO . print . unstack
      | otherwise = error "eval: not implemented"
