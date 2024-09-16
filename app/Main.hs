module Main where

import Repl (repl)

import System.Environment (getArgs)
import Dictionary (ReadMode(..))

main :: IO ()
main = do 
  args <- getArgs
  if (length args == 1) then do
    let path = head args
    repl (File path)
  else 
    repl Repl

