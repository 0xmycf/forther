module Main where

import Repl.Repl (repl)
import Repl.Types
import System.IO (IOMode(ReadMode), withFile)

import System.Environment (getArgs)

main :: IO ()
main = do 
  args <- getArgs
  if (length args == 1) then do
    let path = head args
    withFile path ReadMode
      ( \file -> repl (File file) )
  else 
    repl Repl

