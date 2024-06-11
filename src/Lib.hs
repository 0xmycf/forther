module Lib (hello, helloString) where

hello :: IO ()
hello = putStrLn helloString

helloString :: String
helloString = "Hello, World!"
