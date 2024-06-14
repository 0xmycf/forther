{-# LANGUAGE ViewPatterns #-}
module Repl
  ( repl ) where

import           Control.Monad  (forM_, void, (>=>))
import           Data.Bifunctor (Bifunctor(first))
import           Data.Char      (isDigit)
import           Data.List      (dropWhileEnd, isPrefixOf, stripPrefix)
import           Save           (headS)
import           Stack          (Stack, empty, popN, popUnsafe, push, unstack)
import qualified State
import           State          (execState, get, liftIO, modify, put)
import           System.IO      (hFlush, stdout)
import           Text.Read      (readMaybe)

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

-- | Use the smart constructor `word` instead
-- An FWord should never be longer than 10 chars
-- and not contain numbers
newtype FWord
  = FWord String
  deriving newtype (Eq, Ord, Show)

-- | Creates a word from a String
word :: String -> Either String FWord
word str
  | length str > 10 = Left "word: too long"
  | any (`elem` ['0'..'9']) str = Left "word: cannot contain numbers"
  | otherwise = Right (FWord str)

-- TODO move this
data Token where
  FBoolT :: Bool -> Token -- ^ A boolean
  FNumberT :: Int -> Token -- ^ A number (pos or negative)
  FWordT :: FWord -> Token -- ^ A word in the forther language
  FTextT :: String -> Token -- ^ A string
  FListT :: [Token] -> Token
  deriving (Show)

-- | recieves the full line
--
-- >>> lexer "1 2 3"
-- [FNumberT 1,FNumberT 2,FNumberT 3]
--
-- >>> lexer "1 2 3 \"b\\ar\""
-- [FNumberT 1,FNumberT 2,FNumberT 3,FTextT "b\\ar"]
--
-- >>> lexer "123 2132 30103 2322"
-- [FNumberT 123,FNumberT 2132,FNumberT 30103,FNumberT 2322]
--
-- >>> lexer "1 2 print"
-- [FNumberT 1,FNumberT 2,FWordT "print"]
--
lexer :: String -> [Token]
lexer line =
  let hd = dropWhile (== ' ') line
  in case headS hd of
    Nothing -> []
    Just '-' -> error "lexer: negative numbers not implemented" -- FWordT (FWord "-") : lexer (drop 1 first)
    Just (isDigit -> True) ->
      let (num, rest) = span isDigit hd
      in FNumberT (read num) : lexer rest
    Just '"' ->
      let (token, rest) = lexString hd
      in token : lexer rest
    Just '{' ->
      let (token, rest) = lexList hd
      in token : lexer rest
    Just _ ->
      let (w, rest) = span (/= ' ') hd
      in case word w of
        Right w'    -> FWordT w' : lexer rest
        Left reason -> error $ "lexer: " <> reason

-- | Token ~ FListT
--
-- >>> lexList "{}"
-- (FListT [],"")
--
-- >>> lexList "{1}"
-- (FListT [FNumberT 1],"")
--
-- >>> lexList "{123 31}"
-- (FListT [FNumberT 123,FNumberT 31],"")
--
-- >>> lexList "{\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}}"
-- (FListT [FTextT "sdlfjsdf",FListT [FNumberT 1,FNumberT 2,FNumberT 3],FTextT "sdfjsdf",FListT [FListT [FNumberT 1],FListT [FNumberT 2,FNumberT 3,FNumberT 4]]],"")
--
lexList :: String -> (Token, String)
lexList ls =
  let (lst, rest) = takeList ls
  in (FListT (lexer lst), rest)

-- |
-- >>> takeList "{\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}}"
-- ("\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}","")
--
takeList :: String -> (String, String)
takeList = first reverse . go [] (0::Int)
  where
    go :: String -> Int -> String -> (String, String)
    go acc n (x:xs)
      | x == '{' && 0 == n = go acc (n + 1) xs
      | x == '{'           = go (x:acc) (n + 1) xs
      | x == '}' && 1 == n = (acc, xs)
      | x == '}'           = go (x:acc) (n - 1) xs
      | otherwise          = go (x:acc) n xs
    go a n [] = error $ "takeList: unterminated list: " <> show (reverse a) <> " {-count:" <> show n

-- | Token ~ FTextT
--
-- \" and " are the same in case of chars
-- >>> '\"' == '"'
-- True
--
-- >>> lexString "foo\""
-- (FTextT "foo","")
--
-- >>> lexString "\"foo\""
-- (FTextT "foo","")
--
-- >>> lexString "\"foo\"bar"
-- (FTextT "foo","bar")
--
-- >>> lexString "\"foo\\\"bar\""
-- (FTextT "foo\\\"bar","")
--
lexString :: String -> (Token, String)
lexString str = go [] (if head str == '"' then tail str else str)
  where
    go _ []       = error "lexString: unterminated string"
    go acc (x:xs)
      | x == '"'  = (FTextT (reverse acc), xs)
      | x == '\\' = go (head xs : x : acc) (tail xs)
      | otherwise = go (x:acc) xs

loop :: State ()
loop = do
  liftIO $ do { putStr prefix; hFlush stdout }
  line <- liftIO getLine
  eval . words $ line -- TODO continue here with the lexer instead
  loop

eval :: [String] -> State ()
eval ws = newState <$ forM_ ws do translate
  where
    newState = undefined
    translate x
      | "-" `isPrefixOf` x && length x > 1 = modify $ push (Exact $ read @Int x)
      | all (`elem` ['0'..'9']) x = modify $ push (Exact $ read @Int x)
      -- the problem is i cannot reasoably convert strings to soemthing, since i split on the words
      | "-" == x = do
        foo <- get
        case popN 2 foo of
          Just ([b, a], stack) -> do
            liftIO $ print (b - a)
            put $ push (b - a) stack
          _ -> error "eval: not implemented"
      | "show" == x = get >>= liftIO . print . unstack
      | otherwise = error "eval: not implemented"
