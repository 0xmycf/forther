{-# LANGUAGE OverloadedRecordDot, ViewPatterns #-}
module Repl
  ( repl ) where

import qualified BinTree                as BT
import           Control.Exception
import           Control.Exception.Base (ErrorCall)
import           Control.Monad          (forM_, join, void, when, (>=>))
import           Data.Bifunctor         (Bifunctor(first))
import           Data.Char              (isDigit)
import           Data.Dynamic           (toDyn)
import           Data.List              (dropWhileEnd, isPrefixOf, stripPrefix)
import qualified Debug.Trace            as Trace
import           Dictionary             (Dict, DictEntry(..), Machine(..), def)
import           Save                   (headS, orElse)
import           Stack                  (Stack, StackElement(..), empty, popN,
                                         popUnsafe, push, toStackElement,
                                         unstack, pop, toToken)
import qualified State
import           State                  (execState, get, liftIO, modify, put)
import           System.IO              (hFlush, stdout)
import           Text.Read              (readMaybe)
import           Token
import qualified Token as T

type State a = State.State Machine IO a

prefix :: String
prefix = "$> "

repl :: IO ()
repl =
  -- TODO currently an error resets the whole state
  void (execState loop (Machine def empty)) `catch` (\(ErrorCall e) -> putStrLn e >> repl)
                                            `catch` (\(e::AsyncException) -> when (e == UserInterrupt) $ putStrLn "" >> putStrLn "goodbye")

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
-- >>> lexer "-2"
-- [FNumberT (-2)]
--
-- >>> lexer "{-2 {\"some string - 2\"}}"
-- [FListT [FNumberT (-2),FListT [FTextT "some string - 2"]]]
--
lexer :: String -> [Token]
lexer line =
  let hd = dropWhile (== ' ') line
  in case headS hd of
    Nothing -> []
    Just (isInt -> True) ->
      let (num, rest) = span isDouble hd
      in if any isDouble' num
          then FDoubleT (read num) : lexer rest
          else FNumberT (read num) : lexer rest
    Just '"' ->
      let (token, rest) = lexString hd
      in token : lexer rest
    Just '{' ->
      let (token, rest) = lexList hd
      in token : lexer rest
    Just 't' -> let (w, rest) = span (/= ' ') hd
                in if w == "true" then FBoolT True : lexer rest
                else mkWord w : lexer rest
    Just 'f' -> let (w,r) = span (/= ' ') hd
                in if w == "false" then FBoolT False : lexer r
                else mkWord w : lexer r
    Just _ ->
      let (w, rest) = span (/= ' ') hd
      in mkWord w : lexer rest

-- | Predicate for matching an integer
isInt :: Char -> Bool
isInt c = c `elem` ['0'..'9'] || c == '-'

-- | Predicate for matching a double
isDouble :: Char -> Bool
isDouble c = isInt c || c == '.' || c == 'e'

-- | Predicate for matching a double without checking isInt
isDouble' :: Char -> Bool
isDouble' c = c == '.' || c == 'e'

mkWord :: String -> Token
mkWord w = case word w of
        Right w'    -> FWordT w'
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
      | x == '}'           = go (x:acc) (n - 1) xs
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
      | x == '"'  = (FTextT (reverse acc), xs)
      | x == '\\' = go (head xs : x : acc) (tail xs)
      | otherwise = go (x:acc) xs

loop :: State ()
loop = do
  liftIO do { putStr prefix; hFlush stdout }
  line <- dropWhile (==' ') <$> liftIO getLine
  case headS line of
    Nothing           -> loop
    Just c | c == ':' -> define line
    Just _            -> eval . lexer $ line
  loop

type Stack' = Stack StackElement

setStack :: (Stack' -> Stack') -> Machine -> Machine
setStack new m = m { stack = new m.stack }

eval :: [Token] -> State ()
eval ws = forM_ ws do translate
  where
    translate = \case
      -- TODO leave this here?
      FWordT Exec -> do
        stack <- stack <$> get
        let (ls, rest) = pop stack `orElse` error "eval: exec: StackUnderflow"
        modify $ setStack $ const rest 
        case ls of
          List ts -> eval (map toToken ts)
          _ -> error "eval: exec: not a list"
      FWordT w   -> do
        dict <- dictionary <$> get
        case dict `BT.lookup` w of
          Just entry -> execute entry
          Nothing    -> error $ "eval: word not found in dictionary; " <> show w
      FListT ts  -> modify $ setStack $ push (List (map toStackElement ts))
      FTextT t   -> modify $ setStack $ push (Text t)
      FNumberT n -> modify $ setStack $ push (Exact n)
      FDoubleT n -> modify $ setStack $ push (Inexact n)
      FBoolT b   -> modify $ setStack $ push (Boolean b)

execute :: DictEntry -> State ()
execute (Literal str) = eval $ lexer str
execute (BuiltIn f)   = void f

-- | Defines a custom new word in the dictionary
define :: String -> State ()
define str =
  let str' = dropWhile (`elem` [' ', ':']) str in
  let (w, rest) = span (/= ' ') str' in
  case word w of
    Left reason -> error $ "define: " <> reason
    Right fword -> modify $ \m -> rest `seq` m { dictionary = BT.insert fword (Literal rest) m.dictionary }
