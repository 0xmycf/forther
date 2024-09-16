{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-| Haddock {{{
Module      : Token
Description : Tokens and Lexing
Copyright   : (c) 0xmycf, 2024
License     : MIT
Maintainer  : mycf.mycf.mycf@gmail.com
Stability   : experimental
Portability : POSIX
}}}-}
module Token
  ( Token(..)
  -- * Words in the Forther langauge
  , FWord
  , pattern Exec
  , pattern Colon
  , word
  , mkWord
  -- * Lexing
  , lexer
  ) where
import           Control.DeepSeq (NFData)
import           Data.Bifunctor  (first)
import           Data.Char       (isSpace)
import           Save            (headS)

-- | Use the smart constructor `word` instead
-- An FWord should never be longer than 10 chars
-- and not contain numbers
--
-- >>> show $ FWord "exec"
-- ":exec"
newtype FWord
  = FWord String
  deriving newtype (Eq, NFData, Ord)

instance Show FWord where
  show (FWord str) = ':' : str

pattern Exec :: FWord
pattern Exec <- FWord "exec"

pattern Colon :: FWord
pattern Colon <- FWord ":"

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
-- [FNumberT 1,FNumberT 2,FWordT :print]
--
-- >>> lexer "-2"
-- [FNumberT (-2)]
--
-- >>> lexer "{-2 {\"some string - 2\"}}"
-- [FListT [FNumberT (-2),FListT [FTextT "some string - 2"]]]
--
-- >>> lexer "{ I } 2"
-- [FListT [FWordT :I],FNumberT 2]
--
-- >>> lexer "{ } 2"
-- [FListT [],FNumberT 2]
--
-- >>> lexer "( { } )"
-- []
--
-- >>> lexer "1 2 ( )"
-- [FNumberT 1,FNumberT 2]
-- 
-- >>> lexer "1\n{\n 1 2 3 \n}\n\"some word\""
-- [FNumberT 1,FListT [FNumberT 1,FNumberT 2,FNumberT 3],FTextT "some word"]
lexer :: String -> [Token]
lexer line =
  let hd = dropWhile isSpace line
  in case headS hd of
    Nothing -> []
    Just ';' -> []
    Just '(' -> -- comments
      let rest = dropWhile (/= ')') hd in
      lexer $ drop 1 rest
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
-- >>> all isInt ([ '0'..'9'] ++ ['-'])
-- True
isInt :: Char -> Bool
isInt c = c `elem` ['0'..'9'] || c == '-'

-- | Predicate for matching a double
-- >>> all isDouble ([ '0'..'9'] ++ ['-', '.', 'e'])
-- True
isDouble :: Char -> Bool
isDouble c = isInt c || c == '.' || c == 'e'

-- | Predicate for matching a double without checking isInt
-- >>> isDouble' '.'
-- True
--
-- >>> isDouble' 'e'
-- True
isDouble' :: Char -> Bool
isDouble' c = c == '.' || c == 'e'

-- | Construct a word token
-- >>> mkWord "foo"
-- FWordT :foo
--
-- >> mkWord "123"
-- lexer: word: cannot start with a number or hypthen
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
-- >>> lexList "{ 1 2 3 } 234"
-- (FListT [FNumberT 1,FNumberT 2,FNumberT 3]," 234")
lexList :: String -> (Token, String)
lexList ls =
  let (lst, rest) = takeList ls
  in (FListT (lexer lst), rest)

-- |
-- >>> takeList "{\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}}"
-- ("\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}","")
--
-- >>> takeList "{ 1 2\n3 } 234"
-- (" 1 2\n3 "," 234")
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
    go a n [] = error $ "takeList: unterminated list: " <> show (reverse a) <> " '{'-count:" <> show n

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
lexString :: String -> (Token, String)
lexString str = go [] (if head str == '"' then tail str else str)
  where
    go _ []       = error "lexString: unterminated string"
    go acc (x:xs)
      | x == '"'  = (FTextT (reverse acc), xs)
      | x == '\\' = go (head xs : x : acc) (tail xs)
      | otherwise = go (x:acc) xs

-- | Creates a word from a String
word :: String -> Either String FWord
word str
  | length str > 10 = Left "word: too long"
  | head str `elem` '-' : ['0'..'9'] = Left "word: cannot start with a number or hypthen"
  | otherwise = Right (FWord str)

data Token where
  FBoolT :: !Bool -> Token -- ^ A boolean
  FNumberT :: !Int -> Token -- ^ A number (pos or negative)
  FDoubleT :: !Double -> Token -- ^ A number (pos or negative)
  FWordT :: !FWord -> Token -- ^ A word in the forther language
  FTextT :: !String -> Token -- ^ A string
  FListT :: ![Token] -> Token
  deriving (Show)

