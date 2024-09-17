{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-| Haddock {{{
Module      : Lexer
Description : The lexer for the forhter language
Copyright   : (c) 0xmycf, 2024
License     : MIT
Maintainer  : mycf.mycf.mycf@gmail.com
Stability   : experimental
Portability : POSIX
}}}-}
module Lexer ( lexer, LexingResult, LexingError(..) ) where

import           Control.Monad (foldM, when)
import           Data.Char     (isSpace)
import           Data.Functor  (void, ($>))
import           Data.Maybe    (fromJust, isJust)
import qualified State
import           Token         (Token(..), word)

-- Setup for doctests
--
-- $setup
-- import Data.Char (isSpace)
-- import Data.Bifunctor

-- |
-- The lexer defines a semigroup and monoid instance.
-- The semigroup is binary function combines the tokens of the two lexers and
-- sets the line number to the maximum of the two lexers, the column to 0.
-- You probably dont want to use that
--
-- this malformed instance probably means that I dont do this the correct way
data LexerState
  = LexerState
      { input  :: String
        -- ^ the input string. possibly empty
      , tokens :: [Token]
        -- ^ the list of already written Tokens ( TODO not sure if I should keep this )
      , column :: Int
        -- ^ The current column the lexer is in
      , line   :: Int
        -- ^ The current line the lexer is in
      }
  deriving (Show)

pos :: LexerState -> (Row, Col)
pos st = (st.line, st.column)

defaultLexerState :: String -> LexerState
defaultLexerState source = LexerState source [] 0 0

type Col = Int
type Row = Int

-- | TODO make a general error type that wraps these sepcialised ones
-- so I dont have to repeat the col and rows all the time
data LexingError
  = LexingError
      { _type :: LexingErrorType
      , row   :: Row
      , col   :: Col
      }
  deriving (Show)

data LexingErrorType
  = GeneralError
      { message :: String
      }
  | UnexpectedChar
      { char :: Char
      }
  | UnexpectedEOF
  | UnterminatedList
  | UnterminatedString
  | EmptyKeyword
  deriving (Show)

newtype Lexer a
  = Lexer { runLexer' :: State.State LexerState (Either LexingError) a }
  deriving newtype (Applicative, Functor, Monad)

instance MonadFail Lexer where
  fail s = failWith (GeneralError s)

type LexingResult = Either LexingError [Token]

runLexer :: String -> Lexer a -> Either LexingError (a, LexerState)
runLexer source = flip State.runState (defaultLexerState source) . runLexer'

evalLexer :: String -> Lexer a -> Either LexingError a
evalLexer source l = fst <$> runLexer source l

-- | returns the current state of the lexer
get :: Lexer LexerState
get = Lexer State.get

put :: LexerState -> Lexer ()
put = Lexer . State.put

failWith :: LexingErrorType -> Lexer a
failWith errorType =
  get >>= \st ->
    Lexer . State.lift . Left $ LexingError errorType st.line st.column

failErr :: LexingError -> Lexer a
failErr err = Lexer . State.lift $ Left err

write :: Token -> Lexer ()
write tkn = do
  st <- get
  put $ st { tokens = tkn : st.tokens  }

-- | increments the current line number by one
-- and sets the col number to 0
incLine :: Lexer ()
incLine = do
  st <- get
  put st { line = st.line + 1, column = 0 }

-- | increments the current column by one
incCol :: Lexer ()
incCol = do
  st <- get
  put st { column = st.column + 1 }


-- | Does not remove chars from the input.
-- Only advances the column and line counters
--
-- >>> pos . snd <$> runLexer "" (advance "")
-- Right (0,0)
--
-- >>> pos . snd <$> runLexer "" (advance "123\n345")
-- Right (1,3)
--
-- >>> pos . snd <$> runLexer "" (advance "123\n345\n")
-- Right (2,0)
advance :: String -> Lexer ()
advance = do
  mapM_ (\case
    '\n' -> incLine
    _ -> incCol)

-- | The next char is the good case, the bad case is the error
--
-- >>> input . snd <$> runLexer "    foo" dropWhitespace
-- Right "foo"
--
-- >>> fst <$> runLexer "foo" nextChar
-- Right 'f'
next :: (Char -> b) -- ^ what happens in the good case
     -> Lexer b -- ^ what happens in the bad case
     -> Lexer b -- ^ the result
next good bad = do
  st <- get
  case st.input of
    [] -> bad
    (x:xs) -> do
      put st { input = xs }
      advance [x]
      pure (good x)

dropWhitespace :: Lexer ()
dropWhitespace = dropWhileL isSpace

nextChar :: Lexer Char
nextChar = next id (failWith UnexpectedEOF)

-- | Returns the next char without consuming it from the input.
--
-- >>> fst <$> runLexer "foo" peekNextChar
-- Right (Just 'f')
--
-- >>> fst <$> runLexer "" peekNextChar
-- Right Nothing
peekNextChar :: Lexer (Maybe Char)
peekNextChar = do
  st <- get
  case st.input of
    []    -> pure Nothing
    (x:_) -> pure $ pure x

dropTakeHelper ::  Either Int (Char -> Bool) -> Lexer String
dropTakeHelper = \case
  Left n          ->
    reverse <$>
      foldM (\acc _ ->
          peekNextChar >>= \case
            Nothing -> pure acc
            Just c -> nextChar $> c:acc) [] [1..n]
  Right predicate ->
    let loop acc = do {
      c <- peekNextChar;
      if maybe False predicate c
        then nextChar >> loop (fromJust c:acc)
        else pure (reverse acc)
    }
    in loop []

-- >>> Data.Bifunctor.second input <$> runLexer "\t\v \nbar" (dropWhileL isSpace)
-- Right ((),"bar")
--
-- >>> Data.Bifunctor.second input <$> runLexer "foo\nbar" (dropWhileL (not . isSpace))
-- Right ((),"\nbar")
dropWhileL :: (Char -> Bool) -> Lexer ()
dropWhileL predicate = void (dropTakeHelper (Right predicate))

-- >>> Data.Bifunctor.second input <$> runLexer "foo\nbar" (dropL 3)
-- Right ((),"\nbar")
dropL :: Int -> Lexer () -- TODO TEST
dropL = void . dropTakeHelper . Left

-- >>> Data.Bifunctor.second input <$> runLexer "foo\nbar" (takeWhileL (not . isSpace))
-- Right ("foo","\nbar")
--
-- >>> Data.Bifunctor.second input <$> runLexer "\t\v \nbar" (takeWhileL isSpace)
-- Right ("\t\v \n","bar")
takeWhileL :: (Char -> Bool) -> Lexer String
takeWhileL predicate = dropTakeHelper (Right predicate)

-- | Either returns the next 'Char' or 'Nothing'. Consumes one char.
--
-- >>> fst <$> runLexer "foo" maybeNextChar
-- Right (Just 'f')
--
-- >>> fst <$> runLexer "" maybeNextChar
-- Right Nothing
maybeNextChar :: Lexer (Maybe Char)
maybeNextChar = next Just (pure Nothing)

-- | Gets the next token from the input.
-- Token here means a vim like word (iw).
nextToken :: Lexer String
nextToken = takeWhileL (not . isSpace)

-- | TODO FIXME recieves the full line
--
-- >>> lexer "1 2 3"
-- Right [FNumberT 1,FNumberT 2,FNumberT 3]
--
-- >>> lexer "1 2 3 \"b\\ar\""
-- Right [FNumberT 1,FNumberT 2,FNumberT 3,FTextT "b\\ar"]
--
-- >>> lexer "123 2132 30103 2322"
-- Right [FNumberT 123,FNumberT 2132,FNumberT 30103,FNumberT 2322]
--
-- >>> lexer "1 2 print"
-- Right [FNumberT 1,FNumberT 2,FWordT :print]
--
-- >>> lexer "-2"
-- Right [FNumberT (-2)]
--
-- >>> lexer "{-2 {\"some string - 2\"}}"
-- Right [FListT [FNumberT (-2),FListT [FTextT "some string - 2"]]]
--
-- >>> lexer "{ I } 2"
-- Right [FListT [FWordT :I],FNumberT 2]
--
-- >>> lexer "{ } 2"
-- Right [FListT [],FNumberT 2]
--
-- >>> lexer "( { } )"
-- Right []
--
-- >>> lexer "1 2 ( )"
-- Right [FNumberT 1,FNumberT 2]
--
-- >>> lexer "1\n{\n 1 2 3 \n}\n\"some word\""
-- Right [FNumberT 1,FListT [FNumberT 1,FNumberT 2,FNumberT 3],FTextT "some word"]
--
-- >>> lexer "#foo"
-- Right [FKeywordT "foo"]
lexer :: String -> LexingResult
lexer line = reverse . tokens .  snd <$> runLexer line lexer'

lexer' :: Lexer ()
lexer' = do
  dropWhitespace
  may_hd <- peekNextChar
  case may_hd of
    Nothing -> pure ()
    Just ';' -> dropL 1 >>
      either
        fail -- on error (should not happen)
        (write . FWordT) -- on success
        (word ";") -- trying to construct the word
    Just '#' -> do
      dropL 1
      a <- peekNextChar
      case a of
        Nothing -> failWith EmptyKeyword
        _       -> takeWhileL (not . isSpace) >>= write . FKeywordT
    Just '(' ->
      dropWhileL (/= ')') >> dropL 1
    Just (isInt -> True) -> do
      num <- takeWhileL isDoubleOrNum
      if any isDouble' num
          then write $ FDoubleT (read num)
          else write $ FNumberT (read num)
    Just '"' -> lexString >>= write
    Just '{' -> lexList >>= write
    Just c | c `elem` ['t', 'f'] -> do
        value <- nextToken
        case value of
          "true"  -> write $ FBoolT True
          "false" -> write $ FBoolT False
          w       -> mkWord w >>= write
    Just _ -> nextToken >>= mkWord >>= write
  when (isJust may_hd) lexer'

-- | Construct a word token. Fails the Lexer if the word is malformed.
-- Does not consume the word. Thus the proper way to use this is:
--
-- >>> pos . snd <$> runLexer "foo" (nextToken >>= mkWord)
-- Right (0,3)
--
-- Compare this with:
--
-- >>> (\s -> (s.input, s.line, s.column)) . snd <$> runLexer "foo" (mkWord "foo")
-- Right ("foo",0,0)
--
-- >>> fst <$> runLexer "" (mkWord "foo")
-- Right (FWordT :foo)
--
-- >>> runLexer "" (mkWord "123")
-- Left (LexingError {_type = GeneralError {message = "lexer: word: cannot start with a number or hypthen"}, row = 0, col = 0})
mkWord :: String -> Lexer Token
mkWord w = case word w of
  Right w'    -> pure $ FWordT w'
  Left reason -> fail $ "lexer: " <> reason

-- | Predicate for matching an integer
-- >>> all isInt ([ '0'..'9'] ++ ['-'])
-- True
isInt :: Char -> Bool
isInt c = c `elem` ['0'..'9'] || c == '-'

-- | Predicate for matching a double
-- >>> all isDoubleOrNum ([ '0'..'9'] ++ ['-', '.', 'e'])
-- True
isDoubleOrNum :: Char -> Bool
isDoubleOrNum c = isInt c || c == '.' || c == 'e'

-- | Predicate for matching a double without checking isInt
-- >>> isDouble' '.'
-- True
--
-- >>> isDouble' 'e'
-- True
isDouble' :: Char -> Bool
isDouble' c = c == '.' || c == 'e'

-- | Token ~ FListT
--
-- >>> fst <$> runLexer "{}" lexList
-- Right (FListT [])
--
-- >>> fst <$> runLexer "{1}" lexList
-- Right (FListT [FNumberT 1])
--
-- >>> fst <$> runLexer "{123 31}" lexList
-- Right (FListT [FNumberT 123,FNumberT 31])
--
-- >>> fst <$> runLexer "{\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}}" lexList
-- Right (FListT [FTextT "sdlfjsdf",FListT [FNumberT 1,FNumberT 2,FNumberT 3],FTextT "sdfjsdf",FListT [FListT [FNumberT 1],FListT [FNumberT 2,FNumberT 3,FNumberT 4]]])
--
-- >>> input . snd <$> runLexer "{ 1 2 3 } 234" lexList
-- Right " 234"
lexList :: Lexer Token
lexList = do
  lst <- takeList
  case lexer lst of
    Right liste -> pure $ FListT liste
    Left err    -> failErr err

-- |
-- >>> evalLexer "{}" takeList
-- Right ""
--
-- >>> evalLexer "{\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}}" takeList
-- Right "\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}"
--
-- >>> Data.Bifunctor.second input <$> runLexer "{ 1 2\n3 } 234" takeList
-- Right (" 1 2\n3 "," 234")
--
-- >>> evalLexer "{ 1 2\n3 234" takeList
-- Left (LexingError {_type = UnterminatedList, row = 1, col = 5})
takeList :: Lexer String
takeList = do
  reverse <$> go [] (0::Int)
  where
    go :: String -> Int -> Lexer String
    go acc n = do
      may_hd <- maybeNextChar
      case may_hd of
        Nothing -> failWith UnterminatedList
        Just x
          | x == '{' && 0 == n -> go acc (n + 1)
          | x == '{'           -> go (x:acc) (n + 1)
          | x == '}' && 1 == n -> pure acc
          | x == '}'           -> go (x:acc) (n - 1)
          | otherwise          -> go (x:acc) n

-- | Token ~ FTextT
--
-- \" and " are the same in case of chars
-- >>> '\"' == '"'
-- True
--
-- >>> fst <$> runLexer "foo\"" lexString
-- Right (FTextT "foo")
--
-- >>> fst <$> runLexer "\"foo\"" lexString
-- Right (FTextT "foo")
--
-- >>> fst <$> runLexer "\"foo\"bar" lexString
-- Right (FTextT "foo")
--
-- >>> (input . snd) <$> runLexer "\"foo\"bar" lexString
-- Right "bar"
--
-- >>> fst <$> runLexer "\"foo\\\"bar\"" lexString
-- Right (FTextT "foo\\\"bar")
lexString :: Lexer Token
lexString = do
  -- removing the first quote if it exists
  hd <- peekNextChar
  when (Just '"' == hd) (dropL 1)
  go []
  where
    go acc = do
      may_hd <- maybeNextChar
      case may_hd of
        Nothing -> failWith UnterminatedString
        Just '"' -> pure (FTextT (reverse acc)) -- force this evaluation?
        -- the order is reversed, because we reverse the string in the end
        Just '\\' ->
          nextChar >>= \c ->
            go (c:'\\':acc)
        Just c -> do
          go (c:acc)
