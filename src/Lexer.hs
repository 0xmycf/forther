{-# LANGUAGE ViewPatterns #-}
{-| Haddock {{{
Module      : Lexer
Description : The lexer for the forhter language
Copyright   : (c) 0xmycf, 2024
License     : MIT
Maintainer  : mycf.mycf.mycf@gmail.com
Stability   : experimental
Portability : POSIX
}}}-}
module Lexer
  ( lexerS
  , lexerIO
  , LexingResult
  , LexingError(..)
  ) where

import                          CharProducer          (CharProducer(..))
import                          Control.Monad         (foldM, when)
import                          Data.Char             (isSpace)
import                          Data.Functor          (void, ($>))
import                          Data.Functor.Identity (Identity, runIdentity)
import                          Data.Maybe            (fromJust, isJust)
import                          IOPeekBuffer          (IOPeekBuffer, withBuffer)
import                qualified Result
import                          Result                (Result)
import                qualified State
import                          System.IO             (Handle)
import                          MonadLift             (MonadLift(..))
import                          HasState                 (HasState(..))
-- this is needed so we dont have a cyclic dependency
-- (for the IsString instance for [Token] (see src/Token.hs))
import {-# SOURCE #-}           Token                 (Token(..), word)

-- Setup for doctests
--
-- $setup
-- import Data.Char (isSpace)
-- import Data.Bifunctor

-- | A mutable 'String' Monad, where mutability is emulated using a 'State' transformer
newtype MString a
  = MString { withString' :: State.State String Identity a }
  deriving newtype (Applicative, Functor, Monad)

withString :: String -> MString a -> a
withString s mstr = runIdentity $ State.evalState (withString' mstr) s
{-# INLINE withString #-}

instance CharProducer MString where
  produceChar = MString $ do
    s <- get
    case s of
      []     -> pure Nothing
      (x:xs) -> put xs $> Just x

  peekChar = MString $ do
    s <- get
    case s of
      []    -> pure Nothing
      (x:_) -> pure $ Just x

-- | The interal state of the Lexer
data LexerState m
  = LexerState
      { produce :: m (Maybe Char)
        -- ^ The input source. If it produces 'Nothing' the source is exhausted.
      , peek    :: m (Maybe Char)
        -- ^ The next char in the input source. Does not consume the char.
        -- as with 'produce' if it produces 'Nothing' the source is exhausted.
      , tokens  :: [Token]
        -- ^ the list of already written Tokens
      , column  :: Int
        -- ^ The current column the lexer is in
      , line    :: Int
        -- ^ The current line the lexer is in
      }

defaultLexerState :: CharProducer m => LexerState m
defaultLexerState = LexerState produceChar peekChar [] 0 0
{-# INLINE defaultLexerState #-}

type Col = Int
type Row = Int

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

newtype Lexer m a
  = Lexer { runLexer' :: State.State (LexerState m) (Result LexingError m) a }
  deriving newtype (Applicative, Functor, HasState (LexerState m), Monad)


instance Monad m => MonadFail (Lexer m) where
  fail s = failWith (GeneralError s)
  {-# INLINE fail #-}

instance MonadLift Lexer where
  lift :: Monad m
       => m a
       -> Lexer m a
  lift = Lexer . MonadLift.lift . MonadLift.lift
  {-# INLINE lift #-}

-- | The result of the lexer.
-- It is either a 'LexingError' if some error was encounterd during the lexing phase
-- or a list of 'Token's and possibly the leftover 'String' of the input.
--
-- Only the 'MString' Monad is able to produce a 'String' as leftover.
type LexingResult = Either LexingError ([Token], Maybe String)

runLexer :: CharProducer m
         => Lexer m a
         -> m (Either LexingError (a, LexerState m))
runLexer = Result.runResult
                . flip State.runState defaultLexerState
                . runLexer'
{-# INLINE runLexer #-}

-- | Lexees a String
stringLexer :: String
            -> Lexer MString a
            -> Either LexingError (a, LexerState MString)
stringLexer source = withString source . runLexer
{-# INLINE lexString #-}

-- | Lexes a file or some other handle (e.g. stdin)
handleLexer :: Handle
            -> Lexer IOPeekBuffer a
            -> IO (Either LexingError (a, LexerState IOPeekBuffer))
handleLexer h = withBuffer h . runLexer
{-# INLINE handleLexer #-}

failErr :: Monad m => LexingError -> Lexer m a
failErr = Lexer . MonadLift.lift . Result.fail
{-# INLINE failErr #-}

failWith :: Monad m => LexingErrorType -> Lexer m a
failWith errorType =
  get >>= \st ->
    failErr $ LexingError errorType st.line st.column

write :: Monad m => Token -> Lexer m ()
write tkn = do
  st <- get
  put $ st { tokens = tkn : st.tokens  }

-- | increments the current line number by one
-- and sets the col number to 0
incLine :: Monad m => Lexer m ()
incLine = do
  st <- get
  put st { line = st.line + 1, column = 0 }

-- | increments the current column by one
incCol :: Monad m => Lexer m ()
incCol = do
  st <- get
  put st { column = st.column + 1 }

-- | Does not remove chars from the input.
-- Only advances the column and line counters
--
-- >>> (\s -> (s.line, s.column)) . snd <$> stringLexer "" (advance '1')
-- Right (0,1)
--
-- >>> (\s -> (s.line, s.column)) . snd <$> stringLexer "" (advance '\n')
-- Right (1,0)
advance :: Monad m => Char -> Lexer m ()
advance = \case
  '\n' -> incLine
  _    -> incCol
{-# INLINE advance #-}

-- | The next char is the good case, the bad case is the error
--
-- >>> fst <$> stringLexer "foo" nextChar
-- Right 'f'
next :: Monad m
     => (Char -> b) -- ^ what happens in the good case
     -> Lexer m b -- ^ what happens in the bad case
     -> Lexer m b -- ^ the result
next good bad = do
  st <- get
  c <- lift st.produce
  case c of
    Nothing -> bad
    Just c' -> advance c' $> good c'

dropWhitespace :: Monad m => Lexer m ()
dropWhitespace = dropWhileL isSpace
{-# INLINE dropWhitespace #-}

nextChar :: Monad m => Lexer m Char
nextChar = next id (failWith UnexpectedEOF)
{-# INLINE nextChar #-}

-- | Returns the next char without consuming it from the input.
--
-- >>> fst <$> stringLexer "foo" peekNextChar
-- Right (Just 'f')
--
-- >>> fst <$> stringLexer "" peekNextChar
-- Right Nothing
peekNextChar :: Monad m => Lexer m (Maybe Char)
peekNextChar = do
  st <- get
  lift st.peek

dropTakeHelper :: Monad m
               => Either Int (Char -> Bool)
               -> Lexer m String
dropTakeHelper = \case
  Left n ->
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

-- >>> snd <$> stringLexer "\t\v \nbar" (dropWhileL isSpace)
--
-- >>> snd <$> stringLexer "foo\nbar" (dropWhileL (not . isSpace))
-- Right ((),"\nbar")
dropWhileL :: Monad m => (Char -> Bool) -> Lexer m ()
dropWhileL predicate = void (dropTakeHelper (Right predicate))

-- >>> snd <$> stringLexer "foo\nbar" (dropL 3)
-- Right ((),"\nbar")
dropL :: Monad m => Int -> Lexer m ()
dropL = void . dropTakeHelper . Left

-- >>> snd <$> stringLexer "foo\nbar" (takeWhileL (not . isSpace))
-- Right ("foo","\nbar")
--
-- >>> snd <$> stringLexer "\t\v \nbar" (takeWhileL isSpace)
-- Right ("\t\v \n","bar")
takeWhileL :: Monad m => (Char -> Bool) -> Lexer m String
takeWhileL predicate = dropTakeHelper (Right predicate)

-- | Either returns the next 'Char' or 'Nothing'. Consumes one char.
--
-- >>> fst <$> stringLexer "foo" maybeNextChar
-- Right (Just 'f')
--
-- >>> fst <$> stringLexer "" maybeNextChar
-- Right Nothing
maybeNextChar :: Monad m => Lexer m (Maybe Char)
maybeNextChar = next Just (pure Nothing)

-- | Gets the next token from the input.
-- Token here means a vim like word (iw).
nextToken :: Monad m => Lexer m String
nextToken = takeWhileL (not . isSpace)

-- | Lexes the input source 'Monad' (m (Maybe Char)) and returns the list of tokens.
--
-- >>> fst <$> lexerS "1 2 3"
-- Right [FNumberT 1,FNumberT 2,FNumberT 3]
--
-- >>> fst <$> lexerS "1 2 3 \"b\\ar\""
-- Right [FNumberT 1,FNumberT 2,FNumberT 3,FTextT "b\\ar"]
--
-- >>> fst <$> lexerS "123 2132 30103 2322"
-- Right [FNumberT 123,FNumberT 2132,FNumberT 30103,FNumberT 2322]
--
-- >>> fst <$> lexerS "1 2 print"
-- Right [FNumberT 1,FNumberT 2,FWordT :print]
--
-- >>> fst <$> lexerS "-2"
-- Right [FNumberT (-2)]
--
-- >>> fst <$> lexerS "{-2 {\"some string - 2\"}}"
-- Right [FListT [FNumberT (-2),FListT [FTextT "some string - 2"]]]
--
-- >>> fst <$> lexerS "{ I } 2"
-- Right [FListT [FWordT :I],FNumberT 2]
--
-- >>> fst <$> lexerS "{ } 2"
-- Right [FListT [],FNumberT 2]
--
-- >>> fst <$> lexerS "( { } )"
-- Right []
--
-- >>> fst <$> lexerS "1 2 ( )"
-- Right [FNumberT 1,FNumberT 2]
--
-- >>> fst <$> lexerS "1\n{\n 1 2 3 \n}\n\"some word\""
-- Right [FNumberT 1,FListT [FNumberT 1,FNumberT 2,FNumberT 3],FTextT "some word"]
--
-- >>> fst <$> lexerS "#foo"
-- Right [FKeywordT "foo"]
lexerS :: String -> LexingResult
lexerS string = do
  (rest, st) <- stringLexer string (lexer' >> leftover)
  pure (reverse . tokens $ st, Just rest)
{-# INLINE lexerS #-}

-- | reads all remainig input
leftover :: Lexer MString String
leftover = unfoldM maybeNextChar
  where
    unfoldM f = f >>= \case
      Nothing -> pure []
      Just c  -> (c:) <$> unfoldM f

lexerIO :: Handle -> IO LexingResult
lexerIO h = fmap ((, Nothing) . reverse . tokens . snd) <$> handleLexer h lexer'
{-# INLINE lexerIO #-}

-- | The main lexing function
lexer' :: Monad m => Lexer m ()
lexer' = do
  dropWhitespace
  may_hd <- peekNextChar
  case may_hd of
    Nothing -> pure ()
    Just c
      | c == ';' || c == ':' -> do
      dropL 1
      either
        fail -- on error (should not happen)
        (write . FWordT) -- on success
        (word [c]) -- trying to construct the word
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

-- | Construct a word token. Fails the Lexer m  if the word is malformed.
-- Does not consume the word. Thus the proper way to use this is:
--
-- >>> (\s -> (s.line, s.column)) . snd <$> stringLexer "foo" (nextToken >>= mkWord)
-- Right (0,3)
--
-- Compare this with (not that 'leftover' causes the lexer to advance to the end of the input
--
-- >>> (\(rest,st) -> (rest, st.line, st.column)) <$> stringLexer "foo" (mkWord "foo" >> leftover)
-- Right ("foo",0,3)
--
-- >>> fst <$> stringLexer "" (mkWord "foo")
-- Right (FWordT :foo)
--
-- >>> fst <$> stringLexer "" (mkWord "123")
-- Left (LexingError {_type = GeneralError {message = "lexer: word: cannot start with a number or hypthen"}, row = 0, col = 0})
mkWord :: Monad m => String -> Lexer m Token
mkWord w = case word w of
  Right w'    -> pure $ FWordT w'
  Left reason -> fail $ "lexer: " <> reason

-- | Predicate for matching an integer
-- >>> all isInt ([ '0'..'9'] ++ ['-'])
-- True
isInt :: Char -> Bool
isInt c = c `elem` ['0'..'9'] || c == '-'
{-# INLINE isInt #-}

-- | Predicate for matching a double
-- >>> all isDoubleOrNum ([ '0'..'9'] ++ ['-', '.', 'e'])
-- True
isDoubleOrNum :: Char -> Bool
isDoubleOrNum c = isInt c || c == '.' || c == 'e'
{-# INLINE isDoubleOrNum #-}

-- | Predicate for matching a double without checking isInt
-- >>> isDouble' '.'
-- True
--
-- >>> isDouble' 'e'
-- True
isDouble' :: Char -> Bool
isDouble' c = c == '.' || c == 'e'
{-# INLINE isDouble' #-}

-- | Token ~ FListT
--
-- >>> fst <$> stringLexer "{}" lexList
-- Right (FListT [])
--
-- >>> fst <$> stringLexer "{1}" lexList
-- Right (FListT [FNumberT 1])
--
-- >>> fst <$> stringLexer "{123 31}" lexList
-- Right (FListT [FNumberT 123,FNumberT 31])
--
-- >>> fst <$> stringLexer "{\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}}" lexList
-- Right (FListT [FTextT "sdlfjsdf",FListT [FNumberT 1,FNumberT 2,FNumberT 3],FTextT "sdfjsdf",FListT [FListT [FNumberT 1],FListT [FNumberT 2,FNumberT 3,FNumberT 4]]])
--
-- >>> fst <$> stringLexer "{ 1 2 3 } 234" (lexList >> leftover)
-- Right " 234"
lexList :: Monad m => Lexer m Token
lexList = do
  lst <- takeList
  let foo = lexerS lst
  case foo of
    Right (liste, _) -> pure $ FListT liste
    Left err         -> failErr err

-- |
-- >>> fst <$> stringLexer "{}" takeList
-- Right ""
--
-- >>> fst <$> stringLexer "{\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}}" takeList
-- Right "\"sdlfjsdf\" {1 2 3} \"sdfjsdf\" {{1} { 2 3 4 }}"
--
-- >>> fst <$> stringLexer "{ 1 2\n3 } 234" (takeList >> leftover)
-- Right " 234"
--
-- >>> fst <$> stringLexer "{ 1 2\n3 234" takeList
-- Left (LexingError {_type = UnterminatedList, row = 1, col = 5})
takeList :: Monad m => Lexer m String
takeList = do
  reverse <$> go [] (0::Int)
  where
    go :: Monad m => String -> Int -> Lexer m String
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
-- >>> fst <$> stringLexer "foo\"" lexString
-- Right (FTextT "foo")
--
-- >>> fst <$> stringLexer "\"foo\"" lexString
-- Right (FTextT "foo")
--
-- >>> fst <$> stringLexer "\"foo\"bar" lexString
-- Right (FTextT "foo")
--
-- >>> fst <$> stringLexer "\"foo\"bar" (lexString >> leftover)
-- Right "bar"
--
-- >>> fst <$> stringLexer "\"foo\\\"bar\"" lexString
-- Right (FTextT "foo\\\"bar")
lexString :: Monad m => Lexer m Token
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
