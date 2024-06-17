{-# LANGUAGE OverloadedRecordDot, PatternSynonyms, ViewPatterns #-}
module Repl ( repl ) where

import qualified BinTree                as BT
import           Control.Exception      (AsyncException, catch)
import           Control.Exception.Base (AsyncException(UserInterrupt))
import           Control.Monad          (forM_, void, when)
import           Data.Bifunctor         (Bifunctor(first))
import           Dictionary             (DictEntry(..), Machine(..), def)
import           Prelude                hiding (fail)
import           Result                 (Result(..), fail, lift, liftIO,
                                         pattern Err, pattern Ok, orFailWith)
import           Save                   (headS)
import           Stack                  (Stack, StackElement(..), empty, pop, prettyPrint, push,
                                         toStackElement, toToken)
import qualified State
import           State                  (execState, get, modify, put)
import           System.IO              (hFlush, stdout)
import           Token                  (Token(..), pattern Exec, word)

type State a = State.State Machine IO a

prefix :: String
prefix = "$> "

repl :: IO ()
repl =
  void (execState loop (Machine def empty)) --`catch` (\(ErrorCall e) -> putStrLn e >> repl)
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
    Nothing           -> pure ()
    Just c | c == ':' -> get >>= \oldState ->
      runResult (define line) >>= \case
      Ok  dr' -> liftIO $ print dr'
      Err e   -> liftIO (putStrLn e) >> put oldState
    Just _            -> do
      res <- runResult (eval . lexer $ line)
      oldState <- get
      case res of
        Err e -> liftIO (putStrLn e) >> put oldState
        Ok _  -> getStack >>= liftIO . putStrLn . ("Stack: " <>) . prettyPrint
  loop

getStack :: State (Stack StackElement)
getStack = stack <$> get
{-# INLINE getStack #-}

type Stack' = Stack StackElement

setStack :: (Stack' -> Stack') -> Machine -> Machine
setStack new m = m { stack = new m.stack }
{-# INLINE setStack #-}

type Res = Result String (State.State Machine IO)

eval :: [Token] -> Res ()
eval ws = forM_ ws translate
  where
    translate :: Token -> Res ()
    translate = \case
      -- TODO leave this here?
      FWordT Exec -> do
        stack <- stack <$> lift get
        (!ls, !rest) <- pop stack `orFailWith` "eval: exec: StackUnderflow"
        lift $ modify $ setStack $ const rest
        case ls of
          List ts -> eval (map toToken ts)
          _       -> fail "eval: exec: not a list"
      FWordT w   -> do
        dict <- dictionary <$> lift get
        case dict `BT.lookup` w of
          Just entry -> execute entry
          Nothing    -> fail $ "eval: word not found in dictionary; " <> show w
      tkn -> modifyStack (toStackElement tkn)

modifyStack :: StackElement -> Res ()
modifyStack = lift . modify . setStack . push
{-# INLINE modifyStack #-}

execute :: DictEntry -> Res ()
execute (Literal str) = eval $ lexer str
execute (BuiltIn f)   = void f
{-# INLINE execute #-}

-- | Defines a custom new word in the dictionary
define :: String -> Res DefResult
define str =
  let str' = dropWhile (`elem` [' ', ':']) str in
  let (w, rest) = span (/= ' ') str' in
  case word w of
    Left reason -> fail $ "define: " <> reason
    Right fword ->
      dr w rest <$ lift (modify $ \m -> rest `seq` m { dictionary = BT.insert fword (Literal rest) m.dictionary })

newtype DefResult
  = DefResult (String, String)

-- >>> dr "foo" "bar"
-- Defined: foo as bar
dr :: String -> String -> DefResult
dr = curry DefResult

instance Show DefResult where
  show (DefResult (w, rest))  = "Defined: " <> w <> " as " <> rest

