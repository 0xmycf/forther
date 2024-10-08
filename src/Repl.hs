{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Repl ( repl ) where

import qualified BinTree           as BT
import           Control.Exception (AsyncException(UserInterrupt),
                                    SomeException(SomeException), catch, try)
import           Control.Monad     (forM_, unless, void, when)
import           Data.Char         (isSpace)
import qualified Data.Char         as Char
import           Data.Functor      (($>))
import           Data.List         (dropWhileEnd)
import           Data.Maybe        (fromMaybe)
import           Dictionary        (DictEntry(..), FortherCompileMode(..),
                                    Machine(..), ReadMode(..), def,
                                    readModeIsFile, setCompileMode, setRunMode)
import           Prelude           hiding (fail)
import           Result            (Result(..), fail, lift, liftIO, orFailWith,
                                    pattern Err, pattern Ok)
import           Save              (headS, lastS)
import           Stack             (Stack, StackElement(..), empty, pop,
                                    prettyPrint, push, toStackElement, toToken)
import qualified State
import           State             (execState, get, modify, put, runState)
import           System.IO         (IOMode(ReadMode), hFlush, hGetLine, stdout,
                                    withFile)
import           System.IO.Error   (isEOFError)
import           Token             (Token(..), lexer, pattern Colon,
                                    pattern Exec, word)

type State a = State.State Machine IO a

prefix :: String
prefix = "$> "

bye :: String
bye = "\ESC[2K\ESC[0Ggoodbye"

repl :: ReadMode -> IO ()
repl readMode =
  case readMode of
    File path ->
      withFile path ReadMode
      ( \file -> run (Just file) )
    Repl -> run Nothing
  where
    run mayFile =
      void (execState loop (Machine (def eval) empty RunMode readMode mayFile))
          -- `catch` (\(ErrorCall e) -> putStrLn e >> repl) -- this resets the dictionary so not good
          `catch` (\(e::AsyncException) -> when (e == UserInterrupt) $ putStrLn "" >> putStrLn bye)
          `catch` (\(e::IOError) -> when (isEOFError e) $ putStrLn bye)

loop :: State ()
loop = do
  line <- promptWithInput
  readMode <- readMode <$> get
  case headS line of
    Nothing           -> pure ()
    Just c | c == ':' -> get >>= \oldState ->
      runResult (define line) >>= \case
        Ok  dr' -> liftIO $ print dr'
        Err e   -> liftIO (putStrLn e) >> put oldState
    Just _ -> do
      oldState <- get
      res <- liftIO $ try $ runState (runResult (eval . lexer $ line)) oldState
      case res of
        Left (SomeException e) -> liftIO (print e)
        Right (Err e, _)       -> liftIO (putStrLn e)
        Right (Ok _, ns)       ->
          put ns >>
            unless (readModeIsFile readMode)
              (getStack >>= liftIO . putStrLn . ("Stack: " <>) . prettyPrint)
  loop

promptWithInput :: State String
promptWithInput = do
  readMode <- readMode <$> get
  mode <- mode <$> get
  when (mode /= CompileMode && readMode == Repl) $
    liftIO do
      putStr prefix
      hFlush stdout
  readInput ""

readInput :: String -> State String
readInput buf = do
  readMode <- readMode <$> get
  file <- file <$> get
  line <- trim <$> liftIO (if readMode == Repl
    then
      getLine
    else
      hGetLine (fromMaybe
        (error "A handle should be provided if ReadMode is set to File")
        file))
  mode <- mode <$> get
  case mode of
    CompileMode -> case lastS line of
      Just ';' -> State.modify setRunMode $> buf <> line
      _        -> readInput $ buf <> line <> " "
    RunMode -> case headS line of
      Just ':' -> case lastS line of
        Just ';' -> pure $ buf <> line
        _        -> State.modify setCompileMode >> readInput (buf <> line <> " ")
      _        -> pure $ buf <> line

trim :: String -> String
trim = dropWhileEnd Char.isSpace .  dropWhile Char.isSpace

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
      -- TODO this is for debugging
      FWordT Colon -> Result.fail "eval: : not allowed in this context"
      FWordT w   -> do
        dict <- dictionary <$> lift get
        case dict `BT.lookup` w of
          Just entry -> execute entry
          Nothing    -> fail $ "eval: word not found in dictionary: " <> show w
      tkn -> modifyStack (toStackElement tkn)

modifyStack :: StackElement -> Res ()
modifyStack = lift . modify . setStack . push
{-# INLINE modifyStack #-}

execute :: DictEntry -> Res ()
execute (Literal str) = eval $ lexer str
execute (BuiltIn f)   = f {- do -}
  -- s <- lift get
  -- foo <- liftIO $ runState (runResult f) s `catch` \(SomeException e) -> error (show e <> " sljfljfd ")
  -- error "execute"
{-# INLINE execute #-}

-- | Defines a custom new word in the dictionary
define :: String -> Res DefResult
define str = do
  let str' = dropWhile (\e -> isSpace e || e `elem` [' ', ':']) str
      (w, trim -> rest) = span (/= ' ') str'
  when (null w) $ Result.fail "define: empty word"
  when (null rest) $ Result.fail "define: empty definition"
  let rest' =
          case lastS rest of
            Just ';' -> init rest
            _        -> rest
  case word w of
    Left reason -> Result.fail $ "define: " <> reason
    Right fword ->
      dr w rest' <$ Result.lift (State.modify $ \m -> rest' `seq` m { dictionary = BT.insert fword (Literal rest') m.dictionary })

newtype DefResult
  = DefResult (String, String)

-- >>> dr "foo" "bar"
-- Defined: foo as bar
dr :: String -> String -> DefResult
dr = curry DefResult

instance Show DefResult where
  -- >>> show (DefResult ("foo", "bar"))
  -- Defined: foo as bar
  --
  -- >>> show (DefResult ("foo", "  bar baz"))
  -- Defined: foo as   bar baz
  show (DefResult (w, rest))  = "Defined: " <> w <> " as " <> rest
