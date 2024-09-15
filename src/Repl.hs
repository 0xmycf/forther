{-# LANGUAGE OverloadedRecordDot, PatternSynonyms, ViewPatterns #-}
module Repl ( repl ) where

import qualified BinTree           as BT
import           Control.Exception (AsyncException(UserInterrupt), catch)
import           Control.Monad     (forM_, void, when)
import           Data.Char         (isSpace)
import           Dictionary        (DictEntry(..), Machine(..), def)
import           Prelude           hiding (fail)
import           Result            (Result(..), fail, lift, liftIO, orFailWith,
                                    pattern Err, pattern Ok)
import           Save              (headS)
import           Stack             (Stack, StackElement(..), empty, pop,
                                    prettyPrint, push, toStackElement, toToken)
import qualified State
import           State             (execState, get, modify, put)
import           System.IO         (hFlush, stdout)
import           System.IO.Error   (isEOFError)
import           Token             (Token(..), lexer, pattern Colon,
                                    pattern Exec, word)

type State a = State.State Machine IO a

prefix :: String
prefix = "$> "

bye :: String
bye = "\ESC[2K\ESC[0Ggoodbye"

repl :: IO ()
repl =
  void (execState loop (Machine (def eval) empty))
      -- `catch` (\(ErrorCall e) -> putStrLn e >> repl) -- TODO this resets the dictionary so not good
      `Control.Exception.catch` (\(e::Control.Exception.AsyncException) -> when (e == Control.Exception.UserInterrupt) $ putStrLn "" >> putStrLn bye)
      `Control.Exception.catch` (\(e::IOError) -> when (isEOFError e) $ putStrLn bye)

loop :: State ()
loop = do
  liftIO do { putStr prefix; hFlush stdout }
  line <- dropWhile isSpace <$> liftIO getLine
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
execute (BuiltIn f)   = void f
{-# INLINE execute #-}

-- | Defines a custom new word in the dictionary
define :: String -> Res DefResult
define str =
  let str' = dropWhile (\e -> isSpace e || e `elem` [' ', ':']) str in
  let (w, dropWhile isSpace -> rest) = span (/= ' ') str' in
  case word w of
    Left reason -> Result.fail $ "define: " <> reason
    Right fword ->
      dr w rest <$ Result.lift (State.modify $ \m -> rest `seq` m { dictionary = BT.insert fword (Literal rest) m.dictionary })

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
