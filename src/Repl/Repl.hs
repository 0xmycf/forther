{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Repl.Repl ( repl ) where

import qualified BinTree                as BT
import           Control.Exception      (AsyncException(UserInterrupt),
                                         SomeException(SomeException), catch,
                                         try)
import           Control.Monad          (MonadFail(..), forM, unless, void,
                                         when)
import qualified Data.Char              as Char
import           Data.Functor           (($>))
import           Data.List              (dropWhileEnd)
import           HasState               (HasState(..))
import           Prelude                hiding (fail)
import           Save                   (headS, lastS)
import           Stack                  (Stack, StackElement(..), empty,
                                         prettyPrint, push, toStackElement)
import           System.IO              (hFlush, hGetLine, stdout)
import           System.IO.Error        (isEOFError)
import           Token                  (FWord, Flag(Immediate), Token(..),
                                         fromToken, isImmediate, isImmediateT,
                                         isKeyword, isSemiColon, isWord,
                                         prettyPrint, setFlags, toKeyword, word)

import qualified BinTree                as Bt
import           Control.DeepSeq        (deepseq, force)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromJust, fromMaybe, isJust, isNothing)
import           Lexer                  (lexerS)
import           Repl.Machine           (DictEntry(..),
                                         FortherCompileMode(RunMode, WordDefineMode),
                                         Interpreter(Interpreter, defineCtx, dictionary, mode, readMode, stack),
                                         ReadMode(..), def, readModeIsFile,
                                         setCompileMode, setMode, setRunMode)
import           Repl.Types             (DefineCtx(..), ReplState,
                                         StackOperation, ctxAdd, mkDefineCtx,
                                         runRepl, runStackOperation, setStack)
import qualified Result

prefix :: String
prefix = "$> "

bye :: String
bye = "\ESC[2K\ESC[0Ggoodbye"

repl :: ReadMode -> IO ()
repl readMode =
  dropLineIfFile readMode >>
  void (runRepl loop (machine readMode))
      -- `catch` (\(ErrorCall e) -> putStrLn e >> repl) -- this resets the dictionary so not good
      `catch` (\(e::AsyncException) -> when (e == UserInterrupt) $ putStrLn "" >> putStrLn bye)
      `catch` (\(e::IOError) -> when (isEOFError e) $ putStrLn bye)
  where
    machine = Interpreter (def eval) Stack.empty RunMode Nothing
    dropLineIfFile = \case
      Repl      -> pure ()
      -- technically I should check whether the first line
      -- is a shebang and then consume it, but we have to refactor
      -- all of this anyway, so I'll leave this for now.
      File file -> void $ hGetLine file

loop :: ReplState ()
loop = do
  line <- promptWithInput
  readMode <- readMode <$> get
  case headS line of
    Nothing -> pure ()
    Just _  -> do
      oldState <- get
      case lexerS line of
        Result.Err err        ->
          liftIO (putStrLn $ "Lexer error: " <> show err) >> put oldState
        Result.Ok (tokens, _) -> do
          res <- liftIO $ try $ runRepl (runStackOperation (eval tokens)) oldState
          case res of
            Left (SomeException  e) -> liftIO (print e)
            Right (Result.Err e, _) -> liftIO (putStrLn e)
            Right (Result.Ok _, ns) ->
              put ns >>
                -- we dont want to print anything when interpreting from a file
                unless (readModeIsFile readMode)
                  (getStack >>= liftIO . putStrLn . ("Stack: " <>) . Stack.prettyPrint)
  loop

promptWithInput :: ReplState String
promptWithInput = do
  readMode <- readMode <$> get
  mode <- mode <$> get
  when (mode /= WordDefineMode && readMode == Repl) $
    Result.liftIO do
      putStr prefix
      hFlush stdout
  readInput -- ""

readInput :: ReplState String
readInput = do
  readMode <- readMode <$> get
  trim <$> Result.liftIO (case readMode of
      Repl      -> getLine
      File file -> hGetLine file)

trim :: String -> String
trim = dropWhileEnd Char.isSpace . dropWhile Char.isSpace

getStack :: ReplState (Stack StackElement)
getStack = stack <$> get
{-# INLINE getStack #-}

withRunMode :: HasState Interpreter m => m b -> m b
withRunMode action = do
  inter <- get
  let mode = inter.mode
  put $ setRunMode inter
  res <- action
  newInter <- get
  put $ setMode mode newInter
  pure res

eval :: [Token] -> StackOperation ()
eval = go
  where
  -- go decides whether to define a word or to run it
  go tkns@(tkn:xs) = do
    mode <- mode <$> get
    case mode of
      WordDefineMode -> do
        may_ctx <- defineCtx <$> get
        when (isNothing may_ctx) $
          mkDefineCtx tkn
        rest <- defineWord tkns
        go rest
      RunMode -> translate tkn >> go xs
  go [] = pure ()

  -- helper function to check whether we are sitll defining a word or running the program
  -- define_ then calls this function instead of itself
  defIfDefining []   = pure []
  defIfDefining tkns = do
    mode <- mode <$> get
    case mode of
      RunMode        -> pure tkns
      WordDefineMode -> defineWord tkns
  {-# INLINE defIfDefining #-}

  -- | handles all the different cases for defining a word
  -- this might be:
  -- executing an immediate word instead of adding it to the dictionary
  -- adding a keyword (flag) to the final word
  defineWord (hd:tkns)
    | FKeywordT _ <- hd = ctxAdd hd >> defIfDefining tkns
    | otherwise = executeIfImmediate hd >>= \case
        True  -> defIfDefining tkns
        False -> ctxAdd hd >> defIfDefining tkns
  defineWord [] = pure []

  -- | if the given token is a word and it is immediate it executes the word
  executeIfImmediate :: Token -> StackOperation Bool
  executeIfImmediate = \case
    (FWordT w) -> do
      dict <- dictionary <$> get
      case dict `BT.lookupKey` w of
        Just key | isImmediate key -> do
          let value = dict `Bt.lookup` key
          execute' True $ fromJust value
          pure True
        _                          -> pure False
    _ -> pure False

  -- | actually excuting the word if it is inside the dictionary
  translate :: Token -> StackOperation ()
  translate = \case
    FWordT w -> do
      dict <- dictionary <$> get
      case dict `BT.lookup` w of
        Just entry -> execute' (isImmediate w) entry
        Nothing    -> fail $ "eval: word not found in dictionary: " <> show w
    FKeywordT _ -> fail "There are currently no Keywords which can be translated"
    tkn -> modifyStack (toStackElement tkn)

modifyStack :: StackElement -> StackOperation ()
modifyStack = modify . setStack . push
{-# INLINE modifyStack #-}

execute' :: Bool -> DictEntry -> StackOperation ()
execute' _ (BuiltIn f)        = f
execute' True (Literal tkns)  = withRunMode $ eval tkns
-- withRunMode makes no difference here,
-- since we are already in RunMode already (hopefully)
execute' False (Literal tkns) = eval tkns
{-# INLINE execute' #-}
