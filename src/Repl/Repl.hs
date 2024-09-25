{-# LANGUAGE PatternSynonyms #-}
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
import           Token                  (FWord, Flag, Token(..), fromToken,
                                         isImmediate, isImmediateT, isKeyword,
                                         isSemiColon, isWord, pattern SemiColon,
                                         setFlags, toKeyword, word)

import           Control.DeepSeq        (deepseq, force)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromJust, fromMaybe, isJust)
import           Lexer                  (lexerS)
import           Repl.Machine           (DictEntry(..),
                                         FortherCompileMode(RunMode, WordDefineMode),
                                         Interpreter(Interpreter, dictionary, mode, readMode, stack),
                                         ReadMode(..), def, readModeIsFile,
                                         setCompileMode, setRunMode)
import           Repl.Types             (ReplState, StackOperation, runRepl,
                                         runStackOperation, setStack)
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
    machine = Interpreter (def eval) empty RunMode
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
    Nothing           -> pure ()
    Just _ -> do
      oldState <- get
      case lexerS line of
        Result.Err err -> liftIO (putStrLn $ "Lexer error: " <> show err) >> put oldState
        Result.Ok (tokens, _) -> do
          res <- liftIO $ try $ runRepl (runStackOperation (eval tokens)) oldState
          case res of
            Left (SomeException e)  -> liftIO (print e)
            Right (Result.Err e, _) -> liftIO (putStrLn e)
            Right (Result.Ok _, ns) ->
              put ns >>
                unless (readModeIsFile readMode)
                  (getStack >>= liftIO . putStrLn . ("Stack: " <>) . prettyPrint)
  loop

promptWithInput :: ReplState String
promptWithInput = do
  readMode <- readMode <$> get
  mode <- mode <$> get
  when (mode /= WordDefineMode && readMode == Repl) $
    Result.liftIO do
      putStr prefix
      hFlush stdout
  readInput ""

readInput :: String -> ReplState String
readInput buf = do
  readMode <- readMode <$> get
  line <- trim <$> Result.liftIO (case readMode of
      Repl      -> getLine
      File file -> hGetLine file)
  mode <- mode <$> get
  case mode of
    WordDefineMode -> case lastS line of
      Just ';' -> modify setRunMode $> buf <> line
      _        -> readInput $ buf <> line <> " "
    RunMode -> case headS line of
      Just ':' -> case lastS line of
        Just ';' -> pure $ buf <> line
        _        -> modify setCompileMode >> readInput (buf <> line <> " ")
      _        -> pure $ buf <> line

trim :: String -> String
trim = dropWhileEnd Char.isSpace . dropWhile Char.isSpace

getStack :: ReplState (Stack StackElement)
getStack = stack <$> get
{-# INLINE getStack #-}

data DefineCtx
  = DefineCtx
      { name  :: FWord
      , flags :: [Flag]
      , body  :: [Token]
      }

defineCtx :: Token -> StackOperation DefineCtx
defineCtx = \case
  FWordT name -> pure $ DefineCtx name [] []
  _ -> fail "defineCtx: expected a word"
{-# INLINE defineCtx #-}

eval :: [Token] -> StackOperation ()
eval = go
  where
    -- go decides whether to define a word or to run it
    go (tkn:xs) = do
      mode <- mode <$> get
      case mode of
        WordDefineMode -> do
          (rest, ctx) <- defineCtx tkn >>= define_ xs
          modify $ \m ->
            ctx.body `deepseq` m
              { dictionary
                = BT.insert
                  (force $ setFlags ctx.name ctx.flags)
                  (Literal . reverse $ ctx.body) m.dictionary
              }
          go rest
        RunMode -> translate tkn >> go xs
    go [] = pure ()

    -- helper function to check whether we are sitll defining a word or running the program
    -- define_ then calls this function instead of itself
    defIfDefining tkns ctx = do
      mode <- mode <$> get
      case mode of
        RunMode        -> pure (tkns, ctx)
        WordDefineMode -> define_ tkns ctx
    {-# INLINE defIfDefining #-}

    -- | handles all the different cases for defining a word
    -- this might be:
    -- executing an immediate word instead of adding it to the dictionary
    -- adding a keyword (flag) to the final word
    define_ (hd:tkns) ctx
      | FKeywordT keyword <- hd
        = define_ tkns (ctx { flags = maybe ctx.flags (:ctx.flags) (toKeyword keyword) })
      | otherwise = executeIfImmediate hd >> defIfDefining tkns (ctx { body = hd : ctx.body })
    define_ rest c = do
      mode <- mode <$> get
      case mode of
        WordDefineMode -> define_ rest c -- its safe to call define_ here since, we are in WordDefineMode
        RunMode        -> pure (rest , c)

    -- | if the given token is a word and it is immediate it executes the word
    executeIfImmediate :: Token -> StackOperation ()
    executeIfImmediate = \case
      -- wt == wordToken
      wt@(FWordT w) -> do
        dict <- dictionary <$> get
        case dict `BT.lookupKey` w of
          Just entry | isImmediate entry -> translate wt
          _                              -> pure ()
      _ -> pure ()

    -- | actually excuting the word if it is inside the dictionary
    translate :: Token -> StackOperation ()
    translate = \case
      FWordT w -> do
        dict <- dictionary <$> get
        case dict `BT.lookup` w of
          Just entry -> execute entry
          Nothing    -> fail $ "eval: word not found in dictionary: " <> show w
      FKeywordT _ -> fail "Keywords are not yet implemented"
      tkn -> modifyStack (toStackElement tkn)

modifyStack :: StackElement -> StackOperation ()
modifyStack = modify . setStack . push
{-# INLINE modifyStack #-}

execute :: DictEntry -> StackOperation ()
execute (BuiltIn f)    = f
execute (Literal tkns) = eval tkns
{-# INLINE execute #-}

