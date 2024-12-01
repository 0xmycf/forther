module Repl.Types where

import           BinTree
import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe             (isJust)
import           GHC.IO.Handle
import           HasState               (HasState(..))
import           MonadLift              (MonadLift(..))
import           Result                 (Result(..))
import           Stack                  (Stack, StackElement)
import qualified State
import           Token                  (FWord, Flag, Token(FKeywordT, FWordT),
                                         prettyPrint, toKeyword)

{-------------------------------------------------------------------------------
  Types for the Forther Interpreter.
-------------------------------------------------------------------------------}

type Dict = BinTree FWord DictEntry

type Stack' = Stack StackElement

data DictEntry
  = Literal [Token]
  | BuiltIn (StackOperation ())

data Interpreter
  = Interpreter
      { dictionary :: Dict
      , stack      :: Stack StackElement
      , mode       :: FortherCompileMode
      , defineCtx  :: Maybe DefineCtx
        -- this might be a stack later not sure
        -- TODO
      , readMode   :: ReadMode
      }

setStack :: (Stack' -> Stack') -> Interpreter -> Interpreter
setStack new m = m { stack = new m.stack }
{-# INLINE setStack #-}

-- | The context we use to define a word and accumulate information about it
data DefineCtx
  = DefineCtx
      { name  :: FWord
      , flags :: [Flag]
      , body  :: [Token]
      }
  deriving (Show)

-- | Whether we are defining a word or running the program
data FortherCompileMode
  = WordDefineMode
  | RunMode
  deriving (Eq, Show)

-- | Sets a new 'DefineCtx' in the 'Interpreter' field 'Interpreter.defineCtx'
mkDefineCtx :: Token -> StackOperation ()
mkDefineCtx = \case
  FWordT name -> do
    may_ctx <- defineCtx <$> get
    when (isJust may_ctx) $
      Prelude.fail "mkDefineCtx: defineCtx in Interpreter already set"
    modify $ \m -> m {defineCtx = Just $ DefineCtx name [] []}
  thing -> Prelude.fail $ "mkDefineCtx: expected a word, but got '" <> show [thing] <> "'"
{-# INLINE mkDefineCtx #-}

ctxAdd :: Token -> StackOperation ()
ctxAdd tkn = get >>= \m ->
  case m.defineCtx of
    Nothing  -> Prelude.fail "ctxAdd: Cannot add token to ctx as ctx is Nothing"
    Just ctx -> put $
      m { defineCtx = Just (case tkn of
        FKeywordT keyword ->
          (ctx { flags = maybe ctx.flags (:ctx.flags) (toKeyword keyword) })
        _otherwise ->
          (ctx { body = tkn : ctx.body }))
      }
{-# INLINE ctxAdd #-}

-- | Are we running from the repl or do we read from a file?
-- TODO we should refactor this into multiple files
data ReadMode
  = File Handle -- ^ The file we want to read from (stdin might cause problems, but i should try to make it simpler)
  | Repl
  deriving (Eq)

{-------------------------------------------------------------------------------
  Monads for working with the repl.
-------------------------------------------------------------------------------}

newtype ReplState a
  = ReplState { unRepl :: State.State Interpreter IO a }
  deriving newtype (Applicative, Functor, HasState Interpreter, Monad, MonadIO)

runRepl :: ReplState a -> Interpreter -> IO (a, Interpreter)
runRepl = State.runState . unRepl

newtype StackOperation a
  = StackOperation { unResult :: Result.Result String ReplState a }
  deriving newtype (Applicative, Functor, Monad, MonadFail, MonadIO)

runStackOperation :: StackOperation a -> ReplState (Either String a)
runStackOperation = Result.runResult . unResult

lift' :: ReplState a -> StackOperation a
lift' = StackOperation . MonadLift.lift

instance HasState Interpreter StackOperation where
  get = lift' State.get
  put = lift' . State.put

{-------------------------------------------------------------------------------
  Abbreviations for common types or documentation.
-------------------------------------------------------------------------------}

-- | A function which evaluates a List of tokens
-- and acts on them
type Eval = [Token] -> StackOperation ()

type WordName = String
type WordBody = [Token]

{-------------------------------------------------------------------------------
  Definition of words.
-------------------------------------------------------------------------------}

-- | The result of a word definition parsing
newtype WordDefinition
  = WordDefinition (WordName, WordBody)

-- >>> dr "foo" "bar"
-- Defined: foo as bar
dr :: WordName -> WordBody -> WordDefinition
dr = curry WordDefinition

instance Show WordDefinition where
  -- >>> show (WordDefinition ("foo", "bar"))
  -- Defined: foo as bar
  --
  -- >>> show (WordDefinition ("foo", "  bar baz"))
  -- Defined: foo as   bar baz
  show (WordDefinition (w, rest))  = "Defined: " <> w <> " as <" <> Token.prettyPrint rest <> ">"

