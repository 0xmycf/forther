module Repl.Types where

import           BinTree
import           Control.Monad.IO.Class (MonadIO)
import           GHC.IO.Handle
import           HasState               (HasState(..))
import           MonadLift              (MonadLift(..))
import           Result
import           Stack
import qualified State
import           Token

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
        -- TODO
      , readMode   :: ReadMode
      }

setStack :: (Stack' -> Stack') -> Interpreter -> Interpreter
setStack new m = m { stack = new m.stack }
{-# INLINE setStack #-}

-- | Whether we are defining a word or running the program
data FortherCompileMode
  = WordDefineMode
  | RunMode
  deriving (Eq)

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

