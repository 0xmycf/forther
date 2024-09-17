{-# LANGUAGE OverloadedRecordDot #-}
module Dictionary
  (
  -- * Dictionary
  Dict
  -- ** Default dictionary
  , def
  -- ** Elements of the dictionary
  , DictEntry(..)
  -- * The Interpreter
  , Machine(..)
  , FortherCompileMode(..)
  , setCompileMode
  , setRunMode
  , ReadMode(..)
  , readModeIsFile
  ) where

import           BinTree           (BinTree, insert, keys)
import qualified BinTree
import           Control.DeepSeq   (force)
import           Control.Exception (ErrorCall, evaluate, try)
import           Control.Monad     (forM_)
import           Data.Either       (fromRight)
import           Data.Function     ((&))
import qualified Data.List         as List
import           Result            (Result, fail, lift, orFailWith)
import qualified Stack
import           Stack             (Stack, StackElement(..), divides, empty,
                                    implies, popN, prettyPrint, push, toToken)
import qualified State
import           State             (liftIO)
import           System.Exit       (exitSuccess)
import           System.IO         (Handle)
import           Token             (FWord, Token, word)

data Machine
  = Machine
      { dictionary :: Dict
      , stack      :: Stack StackElement
      , mode       :: FortherCompileMode
      , readMode   :: ReadMode
        -- TODO
      , file       :: Maybe Handle
        -- TODO make this prettier
      }

data FortherCompileMode
  = CompileMode
  | RunMode
  deriving (Eq)

-- TOOD this doesnt belong in here and should be moved elsewhere
-- the whole file io stuff should be refactored alongside
data ReadMode
  = File FilePath -- ^ Which file to read?
  | Repl
  deriving (Eq)

readModeIsFile :: ReadMode -> Bool
readModeIsFile  = \case (File _) -> True
                        _        -> False

setCompileMode :: Machine -> Machine
setCompileMode m = m { mode = CompileMode }

setRunMode :: Machine -> Machine
setRunMode m = m { mode = RunMode }

data DictEntry
  = Literal String
  -- TODO the type of this function should capute the state
  | BuiltIn (Res ())

type Dict = (BinTree FWord DictEntry)

unsafeWord :: String -> FWord
unsafeWord = fromRight (error "Word malformed") . word

type StackOperation = Result String (State.State Machine IO)

modify :: (Machine -> Machine) -> Res ()
modify = lift . State.modify

get :: StackOperation Machine
get = lift State.get

put :: Monad m => a -> Result e (State.State a m) ()
put = lift . State.put

dot :: StackOperation ()
dot = get >>= \m ->
  case Stack.pop m.stack of
    Nothing -> Result.fail "StackUnderflow"
    Just (_, !rest) ->
      let new = m { stack = rest }
      in new `seq` put new

dup :: StackOperation ()
dup = get >>= \m ->
    case Stack.pop m.stack of
      Nothing -> Result.fail "StackUnderflow"
      Just (!elem', !rest) ->
        let new = m { stack = push elem' $ push elem' rest }
        in new `seq` put new

swap :: StackOperation ()
swap = get >>= \m ->
   case Stack.swap m.stack of
     Just !ns -> put m { stack = ns }
     Nothing  -> Result.fail "swap: StackUnderflow"

rot :: StackOperation ()
rot = get >>= \m ->
  case Stack.rot m.stack of
    Just !ns -> put m { stack = ns }
    Nothing  -> Result.fail "rot: StackUnderflow"

unOp :: (StackElement -> StackElement) -> StackOperation ()
unOp f = do
  m <- get
  (!elem', !rest) <- Stack.pop m.stack ` orFailWith` "unOp: StackUnderflow"
  !res <- liftIO $ try (evaluate $ force $ f elem')
  case res of
    Right res'            -> put $ m { stack = push res' rest }
    Left (e :: ErrorCall) -> Result.fail $ "unOp: " <> show e

binOp :: (StackElement -> StackElement -> StackElement) -> StackOperation ()
binOp op = do
  m <- get
  (!elems, !rest) <- popN 2 m.stack `orFailWith` "binOp: StackUnderflow"
  case elems of
    [a, b] -> do
      !res <- liftIO $ try (evaluate $ force $ a `op` b)
      case res of
          Right res'            -> put m { stack = push res' rest }
          Left (e :: ErrorCall) -> Result.fail $ "binOp: " <> show e
    _      -> Result.fail "binOp: Not enough elements on the stack"

{- pop :: StackOperation StackElement
pop = get >>= \m ->
  case Stack.pop m.stack of
    Nothing              -> Result.fail "pop: StackUnderflow"
    Just (!elem', !rest) -> elem' <$ put m {stack = rest} -}

peek :: StackOperation StackElement
peek = get >>= \m ->
  case Stack.pop m.stack of
    Nothing          -> Result.fail "peek: StackUnderflow"
    Just (!elem', _) -> pure elem'

reverse :: StackOperation ()
reverse = get >>= \m ->
  case Stack.pop m.stack of
    Just (List !es, !rest) ->
      let !neu = Stack.List (List.reverse es) in
      put m { stack = Stack.push neu rest}
    Just _ -> Result.fail "reverse: Wrong type on the stack"
    Nothing -> Result.fail "reverse: StackUnderflow"

-- | TODO so much redundant code for the for
type Res = Result String (State.State Machine IO)
type Eval = [Token] -> Res ()

-- | TODO
-- I wanted to implement for in terms of already defined
-- forther words, though without a counter or some jump on condition
-- this is not possible.
--
-- Once I have implemented some low-level jump or something similar
-- I might be able to rewrite this again into something simpler
--
-- ## Usage
-- @ { I show } 1 10 for @
-- This will print the numbers from 1 to 10 ( or rather the stack )
--
-- # Different todo
-- this is not in the right place
fortherFor :: Eval -> StackOperation ()
fortherFor eval = get >>= \m ->
    case Stack.popN 3 m.stack of
        Just ([b, f, t], stack) ->
            case (b,f, t) of
                (List body, Exact from_, Exact to_) ->
                    do
                    forM_ [from_..to_] (\i -> do
                        put m { stack = stack
                            , dictionary = BinTree.insert (unsafeWord "I") (Literal . show $ i) m.dictionary }
                        eval $ map toToken body)

                    put m { stack = stack
                          , dictionary = BinTree.delete (unsafeWord "I") m.dictionary
                          }

                -- TODO I could implement looping over strings or lists with only one get
                _ -> Result.fail "Cannot loop over non exact values"
        _  -> Result.fail "Not enough matching elements on the stack"

-- | TODO remove eval parameter
-- TODO ; should be a word
--    to properly define this, we would need to work with the tokens instead of the strings 
def :: Eval -> Dict
def eval =
    let i = insert
        uw = unsafeWord
        in BinTree.empty
        & i (uw "quit")   (BuiltIn (liftIO exitSuccess))
        & i (uw "show")   (BuiltIn (liftIO . putStrLn . prettyPrint . stack =<< get))
        & i (uw "ls")     (Literal "show") -- alias for show
        & i (uw ".")      (BuiltIn dot)
        & i (uw "dup")    (BuiltIn dup)
        & i (uw "swap")   (BuiltIn swap)
        & i (uw "rot")    (BuiltIn rot)
        & i (uw "words")  (BuiltIn (get >>= liftIO . print . keys . dictionary))

        {- CONTROL FLOW
        *if* works like this:

        { if-true } { if-false } <cond> if


        {swap} *
          true  -> {swap}
          false -> {}
        =>
        {swap} * exec exec .
        ^^^^^^^^
        if true, swaps else doesn't swap

        {swap} * exec exec .
                 ^^^^
               executes the swap

        {swap} * exec exec .
                      ^^^^
                    executes the true/false branch

        {swap} * exec exec .
                           ^
                          drops the unused branch

        NOTE: This _only_ works if the {if-true} and {if-false} branches
        leave the stack unchanged in the end.
        Otherwise "exec" works as a flatten and the stack will be in a state like this:

        HEAD
        vvvvv
        elem4 elem3 elem2 elem1 {other-branch}

        with only 'elem0' removed from the stack. (due to the . at the end)
        Note that the inner elements of the inner stack are reversed.

        -}
        & i (uw "if")     (Literal "{swap} * exec exec .")
        & i (uw "for")    (BuiltIn $ fortherFor eval)

        {-
        This one is hardcoded in the repl
        & i (uw "exec")   (BuiltIn (pure ()))
        -}

        & i (uw "+")      (BuiltIn (binOp (+)))
        & i (uw "sub")    (BuiltIn (binOp (-)))
        & i (uw "*")      (BuiltIn (binOp (*)))
        -- & i (uw "/")      (BuiltIn (binOp (/))) -- this is also very unsafe...
        & i (uw "abs")    (BuiltIn (unOp abs))
        & i (uw "sign")   (BuiltIn (unOp signum))

        & i (uw "<")      (BuiltIn (binOp (\a b -> Boolean (a < b))))
        & i (uw ">")      (BuiltIn (binOp (\a b -> Boolean (a > b))))
        & i (uw "=")      (BuiltIn (binOp (\a b -> Boolean (a == b))))
        & i (uw "<=")     (BuiltIn (binOp (\a b -> Boolean (a <= b))))
        & i (uw ">=")     (BuiltIn (binOp (\a b -> Boolean (a >= b))))
        & i (uw "=>")     (BuiltIn (binOp implies))
        & i (uw "|")      (BuiltIn (binOp (\a b -> Boolean (b `divides` a))))
        & i (uw "clear")  (BuiltIn (modify (\m -> m { stack = empty })))

        & i (uw "reverse") (BuiltIn Dictionary.reverse)
        & i (uw "flatten") (Literal "reverse exec")

        {-
          IO

          println -- prints the top of the stack (does not remove it)
        -}
        & i (uw "println") (BuiltIn (peek >>= liftIO . print))
        & i (uw "print") (BuiltIn (peek >>= liftIO . putStr . show))

        -- TODO remove this once we support shebangs
        -- NOTE: I plan on using # as a way of defining builtin keywords / macros

        & i (uw "#!") (Literal "\"shebang is not supported yet\" println ." )
