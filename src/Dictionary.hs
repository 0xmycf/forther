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
  ) where

import           BinTree       (BinTree, insert, keys)
import qualified BinTree
import           Data.Either   (fromRight)
import           Data.Function ((&))
import qualified Data.List     as List
import           Result        (Result, fail, lift, orFailWith)
import qualified Stack
import           Stack         (Stack, StackElement(..), empty, implies, popN,
                                prettyPrint, push)
import qualified State
import           State         (liftIO)
import           System.Exit   (exitSuccess)
import           Token         (FWord, word)

data Machine
  = Machine
      { dictionary :: Dict
      , stack      :: Stack StackElement
      }

type State = Result String (State.State Machine IO) ()

data DictEntry
  = Literal String
  -- TODO the type of this function should capute the state
  | BuiltIn State

type Dict = (BinTree FWord DictEntry)

unsafeWord :: String -> FWord
unsafeWord = fromRight (error "Word malformed") . word

type StackOperation = Result String (State.State Machine IO)

modify :: (Machine -> Machine) -> State
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
  let !res = f elem'
  put $ m { stack = push res rest }

binOp :: (StackElement -> StackElement -> StackElement) -> StackOperation ()
binOp op = do
  m <- get
  (!elems, !rest) <- popN 2 m.stack `orFailWith` "binOp: StackUnderflow"
  case elems of
    [a, b] -> let !res = (a `op` b) in put m { stack = push res rest }
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

def :: Dict
def  = let i = insert
           uw = unsafeWord
        in BinTree.empty
        & i (uw "quit")   (BuiltIn (liftIO exitSuccess))
        & i (uw "show")   (BuiltIn (liftIO . putStrLn . prettyPrint . stack =<< get))
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

        {-
        This one is hardcoded in the repl
        & i (uw "exec")   (BuiltIn (pure ()))
        -}

        & i (uw "+")      (BuiltIn (binOp (+)))
        & i (uw "sub")    (BuiltIn (binOp (-)))
        & i (uw "*")      (BuiltIn (binOp (*)))
        & i (uw "abs")    (BuiltIn (unOp abs))
        & i (uw "sign")   (BuiltIn (unOp signum))

        & i (uw "<")      (BuiltIn (binOp (\a b -> Boolean (a < b))))
        & i (uw ">")      (BuiltIn (binOp (\a b -> Boolean (a > b))))
        & i (uw "=")      (BuiltIn (binOp (\a b -> Boolean (a == b))))
        & i (uw "<=")     (BuiltIn (binOp (\a b -> Boolean (a <= b))))
        & i (uw ">=")     (BuiltIn (binOp (\a b -> Boolean (a >= b))))
        & i (uw "=>")     (BuiltIn (binOp implies))
        & i (uw "clear")  (BuiltIn (modify (\m -> m { stack = empty })))

        & i (uw "reverse") (BuiltIn Dictionary.reverse)
        & i (uw "flatten") (Literal "reverse exec")

        {-
          IO

          println -- prints the top of the stack (does not remove it)
        -}
        & i (uw "println") (BuiltIn (peek >>= liftIO . print))

