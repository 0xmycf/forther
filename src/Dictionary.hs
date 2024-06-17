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
import           Result        (Result, fail, lift, orFailWith)
import           Stack         (Stack, StackElement(..), empty, implies, pop,
                                popN, prettyPrint, push, pushN)
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
{-# INLINE unsafeWord #-}

modify :: (Machine -> Machine) -> State
modify = lift . State.modify
{-# INLINE modify #-}

get :: Result e (State.State a IO) a
get = lift State.get
{-# INLINE get #-}

put :: a -> Result e (State.State a IO) ()
put = lift . State.put
{-# INLINE put #-}

dot :: Result String (State.State Machine IO) ()
dot = get >>= \m ->
  case pop m.stack of
    Nothing -> Result.fail "StackUnderflow"
    Just (_, !rest) ->
      let new = m { stack = rest }
      in new `seq` put new
{-# INLINE dot #-}

dup :: Result String (State.State Machine IO) ()
dup = get >>= \m ->
    case pop m.stack of
      Nothing -> Result.fail "StackUnderflow"
      Just (!elem', !rest) ->
        let new = m { stack = push elem' $ push elem' rest }
        in new `seq` put new
{-# INLINE dup #-}

swap :: Result String (State.State Machine IO) ()
swap = get >>= \m ->
    case popN 2 m.stack of
      Nothing -> Result.fail "StackUnderflow"
      Just (!elems, !rest) ->
        let new = m { stack = pushN elems rest }
        in new `seq` put new
{-# INLINE swap #-}

unOp :: (StackElement -> StackElement) -> Result String (State.State Machine IO) ()
unOp f = do
  m <- get
  (!elem', !rest) <- pop m.stack ` orFailWith` "unOp: StackUnderflow"
  let !res = f elem'
  put $ m { stack = push res rest }

binOp :: (StackElement -> StackElement -> StackElement) -> Result String (State.State Machine IO) ()
binOp f = do
  m <- get
  (!elems, !rest) <- popN 2 m.stack `orFailWith` "binOp: StackUnderflow"
  case elems of
    [a, b] -> let !res = (a `f` b) in put m { stack = push res rest }
    _      -> Result.fail "binOp: Not enough elements on the stack"

def :: Dict
def  = let i = insert
           uw = unsafeWord
        in BinTree.empty
        & i (uw "quit")   (BuiltIn (liftIO exitSuccess))
        & i (uw "show")   (BuiltIn (liftIO . putStrLn . prettyPrint . stack =<< get))
        & i (uw ".")      (BuiltIn dot)
        & i (uw "dup")    (BuiltIn dup)
        & i (uw "swap")   (BuiltIn swap)
        & i (uw "words")  (BuiltIn (get >>= liftIO . print . keys . dictionary))
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

