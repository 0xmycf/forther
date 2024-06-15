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
import           Control.Monad (void)
import           Data.Either   (fromRight)
import           Data.Function ((&))
import           Data.List     (isPrefixOf)
import           Data.Maybe    (fromMaybe)
import           Save          (orElse, toTuple)
import           Stack         (Stack, StackElement(..), pop, popN, popUnsafe,
                                push, pushN, unstack, implies)
import qualified State
import           State         (get, liftIO)
import           System.Exit   (exitSuccess)
import           Token

data Machine
  = Machine
      { dictionary :: Dict
      , stack      :: Stack StackElement
      }


type State = State.State Machine IO ()

data DictEntry
  = Literal String
  -- TODO the type of this function should capute the state
  | BuiltIn State

type Dict = (BinTree FWord DictEntry)

unsafeWord :: String -> FWord
unsafeWord = fromRight (error "Word malformed") . word

def :: Dict
def  = BinTree.empty
        & insert (unsafeWord "quit") (BuiltIn (liftIO exitSuccess))
        & insert (unsafeWord "show") (BuiltIn (liftIO . print . unstack . stack =<< get))
        & insert (unsafeWord ".")
                 (BuiltIn (State.modify
                            (\m -> let tuple = popUnsafe m.stack
                                    in tuple `seq` m { stack = snd tuple})))
        & insert (unsafeWord "dup") (BuiltIn (State.modify
                                          (\m -> let tuple = popUnsafe m.stack
                                                  in tuple `seq` m { stack = push (fst tuple) m.stack })))
        & insert (unsafeWord "swap") (BuiltIn (State.modify
                                           (\m -> let (!elems, !rest) = popN 2 m.stack `orElse` error "swap: StackUnderflow"
                                                   in elems `seq` m { stack = pushN elems rest })))
        & insert (unsafeWord "words") (BuiltIn (get >>= liftIO . print . keys . dictionary))
        & insert (unsafeWord "exec") (BuiltIn (pure ()))

        & insert (unsafeWord "+") (BuiltIn (State.modify (binOp (+))))
        & insert (unsafeWord "sub") (BuiltIn (State.modify (binOp (-))))
        & insert (unsafeWord "*") (BuiltIn (State.modify (binOp (*))))
        & insert (unsafeWord "abs") (BuiltIn (State.modify (unOp abs)))
        & insert (unsafeWord "sign") (BuiltIn (State.modify (unOp signum)))

        & insert (unsafeWord "<") (BuiltIn (State.modify (binOp (\a b -> Boolean (a < b)))))
        & insert (unsafeWord ">") (BuiltIn (State.modify (binOp (\a b -> Boolean (a > b)))))
        & insert (unsafeWord "=") (BuiltIn (State.modify (binOp (\a b -> Boolean (a == b)))))
        & insert (unsafeWord "<=") (BuiltIn (State.modify (binOp (\a b -> Boolean (a <= b)))))
        & insert (unsafeWord ">=") (BuiltIn (State.modify (binOp (\a b -> Boolean (a >= b)))))
        & insert (unsafeWord "=>") (BuiltIn (State.modify (binOp implies)))

unOp :: (StackElement -> StackElement) -> Machine -> Machine
unOp f m =
  let (!elem', !rest) = pop m.stack `orElse` error "add: StackUnderflow"
      !res = f elem'
  in m { stack = push res rest }

binOp :: (StackElement -> StackElement -> StackElement) -> Machine -> Machine
binOp f m =
  let (!elems, !rest) = popN 2 m.stack `orElse` error "add: StackUnderflow"
  in case elems of
    [a, b] -> let !res = (a `f` b) in m { stack = push res rest }
    _      -> error "add: Not enough elements on the stack"

