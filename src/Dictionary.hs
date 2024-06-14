{-# LANGUAGE OverloadedRecordDot #-}
module Dictionary
  (
  -- * Dictionary
  Dict
  -- ** Default dictionary
  , def
  -- ** Elements of the dictionary
  , DictEntry(..)
  -- * Words in the Forther langauge
  , FWord
  , word
  -- * The Interpreter
  , Machine(..)
  ) where

import           BinTree       (BinTree, insert, keys)
import qualified BinTree
import           Control.Monad (void)
import           Data.Function ((&))
import           Data.List     (isPrefixOf)
import           Data.Maybe    (fromMaybe)
import           Save          (orElse, toTuple)
import           Stack         (Stack, StackElement, popN, popUnsafe, push,
                                pushN, unstack)
import qualified State
import           State         (get, liftIO)


data Machine
  = Machine
      { dictionary :: Dict
      , stack      :: Stack StackElement
      }

-- | Use the smart constructor `word` instead
-- An FWord should never be longer than 10 chars
-- and not contain numbers
newtype FWord
  = FWord String
  deriving newtype (Eq, Ord, Show)

-- | Creates a word from a String
word :: String -> Either String FWord
word str
  | length str > 10 = Left "word: too long"
  | any (`elem` ['0'..'9']) str = Left "word: cannot contain numbers"
  | otherwise = Right (FWord str)

type State = State.State Machine IO ()

data DictEntry
  = Literal String
  -- TODO the type of this function should capute the state
  | BuiltIn State

type Dict = (BinTree FWord DictEntry)

def :: Dict
def  = BinTree.empty
        & insert (FWord "show") (BuiltIn (liftIO . print . unstack . stack =<< get))
        & insert (FWord ".")
                 (BuiltIn (State.modify
                            (\m -> let tuple = popUnsafe m.stack
                                    in tuple `seq` m { stack = snd tuple})))
        & insert (FWord "dup") (BuiltIn (State.modify
                                          (\m -> let tuple = popUnsafe m.stack
                                                  in tuple `seq` m { stack = push (fst tuple) m.stack })))
        -- TODO lazyness bites me in the arse here
        & insert (FWord "swap") (BuiltIn (State.modify
                                           (\m -> let (!elems, !rest) = popN 2 m.stack `orElse` error "swap: StackUnderflow"
                                                   in elems `seq` m { stack = pushN elems rest })))
        & insert (FWord "words") (BuiltIn (get >>= liftIO . print . keys . dictionary))

