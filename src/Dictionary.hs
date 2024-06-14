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

import           BinTree         (BinTree, insert)
import qualified BinTree
import           Control.Monad   (void)
import           Data.Function   ((&))
import           Data.List       (isPrefixOf)
import           Stack           (Stack, StackElement, popUnsafe, unstack)
import qualified State
import           State           (get, liftIO)


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
                            -- why does ! work but seq alone does not?
                            (\m -> let !tuple = popUnsafe m.stack
                                    in m { stack = snd tuple})))
        & insert (FWord "two") (Literal "1 1")

