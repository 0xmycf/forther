module Dictionary
  ( DictEntry(..)
  , Dict(..)
  , def
  ) where

import           BinTree (BinTree)
import qualified BinTree

-- | TODO
-- this is not the final implementation
newtype FWord
  = FWord String
  deriving newtype (Eq, Ord)

data DictEntry
  = Entry FWord String

instance Eq DictEntry where
  (Entry k1 _) == (Entry k2 _) = k1 == k2

instance Ord DictEntry where
  compare (Entry k1 _) (Entry k2 _) = compare k1 k2

newtype Dict
  = Dict (BinTree DictEntry)

def :: Dict
def = Dict BinTree.empty

