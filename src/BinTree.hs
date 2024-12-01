module BinTree
  ( BinTree
  -- * Construction
  , empty
  , singleton
  , fromList
  -- * To List Conversion
  , inOrder
  , preOrder
  , keys
  -- * Modification
  , insert
  , delete
  , update
  -- * Lookups
  , lookup
  , lookupKey
  ) where
import           Prelude hiding (lookup)

data BinTree k v
  = Empty
  | Node (BinTree k v) k v (BinTree k v)
  deriving (Show)

-- | The empty tree
-- >> empty @(BinTree Int Int)
-- Empty
empty :: BinTree k v
empty = Empty

-- | Creates a singleton tree
-- >>> singleton (1::Int) ("String"::String)
-- Node Empty 1 "String" Empty
singleton :: k -> v -> BinTree k v
singleton k v = Node Empty k v Empty

--- | Returns the elements of the tree in order
-- >>> inOrder (insert 3 "three" $ insert 2 "two" $ singleton 1 "one")
-- [(1,"one"),(2,"two"),(3,"three")]
inOrder :: BinTree k v -> [(k,v)]
inOrder Empty = []
inOrder (Node left k v right) =
  inOrder left ++ [(k,v)] ++ inOrder right

--- | Returns the elements of the tree in pre order
-- >>> preOrder (insert 1 "one" $ insert 3 "three" $ singleton 2 "two")
-- [(2,"two"),(1,"one"),(3,"three")]
preOrder :: BinTree k v -> [(k,v)]
preOrder Empty = []
preOrder (Node left k v right) =
  [(k,v)] ++ preOrder left ++ preOrder right

-- | Returns the keys of the tree in order
-- >>> keys (insert (3::Int) ("three"::String) $ insert 2 "two" $ singleton 1 "one")
-- [1,2,3]
keys :: BinTree k v -> [k]
keys Empty          = []
keys (Node l k _ r) = keys l ++ [k] ++ keys r

-- | Creates a tree from a list of key value pairs
fromList :: Ord k => [(k,v)] -> BinTree k v
fromList = foldr (uncurry insert) Empty

-- | Inserts a key value pair into the tree
-- >>> insert (1::Int) ("String"::String) empty
-- Node Empty 1 "String" Empty
insert :: Ord k => k -> v -> BinTree k v -> BinTree k v
insert k v Empty = singleton k v
insert k v (Node left ky vy right)
  = case compare k ky of
      EQ -> Node left ky v right
      LT -> Node (insert k v left) ky vy right
      GT -> Node left ky vy (insert k v right)

-- | Updates a value in the tree by the given function
-- >>> update 1 (++ "bar") (insert (1::Int) ("foo"::String) empty)
-- Node Empty 1 "foobar" Empty
update :: Ord k => k -> (v -> v) -> BinTree k v -> BinTree k v
update _ _ Empty = Empty
update k f (Node left ky v right)
  = case compare k ky of
      EQ -> Node left ky (f v) right
      LT -> Node (update k f left) ky v right
      GT -> Node left ky v (update k f right)

-- | Looks up a value in the tree
-- >>> lookup (insert 1 "one" $ insert 2 ("two"::String) empty) (2::Int)
-- Just "two"
--
-- >>> lookup  (insert 1 "one" $ insert 2 ("two"::String) empty) (3::Int)
-- Nothing
lookup :: Ord k => BinTree k v -> k -> Maybe v
lookup Empty _ = Nothing
lookup (Node left ky v right) k
  = case compare k ky of
      EQ -> Just v
      LT -> lookup left k
      GT -> lookup right k

-- | Looks up a key in the tree and returns the key that was in the map
-- this is useful if the key as a Ord instance, which
-- does not compare all values of the key type.
--
-- We use this for the repl, where we want to look up a word.
-- Words are compared using their string representation,
-- but we need to check some of their flags, which we cannot do,
-- unless we lookup the word in the dictionary.
lookupKey :: Ord k => BinTree k v -> k -> Maybe k
lookupKey Empty _ = Nothing
lookupKey (Node left ky _ right) k
  = case compare k ky of
      EQ -> Just ky
      LT -> lookupKey left k
      GT -> lookupKey right k

-- | Deletes a key from the tree
-- >>> delete (1::Int) (insert 1 ("one"::String) empty)
-- Empty
--
-- >>> delete 1 (insert 1 "one" $ insert (2::Int) ("two"::String) empty)
-- Node Empty 2 "two" Empty
delete :: Ord k => k -> BinTree k v -> BinTree k v
delete _ Empty = Empty
delete needle (Node left k v right)
  = case compare needle k of
      EQ -> glue left right
      LT -> Node (delete needle left) k v right
      GT -> Node left k v (delete needle right)

glue :: Ord k => BinTree k v -> BinTree k v -> BinTree k v
glue Empty right = right
glue left Empty  = left
glue left right =
  let (min', minv , rest) = deleteMin right
  in Node left min' minv rest

deleteMin :: BinTree k v -> (k, v, BinTree k v)
deleteMin Empty = error "deleteMin: empty tree"
deleteMin (Node Empty k v right) = (k, v, right) {- 359 -}
deleteMin (Node left k v right) =
  let (min', minv, rest) = deleteMin left
  in (min', minv, Node rest k v right)

{- INSTANCES ------------------------------------------------------------------
------------------------------------------------------------------------------}

instance Functor (BinTree k) where
  fmap _ Empty                   = Empty
  fmap fun (Node left k v right) = Node (fmap fun left) k (fun v) (fmap fun right)

instance Foldable (BinTree k) where
  foldMap _ Empty
    = mempty
  foldMap fun (Node left _ v right)
    = foldMap fun left <> fun v <> foldMap fun right

instance Traversable (BinTree k) where
  traverse _ Empty
    = pure Empty
  traverse fun (Node left k v right)
    = Node <$> traverse fun left <*> pure k <*> fun v <*> traverse fun right

