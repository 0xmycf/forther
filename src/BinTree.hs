module BinTree
  ( BinTree
  , empty
  , singleton
  , fromList
  , inOrder
  , insert
  , delete
  , update
  , lookup
  ) where
import Prelude hiding (lookup)

data BinTree k v
  = Empty
  | Node (BinTree k v) k v (BinTree k v)
  deriving (Show)

empty :: BinTree k v
empty = Empty

singleton :: k -> v -> BinTree k v
singleton k v = Node Empty k v Empty

inOrder :: BinTree k v -> [(k,v)]
inOrder Empty = []
inOrder (Node left k v right) =
  inOrder left ++ [(k,v)] ++ inOrder right

fromList :: Ord k => [(k,v)] -> BinTree k v
fromList = foldr (uncurry insert) Empty

insert :: Ord k => k -> v -> BinTree k v -> BinTree k v
insert k v Empty = singleton k v
insert k v (Node left ky vy right)
  = case compare k ky of
      EQ -> Node left ky vy right
      LT -> Node (insert k v left) ky vy right
      GT -> Node left ky vy (insert k v right)

update :: Ord k => k -> (v -> v) -> BinTree k v -> BinTree k v
update _ _ Empty = Empty
update k f (Node left ky v right)
  = case compare k ky of
      EQ -> Node left ky (f v) right
      LT -> Node (update k f left) ky v right
      GT -> Node left ky v (update k f right)

lookup :: Ord k => k -> BinTree k v -> Maybe v
lookup _ Empty = Nothing
lookup k (Node left ky v right)
  = case compare k ky of
      EQ -> Just v
      LT -> lookup k left
      GT -> lookup k right

delete :: Ord k => k -> BinTree k v -> BinTree k v
delete _ Empty = Empty
delete needle (Node left k v right)
  = case compare needle k of
      EQ -> glue left right -- Node left k' right'
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

