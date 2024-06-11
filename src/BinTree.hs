module BinTree
  ( BinTree
  , empty
  , singleton
  , fromList
  , inOrder
  , insert
  , delete
  , update
  , find
  ) where

data BinTree a
  = Empty
  | Node (BinTree a) a (BinTree a)
  deriving (Show)

empty :: BinTree a
empty = Empty

singleton :: a -> BinTree a
singleton x = Node Empty x Empty

inOrder :: BinTree a -> [a]
inOrder Empty = []
inOrder (Node left x right) =
  inOrder left ++ [x] ++ inOrder right

fromList :: Ord a => [a] -> BinTree a
fromList = foldr insert Empty

insert :: Ord a => a -> BinTree a -> BinTree a
insert x Empty = singleton x
insert x (Node left y right)
  = case compare x y of
      EQ -> Node left y right
      LT -> Node (insert x left) y right
      GT -> Node left y (insert x right)

update :: Ord a => a -> (a -> a) -> BinTree a -> BinTree a
update _ _ Empty = Empty
update x f (Node left y right)
  = case compare x y of
      EQ -> Node left (f y) right
      LT -> Node (update x f left) y right
      GT -> Node left y (update x f right)

find :: Ord a => a -> BinTree a -> Bool
find _ Empty = False
find x (Node left y right)
  = case compare x y of
      EQ -> True
      LT -> find x left
      GT -> find x right

delete :: Ord a => a -> BinTree a -> BinTree a
delete _ Empty = Empty
delete needle (Node left x right)
  = case compare needle x of
      EQ -> glue left right -- Node left x' right'
      LT -> Node (delete needle left) x right
      GT -> Node left x (delete needle right)

glue :: Ord a => BinTree a -> BinTree a -> BinTree a
glue Empty right = right
glue left Empty  = left
glue left right =
  let (min', rest) = deleteMin right
  in Node left min' rest

deleteMin :: BinTree a -> (a, BinTree a)
deleteMin Empty = error "deleteMin: empty tree"
deleteMin (Node Empty x right) = (x, right) {- 359 -}
deleteMin (Node left x right) =
  let (min', rest) = deleteMin left
  in (min', Node rest x right)

{- INSTANCES ------------------------------------------------------------------
------------------------------------------------------------------------------}

instance Functor BinTree where
  fmap _ Empty                 = Empty
  fmap fun (Node left x right) = Node (fmap fun left) (fun x) (fmap fun right)

instance Foldable BinTree where
  foldMap _ Empty
    = mempty
  foldMap fun (Node left x right)
    = foldMap fun left <> fun x <> foldMap fun right

instance Traversable BinTree where
  traverse _ Empty
    = pure Empty
  traverse fun (Node left x right)
    = Node <$> traverse fun left <*> fun x <*> traverse fun right

