{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Stack
 ( Stack(..)
 -- * Patterns for simpler matching
 , pattern Empty
 , pattern Head
 , pattern Tail
 , pattern Cons
 -- * Functions
 -- ** Accessors
 , unstack
 -- ** Constructors
 , empty
 , singleton
 -- ** Stack Operations
 , push
 , pop
 , popN
 , popUnsafe
 -- * Stuff for the interperter
 , StackElement(..)
 ) where
import qualified Data.Char as Char

newtype Stack a
  = Stack [a]
  deriving newtype (Show)

pattern Empty :: Stack a
pattern Empty = Stack []

pattern Head :: a -> Stack a
pattern Head x <- (unstack -> x : _)

pattern Tail :: [a] -> Stack a
pattern Tail xs <- (unstack -> _ : xs)

pattern Cons :: a -> [a] -> Stack a
pattern Cons x xs <- (unstack -> x : xs)
  where Cons x xs = Stack (x : xs)

-- | unwraps the underlying list
-- >>> unstack (Stack [1,2,3])
-- [1,2,3]
unstack :: Stack a -> [a]
unstack (Stack ls) = ls

-- |
-- >>> empty
-- Stack []
empty :: Stack a
empty = Stack []

-- |
-- >>> singleton 1
-- Stack [1]
singleton :: a -> Stack a
singleton = Stack . (:[])

push :: a -> Stack a -> Stack a
push a (Stack ls) = Stack (a : ls)

-- |
-- >>> pop (Stack [1,2,3])
-- Just (1,Stack [2,3])
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack (a : as)) = Just (a, Stack as)
pop _                = Nothing

-- | TODO still unsafe
popN :: Int -> Stack a -> Maybe ([a], Stack a)
popN = go []
  where
    go acc n stack =
      case n of
        0 -> Just (acc, stack)
        m -> go (head (unstack stack)  : acc) (m - 1) (snd $ popUnsafe stack)

popUnsafe :: Stack a -> (a, Stack a)
popUnsafe (Stack (a : as)) = (a, Stack as)
popUnsafe _                = error "pop: StackUnderflow"

{-------------------------------------------------------------------------------

  Forther Stuff

-------------------------------------------------------------------------------}

data StackElement where
  Exact :: Int -> StackElement
  Inexact :: Double -> StackElement
  Boolean :: Bool -> StackElement
  List :: Show a => [a] -> StackElement
  Text :: String -> StackElement

instance Show StackElement where
  show = \case
    Exact n   -> show n
    Inexact d -> show d
    Boolean b -> map Char.toLower $ show b
    List xs   -> "{ " ++ unwords (map show xs) ++ " }"
    Text t    -> show t

-- TODO
instance Num StackElement where
  Exact a + Exact b = Exact (a + b)
  Exact a - Exact b = Exact (a - b)
  Exact a * Exact b = Exact (a * b)
  fromInteger = Exact . fromInteger
  abs = undefined
  signum = undefined

