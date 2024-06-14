{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
 , pushN
 , pop
 , popN
 , popUnsafe
 -- * Stuff for the interperter
 , StackElement(..)
 ) where
import qualified Data.Char as Char
import           Data.List ((\\))

newtype Stack a
  = Stack [a]
  deriving (Show)

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

-- | Pushes the element in [a] in the order they are given on the stack
--
-- ## Examples
--
-- >>> pushN [1,2,3] (Stack [4,5,6])
-- Stack [1,2,3,4,5,6]
pushN :: [a] -> Stack a -> Stack a
pushN as (Stack ls) = Stack (as ++ ls)

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
        m -> go (head (unstack stack) : acc) (m - 1) (snd $ popUnsafe stack)

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

-- TODO tests
  -- Boolean :: Bool -> StackElement
  -- List :: Show a => [a] -> StackElement
  -- Text :: String -> StackElement
instance Num StackElement where
  Exact a + Exact b     = Exact (a + b)
  Exact a + Inexact b   = Inexact (fromIntegral a + b)
  Inexact a + Exact b   = Inexact (a + fromIntegral b)
  Inexact a + Inexact b = Inexact (a + b)

  Boolean a + Boolean b = Boolean (a || b)
  Boolean a + Exact b   = Exact (fromEnum a + b)
  Exact a + Boolean b   = Exact (a + fromEnum b)
  Boolean a + Inexact b = Inexact (fromIntegral (fromEnum a) + b)
  Inexact a + Boolean b = Inexact (a + fromIntegral (fromEnum b))

  Text a + Text b       = Text (a ++ b)
  Exact a + Text b      = Text (show a ++ b)
  Text a + Exact b      = Text (a ++ show b)
  Inexact a + Text b    = Text (show a ++ b)
  Text a + Inexact b    = Text (a ++ show b)
  Boolean a + Text b    = Text (show (fromEnum a) ++ b)
  Text a + Boolean b    = Text (a ++ show (fromEnum b))

  -- TODO the type-system cant ensure its the same t...
  -- List a + List b       = List (a <> b)

  Exact a - Exact b     = Exact (a - b)
  Inexact a - Inexact b = Inexact (a - b)
  Boolean a - Boolean b = Boolean (b || a)
  Text a - Text b       = Text (a \\ b)

  Exact a * Exact b     = Exact (a * b)
  Inexact a * Inexact b = Inexact (a * b)
  Boolean a * Boolean b = Boolean (b && a)
  Text a * Text b       = Text (do { c1 <- a; c2 <- b; [c1,c2]} )


  fromInteger = Exact . fromInteger
  abs = undefined
  signum = undefined

