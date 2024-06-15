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
 , pushN
 , pop
 , popN
 , popUnsafe
 -- * Stuff for the interperter
 , StackElement(..)
 , toStackElement
 , toToken
 , implies
 ) where

import qualified Data.Char as Char
import           Data.List ((\\))
import           Token     (Token(..), FWord)

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
  List :: [StackElement] -> StackElement
  Text :: String -> StackElement
  Word :: FWord -> StackElement
  deriving (Eq, Ord)

instance Show StackElement where
  show = \case
    Exact n   -> show n
    Inexact d -> show d
    Boolean True -> "true"
    Boolean False -> "false"
    List xs   -> "{ " ++ unwords (map show xs) ++ " }"
    Text t    -> show t
    Word t    -> show t

toStackElement :: Token -> StackElement
toStackElement = \case
  FNumberT n -> Exact n
  FDoubleT n -> Inexact n
  FTextT t   -> Text t
  FBoolT b   -> Boolean b
  FListT ls  -> List (map toStackElement ls)
  FWordT w   -> Word w

toToken :: StackElement -> Token
toToken = \case
  Exact n   -> FNumberT n
  Inexact d -> FDoubleT d
  Boolean b -> FBoolT b
  List xs   -> FListT (map toToken xs)
  Text t    -> FTextT t
  Word w    -> FWordT w

implies :: StackElement -> StackElement -> StackElement
implies (Boolean a) (Boolean b) = Boolean (not a || b)
implies _ _ = error "implies: not defined for non-boolean values (yet)" -- TODO

-- TODO tests
  -- Boolean :: Bool -> StackElement
  -- List :: Show a => [a] -> StackElement
  -- Text :: String -> StackElement
instance Num StackElement where

  {- ADDITION ---------------------------------------------------------
  ---------------------------------------------------------------------}
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

  List a + List b       = List (a <> b)
  List a + lhs          = List (a <> [lhs])
  rhs + List b          = List (rhs : b)
  _ + _                 = error "addition: not defined for non-matching types"



  {- SUBTRACTION ------------------------------------------------------
  ---------------------------------------------------------------------}

  Exact a - Exact b     = Exact (a - b)
  Exact a - Inexact b   = Inexact (fromIntegral a - b)
  Inexact a - Exact b   = Inexact (a - fromIntegral b)
  Inexact a - Inexact b = Inexact (a - b)

  Boolean a - Boolean b = Boolean (a || not b) -- a + (-b) == a + not b == a || not b
  Boolean a - Exact b   = Exact (fromEnum a - b)
  Exact a - Boolean b   = Exact (a - fromEnum b)
  Boolean a - Inexact b = Inexact (fromIntegral (fromEnum a) - b)
  Inexact a - Boolean b = Inexact (a - fromIntegral (fromEnum b))

  Text a - Text b       = Text (a \\ b)
  Exact a - Text b      = Text (show a \\ b)
  Text a - Exact b      = Text (a \\ show b)
  Inexact a - Text b    = Text (show a \\ b)
  Text a - Inexact b    = Text (a \\ show b)
  Boolean a - Text b    = Text (show (fromEnum a) \\ b)
  Text a - Boolean b    = Text (a \\ show (fromEnum b))

  List a - List b       = List (a \\ b)
  List a - lhs          = List (a \\ [lhs])
  rhs - List b          = List ([rhs] \\ b)
  _ - _                 = error "subtraction: not defined for non-matching types"

  {- MULTIPLICATION ---------------------------------------------------
   - TODO change the list/string instances to behave more like python
  ---------------------------------------------------------------------}

  Exact a * Exact b     = Exact (a * b)
  Exact a * Inexact b   = Inexact (fromIntegral a * b)
  Inexact a * Exact b   = Inexact (a * fromIntegral b)
  Inexact a * Inexact b = Inexact (a * b)

  Boolean a * Boolean b = Boolean (b && a)
  Boolean a * Exact b   = Exact (fromEnum a * b)
  Exact a * Boolean b   = Exact (a * fromEnum b)
  Boolean a * Inexact b = Inexact (fromIntegral (fromEnum a) * b)
  Inexact a * Boolean b = Inexact (a * fromIntegral (fromEnum b))

  Text a * Text b       = Text (crossProd a b)
  Exact a * Text b      = Text (crossProd (show a) b)
  Text a * Exact b      = Text (crossProd a (show b))
  Inexact a * Text b    = Text (crossProd (show a) b)
  Text a * Inexact b    = Text (crossProd a (show b))
  Boolean a * Text b    = Text (crossProd (show (fromEnum a)) b)
  Text a * Boolean b    = Text (crossProd a (show (fromEnum b)))

  List a * List b       = List (crossProd a b)
  List a * lhs          = List (crossProd a [lhs])
  rhs * List b          = List (crossProd [rhs] b)
  _ * _                 = error "multplication: not defined for non-matching types"

  fromInteger = Exact . fromInteger

  abs = \case
    Exact n   -> Exact (abs n)
    Inexact d -> Inexact (abs d)
    Boolean b -> Boolean b
    x         -> error $ "abs: not defined for " <> show x

  signum = \case
    Exact n   -> Exact (signum n)
    Inexact d -> Inexact (signum d)
    Boolean b -> Boolean b
    Text t    -> Text (show . signum $ length t)
    List ls   -> List [Exact . signum $ length ls ]
    Word _    -> error "signum: not defined for words"

crossProd :: [b] -> [b] -> [b]
crossProd c1 c2 = do { x <- c1; y <- c2; [x,y] }

