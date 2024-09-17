
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Stack
 ( Stack(..)
 -- * Functions
 -- ** Accessors
 , unstack
 -- ** Constructors
 , empty
 , singleton
 -- ** combinators
 , Stack.concat
 -- ** Stack Operations
 , push
 , pushN
 , pop
 , popN
 -- ** Stack Manipulation Forth-Style
 , swap
 , rot
 -- ** Pretty Printing
 , prettyPrint
 -- * Stuff for the interperter
 , StackElement(..)
 , toStackElement
 , toToken
 , implies
 -- * Arithmetic
 , divides
 ) where

import           Control.DeepSeq (NFData)
import           Data.List       ((\\))
import qualified Data.List       as List
import           GHC.Generics    (Generic)
import           Token           (FWord, Token(..))

newtype Stack a
  = Stack [a]
  deriving (Show)

-- | unwraps the underlying list
-- >>> unstack (Stack [1,2,3::Int])
-- [1,2,3]
unstack :: Stack a -> [a]
unstack (Stack ls) = ls

-- |
-- >>> empty @(Stack Int)
-- Stack []
empty :: Stack a
empty = Stack []

-- |
-- >>> singleton (1::Int)
-- Stack [1]
singleton :: a -> Stack a
singleton = Stack . (:[])

concat :: [Stack a] -> Stack a
concat stacks = Stack (stacks >>= unstack)

push :: a -> Stack a -> Stack a
push a (Stack ls) = Stack (a : ls)

-- | Pushes the element in [a] in the order they are given on the stack
--
-- ## Examples
--
-- >>> pushN [1,2,3] (Stack [4,5,6::Int])
-- Stack [1,2,3,4,5,6]
pushN :: [a] -> Stack a -> Stack a
pushN as (Stack ls) = Stack (as ++ ls)

-- |
-- >>> pop (Stack [1,2,3::Int])
-- Just (1,Stack [2,3])
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack (a : as)) = Just (a, Stack as)
pop _                = Nothing

-- |
-- Returns the first n (Int) elements of the given `Stack a`
-- and the new stack without the first n (Int) elements
-- * the left most element in the resulting list is the n-th popped element
-- * the right most element was the first element on the list (the first popped one)
popN :: Int -> Stack a -> Maybe ([a], Stack a)
popN = go []
  where
    go acc n stack =
      case n of
        0 -> Just (acc, stack)
        m -> case pop stack of
              Just (a, stack') -> go (a : acc) (m - 1) stack'
              Nothing          -> Nothing

-- | Swaps the top two elements of the stack
-- >>> swap (Stack [1,2,3::Int])
-- Just (Stack [2,1,3])
swap :: Stack a -> Maybe (Stack a)
swap = \case
  Stack (a : b : as) -> Just $ Stack (b : a : as)
  _                  -> Nothing

-- | Rotates the top three elements of the stack
-- >>>rot (Stack [1,2,3::Int])
-- Just (Stack [3,1,2])
rot :: Stack a -> Maybe (Stack a)
rot = \case
  Stack (a : b : c : as) -> Just $ Stack (c : a : b : as)
  _                      -> Nothing

-- | Pretty prints the stack
-- >>> prettyPrint (Stack [(1::Int),2,3,4,5,6,7,8,9,10])
-- "{ ->1 2 3 4 5 6 7 8 9 10 }"
--
-- >>> prettyPrint (Stack [(1::Int)..20])
-- "{ ->1 2 3 4 5 6 7 8 9 10 ... }"
--
-- >>> prettyPrint (Stack ([]::[Int]))
-- "{  }"
prettyPrint :: Show a => Stack a -> String
prettyPrint (Stack ls) =
  "{ " <> (if null ls then "" else "->") <> (if length (map show ls) > 10
    then unwords (map show (take 10 ls)) <> " ..."
    else unwords (map show ls)) <> " }"

{-------------------------------------------------------------------------------

  Forther Stuff

-------------------------------------------------------------------------------}

data StackElement where
  Exact :: !Int -> StackElement
  Inexact :: !Double -> StackElement
  Boolean :: !Bool -> StackElement
  List :: ![StackElement] -> StackElement -- this is problematic bcs its lazy
  Text :: !String -> StackElement
  Word :: !FWord -> StackElement
  deriving (Eq, Generic, NFData, Ord)

instance Show StackElement where
  show = \case
    Exact n   -> show n
    Inexact d -> show d
    Boolean True  -> "true"
    Boolean False -> "false"
    List xs   -> "{ " ++ unwords (map show xs) ++ " }"
    Text t    -> t
    Word t    -> show t

-- | TODO may error if trying ot convert a keyword
toStackElement :: Token -> StackElement
toStackElement = \case
  FNumberT n -> Exact n
  FDoubleT n -> Inexact n
  FTextT t   -> Text t
  FBoolT b   -> Boolean b
  FListT ls  -> List (map toStackElement ls)
  FWordT w   -> Word w
  FKeywordT _ -> error "toStackElement: not defined for keywords"

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
implies _ _ = error "implies: not defined for non-boolean values" -- TODO

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
  List a * (Exact b)    = List (listMult a b)
  List a * (Inexact b)  = List (listMult a (floor b))
  List a * (Boolean b)  = List (listMult a (fromEnum b))
  List a * lhs          = List (crossProd a [lhs])
  (Exact a) * List b    = List (listMult b a)
  (Inexact a) * List b  = List (listMult b (floor a))
  (Boolean a) * List b  = List (listMult b (fromEnum a))
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

listMult :: [a] -> Int -> [a]
listMult ls n = List.concat $ replicate n ls

-- >>> (Exact 2) `divides` (Exact 4)
-- True
--
-- >>> (Exact 2) `divides` (Exact 5)
-- False
--
-- >>> (Exact 2) `divides` (Inexact 4.0)
-- divides: not defined for non-exact values
divides :: StackElement -> StackElement -> Bool
divides a b = case (a,b) of
  (Exact a', Exact b') -> b' `mod` a' == 0
  _                    -> error "divides: not defined for non-exact values"
