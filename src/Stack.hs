module Stack
 ( Stack(..)
 , unstack
 , empty
 , singleton
 , push
 , pop
 , popN
 , popUnsafe
 ) where

newtype Stack a
  = Stack [a]

unstack :: Stack a -> [a]
unstack (Stack ls) = ls

empty :: Stack a
empty = Stack []

singleton :: a -> Stack a
singleton = Stack . (:[])

push :: a -> Stack a -> Stack a
push a (Stack ls) = Stack (a : ls)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack (a : as)) = Just (a, Stack as)
pop _                = Nothing

-- TODO still unsafe
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
