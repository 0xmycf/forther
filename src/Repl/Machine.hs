module Repl.Machine
  (
  -- * Dictionary
  Dict
  -- ** Default dictionary
  , def
  -- ** Elements of the dictionary
  , DictEntry(..)
  -- * The Interpreter
  , Interpreter(..)
  , FortherCompileMode(..)
  , setCompileMode
  , setRunMode
  , setMode
  , ReadMode(..)
  , readModeIsFile
) where

import           BinTree                (insert, keys)
import qualified BinTree
import qualified BinTree                as BT
import           Control.DeepSeq        (deepseq, force)
import           Control.Exception      (ErrorCall, evaluate, try)
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Either            (fromRight)
import           Data.Function          ((&))
import qualified Data.List              as List
import           Data.String            (fromString)
import           HasState               (HasState(..))
import           Repl.Types             (DefineCtx(..), Dict, DictEntry(..),
                                         Eval, FortherCompileMode(..),
                                         Interpreter(..), ReadMode(..),
                                         StackOperation, defineCtx, setStack)
import           Result                 (orFailWith)
import qualified Stack
import           Stack                  (StackElement(..), divides, empty,
                                         implies, pop, popN, prettyPrint, push,
                                         toToken)
import           System.Exit            (exitSuccess)
import           Token                  (FWord, Flag(..), setFlags, withFlags,
                                         word)

readModeIsFile :: ReadMode -> Bool
readModeIsFile  = \case
  (File _) -> True
  _        -> False

setCompileMode :: Interpreter -> Interpreter
setCompileMode m = m { mode = WordDefineMode }

setRunMode :: Interpreter -> Interpreter
setRunMode m = m { mode = RunMode }

setMode :: FortherCompileMode -> Interpreter -> Interpreter
setMode mode m = m { mode = mode }

unsafeWord :: String -> FWord
unsafeWord = fromRight (error "Word malformed") . word

unsafeWordFlags :: String -> [Flag] -> FWord
unsafeWordFlags str = fromRight (error "Word malformed") . withFlags str

dot :: StackOperation ()
dot = get >>= \m ->
  case Stack.pop m.stack of
    Nothing -> fail "StackUnderflow"
    Just (_, !rest) ->
      let new = m { stack = rest }
      in new `seq` put new

dup :: StackOperation ()
dup = get >>= \m ->
    case Stack.pop m.stack of
      Nothing -> fail "StackUnderflow"
      Just (!elem', !rest) ->
        let new = m { stack = push elem' $ push elem' rest }
        in new `seq` put new

swap :: StackOperation ()
swap = get >>= \m ->
   case Stack.swap m.stack of
     Just !ns -> put m { stack = ns }
     Nothing  -> fail "swap: StackUnderflow"

rot :: StackOperation ()
rot = get >>= \m ->
  case Stack.rot m.stack of
    Just !ns -> put m { stack = ns }
    Nothing  -> fail "rot: StackUnderflow"

unOp :: (StackElement -> StackElement) -> StackOperation ()
unOp f = do
  m <- get
  (!elem', !rest) <- Stack.pop m.stack `orFailWith` "unOp: StackUnderflow"
  !res <- liftIO $ try (evaluate $ force $ f elem')
  case res of
    Right res'            -> put $ m { stack = push res' rest }
    Left (e :: ErrorCall) -> fail $ "unOp: " <> show e

binOp :: (StackElement -> StackElement -> StackElement) -> StackOperation ()
binOp op = do
  m <- get
  (!elems, !rest) <- popN 2 m.stack `orFailWith` "binOp: StackUnderflow"
  case elems of
    [a, b] -> do
      !res <- liftIO $ try (evaluate $ force $ a `op` b)
      case res of
          Right res'            -> put m { stack = push res' rest }
          Left (e :: ErrorCall) -> fail $ "binOp: " <> show e
    _      -> fail "binOp: Not enough elements on the stack"

{- pop :: StackOperation StackElement
pop = get >>= \m ->
  case Stack.pop m.stack of
    Nothing              -> Result.Result.fail "pop: StackUnderflow"
    Just (!elem', !rest) -> elem' <$ put m {stack = rest} -}

peek :: StackOperation StackElement
peek = get >>= \m ->
  case Stack.pop m.stack of
    Nothing          -> fail "peek: StackUnderflow"
    Just (!elem', _) -> pure elem'

reverse :: StackOperation ()
reverse = get >>= \m ->
  case Stack.pop m.stack of
    Just (List !es, !rest) ->
      let !neu = Stack.List (List.reverse es) in
      put m { stack = Stack.push neu rest}
    Just _ -> fail "reverse: Wrong type on the stack"
    Nothing -> fail "reverse: StackUnderflow"

-- | TODO
-- I wanted to implement for in terms of already defined
-- forther words, though without a counter or some jump on condition
-- this is not possible.
--
-- Once I have implemented some low-level jump or something similar
-- I might be able to rewrite this again into something simpler
--
-- ## Usage
-- @ { I show } 1 10 for @
-- This will print the numbers from 1 to 10 ( or rather the stack )
--
-- # Different todo
-- this is not in the right place
fortherFor :: Eval -> StackOperation ()
fortherFor eval = get >>= \m ->
    case Stack.popN 3 m.stack of
        Just ([b, f, t], stack) ->
            case (b,f, t) of
                (List body, Exact from_, Exact to_) ->
                    do
                    forM_ [from_..to_] (\i -> do
                        put m { stack = stack
                              , dictionary =
                                  BinTree.insert
                                    (unsafeWord "I")
                                    (Literal . fromString . show $ i) m.dictionary }
                        eval $ map toToken body)

                    put m { stack = stack
                          , dictionary = BinTree.delete (unsafeWord "I") m.dictionary
                          }
                -- TODO I could implement looping over strings or lists with only one get
                _ -> fail "Cannot loop over non exact values"
        _  -> fail "Not enough matching elements on the stack"

exec :: Eval -> StackOperation ()
exec eval = do
  stack <- stack <$> get
  (!ls, !rest) <- pop stack `orFailWith` "eval: exec: StackUnderflow"
  modify $ setStack $ const rest
  case ls of
    List ts -> eval (map toToken ts)
    _       -> fail "eval: exec: not a list"

colon :: StackOperation ()
colon = get >>= \m -> put m { mode = WordDefineMode }

semicolon :: StackOperation ()
semicolon = get >>=
  \m -> do
    may_ctx <- defineCtx <$> get
    case may_ctx of
      Nothing -> fail "Nothing to define! (defineCtx was Nothing)"
      Just ctx ->
        put $
          ctx.body `deepseq` m
            { mode = RunMode
            , defineCtx = Nothing
            , dictionary
              = BT.insert
                (force $ setFlags ctx.name ctx.flags)
                -- drop 1 for the word itself
                (Literal . drop 1 . Prelude.reverse $ ctx.body) m.dictionary
            }

-- | TODO remove eval parameter
-- TODO ; should be a word
--    to properly define this, we would need to work with the tokens instead of the strings
def :: Eval -> Dict
def eval =
    let i = insert
        uw = unsafeWord
        -- german for immediate
        -- this is shorter than immediate so I dont have to format everything right now
        -- this is due to change anyway, so for now its fine
        sofort = flip unsafeWordFlags [Immediate]
        in BinTree.empty
        & i (uw "quit")   (BuiltIn (liftIO exitSuccess))
        & i (uw "show")   (BuiltIn (liftIO . putStrLn . prettyPrint . stack =<< get))
        & i (uw "ls")     (Literal "show") -- alias for show
        & i (uw ".")      (BuiltIn dot)
        & i (uw "dup")    (BuiltIn dup)
        & i (uw "swap")   (BuiltIn swap)
        & i (uw "rot")    (BuiltIn rot)
        & i (uw "words")  (BuiltIn (get >>= liftIO . print . keys . dictionary))

        & i (uw ":")      (BuiltIn colon)
        & i (sofort ";")  (BuiltIn semicolon)

        -- for use in : bar : foo 1 ;; ; (when bar is called, foo is defined)
        -- this doesnt work currently as a subsequent definition would
        -- be in compile mode again
        -- & i (uw "semi")   (BuiltIn semicolon)

        -- -- Word definition
        -- & i (uw ":")      (BuiltIn setDefine)

        {- CONTROL FLOW
        *if* works like this:

        { if-true } { if-false } <cond> if


        {swap} *
          true  -> {swap}
          false -> {}
        =>
        {swap} * exec exec .
        ^^^^^^^^
        if true, swaps else doesn't swap

        {swap} * exec exec .
                 ^^^^
               executes the swap

        {swap} * exec exec .
                      ^^^^
                    executes the true/false branch

        {swap} * exec exec .
                           ^
                          drops the unused branch

        NOTE: This _only_ works if the {if-true} and {if-false} branches
        leave the stack unchanged in the end.
        Otherwise "exec" works as a flatten and the stack will be in a state like this:

        HEAD
        vvvvv
        elem4 elem3 elem2 elem1 {other-branch}

        with only 'elem0' removed from the stack. (due to the . at the end)
        Note that the inner elements of the inner stack are reversed.

        -}
        & i (uw "if")     (Literal "{swap} * exec exec .")
        & i (uw "for")    (BuiltIn $ fortherFor eval)

      {- The 'exec word'
         This word acts on the first element of the stack, if the
         element is also a stack it will evaluate it using the passed evaluater 'eval'.
      -}
        & i (uw "exec" )  (BuiltIn (exec eval))

        & i (uw "+")      (BuiltIn (binOp (+)))
        & i (uw "sub")    (BuiltIn (binOp (-)))
        & i (uw "*")      (BuiltIn (binOp (*)))
        -- & i (uw "/")      (BuiltIn (binOp (/))) -- this is also very unsafe...
        & i (uw "abs")    (BuiltIn (unOp abs))
        & i (uw "sign")   (BuiltIn (unOp signum))

        & i (uw "<")      (BuiltIn (binOp (\a b -> Boolean (a < b))))
        & i (uw ">")      (BuiltIn (binOp (\a b -> Boolean (a > b))))
        & i (uw "=")      (BuiltIn (binOp (\a b -> Boolean (a == b))))
        & i (uw "<=")     (BuiltIn (binOp (\a b -> Boolean (a <= b))))
        & i (uw ">=")     (BuiltIn (binOp (\a b -> Boolean (a >= b))))
        & i (uw "=>")     (BuiltIn (binOp implies))
        & i (uw "|")      (BuiltIn (binOp (\a b -> Boolean (b `divides` a))))
        & i (uw "clear")  (BuiltIn (modify (\m -> m { stack = empty })))

        & i (uw "reverse") (BuiltIn Repl.Machine.reverse)
        & i (uw "flatten") (Literal "reverse exec")

        {-
          IO

          println -- prints the top of the stack (does not remove it)
        -}
        & i (uw "println") (BuiltIn (peek >>= liftIO . print))
        & i (uw "print") (BuiltIn (peek >>= liftIO . putStr . show))

        -- TODO remove this once we support shebangs
        -- NOTE: I plan on using # as a way of defining builtin keywords / macros

        & i (uw "#!") (Literal "\"shebang is not supported yet\" println ." )
