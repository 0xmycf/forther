{-# LANGUAGE PatternSynonyms #-}
{-| Haddock {{{
Module      : Token
Description : Tokens and Lexing
Copyright   : (c) 0xmycf, 2024
License     : MIT
Maintainer  : mycf.mycf.mycf@gmail.com
Stability   : experimental
Portability : POSIX
}}}-}
module Token
  ( Token(..)
  -- * Words in the Forther langauge
  , FWord
  , word
  , withFlags
  , withFlag
  -- * Flags or Keywords
  , Flag(..)
  , isImmediate
  -- * Special Forther words
  , pattern Exec
  , pattern Colon
  , pattern SemiColon
  -- * Utility Functions
  , fromToken
  , toKeyword
  , isKeyword
  , isWord
  , isSemiColon
  , isImmediateT
  , setFlags
  -- * Pretty printing
  , prettyPrint
  ) where

import           Control.DeepSeq (NFData)
import           Data.Bits       (Bits(setBit))
import           Data.String     (IsString(..))
import           Foreign         (testBit)
import           GHC.Generics    (Generic)
import           Lexer           (lexerS)

-- | Use the smart constructor `word` instead
-- An FWord should never be longer than 10 chars
-- and not contain numbers.
--
-- Words ordered and compared using only their string representation
--
-- >>> show $ FWord "exec" 0
-- ":exec"
data FWord
  = FWord
      { str   :: String
      , flags :: Int
      }
  deriving (Generic, NFData)

instance Eq FWord where
  (==) (FWord str1 _) (FWord str2 _) = str1 == str2

instance Ord FWord where
  compare (FWord str1 _) (FWord str2 _) = compare str1 str2

instance Show FWord where
  show (FWord str _) = ':' : str

-- | the "exec" word
-- It executes the top of the stack (if its a stack / list)
pattern Exec :: FWord
pattern Exec <- FWord "exec" _

-- | the ":" word
-- It is used to define a new word
pattern Colon :: FWord
pattern Colon <- FWord ":" _

-- | the ";" word
-- Its used to end a word definition
pattern SemiColon :: FWord
pattern SemiColon <- FWord ";" _

-- | Safely creates a word from a String
word :: String -> Either String FWord
word str = withFlags str []
{-# INLINE word #-}

withFlags :: String -> [Flag] -> Either String FWord
withFlags str flags
  | length str > 10 = Left "word: too long"
  | head str `elem` '-' : ['0'..'9'] = Left "word: cannot start with a number or hypthen"
  | otherwise = Right (FWord str flgs)
  where flgs = foldr (setBit . fromEnum) 0 flags
{-# INLINABLE withFlags #-}

withFlag :: FWord -> Flag -> FWord
withFlag (FWord str flags) flag = FWord str (setBit flags (fromEnum flag))
{-# INLINE withFlag #-}

setFlags :: FWord -> [Flag] -> FWord
setFlags = foldr (flip withFlag)
{-# INLINE setFlags #-}

fromToken :: Token -> Maybe FWord
fromToken = \case
  FWordT w -> Just w
  _        -> Nothing
{-# INLINE fromToken #-}

data Flag
  = Immediate
  deriving (Enum)

-- |
-- >>> import Data.Either (fromRight)
-- >>> isImmediate $ fromRight undefined $ withFlags "foo" [Immediate]
-- True
isImmediate :: FWord -> Bool
isImmediate (FWord _ flags) = testBit flags (fromEnum Immediate)
{-# INLINE isImmediate #-}

data Token where
  FBoolT :: !Bool -> Token -- ^ A boolean
  FNumberT :: !Int -> Token -- ^ A number (pos or negative)
  FDoubleT :: !Double -> Token -- ^ A number (pos or negative)
  FWordT :: !FWord -> Token -- ^ A word in the forther language
  FTextT :: !String -> Token -- ^ A string
  FListT :: ![Token] -> Token -- ^ A stack
  FKeywordT :: !String -> Token -- ^ A keyword. All keywords start with #, which is not included in the String
  deriving (Generic, NFData, Show)

{- CURRENT DEFINED KEYWORDS

  Flag   Keyword      Description
  1      #immediate   A word that is executed immediately instead of being compoilied into a word definition

-}

toKeyword :: String -> Maybe Flag
toKeyword = \case
  "immediate" -> Just Immediate
  _           -> Nothing
{-# INLINE toKeyword #-}

isKeyword :: Token -> Bool
isKeyword = \case
  FKeywordT _ -> True
  _           -> False
{-# INLINE isKeyword #-}

isWord :: Token -> Bool
isWord = \case
  FWordT _ -> True
  _        -> False
{-# INLINE isWord #-}

isSemiColon :: Token -> Bool
isSemiColon = \case
  FWordT SemiColon -> True
  _                -> False
{-# INLINE isSemiColon #-}

isImmediateT :: Token -> Bool
isImmediateT = \case
  (FWordT w) -> isImmediate w
  _          -> False
{-# INLINE isImmediateT #-}

instance {-# OVERLAPPING #-} IsString [Token] where
  fromString :: String -> [Token]
  fromString s = case lexerS s of
    Right (tokens, _) -> tokens
    Left _            -> []

-- |
-- >>> str =  "1 2.2 one {1 {#bar 4.3} 3} #foo"
-- >>> foo :: [Token] = str
-- >>> prettyPrint foo == str
-- True
prettyPrint :: [Token] -> String
prettyPrint = unwords . map prettyPrint'
  where
    prettyPrint' :: Token -> String
    prettyPrint' = \case
      FBoolT b -> show b
      FNumberT n -> show n
      FDoubleT d -> show d
      FWordT w -> tail $ show w
      FTextT t -> show t
      FListT ts -> '{' : prettyPrint ts ++ "}"
      FKeywordT k -> '#' : k

{- TODO
    using keywords <> flags is not a good thing, since we later want to use keywords
    for stuff like classes
-}
