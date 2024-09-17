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
  , pattern Exec
  , pattern Colon
  , word
  ) where
import           Control.DeepSeq (NFData)

-- | Use the smart constructor `word` instead
-- An FWord should never be longer than 10 chars
-- and not contain numbers
--
-- >>> show $ FWord "exec"
-- ":exec"
newtype FWord
  = FWord String
  deriving newtype (Eq, NFData, Ord)

instance Show FWord where
  show (FWord str) = ':' : str

pattern Exec :: FWord
pattern Exec <- FWord "exec"

pattern Colon :: FWord
pattern Colon <- FWord ":"

-- | Safely creates a word from a String
word :: String -> Either String FWord
word str
  | length str > 10 = Left "word: too long"
  | head str `elem` '-' : ['0'..'9'] = Left "word: cannot start with a number or hypthen"
  | otherwise = Right (FWord str)

data Token where
  FBoolT :: !Bool -> Token -- ^ A boolean
  FNumberT :: !Int -> Token -- ^ A number (pos or negative)
  FDoubleT :: !Double -> Token -- ^ A number (pos or negative)
  FWordT :: !FWord -> Token -- ^ A word in the forther language
  FTextT :: !String -> Token -- ^ A string
  FListT :: ![Token] -> Token -- ^ A stack
  FKeywordT :: !String -> Token -- ^ A keyword. All keywords start with #, which is not included in the String
  deriving (Show)

