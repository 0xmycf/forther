{-# LANGUAGE PatternSynonyms #-}
module Token 
  ( Token(..)
  -- * Words in the Forther langauge
  , FWord
  , pattern Exec
  , pattern Colon
  , word
  ) where

-- | Use the smart constructor `word` instead
-- An FWord should never be longer than 10 chars
-- and not contain numbers
--
-- >>> show $ FWord "exec"
-- ":exec"
newtype FWord
  = FWord String
  deriving newtype (Eq, Ord)

instance Show FWord where
  show (FWord str) = ':' : str

pattern Exec :: FWord
pattern Exec <- FWord "exec"

pattern Colon :: FWord
pattern Colon <- FWord ":"

-- | Creates a word from a String
word :: String -> Either String FWord
word str
  | length str > 10 = Left "word: too long"
  | head str `elem` '-' : ['0'..'9'] = Left "word: cannot start with a number or hypthen"
  | otherwise = Right (FWord str)

-- TODO move this
data Token where
  FBoolT :: Bool -> Token -- ^ A boolean
  FNumberT :: Int -> Token -- ^ A number (pos or negative)
  FDoubleT :: Double -> Token -- ^ A number (pos or negative)
  FWordT :: FWord -> Token -- ^ A word in the forther language
  FTextT :: String -> Token -- ^ A string
  FListT :: [Token] -> Token
  deriving (Show)

