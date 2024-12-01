module Token (Token(..), FWord, word) where

word :: String -> Either String FWord

data FWord
  = FWord  
      { str :: String
      , flags :: Int
      }

data Token where
  FBoolT :: !Bool -> Token -- ^ A boolean
  FNumberT :: !Int -> Token -- ^ A number (pos or negative)
  FDoubleT :: !Double -> Token -- ^ A number (pos or negative)
  FWordT :: !FWord -> Token -- ^ A word in the forther language
  FTextT :: !String -> Token -- ^ A string
  FListT :: ![Token] -> Token -- ^ A stack
  FKeywordT :: !String -> Token
