module Util where

import           Data.Char  (toLower)
import           Data.List  (stripPrefix)
import           Data.Maybe (fromMaybe)

-- | Drop the prefix, downcase the next letter
fieldLabelModifier :: String -> String -> String
fieldLabelModifier prefix fieldName = case stripPrefix prefix fieldName of
  Just (c:cs) -> toLower c : cs
  _           -> fieldName

-- | Bound the size of a response.
toLimit :: Maybe Int -> Int
toLimit (Just n)
 | n >= 0 && n <= 150 = n
toLimit _  = 150
