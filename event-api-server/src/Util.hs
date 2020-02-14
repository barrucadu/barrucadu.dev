module Util where

import           Data.Char (toLower)
import           Data.List (stripPrefix)

-- | Drop the prefix, downcase the next letter
fieldLabelModifier :: String -> String -> String
fieldLabelModifier prefix fieldName = case stripPrefix prefix fieldName of
  Just (c:cs) -> toLower c : cs
  _           -> fieldName
