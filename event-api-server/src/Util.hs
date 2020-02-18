module Util where

import           Control.Monad          (guard)
import           Data.Char              (toLower)
import           Data.List              (stripPrefix)
import           Data.UUID              (UUID)
import qualified Data.UUID              as UUID
import qualified System.Random.SplitMix as R
import           Text.Read              (readMaybe)

-- | Drop the prefix, downcase the next letter
fieldLabelModifier :: String -> String -> String
fieldLabelModifier prefix fieldName = case stripPrefix prefix fieldName of
  Just "UUID" -> "uuid"
  Just (c:cs) -> toLower c : cs
  _           -> fieldName

-- | Bound the size of a response.
toLimit :: Maybe Int -> Int
toLimit (Just n)
 | n >= 0 && n <= 150 = n
toLimit _  = 150

-- | Parse a port number.
readPort :: String -> Maybe Int
readPort str = do
  p <- readMaybe str
  guard (p > 0)
  guard (p < 65536)
  pure p

-- | Parse a boolean.
readBool :: String -> Maybe Bool
readBool str
  | str `elem` ["true", "True", "TRUE"] = Just True
  | str `elem` ["false", "False", "FALSE"] = Just False
  | otherwise = Nothing

-- | Generate a random UUID.
genUUID :: IO UUID
genUUID = do
  gen <- R.newSMGen
  let (w0, w1, gen') = R.nextTwoWord32 gen
  let (w2, w3, _)    = R.nextTwoWord32 gen'
  pure (UUID.fromWords w0 w1 w2 w3)
