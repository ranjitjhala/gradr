module Utils where

import           Import
import           Data.Char           (isSpace)
import           Data.Conduit.Binary (sinkLbs)
import qualified Data.ByteString.Lazy.Char8 as LB8

stringFields :: Int -> String -> Either String [String]
stringFields n s
  | length xs == n = Right (snipSpaces <$> xs)
  | otherwise      = Left err
  where
    xs             = splitOn ',' s
    err            = s ++ " does not have " ++ show n ++ " fields!"

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ []     = [[]]
splitOn x (y:ys)
  | x == y       = [] : splitOn x ys
  | otherwise    = (y:zs) : zss where zs:zss = splitOn x ys

snipSpaces :: String -> String
snipSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

fileLines :: FileInfo -> IO [String]
fileLines file = do
  bytes <- runResourceT $ fileSource file $$ sinkLbs
  return (lines . LB8.unpack $ bytes)
