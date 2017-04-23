module Utils where

import           Import       hiding (group)
import           Data.Char           (isSpace)
import           Data.Conduit.Binary (sinkLbs)
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.HashMap.Strict        as M
import qualified Data.List                  as L
-- import           Data.Hashable

stringFields :: Int -> String -> Either String [String]
stringFields n s
  | length xs == n = Right xs
  | otherwise      = Left err
  where
    xs             = stringFields' s
    err            = s ++ " does not have " ++ show n ++ " fields!"

stringFields' :: String -> [String]
stringFields' s = snipSpaces <$> (splitOn ',' s)

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

textString :: (IsString a) => Text -> a
textString = fromString . unpack

group :: (Eq k, Hashable k) => [(k, v)] -> M.HashMap k [v]
group = groupBase M.empty

groupBase :: (Eq k, Hashable k) => M.HashMap k [v] -> [(k, v)] -> M.HashMap k [v]
groupBase = L.foldl' (\m (k, v) -> inserts k v m)

groupList :: (Eq k, Hashable k) => [(k, v)] -> [(k, [v])]
groupList = M.toList . group

groupBy   :: (Eq k, Hashable k) => (a -> k) -> [a] -> M.HashMap k [a]
groupBy f = L.foldl' (\m x -> inserts (f x) x m) M.empty

inserts ::  (Eq k, Hashable k) => k -> v -> M.HashMap k [v] -> M.HashMap k [v]
inserts k v m = M.insert k (v : M.lookupDefault [] k m) m
