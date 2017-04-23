-- | Common handler functions.
module Handler.Common where

import           Import
import           Data.FileEmbed (embedFile)
import qualified Data.Text.Encoding     as T
import qualified Data.HashMap.Strict    as M
import qualified Data.ByteString.Char8 as B

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")


data ClassCsv = ClassCsv
  { csvAsgns  :: Int
  , csvNames  :: [Text]           -- List Text csvAsgns
  , csvPoints :: [Int]            -- List Int  csvAsgns
  , csvScores :: [(Text, [Int])]  -- [(Text, List Int csvAsgns)]
  }
  deriving (Show)

type Scores = [(Entity Assignment, [(Text, Int)])]
scoresCsv :: Scores -> ClassCsv
scoresCsv sc  = ClassCsv (length sc) ns pts (studentScores sc)
  where
    (ns, pts) = unzip [(n, pt) | (Entity _ (Assignment n pt _), _) <- sc]

studentScores    :: Scores -> [(Text, [Int])]
studentScores sc = [(e, eScore e <$> aTables) | e <- emails]
  where
    eScore e     = M.lookupDefault 0 e
    aTables      :: [M.HashMap Text Int]
    aTables      = [M.fromList m | m <- hscores]
    emails       :: [Text]
    emails       = M.keys . M.fromList $ concat hscores
    hscores      :: [[(Text, Int)]]
    hscores      = snd <$> sc

csvBytes :: ClassCsv -> ByteString
csvBytes c = B.unlines $ namesBS  (csvNames  c)
                       : pointsBS (csvPoints c)
                       : (scoresBS <$> csvScores c)

namesBS :: [Text] -> ByteString
namesBS asgns = commaCat ("names" : (T.encodeUtf8 <$> asgns))

pointsBS :: [Int] -> ByteString
pointsBS pts = commaCat ("points" : (intBS <$> pts))

scoresBS :: (Text, [Int]) -> ByteString
scoresBS (u, ns) = commaCat (T.encodeUtf8 u : (intBS <$> ns))

intBS :: Int -> ByteString
intBS = fromString . show

commaCat :: [ByteString] -> ByteString
commaCat = intercalate ","
