module DB where

import           Import
-- import qualified Data.List as L
-- import qualified Data.Text as T
-- import qualified Database.Esqueleto as E
-- import           Database.Esqueleto ((^.))

getClassesByUser :: Handler [Class]
getClassesByUser = do
  (uid, _) <- requireAuthPair
  classes  <- runDB $ selectList [ClassInstructor ==. uid] []
  return      (entityVal <$> classes)

{- do
    papers <- runDB
           $ E.select
           $ E.from $ \(author `E.InnerJoin` paper) -> do
                E.on $ (paper ^. PaperId E.==. author ^. AuthorPaper) E.&&.
                  ((author ^. AuthorAuthor) `E.like` (E.%) E.++. E.val authorName E.++. (E.%))
                return
                    ( paper ^. PaperId
                    , paper ^. PaperTitle
                    , paper ^. PaperFilepath
                    , paper ^. PaperAbstract
                    )
    return papers

-}
