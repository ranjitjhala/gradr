module DB where

import           Import
import qualified Data.List as L
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))

-- | Gets papers with authors matching the input string
-- Lift/LH: u:User
--          -> {w:World | inDecisionPhase w}
--          -> s:Text
--          -> Handler [{p:Entity Paper | p ^. accepted}]
--
-- UrFlow:
-- policy sendClient (SELECT *
--         FROM phase, paper, review
--         WHERE phase = 3
--           AND review.id = paper.id
--           AND review.status = Accepted
getClassesByUser :: User -> Handler [(E.Value Text, E.Value Text)]
getClassesByUser =
  undefined

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
