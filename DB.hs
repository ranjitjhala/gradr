module DB where

-- import qualified Data.List as L
import qualified Data.HashMap.Strict as M
import           Import
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))

getClassesByUser :: Handler [Entity Class]
getClassesByUser = do
  (uid, _) <- requireAuthPair
  runDB $ selectList [ClassInstructor ==. uid] []

getClassById :: Key Class -> Handler Class
getClassById ident = do
  mfile <- runDB $ get ident
  maybe notFound return mfile

getUserById :: Key User -> Handler User
getUserById ident = do
  mfile <- runDB $ get ident
  maybe notFound return mfile

getAssignmentById :: Key Assignment -> Handler Assignment
getAssignmentById ident = do
  mfile <- runDB $ get ident
  maybe notFound return mfile

getUserByEmail :: Text -> Handler (Maybe (Entity User))
getUserByEmail email =
  runDB $ selectFirst [UserEmailAddress ==. email] []

getAssignmentsByClass :: Key Class -> Handler [Entity Assignment]
getAssignmentsByClass classId =
  runDB $ selectList [AssignmentClass ==. classId] []

getStudentsByClass :: Key Class -> Handler [Entity User]
getStudentsByClass classId =
  runDB $ E.select
          $ E.from
            $ \(student `E.InnerJoin` user) -> do
               E.on $ (student ^. StudentName  E.==. user ^. UserId)
                      E.&&.
                      (student ^. StudentClass E.==. E.val classId)
               return user

getRawScores :: Key Assignment -> Handler [(Text, Int)]
getRawScores assignId = do
  scores <- runDB $ selectList [ScoreAssignment ==. assignId] []
  forM scores $ \(Entity _ (Score uid _ pts)) -> do
    user <- getUserById uid
    return (userEmailAddress user, pts)

getAssignmentScores :: ClassId -> AssignmentId -> Handler [(Entity User, Int)]
getAssignmentScores classId assignId = do
  students  <- getStudentsByClass classId
  rawScores <- getRawScores assignId
  return     $ updScores [ (u, 0) | u <- students ] rawScores

updScores :: [(Entity User, Int)] -> [(Text, Int)] -> [(Entity User, Int)]
updScores us ens = [ (fst u, score u) | u <- us ]
  where
    score (u, n) = M.lookupDefault n (userKey u) scorem
    scorem       = M.fromList [(e, n) | (e, n) <- ens ]
    userKey      = userEmailAddress . entityVal

updAssignmentScores :: AssignmentId -> [(Entity User, Int)] -> Handler ()
updAssignmentScores assignId scores = do
  _ <- runDB $ deleteWhere [ScoreAssignment ==. assignId]
  _ <- runDB $ insertMany [Score uid assignId pts | (Entity uid _, pts) <- scores]
  return ()

{-
  getPapersToReview :: Handler [(E.Value (Key Review)
                               , E.Value PaperStatus
                               , E.Value Text
                               , E.Value Text
                               , E.Value Text)]
  getPapersToReview = do
      (uid, _user) <- requireAuthPair
      runDB
        $ E.select
          $ E.from $ \(review `E.InnerJoin` paper) -> do
                  E.on $ (paper ^. PaperId E.==. review ^. ReviewPaper ) E.&&.
                         ((paper ^. PaperReady E.==. E.val True)) E.&&.
                         ((review ^. ReviewUser) E.==. E.val uid)
                  return
                      ( review ^. ReviewId
                      , review ^. ReviewStatus
                      , review ^. ReviewComments
                      , paper ^. PaperTitle
                      , paper ^. PaperAbstract
                      )
      return papers

-}
