module DB where

-- import qualified Data.List as L
import qualified Data.HashMap.Strict as M
import           Import
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))

getClassesInsByUser :: Key User -> Handler [Entity Class]
getClassesInsByUser userId = do
  es  <- runDB $ selectList [ClassInstructor ==. userId] []
  es' <- runDB $ E.select
                  $ E.from
                    $ \(teacher `E.InnerJoin` clss) -> do
                      E.on $ (teacher ^. TeacherName  E.==. E.val userId)
                             E.&&.
                             (teacher ^. TeacherClass E.==. clss ^. ClassId)
                      return clss
  return (es ++ es')

getClassesStdByUser :: Key User -> Handler [Entity Class]
getClassesStdByUser userId =
  runDB $ E.select
            $ E.from
               $ \(student `E.InnerJoin` clss) -> do
                    E.on $ (student ^. StudentName  E.==. E.val userId)
                           E.&&.
                           (student ^. StudentClass E.==. clss ^. ClassId)
                    return clss

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

getInstructorsByClass :: Key Class -> Handler [Entity User]
getInstructorsByClass classId =
  runDB $ E.select
          $ E.from
            $ \(teacher `E.InnerJoin` user) -> do
               E.on $ (teacher ^. TeacherName  E.==. user ^. UserId)
                      E.&&.
                      (teacher ^. TeacherClass E.==. E.val classId)
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

updUserIdent :: UserId -> Text -> Handler ()
updUserIdent uid name = runDB $ update uid [UserIdent =. name]

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

getScoresByUser :: UserId -> ClassId -> Handler [(Assignment, Int)]
getScoresByUser userId classId = do
  asgns <- getAssignmentsByClass classId
  forM asgns $ \ (Entity asgnId a) -> do
    mbSc  <- runDB $ getBy (UniqueScore userId asgnId)
    return (a, maybe 0 (scorePoints . entityVal) mbSc)

--  ass <- runDB $ E.select
--           $ E.from
--             $ \(assign `E.InnerJoin` score) -> do
--               E.on $ (assign ^. AssignmentClass E.==. E.val classId)
--                      E.&&.
--                      (score  ^. ScoreStudent    E.==. E.val userId)
--               return ( assign, score )
--  return [ (a, scorePoints s) | (Entity _ a, Entity _ s) <- ass ]
