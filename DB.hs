module DB where

import           Import
-- import qualified Data.List as L
-- import qualified Data.Text as T
-- import qualified Database.Esqueleto as E
-- import           Database.Esqueleto ((^.))

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

getAssignmentsByClass :: Key Class -> Handler [Entity Assignment]
getAssignmentsByClass classId =
  runDB $ selectList [AssignmentClass ==. classId] []
