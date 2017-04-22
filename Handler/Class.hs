module Handler.Class where

import           Import
import           DB
import           Utils
import qualified Text.Blaze   as TB
import qualified Auth.Account as Auth
import           Text.Read (readMaybe)

-- import qualified Data.Vector  as V
-- import qualified Data.Csv     as Csv
-- import qualified Util as Util
-- import qualified Data.ByteString as S
-- import qualified Data.ByteString.Lazy as L
-- import Data.Conduit.Binary

-- import Yesod.Form.MassInput
-- import qualified Yesod.Form.Bootstrap3 as BS3 -- (BootstrapSubmit, BootstrapFormLayout (..), renderBootstrap3)
-- import Yesod.Form.Jquery (jqueryAutocompleteField)

--------------------------------------------------------------------------------
-- | Viewing/Changing User Settings --------------------------------------------
--------------------------------------------------------------------------------
getEditUserR :: Handler Html
getEditUserR = do
  (_, user)           <- requireAuthPair
  (setWidget, setEnc) <- generateFormPost (settingsForm user)
  defaultLayout $ do
    setTitle . toHtml $ userIdent user <> "'s Settings page"
    $(widgetFile "settings")

postEditUserR :: Handler Html
postEditUserR = do
  (uid, u)       <- requireAuthPair
  ((result,_),_) <- runFormPost (settingsForm u)
  case result of
    FormSuccess o -> DB.updUser uid (setName o)
    _             -> setMessage "Something went wrong!"
  redirect ProfileR

data SettingsForm = SettingsForm
  { setName :: Text }
  deriving (Show)

settingsForm :: User -> Form SettingsForm
settingsForm u = renderForm $ SettingsForm
  <$> areq textField  "Name"  (Just (userIdent u))
  <*  submitButton "Update"

--------------------------------------------------------------------------------
-- | Viewing Existing Classes --------------------------------------------------
--------------------------------------------------------------------------------
getClassInsR :: ClassId -> Handler Html
getClassInsR classId = do
  klass                 <- getClassById classId
  instructor            <- getUserById (classInstructor klass)
  asgns                 <- getAssignmentsByClass classId
  students              <- getStudentsByClass    classId
  teachers              <- getInstructorsByClass classId
  (asgnWidget, asgnEnc) <- generateFormPost assignForm
  (stdWidget,  stdEnc)  <- generateFormPost addUserForm
  (csvWidget,  csvEnc)  <- generateFormPost fileForm
  (insWidget,  insEnc)  <- generateFormPost addUserForm
  (clsWidget,  clsEnc)  <- generateFormPost classForm
  defaultLayout $
    $(widgetFile "viewClassInstructor")


getClassStdR :: ClassId -> Handler Html
getClassStdR classId = do
  (userId, _) <- requireAuthPair
  klass       <- getClassById classId
  scores      <- getScoresByUser userId classId
  defaultLayout $
    $(widgetFile "viewClassStudent")

getAssignmentR :: ClassId -> AssignmentId -> Handler Html
getAssignmentR classId assignId = do
  klass               <- getClassById classId
  instr               <- getUserById  (classInstructor klass)
  asgn                <- getAssignmentById assignId
  scores              <- getAssignmentScores classId assignId
  (stdWidget, stdEnc) <- generateFormPost (scoreForm scores)
  (asgWidget, asgEnc) <- generateFormPost assignForm
  (scoWidget, scoEnc) <- generateFormPost fileForm
  defaultLayout $
    $(widgetFile "viewAssignment")

--------------------------------------------------------------------------------
-- | Edit Name/Term for Class --------------------------------------------------
--------------------------------------------------------------------------------
postEditClassR :: ClassId -> Handler Html
postEditClassR classId =
  extendClassFormR
    "edit class name"
    classForm
    (\cf -> DB.updClass classId (cName cf) (cTerm cf))
    classId
    (ClassInsR classId)

--------------------------------------------------------------------------------
-- | Edit Name/Points for Assignment -------------------------------------------
--------------------------------------------------------------------------------
postEditAssignmentR :: ClassId -> AssignmentId -> Handler Html
postEditAssignmentR classId asgnId =
  extendClassFormR
    "edit assignment"
    assignForm
    (\af -> DB.updAssign asgnId (asgnName af) (asgnPoints af))
    classId
    (AssignmentR classId asgnId)

getDelAssignmentR :: ClassId -> AssignmentId -> Handler Html
getDelAssignmentR classId asgnId =
  extendClassHandleR
    "delete assignment"
    (return (Just asgnId))
    (DB.delAssign classId)
    classId
    (ClassInsR classId)

--------------------------------------------------------------------------------
-- | Update Scores for Assignment ----------------------------------------------
--------------------------------------------------------------------------------
postScoreR :: ClassId -> AssignmentId -> Handler Html
postScoreR classId assignId = do
  oldScores <- getAssignmentScores classId assignId
  extendClassFormR
    "update scores"
    (scoreForm oldScores)
    (updAssignmentScores assignId oldScores)
    classId
    (AssignmentR classId assignId)

postScoresR :: ClassId -> AssignmentId -> Handler Html
postScoresR classId assignId = do
  oldScores <- getAssignmentScores classId assignId
  extendClassFormR
    "update scores by .csv"
    fileForm
    (addScoresR classId assignId oldScores)
    classId
    (AssignmentR classId assignId)


-- | Add multiple scores via CSV upload
addScoresR :: ClassId -> AssignmentId -> [(Entity User, Int)] -> FileForm -> Handler ()
addScoresR classId assignId oldScores scoresForm = do
  res <- liftIO $ fileScores (fFile scoresForm)
  case res of
    Left err     -> setMessage $ "Error updating scores: " ++ TB.text (fromString err)
    Right scores -> updAssignmentScores assignId oldScores scores

fileScores :: FileInfo -> IO (Either String [(Text, Int)])
fileScores file = sequenceA . map stringScore <$> fileLines file

stringScore :: String -> Either String (Text, Int)
stringScore s = case stringFields 2 s of
                  Left err       -> Left err
                  Right [em, s2] -> case readMaybe s2 of
                                      Nothing -> Left ("Malformed score: " ++ s)
                                      Just n  -> Right (fromString em, n)
--------------------------------------------------------------------------------
-- | Deleting Instructors/Students from a Class --------------------------------
--------------------------------------------------------------------------------
getDelInsR :: ClassId -> UserId -> Handler Html
getDelInsR classId userId =
  extendClassHandleR
    "remove teacher"
    (return (Just userId))
    (DB.delTeacher classId)
    classId
    (ClassInsR classId)

getDelStdR :: ClassId -> UserId -> Handler Html
getDelStdR classId userId =
  extendClassHandleR
    "remove student"
    (return (Just userId))
    (DB.delStudent classId)
    classId
    (ClassInsR classId)

--------------------------------------------------------------------------------
-- | Adding New Instructors ----------------------------------------------------
--------------------------------------------------------------------------------
postNewInstructorR :: ClassId -> Handler Html
postNewInstructorR classId =
  extendClassFormR
    "add instructor"
    addUserForm
    (addInstructorR classId)
    classId
    (ClassInsR classId)

addInstructorR :: ClassId -> AddUserForm -> Handler ()
addInstructorR classId userForm = do
  mbStd <- addUserR classId userForm
  case mbStd of
    Nothing ->    setMessage $ "Error adding instructor: " ++ TB.text (auEmail userForm)
    Just e  -> do setMessage $ "Added instructor: "        ++ TB.text (auEmail userForm)
                  void $ updTeacher classId (entityKey e)
                  -- runDB (insert (Teacher (entityKey e) classId))

--------------------------------------------------------------------------------
-- | Creating New Assignments --------------------------------------------------
--------------------------------------------------------------------------------
postNewAssignR :: ClassId -> Handler Html
postNewAssignR classId =
  extendClassFormR
    "create assignment"
    assignForm
    (addAssignR classId)
    classId
    (ClassInsR classId)

addAssignR :: ClassId -> AssignForm -> Handler ()
addAssignR classId (AssignForm aName aPts) = do
  _ <- runDB $ insert $ Assignment aName aPts classId
  setMessage $ "Added new assignment: " ++ TB.text aName

scoreForm :: [(Entity User, Int)] -> Form [(Text, Int)]
scoreForm scores = renderForm (sequenceA (userForm <$> scores) <* submitButton "Submit")
  -- BS3.bootstrapSubmit (BS3.BootstrapSubmit ("Submit" :: Text) "btn-default" [("attribute-name","attribute-value")])
  where
    userForm (user, score) = (,) <$> pure email
                                 <*> areq intField (textString email) (Just score)
      where
        email = userEmailAddress (entityVal user)

textString :: (IsString a) => Text -> a
textString = fromString . unpack

dummyScores :: [(Text, Int)]
dummyScores = [ ("Michael", 26)
              , ("Alice"  , 10)
              , ("Robert" , 19)
              ]

data AssignForm = AssignForm
  { asgnName   :: Text
  , asgnPoints :: Int
  }
  deriving (Show)

assignForm :: Form AssignForm
assignForm = renderForm $ AssignForm
  <$> areq textField "Name"   Nothing
  <*> areq intField  "Points" Nothing
  <*  submitButton   "Submit"

--------------------------------------------------------------------------------
-- | Enrolling New Students ----------------------------------------------------
--------------------------------------------------------------------------------
data AddUserForm = AddUserForm
  { auEmail :: Text }
  deriving (Show)

addUserForm :: Form AddUserForm
addUserForm = renderForm $ AddUserForm
  <$> areq textField "Email" Nothing
  <*  submitButton   "Enroll"

data FileForm = FileForm
  { fFile :: FileInfo }

fileForm :: Form FileForm
fileForm = renderForm $ FileForm
  <$> areq fileField ".csv file" Nothing
  <*  submitButton   "Upload"

postNewStudentsR :: ClassId -> Handler Html
postNewStudentsR classId =
  extendClassFormR
    "enroll students by .csv"
    fileForm
    (addStudentsR classId)
    classId
    (ClassInsR classId)

postNewStudentR :: ClassId -> Handler Html
postNewStudentR classId =
  extendClassFormR
    "enroll student"
    addUserForm
    (addStudentR classId)
    classId
    (ClassInsR classId)


-- | Enroll multiple students via CSV upload
addStudentsR :: ClassId -> FileForm -> Handler ()
addStudentsR classId usersForm = do
  res <- liftIO $ fileUsers (fFile usersForm)
  case res of
    Left err     -> setMessage $ "Error enrolling students: " ++ TB.text (fromString err)
    Right emails -> mapM_ (addStudentR classId) emails

fileUsers :: FileInfo -> IO (Either String [AddUserForm])
fileUsers file = Right . map stringUser <$> fileLines file
  where
    stringUser :: String -> AddUserForm
    stringUser = AddUserForm . fromString . snipSpaces



-- | Enroll a single student to the class
addStudentR :: ClassId -> AddUserForm -> Handler ()
addStudentR classId userForm = do
  mbStd <- addUserR classId userForm
  case mbStd of
    Nothing ->    setMessage $ "Error enrolling student: " ++ TB.text (auEmail userForm)
    Just e  -> do setMessage $ "Enrolled student: "        ++ TB.text (auEmail userForm)
                  void $ runDB (insert (Student (entityKey e) classId))

addUserR :: ClassId -> AddUserForm -> Handler (Maybe (Entity User))
addUserR classId (AddUserForm sEmail) = do
  mbU <- getUserByEmail sEmail
  case mbU of
    Just _  -> return mbU
    Nothing -> do _ <- Auth.createNewCustomAccount
                         (Auth.CustomNewAccountData sEmail ("?" ++ sEmail ++ "?") sEmail sEmail)
                         (const (ClassInsR classId))
                  getUserByEmail sEmail

--------------------------------------------------------------------------------
-- | Generic Class Extension ---------------------------------------------------
--------------------------------------------------------------------------------
extendClassFormR :: Html
                 -> Form a
                 -> (a -> Handler ())
                 -> ClassId
                 -> Route App
                 -> Handler Html
extendClassFormR msg form =
  extendClassHandleR msg (runFormHandler form)

runFormHandler :: Form a -> Handler (Maybe a)
runFormHandler form = do
  ((result,_),_) <- runFormPost form
  case result of
    FormSuccess o -> return (Just o)
    _             -> return Nothing

extendClassHandleR
  :: Html -> Handler (Maybe a) -> (a -> Handler ()) -> ClassId -> Route App -> Handler Html
extendClassHandleR msg h extR classId r = do
  instrId      <- classInstructor <$> getClassById classId
  (uid    , _) <- requireAuthPair
  if uid /= instrId
    then setMessage ("Sorry, can only " ++ msg ++ "for your own class!")
    else do
      result <- h
      case result of
        Just o -> extR o
        _      -> setMessage "Something went wrong!"
  redirect r


--------------------------------------------------------------------------------
-- | Creating New Classes ------------------------------------------------------
--------------------------------------------------------------------------------
data ClassForm = ClassForm
    { cName :: Text
    , cTerm :: Text
    }
    deriving (Show)

postNewClassR :: Handler Html
postNewClassR = do
  (uid    , _)     <- requireAuthPair
  ((result, _), _) <- runFormPost classForm
  case result of
    FormSuccess (ClassForm cName cTerm) -> do
      _ <- runDB $ insert $ Class cName cTerm uid
      setMessage $ "Added new class! " ++ TB.text cName ++ " in term " ++ TB.text cTerm
      redirect ProfileR
    _ -> do
      setMessage "Yikes! Something went wrong"
      redirect NewClassR

getNewClassR :: Handler Html
getNewClassR = do
  (formWidget, formEnctype) <- generateFormPost classForm
  defaultLayout $
    $(widgetFile "newclass")

classForm :: Form ClassForm
classForm = renderForm $ ClassForm
    <$> areq textField "Name" Nothing
    <*> areq textField "Term" Nothing
    <*  submitButton "Submit"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

classEditWidget classId clsEnc clsWidget
  = $(widgetFile "classEdit")

classInstructors classId instructor teachers insEnc insWidget
  = $(widgetFile "classInstructors")

classAssignments classId asgns asgnEnc asgnWidget
  = $(widgetFile "classAssignments")

classStudents classId students stdEnc stdWidget stdCsvEnc stdCsvWidget
  = $(widgetFile "classStudents")
