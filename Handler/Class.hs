module Handler.Class where

import           Import
import qualified Text.Blaze   as TB
import           DB
import qualified Auth.Account as Auth

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
  instr                 <- getUserById (classInstructor klass)
  asgns                 <- getAssignmentsByClass classId
  students              <- getStudentsByClass    classId
  teachers              <- fmap entityVal <$> getInstructorsByClass classId
  let instructors        = instr : teachers
  (asgnWidget, asgnEnc) <- generateFormPost assignForm
  (stdWidget,  stdEnc)  <- generateFormPost addUserForm
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
  defaultLayout $
    $(widgetFile "viewassignment")

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

--------------------------------------------------------------------------------
-- | Update Scores for Assignment ----------------------------------------------
--------------------------------------------------------------------------------
postScoreR :: ClassId -> AssignmentId -> Handler Html
postScoreR classId assignId = do
  oldScores <- getAssignmentScores classId assignId
  extendClassFormR
    "update scores"
    (scoreForm oldScores)
    (updAssignmentScores assignId . updScores oldScores)
    classId
    (AssignmentR classId assignId)

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
                  void $ runDB (insert (Teacher (entityKey e) classId))

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

postNewStudentR :: ClassId -> Handler Html
postNewStudentR classId =
  extendClassFormR
    "enroll student"
    addUserForm
    (addStudentR classId)
    classId
    (ClassInsR classId)

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
extendClassFormR
  :: Html -> Form a -> (a -> Handler ()) -> ClassId -> Route App -> Handler Html
extendClassFormR msg form extR classId r = do
  instrId      <- classInstructor <$> getClassById classId
  (uid    , _) <- requireAuthPair
  if uid /= instrId
    then setMessage ("Sorry, can only " ++ msg ++ "for your own class!")
    else do
      ((result, _), _) <- runFormPost form
      case result of
        FormSuccess o -> extR o
        _             -> setMessage "Something went wrong!"
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
