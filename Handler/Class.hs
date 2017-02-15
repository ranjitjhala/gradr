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
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Yesod.Form.Jquery (jqueryAutocompleteField)

--------------------------------------------------------------------------------
-- | Viewing Existing Classes --------------------------------------------------
--------------------------------------------------------------------------------
getClassInsR :: ClassId -> Handler Html
getClassInsR classId = do
  klass    <- getClassById classId
  instr    <- getUserById (classInstructor klass)
  asgns    <- getAssignmentsByClass classId
  students <- getStudentsByClass    classId
  (asgnWidget, asgnEnc) <- generateFormPost newAssignForm
  (stdWidget,  stdEnc)  <- generateFormPost newStudentForm
  defaultLayout $
    $(widgetFile "viewclass")

getAssignmentR :: ClassId -> AssignmentId -> Handler Html
getAssignmentR classId assignId = do
  klass               <- getClassById classId
  instr               <- getUserById  (classInstructor klass)
  asgn                <- getAssignmentById assignId
  scores              <- getAssignmentScores classId assignId
  (stdWidget, stdEnc) <- generateFormPost (scoreForm scores)
  defaultLayout $
    $(widgetFile "viewassignment")

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
-- | Creating New Assignments --------------------------------------------------
--------------------------------------------------------------------------------
postNewAssignR :: ClassId -> Handler Html
postNewAssignR classId =
  extendClassFormR
    "create assignment"
    newAssignForm
    (addAssignR classId)
    classId
    (ClassInsR classId)

addAssignR :: ClassId -> NewAssignForm -> Handler ()
addAssignR classId (NewAssignForm aName aPts) = do
  _ <- runDB $ insert $ Assignment aName aPts classId
  setMessage $ "Added new assignment: " ++ TB.text aName

{-
postNewAssignR classId = do
  instrId          <- classInstructor <$> getClassById classId
  (uid    , _)     <- requireAuthPair
  if uid /= instrId
    then do setMessage "Sorry, can only create assignments for your own class!"
            redirect (ClassR classId)
    else do
      ((result, _), _) <- runFormPost newAssignForm
      case result of
        FormSuccess (NewAssignForm aName aPts) -> do
          _ <- runDB $ insert $ Assignment aName aPts classId
          setMessage $ "Added new assignment: " ++ TB.text aName
          redirect (ClassR classId)
        _ -> do
          setMessage "Yikes! Something went wrong"
          redirect NewClassR
-}


scoreForm :: [(Entity User, Int)] -> Form [(Text, Int)]
scoreForm scores = renderBootstrap3 BootstrapBasicForm
                     $ sequenceA $ map userForm scores
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

{-
scoreForm = renderBootstrap3 BootstrapBasicForm $ inputList "People" massTable
  (\x -> (,)
    <$> pure (fromMaybe "???" (fst <$> x))
    -- areq textField "Email" (fmap fst x)
    <*> areq intField  "Score"   (fmap snd x))
  (Just dummyScores)


newStudentForm :: Form NewStudentForm
newStudentForm = renderBootstrap3 BootstrapBasicForm $ NewStudentForm
  <$> areq textField "Name"  Nothing
  <*> areq textField "Email" Nothing
-}

data NewAssignForm = NewAssignForm
  { asgnName   :: Text
  , asgnPoints :: Int
  }
  deriving (Show)

newAssignForm :: Form NewAssignForm
newAssignForm = renderBootstrap3 BootstrapBasicForm $ NewAssignForm
  <$> areq textField "Name"   Nothing -- (Just "e.g. HW 1")
  <*> areq intField  "Points" Nothing -- (Just 10)

--------------------------------------------------------------------------------
-- | Enrolling New Students ----------------------------------------------------
--------------------------------------------------------------------------------
data NewStudentForm = NewStudentForm
    { studentName  :: Text
    , studentEmail :: Text
    }
    deriving (Show)

newStudentForm :: Form NewStudentForm
newStudentForm = renderBootstrap3 BootstrapBasicForm $ NewStudentForm
  <$> areq textField "Name"  Nothing
  <*> areq textField "Email" Nothing

postNewStudentR :: ClassId -> Handler Html
postNewStudentR classId =
  extendClassFormR
    "enroll student"
    newStudentForm
    (addStudentR classId)
    classId
    (ClassInsR classId)

addStudentR :: ClassId -> NewStudentForm -> Handler ()
addStudentR classId (NewStudentForm sName sEmail) = do
  _     <- Auth.createNewCustomAccount
            (Auth.CustomNewAccountData sEmail sName sEmail sEmail)
            (const (ClassInsR classId))
  mbStd <- getUserByEmail sEmail
  case mbStd of
    Nothing -> setMessage ("Error enrolling student: " ++ TB.text sEmail)
    Just e  -> do setMessage ("Enrolled student: "     ++ TB.text sEmail)
                  void $ runDB (insert (Student (entityKey e) classId))

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
data NewClassForm = NewClassForm
    { name       :: Text
    , term       :: Text
    }
    deriving (Show)

postNewClassR :: Handler Html
postNewClassR = do
  (uid    , _)     <- requireAuthPair
  ((result, _), _) <- runFormPost newClassForm
  case result of
    FormSuccess (NewClassForm cName cTerm) -> do
      _ <- runDB $ insert $ Class cName cTerm uid
      setMessage $ "Added new class! " ++ TB.text cName ++ " in term " ++ TB.text cTerm
      redirect ProfileR
    _ -> do
      setMessage "Yikes! Something went wrong"
      redirect NewClassR

getNewClassR :: Handler Html
getNewClassR = do
  (formWidget, formEnctype) <- generateFormPost newClassForm
  defaultLayout $
    $(widgetFile "newclass")

newClassForm :: Form NewClassForm
newClassForm = renderBootstrap3 BootstrapBasicForm $ NewClassForm
    <$> areq textField "Name" Nothing -- (Just "CSE 130: Programming Languages")
    <*> areq textField "Term" Nothing -- (Just "Fall 2017")
