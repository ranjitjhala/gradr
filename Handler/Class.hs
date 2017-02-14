module Handler.Class where

import           Import
import qualified Text.Blaze as TB
import           DB

-- import qualified Util as Util
-- import qualified Data.ByteString as S
-- import qualified Data.ByteString.Lazy as L
-- import Data.Conduit.Binary

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Yesod.Form.Jquery (jqueryAutocompleteField)

--------------------------------------------------------------------------------
-- | Viewing Existing Classes --------------------------------------------------
--------------------------------------------------------------------------------
getClassR :: ClassId -> Handler Html
getClassR classId = do
  klass <- getClassById classId
  instr <- getUserById (classInstructor klass)
  asgns <- getAssignmentsByClass classId
  (asgnWidget, asgnEnc) <- generateFormPost newAssignForm
  defaultLayout $
    $(widgetFile "viewclass")

--------------------------------------------------------------------------------
-- | Creating New Assignments --------------------------------------------------
--------------------------------------------------------------------------------
postNewAssignR :: ClassId -> Handler Html
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
