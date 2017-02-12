module Handler.NewClass where

import Import
import Text.Blaze as TB
-- import DB
-- import qualified Util as Util
-- import qualified Data.ByteString as S
-- import qualified Data.ByteString.Lazy as L
-- import Data.Conduit.Binary

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Yesod.Form.Jquery (jqueryAutocompleteField)

data NewClassForm = NewClassForm
    { name       :: Text
    , term       :: Text
    }
    deriving (Show)

newClassForm ::  Form NewClassForm
newClassForm = renderBootstrap3 BootstrapBasicForm $ NewClassForm
    <$> areq textField "Name" (Just "CSE 130: Programming Languages")
    <*> areq textField "Term" (Just "Fall 2017")

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
  (_uid, user)              <- requireAuthPair
  (formWidget, formEnctype) <- generateFormPost newClassForm
  defaultLayout $
    $(widgetFile "newclass")
