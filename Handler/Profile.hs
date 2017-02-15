module Handler.Profile where

import           Import
import           DB

getProfileR :: Handler Html
getProfileR = do
    (uid, user) <- requireAuthPair
    insClasses  <- getClassesInsByUser uid
    stdClasses  <- getClassesStdByUser uid
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")
