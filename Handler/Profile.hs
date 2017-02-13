module Handler.Profile where

import Import
import DB


getProfileR :: Handler Html
getProfileR = do
    (_, user)     <- requireAuthPair
    classesByUser <- getClassesByUser
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")
