module Auth.Account where

import Import.NoFoundation
import Yesod.Auth.Account
import qualified Yesod.Auth.Message as Msg
import qualified Data.Text as T

getNewAccountR :: YesodAuthAccount db site 
                  => HandlerT Auth (HandlerT site IO) Html
getNewAccountR = do
    tm <- getRouteToParent
    lift $ defaultLayout $ do
        setTitleI Msg.RegisterLong
        customNewAccountWidget tm 

postNewAccountR :: YesodAuthAccount db site 
                   => HandlerT Auth (HandlerT site IO) Html
postNewAccountR = do
    tm <- getRouteToParent
    mr <- lift getMessageRender
    ((result, _), _) <- lift $ runFormPost $ renderDivs customNewAccountForm 
    mdata <- case result of
                FormMissing -> invalidArgs ["Form is missing"]
                FormFailure msg -> return $ Left msg
                FormSuccess d -> return $ if password1 d == password2 d
                                    then Right d
                                    else Left [mr Msg.PassMismatch]
    case mdata of
        Left errs -> do
            setMessage $ toHtml $ T.concat errs
            redirect newAccountR

        Right d -> do route <- lift $ createNewCustomAccount d tm
                      redirect route

-- | The data collected in the new account form.
data CustomNewAccountData = CustomNewAccountData {
      newAccountEmail :: Text
    , newAccountName :: Text
    , password1 :: Text
    , password2 :: Text
} deriving Show

-- | Custom form for creating a new account
customNewAccountForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
                        => AForm m CustomNewAccountData
customNewAccountForm = CustomNewAccountData <$> areq textField "Email" Nothing
                                            <*> areq textField "Name" Nothing
                                            <*> areq passwordField "Password" Nothing
                                            <*> areq passwordField "Confirm Password" Nothing
-- | The registration form
customNewAccountWidget :: YesodAuthAccount db master
                          => (Route Auth -> Route master)
                          -> WidgetT master IO ()
customNewAccountWidget tm = do
    ((_,widget), enctype) <- liftHandlerT $ runFormPost $ renderDivs customNewAccountForm
    [whamlet|
<div .newaccountDiv>
    <form method=post enctype=#{enctype} action=@{tm newAccountR}>
        ^{widget}
        <input type=submit value=_{Msg.Register}>
|]

-- | Creates a new custom account
createNewCustomAccount :: YesodAuthAccount db master
                          => CustomNewAccountData
                          -> (Route Auth -> Route master)
                          -> HandlerT master IO (AuthRoute)
createNewCustomAccount (CustomNewAccountData email name pwd _) tm = do
    muser <- runAccountDB $ loadUser email
    case muser of
        Just _ -> do setMessageI $ email ++ " already exists."
                     redirect $ tm newAccountR
        Nothing -> return ()

    key <- newVerifyKey
    hashed <- hashPassword pwd

    mnew <- runAccountDB $ addNewUser name email key hashed
    _ <- case mnew of
        Left err -> do setMessage $ toHtml err
                       redirect $ tm newAccountR
        Right x -> return x

    return $ verifyR email key
