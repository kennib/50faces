{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE GADTs,OverloadedStrings,FlexibleContexts, FlexibleInstances #-}
import           Control.Applicative
import           Control.Monad.Logger        (runNoLoggingT)
import           Data.Default                (def)
import           Data.Text                   (Text, pack)
import           Data.Time
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Database.Persist
import           Database.Persist.Sqlite
import           Yesod
import           Yesod.Default.Util
import           Yesod.Auth
import           Yesod.Auth.BrowserId
import           Yesod.Auth.GoogleEmail
import           System.Environment

import Debug.Trace

import Model

data AppBackend = AppBackend
    { httpManager :: Manager
    , sqlBackend  :: SqlBackend
    , root        :: Text
    }
data App = App AppBackend

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
/addface FaceR GET POST
|]

instance Yesod App where
    approot = ApprootMaster $ \(App backend) -> root backend

    isAuthorized FaceR True = isLoggedIn
    isAuthorized FaceR _ = isLoggedIn

    isAuthorized _ _ = return Authorized


instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        App backend <- getYesod
        runSqlConn f $ sqlBackend backend

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ =
        [ authBrowserId def
        , authGoogleEmail
        ]

    -- Need to find the UserId for the given email address.
    getAuthId creds = runDB $ do
        x <- insertBy $ User (credsIdent creds) Nothing Nothing False
        return $ Just $
            case x of
                Left (Entity userid _) -> userid -- newly added user
                Right userid -> userid -- existing user

    authHttpManager (App backend) = httpManager backend

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

isLoggedIn = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{FaceR}>Update your face
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

getFaceR :: Handler Html
getFaceR = do
    maid <- maybeAuthId
    ((result, widget), enctype) <- case maid of
        Just id -> runFormPost $ faceForm id
        Nothing -> error "No user"

    mface <- case (result, maid) of
        (FormSuccess face, Just user) -> do
            runDB $ insert face
            return $ Just face
        (_, Just user) -> do
            mface <- runDB $ selectFirst [] [Desc FaceTime]
            return $ fmap entityVal mface
        _ -> return Nothing

    defaultLayout $ do
        setTitle "Update your face"
        [whamlet|
            $maybe face <- mface
                <img src=#{faceImage face}>
            $nothing
                <p>
            <form method=post action=@{FaceR} enctype=#{enctype}>
                ^{widget}
                <input type=submit value="Update your face">
        |]

postFaceR :: Handler Html
postFaceR = getFaceR

faceForm userId = renderDivs $ Face
    <$> pure userId
    <*> areq textField "Face URL" Nothing
    <*> lift (liftIO getCurrentTime)

main :: IO ()
main = runNoLoggingT $ withSqliteConn "faces" $ \conn -> liftIO $
    do
        man <- newManager
        root <- fmap pack $ getEnv "APPROOT"
        let backend = AppBackend man conn root
        runSqlConn (runMigration migrateAll) conn
        warpEnv $ App backend