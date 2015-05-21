{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Monad.Logger        (runNoLoggingT)
import           Data.Default                (def)
import           Data.Text                   (Text)
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Database.Persist.Sqlite
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.BrowserId
import           Yesod.Auth.GoogleEmail

import Model

data AppBackend = AppBackend
    { httpManager :: Manager
    , sqlBackend  :: SqlBackend 
    }
data App = App AppBackend

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    approot = ApprootStatic "https://df60e5f6-f4c7-422c-ba32-e1ec1f5f26c2-app.fpcomplete.com"

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

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

main :: IO ()
main = runNoLoggingT $ withSqliteConn "faces" $ \conn -> liftIO $
    do
        man <- newManager
        let backend = AppBackend man conn
        runSqlConn (runMigration migrateAll) conn
        warpEnv $ App backend