{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE GADTs,OverloadedStrings,FlexibleContexts, FlexibleInstances #-}
import           Data.Monoid                 ((<>))
import qualified Data.Map as Map
import           Control.Applicative
import           Control.Monad.Logger        (runNoLoggingT)
import           Data.Default                (def)
import           Data.Text                   (Text, pack)
import           Data.Time
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Database.Persist
import           Database.Persist.Sqlite
import           Yesod
import           Yesod.Static
import           Yesod.Default.Util
import           Yesod.Auth
import           Yesod.Auth.BrowserId
import           Yesod.Auth.GoogleEmail
import           System.Environment

import Model
import Examples

staticFiles "src/static"

data AppBackend = AppBackend
    { httpManager :: Manager
    , sqlBackend  :: SqlBackend
    , root        :: Text
    , staticBackend   :: Static
    }

getStatic (App backend) = staticBackend backend

data App = App AppBackend

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
/faces FacesR GET
/message/#Integer MessageR GET POST
/addface FaceR GET POST
/profile ProfileR GET POST
/static StaticR Static getStatic
|]

instance Yesod App where
    approot = ApprootMaster $ \(App backend) -> root backend

    isAuthorized FacesR _ = isLoggedIn
    isAuthorized (MessageR id) _ = isLoggedIn

    isAuthorized FaceR _ = isLoggedIn
    isAuthorized ProfileR _ = isLoggedIn

    isAuthorized _ _ = return Authorized

    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent contents
        mmsg <- getMessage

        maid <- maybeAuthId
        mface <- case maid of
            Just user -> do
                mface <- runDB $ selectFirst [FaceUser ==. user, FaceCurrent ==. True] [Desc FaceTime]
                return $ fmap entityVal mface
            Nothing -> return Nothing
        mprofile <- case maid of
            Just user -> do
                mprofile <- runDB $ selectFirst [ProfileUser ==. user, ProfileCurrent ==. True] [Desc ProfileTime]
                return $ fmap entityVal mprofile
            Nothing -> return Nothing

        withUrlRenderer [hamlet|
            $doctype 5

            <html>
                <head>
                    <title>#{title}
                    <link rel=stylesheet type=text/css href=@{StaticR style_page_css}>
                    ^{headTags}
                <body>
                    <header .header>
                        <span .logo>
                            <a href=@{HomeR}>50 Faces

                        $maybe id <- maid
                            <span .page-profile>
                                $maybe face <- mface
                                    <a href=@{FaceR}>
                                        <img src=#{faceImage face}>
                                $maybe profile <- mprofile
                                    <a .name href=@{ProfileR}>#{profileName profile}
                                $nothing
                                    <a .name href=@{ProfileR}>New User
                                <a .logout href=@{AuthR LogoutR}>Logout
                        $nothing
                            <span .page-profile>
                                <a .login href=@{AuthR LoginR}>Sign up or login

                    $maybe msg <- mmsg
                        <div .page-message>#{msg}

                    ^{bodyTags}
        |]

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
    case maid of
        Just id -> getFacesR
        Nothing -> defaultLayout
            [whamlet|
                <p>
                    <a href=@{AuthR LoginR}>Sign up or login
            |]

getFacesR :: Handler Html
getFacesR = do
    let profileId = toInteger . fromSqlKey . profileUser
    maid <- maybeAuthId

    friends <- fmap (map (friendFriend . entityVal)) $ case maid of
        Just user -> runDB $ selectList [FriendUser ==. user, FriendCurrent ==. True] [Desc FriendTime]
        Nothing -> return []

    profiles <- fmap (map entityVal) $ do
        runDB $ selectList [ProfileUser <-. friends, ProfileCurrent ==. True] [Desc ProfileTime]

    faces <- fmap (map entityVal) $ do
        runDB $ selectList [FaceUser <-. friends, FaceCurrent ==. True] [Desc FaceTime]

    let friendlyFaces = zip profiles faces

    defaultLayout $ do
        setTitle "Friendly faces"
        addStylesheet $ StaticR style_faces_css
        [whamlet|
            $if null faces
                <p>No friendly faces
            $else
                <ul .faces>
                    $forall (profile, face) <- friendlyFaces
                        <li .face>
                            <a href=@{MessageR (profileId profile)}>
                                <img src=#{faceImage face}>
                                <p .name>#{profileName profile}
        |]

getMessageR :: Integer -> Handler Html
getMessageR friendId = do
    let friendKey = toSqlKey (fromInteger friendId)
    maid <- maybeAuthId

    ((result, widget), enctype) <- case maid of
        Just id -> runFormPost $ messageForm id friendKey
        Nothing -> error "No user"

    mmessage <- case (result, maid) of
        (FormSuccess message, Just user) -> do
            mmessage <- runDB $ do
                insert (message :: Message)
            return $ Just mmessage
        _ -> return Nothing

    mface <- do
        mface <- runDB $ selectFirst [FaceUser ==. friendKey, FaceCurrent ==. True] [Desc FaceTime]
        return $ fmap entityVal $ mface

    mprofile <- do
        mprofile <- runDB $ selectFirst [ProfileUser ==. friendKey, ProfileCurrent ==. True] [Desc ProfileTime]
        return $ fmap entityVal $ mprofile

    messages <- do
        messages <- runDB $ selectList [MessageTo ==. friendKey] [Desc MessageTime, LimitTo 50]
        return $ reverse $ fmap entityVal messages

    profiles <- fmap (map entityVal) $ do
        runDB $ selectList [ProfileUser <-. map messageFrom messages, ProfileCurrent ==. True] [Desc ProfileTime]

    faces <- fmap (map entityVal) $ do
        runDB $ selectList [FaceUser <-. map messageFrom messages, FaceCurrent ==. True] [Desc FaceTime]

    let userProfiles = Map.fromList $ zip (map profileUser profiles) profiles
    let messageProfile = (flip Map.lookup) userProfiles . messageFrom

    let userFaces = Map.fromList $ zip (map faceUser faces) faces
    let messageFace = (flip Map.lookup) userFaces . messageFrom

    defaultLayout $ do
        setTitle $ case mprofile of
            Just profile-> toHtml $ "Message " <> profileName profile
            Nothing -> "Message no one"

        addStylesheet $ StaticR style_messages_css

        [whamlet|
            $maybe profile <- mprofile
                <aside .profile>
                    $maybe face <- mface
                        <img src=#{faceImage face}>
                    <p>#{profileName profile}

                <ul .messages>
                    $forall message <- messages
                        <li .message>
                            <div .message-profile>
                                $maybe face <- messageFace message
                                    <img src=#{faceImage face}>
                                $maybe profile <- messageProfile message
                                    <p>#{profileName profile}
                            #{messageMessage message}

                <form .message-post method=post action=@{MessageR friendId} enctype=#{enctype}>
                    ^{widget}
                    <input type=submit value="Post message">
            $nothing
                <p>No one exists with ID #{friendId}
        |]

postMessageR :: Integer -> Handler Html
postMessageR = getMessageR

messageForm from to = renderDivs $ Message
    <$> pure from
    <*> pure to
    <*> areq textField "" Nothing
    <*> lift (liftIO getCurrentTime)

getFaceR :: Handler Html
getFaceR = do
    maid <- maybeAuthId
    ((result, widget), enctype) <- case maid of
        Just id -> runFormPost $ faceForm id
        Nothing -> error "No user"

    mface <- case (result, maid) of
        (FormSuccess face, Just user) -> do
            runDB $ do
                notCurrent FaceCurrent [FaceUser ==. user]
                insert face
            return $ Just face
        (_, Just user) -> do
            mface <- runDB $ selectFirst [FaceUser ==. user, FaceCurrent ==. True] [Desc FaceTime]
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
    <*> pure True

getProfileR :: Handler Html
getProfileR = do
    maid <- maybeAuthId
    mprofile <- case maid of
        Just user -> do
            mprofile <- runDB $ selectFirst [ProfileUser ==. user, ProfileCurrent ==. True] [Desc ProfileTime]
            return $ fmap entityVal mprofile
        Nothing -> return Nothing

    ((result, widget), enctype) <- case maid of
        Just user -> runFormPost $ profileForm $ case mprofile of
            Just profile -> Left profile
            Nothing -> Right user
        Nothing -> error "No user"

    mprofile <- case (result, maid) of
        (FormSuccess profile, Just user) -> do
            runDB $ do
                notCurrent ProfileCurrent [ProfileUser ==. user]
                insert profile
            return $ Just profile
        (_, Just user) -> do
            return mprofile
        _ -> return Nothing

    defaultLayout $ do
        setTitle "Update your profile"
        [whamlet|
            $maybe profile <- mprofile
                <p>#{profileName profile}'s Profile
            $nothing
                <p>You don't have a profile yet!
            <form method=post action=@{ProfileR} enctype=#{enctype}>
                ^{widget}
                <input type=submit value="Update your profile">
        |]

postProfileR :: Handler Html
postProfileR = getProfileR

profileForm defaults = renderDivs $ case defaults of
    Left profile -> Profile
        <$> pure (profileUser profile)
        <*> areq textField "Name" (Just (profileName profile))
        <*> aopt intField "Age" (Just (profileAge profile))
        <*> aopt textField "Gender" (Just (profileGender profile))
        <*> aopt textField "Location" (Just (profileLocation profile))
        <*> lift (liftIO getCurrentTime)
        <*> pure True
    Right user -> Profile
        <$> pure user
        <*> areq textField "Name" Nothing
        <*> aopt intField "Age" Nothing
        <*> aopt textField "Gender" Nothing
        <*> aopt textField "Location" Nothing
        <*> lift (liftIO getCurrentTime)
        <*> pure True

main :: IO ()
main = runNoLoggingT $ withSqliteConn "50faces" $ \conn -> liftIO $
    do
        man <- newManager
        root <- fmap pack $ getEnv "APPROOT"
        static@(Static settings) <- staticDevel "src/static"
        let backend = AppBackend man conn root static
        runSqlConn (runMigration migrateAll) conn
        runSqlConn loadExamples conn
        warpEnv $ App backend