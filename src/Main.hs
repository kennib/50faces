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
import           Data.Monoid                 ((<>))
import qualified Data.Map as Map

import           Database.Persist
import           Database.Persist.Sqlite

import           Network.HTTP.Client.Conduit (Manager, newManager)

import           Yesod
import           Yesod.Static
import           Yesod.Default.Util
import           Yesod.Auth
import           Yesod.Auth.BrowserId
import           Yesod.Auth.GoogleEmail
import           Yesod.Auth.Facebook.ServerSide
import           Yesod.Facebook
import           Text.Hamlet

import           Facebook                    (Credentials(..))

import           System.Environment

import           Model
import           Examples

staticFiles "src/static"

data AppBackend = AppBackend
    { httpManager :: Manager
    , sqlBackend  :: SqlBackend
    , root        :: Text
    , staticBackend   :: Static
    , fbCreds     :: Credentials
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

profileId = toInteger . fromSqlKey . profileUser

getFace :: Key User -> Handler (Maybe Face)
getFace user = do
    mface <- runDB $ selectFirst [FaceUser ==. user, FaceCurrent ==. True] [Desc FaceTime]
    return $ fmap entityVal mface

getProfile :: Key User -> Handler (Maybe Profile)
getProfile user = do
    mprofile <- runDB $ selectFirst [ProfileUser ==. user, ProfileCurrent ==. True] [Desc ProfileTime]
    return $ fmap entityVal mprofile

instance Yesod App where
    approot = ApprootMaster $ \(App backend) -> root backend

    isAuthorized FacesR _ = isLoggedIn

    isAuthorized FaceR _ = isLoggedIn
    isAuthorized ProfileR _ = isLoggedIn

    isAuthorized _ _ = return Authorized

    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent contents
        mmsg <- getMessage

        maid <- maybeAuthId
        mface <- case maid of
            Just user -> getFace user
            Nothing -> return Nothing
        mprofile <- case maid of
            Just user -> getProfile user
            Nothing -> return Nothing

        withUrlRenderer $(hamletFile "src/templates/page.hamlet")

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        App backend <- getYesod
        runSqlConn f $ sqlBackend backend

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = FacesR
    logoutDest _ = HomeR

    authPlugins _ =
        [ authBrowserId def
        , authGoogleEmail
        , authFacebook ["user_about_me", "email"]
        ]

    -- Need to find the UserId for the given email address.
    getAuthId creds = runDB $ do
        x <- insertBy $ User (credsIdent creds) Nothing Nothing False
        return $ Just $
            case x of
                Left (Entity userid _) -> userid -- newly added user
                Right userid -> userid -- existing user

    authHttpManager (App backend) = httpManager backend

instance YesodFacebook App where
    fbCredentials (App backend) = fbCreds backend
    fbHttpManager (App backend) = httpManager backend
    fbUseBetaTier _ = False

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
    
    friends <- do
        friends <- runDB $ selectList [FriendCurrent ==. True] [Desc FriendTime]
        return $ map (friendFriend . entityVal) friends

    profiles <- fmap (map entityVal) $ do
        runDB $ selectList [ProfileUser <-. friends, ProfileCurrent ==. True] [Desc ProfileTime]

    faces <- fmap (map entityVal) $ do
        runDB $ selectList [FaceUser <-. friends, FaceCurrent ==. True] [Desc FaceTime]

    let friendlyFaces = zip profiles faces

    defaultLayout $ do
        addStylesheet $ StaticR style_faces_css
        addStylesheet $ StaticR style_home_css
        $(widgetFileNoReload def "home")

getFacesR :: Handler Html
getFacesR = do
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
        $(widgetFileNoReload def "faces")

getMessageR :: Integer -> Handler Html
getMessageR friendId = do
    let friendKey = toSqlKey (fromInteger friendId)
    maid <- maybeAuthId

    mform <- case maid of
        Just id -> fmap Just $ runFormPost $ messageForm id friendKey
        Nothing -> return Nothing

    mmessage <- case (mform, maid) of
        (Just ((FormSuccess message, _), _), Just user) -> do
            mmessage <- runDB $ do
                insert (message :: Message)
            return $ Just mmessage
        _ -> return Nothing

    mface <- getFace friendKey
    mprofile <- getProfile friendKey

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
        $(widgetFileNoReload def "messages")

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
        (_, Just user) -> getFace user
        _ -> return Nothing

    defaultLayout $ do
        setTitle "Update your face"
        $(widgetFileNoReload def "face")

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
        Just user -> getProfile user
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
        $(widgetFileNoReload def "profile")

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
        let fbCreds = Credentials "50people" "105423129793938" "19b642874ef16561acbf7dfd748dc16f"

        let backend = AppBackend man conn root static fbCreds

        runSqlConn (runMigration migrateAll) conn
        runSqlConn loadExamples conn

        warpEnv $ App backend