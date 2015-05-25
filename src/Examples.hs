{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
module Examples where

import Data.Text (Text)
import Data.Monoid ((<>))

import Control.Monad (mapM, zipWithM)
import Control.Monad.IO.Class (liftIO)

import Database.Persist
import Database.Persist.Sqlite

import Data.Time

import Model

loadExamples :: SqlPersistT IO ()
loadExamples = do
    now <- liftIO getCurrentTime

    let users = ["Jenny", "George", "Ashley", "Danni", "Peter", "Isaac"]
    let faces = ["http://i.imgur.com/WWcyFAUb.jpg", "http://i.imgur.com/rlsVPERb.jpg", "http://i.imgur.com/4ydgFonb.jpg", "http://i.imgur.com/lWBwwT6b.jpg", "http://i.imgur.com/OtqLwRqb.jpg", "http://i.imgur.com/yhozP6yb.jpg"]

    userIds <- zipWithM loadExample users faces
    kenni <- loadExample "Kenni" "http://i.imgur.com/PUZeZ6Ab.jpg"

    notCurrent FriendCurrent $ [FilterOr [FriendUser ==. kenni]]
    mapM (\user -> insert (Friend kenni user now True)) userIds

    return ()

loadExample :: Text -> Text -> SqlPersistT IO (Key User)
loadExample name face = do
    now <- liftIO getCurrentTime

    user <- fmap entityKey $ upsert (User (name <> "@example.com") Nothing Nothing True) []

    notCurrent ProfileCurrent [ProfileUser ==. user]
    insert (Profile user name Nothing Nothing Nothing now True)

    notCurrent FaceCurrent [FaceUser ==. user]
    insert (Face user face now True)

    return user