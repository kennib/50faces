{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
module Examples where

import Control.Monad.IO.Class (liftIO)

import Database.Persist
import Database.Persist.Sqlite

import Data.Time

import Model

loadExamples :: SqlPersistT IO ()
loadExamples = do
    now <- liftIO getCurrentTime

    jenny  <- fmap entityKey $ upsert (User "jenny@example.com" Nothing Nothing True) []
    george <- fmap entityKey $ upsert (User "george@example.com" Nothing Nothing True) []
    kenni  <- fmap entityKey $ upsert (User "kenni.bawden@gmail.com" Nothing Nothing True) []

    notCurrent ProfileCurrent $ [FilterOr [ProfileUser ==. jenny, ProfileUser ==. george]]
    insert (Profile jenny "Jenny" Nothing Nothing Nothing now True)
    insert (Profile george "The Big G" Nothing Nothing Nothing now True)

    notCurrent FaceCurrent $ [FilterOr [FaceUser ==. jenny, FaceUser ==. george, FaceUser ==. kenni]]
    insert (Face jenny "http://i.imgur.com/WWcyFAUb.jpg" now True)
    insert (Face george "http://i.imgur.com/rlsVPERb.jpg" now True)
    insert (Face kenni "http://i.imgur.com/PUZeZ6Ab.jpg" now True)

    notCurrent FriendCurrent $ [FilterOr [FriendUser ==. kenni]]
    insert (Friend kenni jenny now True)
    insert (Friend kenni george now True)

    return ()