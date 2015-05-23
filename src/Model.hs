{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where
import           Data.Text                   (Text)
import           Data.Typeable               (Typeable)
import           Data.Time.Clock             (UTCTime)
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings { mpsGeneric = False }, mkMigrate "migrateAll"] [persistLowerCase|
User
    email Text
    password Text Maybe -- Password may not be set yet
    verkey Text Maybe -- Used for resetting passwords
    verified Bool
    UniqueUser email
    deriving Typeable
Face
    user UserId
    image Text
    time UTCTime
    current Bool
    deriving Typeable
Friend
    user UserId
    friend UserId
    time UTCTime
    current Bool
    deriving Typeable
|]