module Diversity where

import           Control.Monad.IO.Class (liftIO)
import           GHC.Exts               (sortWith)
import           Data.Time              (getCurrentTime)

import           Database.Persist
import           Database.Persist.Sqlite

import           Model

disimilarity :: Profile -> Profile -> Float
disimilarity p p' = 1.0 - avg
    [ similar profileAge
    , similar profileGender
    , similar profileLocation
    ]
    where similar f = similarity (f p) (f p')
          avg l = sum l / (fromIntegral $ length l)

similarity :: Eq a => a -> a -> Float
similarity x y = if x == y then 1 else 0

diverseFriends :: Profile -> [Profile] -> [UserId]
diverseFriends p fs = map (profileUser . fst) $ sortWith ((1.0 -) . snd) $ zip fs $ map (disimilarity p) fs

setDiverseFriends user = do
    now <- liftIO getCurrentTime

    mprofile <- do
        mprofile <- selectFirst [ProfileUser ==. user, ProfileCurrent ==. True] []
        return $ fmap entityVal mprofile

    people <- fmap (map entityVal) $ selectList [ProfileCurrent ==. True] []

    case mprofile of
        Just profile -> do
            let friends = take 50 $ diverseFriends profile people

            notCurrent FriendCurrent $ [FilterOr [FriendUser ==. user]]
            mapM (\friend -> insert (Friend user friend now True)) friends

            return ()

        Nothing -> return ()
