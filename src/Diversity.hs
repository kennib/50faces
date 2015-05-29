module Diversity where

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

