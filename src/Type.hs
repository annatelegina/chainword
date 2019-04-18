module Type where

import Graphics.Gloss

data Letter = Letter
    { symb :: Char
    , border :: Maybe Int
    }
data Action = Insert | Delete 

data World = World
    { field :: [Maybe Char]
    , answers :: [Letter]
    , list :: [String]
    , marker :: Int
    , colour_marker :: Color
    , colour_letter :: Color
    , colour_number :: Color
    , time :: Float
    , state :: Int --0 - playing, 1 - empty cells, 2 - end
    }
    
fromJust :: Maybe a -> a
fromJust (Just x) = x
