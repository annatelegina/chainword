module Handler where

import Graphics.Gloss
import Prelude
import Graphics.Gloss.Interface.Pure.Game
import System.IO

import Type
import Config
import Utils

--playing game
handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (World x y num colour lett numcol 0)| (checkEnd x) == False = World x y num colour lett numcol 1
                                                                                          | otherwise = World x y num colour lett numcol 2
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (World x y num colour lett numcol 1) = World x y num colour lett numcol 0
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (World x y num colour lett numcol 2)| (allRight x y) == False = World x y num colour lett numcol 0
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (World x y num colour lett numcol fin) 
                                                                                | n < 8 = World x y newNum colour lett numcol fin
                                                                                | otherwise = World x y num colour lett numcol fin
                                                                                where 
                                                                                    n = snd point
                                                                                    newNum = getMarker (fst point, snd point  + 1)
                                                                                    point = getCell num  
    
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) (World x y num colour lett numcol fin) 
                                                                                | n > 1 = World x y newNum colour lett numcol fin
                                                                                | otherwise = World x y num colour lett numcol fin
                                                                                where 
                                                                                    n = snd point
                                                                                    newNum = getMarker (fst point, snd point  - 1)
                                                                                    point = getCell num 
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (World x y num colour lett numcol fin) 
                                                                                | n > 1 = World x y newNum colour lett numcol fin
                                                                                | otherwise = World x y num colour lett numcol fin
                                                                                where 
                                                                                    n = fst point
                                                                                    newNum = getMarker (fst point - 1, snd point)
                                                                                    point = getCell num 
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (World x y num colour lett numcol fin) 
                                                                                | n < 8 = World x y newNum colour lett numcol fin
                                                                                | otherwise = World x y num colour lett numcol fin
                                                                                where 
                                                                                    n = fst point
                                                                                    newNum = getMarker (fst point + 1, snd point)
                                                                                    point = getCell num
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) (World x y num colour lett numcol fin) = World newAns y num colour lett numcol fin
                                                                       where
                                                                           newAns = smthLetter Delete num 'a' x
handleEvent (EventKey (Char sym) Down _ _) (World x y num colour lett numcol fin) = World newAns y num colour lett numcol fin
                                                                       where
                                                                           newAns = smthLetter Insert num sym x
handleEvent _ w = w
