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
handleEvent _ (World x y a num colour lett numcol t p) | t > mTime = World x y a num colour lett numcol t 2
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (World x y a num colour lett numcol t 0)| (checkEnd x) == False && (t < mTime ) = World x y a num colour lett numcol t 1
                                                                                          | otherwise = World x y a num colour lett numcol t 2
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (World x y a num colour lett numcol t 1) | t > mTime = World x y a num colour lett numcol t 2
                                                                                               | otherwise =  World x y a num colour lett numcol t 0
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (World x y a num colour lett numcol t 2)| (allRight x y) == False && (t < mTime) = World x y a num colour lett numcol t 0
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (World x y a num colour lett numcol t fin) 
                                                                                | n < 8 = World x y a newNum colour lett numcol t fin
                                                                                | otherwise = World x y a num colour lett numcol t fin
                                                                                where 
                                                                                    n = snd point
                                                                                    newNum = getMarker (fst point, snd point  + 1)
                                                                                    point = getCell num  
    
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) (World x y a num colour lett numcol t fin) 
                                                                                | n > 1 = World x y a newNum colour lett numcol t fin
                                                                                | otherwise = World x y a num colour lett numcol t fin
                                                                                where 
                                                                                    n = snd point
                                                                                    newNum = getMarker (fst point, snd point  - 1)
                                                                                    point = getCell num 
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (World x y a num colour lett numcol t fin) 
                                                                                | n > 1 = World x y a newNum colour lett numcol t fin
                                                                                | otherwise = World x y a num colour lett numcol t fin
                                                                                where 
                                                                                    n = fst point
                                                                                    newNum = getMarker (fst point - 1, snd point)
                                                                                    point = getCell num 
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (World x y a num colour lett numcol t fin) 
                                                                                | n < 8 = World x y a newNum colour lett numcol t fin
                                                                                | otherwise = World x y a num colour lett numcol t fin
                                                                                where 
                                                                                    n = fst point
                                                                                    newNum = getMarker (fst point + 1, snd point)
                                                                                    point = getCell num
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) (World x y a num colour lett numcol t fin) = World newAns y a num colour lett numcol t fin
                                                                       where
                                                                           newAns = smthLetter Delete num 'a' x
handleEvent (EventKey (Char sym) Down _ _) (World x y a num colour lett numcol t fin) = World newAns y a num colour lett numcol t fin
                                                                       where
                                                                           newAns = smthLetter Insert num sym x
handleEvent _ w = w
