module Graphics where

import Graphics.Gloss

import Type
import Config


------------------------------drawing marker on the field-----------------------------

drawMarker:: Int -> Picture
drawMarker n = translate (fst cord) (snd cord) $ color green $ rectangleSolid size size
               where 
                     size = fromIntegral cellSize
                     cord = getCenter (getCell n) 


-------------------------------------------------------------------------
-----------------------function for drawing lines------------------------

makeHorizontal :: Int -> Picture
makeHorizontal n
            | n < 0 = Blank
            | otherwise = (color green (line [(lineCoord 0, lineCoordh n), (lineCoord cellDim, lineCoordh n)])) <> (makeHorizontal (n-1))
            
makeVertical :: Int -> Picture
makeVertical n
            | n < 0 = Blank
            | otherwise = (color green (line [(lineCoord n, lineCoordh 0), (lineCoord n, lineCoordh cellDim)])) <> (makeVertical (n-1))


chainLines :: Picture
chainLines = mconcat [color (dark black) (line [(lineCoord n, lineCoordh 0), (lineCoord n, lineCoordh (cellDim -1))] ) | n <- [1,3..cellDim-1] ]<>
            mconcat [color (dark black) (line [(lineCoord n, lineCoordh 1), (lineCoord n, lineCoordh cellDim)]) | n <- [2,4..cellDim-2]]


-------------WORLD DRAWING---------------------------------------------------
-----------------------------------------------------------------------------
	   
drawWorld :: World -> Picture
drawWorld (World x y a num colour lett numcol t 0) | t > mTime = scale 0.15 0.15 $ color red $ Text ("YOUR TIME IS UP!!!" ++ (ending x y a)) 
drawWorld (World x y a num colour lett numcol t 0)  = (base<>marker<>words<>numbers<>timer)
                                                    where
                                                    base = (makeVertical cellDim) <> (makeHorizontal cellDim) <> chainLines
                                                    marker = drawMarker num
                                                    words = mconcat [color lett $ translate (fst $ getCenter (getCell n)) (snd $ getCenter (getCell n)) $ scale 0.2 0.2 $ Text $ [ch] | n<- [1..length x], Just ch <- [x!!(n-1)]]
                                                    numbers = mconcat [color numcol $ translate (fst $ getCorner (getCell n)) (snd $ getCorner (getCell  n)) $ scale 0.1 0.1 $ Text $ show k | n<- [1..length cell], Just k<- [cell!!(n-1)] ]
                                                    cell = getNum y
                                                    timer = color numcol $ translate 0 (-250) $ scale 0.1 0.1 $ Text $ (show "Seconds to play: ") ++  (show (truncate (mTime-t)))
drawWorld (World x y a num colour lett numcol t 1) = scale 0.15 0.15 $ color red $ Text $ ending x y a
drawWorld (World x y a num colour lett numcol t 2) = scale 0.15 0.15 $ color red $ Text $ ending x y a

-----functions for getting coordinates---------------------------
-----------------------------------------------------------------

getNum :: [Letter] -> [Maybe Int]
getNum [] = []
getNum (Letter a (Just n):xs) = [Just n] <> getNum xs
getNum (Letter a Nothing:xs) = [Nothing] <> getNum xs

getCorner :: (Int, Int) -> (Float, Float)
getCorner (x, y) = (fst s, snd s-2*l)
                   where s = position (x-1, y-1)
                         l = fromIntegral(div cellSize 2)

getCenter :: (Int, Int) -> (Float, Float)
getCenter (x, y) = (fst s+l, snd s-l)
                   where s = position (x-1, y-1)
                         l = fromIntegral(div cellSize 2)

position :: (Int, Int) -> (Float, Float)
position (x, y) = (fromIntegral(- div sizeWin 2 + y*cellSize), fromIntegral(div (sizeWin+heiOffset) 2 - x*cellSize))


--------text for end the game----------------------------
ending :: [Maybe Char] -> [Letter] -> [String] -> String
ending l p answers = finish (makeWords [] l p) answers

finish :: [String] -> [String] -> String
finish [] [] = "\n"
finish (x:xs) (y:ys) | x == y = x ++ (show "\n") ++ finish xs ys
                     | otherwise = finish xs ys


endOfGame :: [Maybe Char] -> [Letter] -> String
endOfGame [] _ = "Completed!\n"
endOfGame (Nothing:xs) _ = "You have empty cells\n"
endOfGame (x:xs) ((Letter ans num):ys) 
                                | (fromJust x) == ans = endOfGame xs ys
                                | otherwise = "You have mistakes!\n"

--for finishing the game
retL :: Maybe Char -> Char
retL (Just k) = k
retL Nothing = '+'

makeWords :: String -> [Maybe Char] -> [Letter] -> [String]
makeWords t [] [] = [t]
makeWords [] [x] [y] = []
makeWords [] (x:xs) (y:ys) = makeWords [(retL x)] xs ys
makeWords tmp (x:xs) (Letter sym Nothing:ys) = makeWords (tmp ++ [(retL x)]) xs ys
makeWords tmp (x:xs) (Letter sym (Just k) :ys) = (tmp ++ [(retL x)]) : (makeWords [] (x:xs) (Letter sym (Just k):ys))

--returns the coordinate of the line            
lineCoord :: Int -> Float
lineCoord n = fromIntegral(div sizeWin 2 - n * cellSize)

lineCoordh :: Int -> Float
lineCoordh n = fromIntegral(-div (sizeWin - heiOffset) 2 + n * cellSize)






