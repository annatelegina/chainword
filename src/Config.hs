module Config where

import Graphics.Gloss

--window parameters
sizeWin = 560 :: Int
centPos  = 100 :: Int
cellDim =  8 :: Int

cellSize :: Int
cellSize = div sizeWin cellDim
heiOffset = 100 :: Int

--constants for rendering
window :: Display
window = InWindow "Chainword" (sizeWin, sizeWin+heiOffset) (centPos, centPos)

background :: Color
background = white

--field help structure
tmpField = [ (a, 1)| a <-[8,7..1]] ++ [(b, 2) |b <-[1,2..8]] ++ [ (a, 3)| a <-[8,7..1]] 
           ++ [(b, 4) |b <-[1,2..8]] ++ [ (a, 5)| a <-[8,7..1]] ++ [(b, 6) |b <-[1,2..8]] 
           ++ [ (a, 7)| a <-[8,7..1]] ++ [(b, 8) |b <-[1,2..8]]
           
           
--function for field
getCell :: Int -> (Int, Int)
getCell n = tmpField!!(n-1)

--find the number of Carteg in random list
findCarteg :: (Eq a) => (Eq b) => (a, b) -> [(a,b)] -> Int
findCarteg (m,n) (x:xs) = if (m == fst x) && (n == snd x) then 1 else (1 + findCarteg (m,n) xs)
findCarteg _ [] = 0

--the opposite to getCell
getMarker :: (Int, Int) -> Int 
getMarker (m,n) = findCarteg (m,n) tmpField

--file for chainword
quest = "./test/questions.txt" :: String
ans = "./test/answers.txt" :: String

--for timer
stepsPerSecond :: Int
stepsPerSecond = 60

