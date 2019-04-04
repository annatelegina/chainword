module MyProj
    ( runMyProj
    ) where

import Graphics.Gloss
import Prelude
sizeWin = 560 :: Int
centPos  = 100 :: Int

data Letter = Letter
    { symb :: Char
    , border :: Bool
    }
data Action = Insert | Delete
data GameState = Error | Ok 

data World = World
    { field :: [Maybe Char]
    , answers :: [Letter]
    , marker :: Int
    , colour_marker :: Color
    , colour_letter :: Color
    }
tmpField = [ (a, 1)| a <-[8,7..1]] ++ [(b, 2) |b <-[1,2..8]] ++ [ (a, 3)| a <-[8,7..1]] 
           ++ [(b, 4) |b <-[1,2..8]] ++ [ (a, 5)| a <-[8,7..1]] ++ [(b, 6) |b <-[1,2..8]] 
           ++ [ (a, 7)| a <-[8,7..1]] ++ [(b, 8) |b <-[1,2..8]]
           
cellDim =  8 :: Int
cellSize :: Int
cellSize = div sizeWin cellDim
heiOffset = 100 :: Int


getCell :: Int -> (Int, Int)
getCell n = tmpField!!(n-1)

getCorner :: (Int, Int) -> (Float, Float)
getCorner (x, y) = (fst s+10, snd s-2*l-10)
                   where s = position (x-1, y-1)
                         l = fromIntegral(div cellSize 2)

getCenter :: (Int, Int) -> (Float, Float)
getCenter (x, y) = (fst s+l, snd s-l)
                   where s = position (x-1, y-1)
                         l = fromIntegral(div cellSize 2)

position :: (Int, Int) -> (Float, Float)
position (x, y) = (fromIntegral(- div sizeWin 2 + y*cellSize), fromIntegral(div (sizeWin+heiOffset) 2 - x*cellSize))

makeWorld :: [String] -> World
makeWorld x = World (replicate (cellDim*cellDim) (Just 'A') ) (concatList x) 1 green black

drawMarker:: Int -> Picture
drawMarker n = translate (fst cord) (snd cord) $ color green $ rectangleSolid size size
               where 
                     size = fromIntegral cellSize
                     cord = getCenter (getCell n) 
	   
drawWorld :: World -> Picture
drawWorld (World x y num colour lett) = (base<>marker<>words<>numbers)
                                where
                                     base = (makeVertical cellDim) <> (makeHorizontal cellDim) <> chainLines
                                     marker = drawMarker num
                                     words = mconcat [color lett $ translate (fst $ getCenter (getCell n)) (snd $ getCenter (getCell n)) $ scale 0.2 0.2 $ Text $ show ch | n<- [1..length x], Just ch <- [x!!(n-1)]]
                                     

--functions for the chain of char symbols
----last elem of the list
lastList :: [a] -> a
lastList [x] = x
lastList (x:xs) = lastList xs

--checking the correct of the file data (answers)
isCorrect :: [String] -> Bool
isCorrect [] = False
isCorrect [x] = True
isCorrect (x:xs) = lastOne == firstTwo && isCorrect xs
    where
        lastOne = lastList x
        firstTwo = head (head xs)

--doing the chain of chars for chainword
concatList :: [String] -> [Letter]
concatList [x] = makeChain x
concatList (x:xs) =  makeChain x ++ (tail $ concatList xs)

makeChain :: [Char] -> [Letter]
makeChain  [x] = [Letter x True]
makeChain  (x:xs) = (Letter x False) : makeChain xs

--util for printing letters
showLetter :: [Letter] -> String
showLetter [] =  "done."
showLetter (x:xs) = "symbol is :" ++ (show (symb x)) ++ ", bool is: "++ (show (border x)) ++ "\n" ++ showLetter xs

showLst :: [Maybe Char] -> String
showLst [] = "The end."
showLst (Just x : xs) = "Char" ++(show x) ++ "\n" ++ showLst xs
showLst (Nothing : xs) = "Nothing" ++ "\n"++ showLst xs

--action for insert/delete symbols in the chain
smthLetter :: Action -> Int -> Int -> String -> [Maybe Char] -> [Maybe Char]
smthLetter Delete 0 0 _ x = x
smthLetter Delete 0 m s (x:xs) = Nothing : smthLetter Delete 0 (m-1) s xs
smthLetter Delete n m s (x:xs) = x : smthLetter Delete (n-1) m s xs
smthLetter Insert 0 0 _ x = x
smthLetter Insert 0 m (s:ss) (x:xs) = Just s : smthLetter Insert 0 (m-1) ss xs
smthLetter Insert n m s (x:xs) = x : smthLetter Insert (n-1) m s xs


--checking our input results
fromJust :: Maybe a -> a
fromJust (Just x) = x

checkRight :: (Eq a) => [a]->[Maybe a] -> Int
checkRight [] _ = 0
checkRight (x:xs) (Nothing : ys) = checkRight xs ys
checkRight (x:xs) (y:ys) = if x ==  (fromJust y) then (1 + checkRight xs ys)  else checkRight xs ys

--initialize the chain--------------------------------------------------------
initList :: Int -> a -> [a]
initList 0 _ = []
initList n x = x : initList (n-1) x

-----some functions for rendering---------------------------------------------
------------------------------------------------------------------------------
window :: Display
window = InWindow "Chainword" (sizeWin, sizeWin+heiOffset) (centPos, centPos)

background :: Color
background = white

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


--returns the coordinate of the line            
lineCoord :: Int -> Float
lineCoord n = fromIntegral(div sizeWin 2 - n * cellSize)

lineCoordh :: Int -> Float
lineCoordh n = fromIntegral(-div (sizeWin - heiOffset) 2 + n * cellSize)



--something help, not need now
-----------------------------------------------
drawing :: Picture
drawing = pictures
  [ translate (fst s) (snd s)  $ color ballColor $ circleSolid 10 
--  , translate 30 50 $ color paddleColor $ rectangleSolid 10 50
  ]
  where
    s = getCenter (getCell 1)
    ballColor = dark red
-----------------------------------------------


testList = ["anna", "apple", "enabl", "look"] :: [String]


runMyProj :: IO ()
runMyProj = do
           -- putStrLn ("Correct answer list is:")
           -- putStrLn( showLetter $ s)
           -- putStrLn ("Our answer list is :")
           -- putStrLn (showLst $ a)
           -- putStrLn( "List after inserting is :")
           -- putStrLn (showLst $ d)
           -- putStrLn("List after deleting is :")
           -- putStrLn (showLst $ smthLetter Delete 2 5 "aaaaaaa" d)
           -- where a = initList b Nothing
           --       b = length $ s
           --       s =  concatList testList
           --       d = smthLetter Insert 1 5 "abcde" a
           -- putStrLn (show (getCenter (getCell 1)) )
           display window background (drawWorld (makeWorld testList))
        
        
        
        
        
