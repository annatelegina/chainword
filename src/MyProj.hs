module MyProj
    ( runMyProj
    ) where

import Graphics.Gloss
import Prelude
import Graphics.Gloss.Interface.Pure.Game
sizeWin = 560 :: Int
centPos  = 100 :: Int

data Letter = Letter
    { symb :: Char
    , border :: Maybe Int
    }
data Action = Insert | Delete
data GameState = Error | Ok 

data World = World
    { field :: [Maybe Char]
    , answers :: [Letter]
    , marker :: Int
    , colour_marker :: Color
    , colour_letter :: Color
    , colour_number :: Color
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

--find the number of Carteg in random list

findCarteg :: (Eq a) => (Eq b) => (a, b) -> [(a,b)] -> Int
findCarteg (m,n) (x:xs) = if (m == fst x) && (n == snd x) then 1 else (1 + findCarteg (m,n) xs)
findCarteg _ [] = 0

--the opposite to getCell
getMarker :: (Int, Int) -> Int 
getMarker (m,n) = findCarteg (m,n) tmpField




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

makeWorld :: [String] -> World
makeWorld x = World (replicate (cellDim*cellDim) Nothing) (concatList 1 x) 1 green black red

drawMarker:: Int -> Picture
drawMarker n = translate (fst cord) (snd cord) $ color green $ rectangleSolid size size
               where 
                     size = fromIntegral cellSize
                     cord = getCenter (getCell n) 
	   
drawWorld :: World -> Picture
drawWorld (World x y num colour lett numcol) = (base<>marker<>words<>numbers)
                                where
                                     base = (makeVertical cellDim) <> (makeHorizontal cellDim) <> chainLines
                                     marker = drawMarker num
                                     words = mconcat [color lett $ translate (fst $ getCenter (getCell n)) (snd $ getCenter (getCell n)) $ scale 0.2 0.2 $ Text $ show ch | n<- [1..length x], Just ch <- [x!!(n-1)]]
                                     numbers = mconcat [color numcol $ translate (fst $ getCorner (getCell n)) (snd $ getCorner (getCell  n)) $ scale 0.1 0.1 $ Text $ show k | n<- [1..length cell], Just k<- [cell!!(n-1)] ]
                                     cell = getNum y



getNum :: [Letter] -> [Maybe Int]
getNum [] = []
getNum (Letter a (Just n):xs) = [Just n] <> getNum xs
getNum (Letter a Nothing:xs) = [Nothing] <> getNum xs
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
concatList :: Int -> [String] -> [Letter]
concatList n [x] = makeChain (Just n) x ++ (getLastLetter x)
concatList n (x:xs) =  makeChain (Just n) x ++ (concatList (n+1) xs)

makeChain :: Maybe Int -> [Char] -> [Letter]
makeChain Nothing [x] = []
makeChain (Just n) [x] = [Letter x (Just n)]
makeChain Nothing (x:xs) = (Letter x Nothing) : makeChain Nothing xs
makeChain (Just n) (x:xs) = (Letter x (Just n)) : makeChain Nothing xs

getLastLetter :: [Char] -> [Letter]
getLastLetter [x] = [Letter x Nothing]
getLastLetter (x:xs) = getLastLetter xs

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


--playing game
handleEvent :: Event -> World -> World

handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (World x y num colour lett numcol) = World x y num colour lett numcol
                              
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (World x y num colour lett numcol) 
                                                                                | n < 8 = World x y newNum colour lett numcol 
                                                                                | otherwise = World x y num colour lett numcol      
                                                                                where 
                                                                                    n = snd point
                                                                                    newNum = getMarker (fst point, snd point  + 1)
                                                                                    point = getCell num  
    
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) (World x y num colour lett numcol) 
                                                                                | n > 1 = World x y newNum colour lett numcol 
                                                                                | otherwise = World x y num colour lett numcol      
                                                                                where 
                                                                                    n = snd point
                                                                                    newNum = getMarker (fst point, snd point  - 1)
                                                                                    point = getCell num 
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (World x y num colour lett numcol) 
                                                                                | n > 1 = World x y newNum colour lett numcol 
                                                                                | otherwise = World x y num colour lett numcol      
                                                                                where 
                                                                                    n = fst point
                                                                                    newNum = getMarker (fst point - 1, snd point)
                                                                                    point = getCell num 
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (World x y num colour lett numcol) 
                                                                                | n < 8 = World x y newNum colour lett numcol 
                                                                                | otherwise = World x y num colour lett numcol      
                                                                                where 
                                                                                    n = fst point
                                                                                    newNum = getMarker (fst point + 1, snd point)
                                                                                    point = getCell num
handleEvent _ w = w

--handleEvent (EventKey (Char key) Up _ _) (World x y num colour lett numcol) = 


testList = ["anna", "apple", "enabl", "look"] :: [String]
test1 = ["teleging", "gnomes", "separate", "equal", "looking"] :: [String]

runMyProj :: IO ()
runMyProj = do
            --  putStrLn ("Correct answer list is:")
            --  putStrLn( showLetter $ s)
            --  putStrLn ("Our answer list is :")
            --  putStrLn (showLst $ a)
            --  putStrLn( "List after inserting is :")
            --  putStrLn (showLst $ d)
            --  putStrLn("List after deleting is :")
            --  putStrLn (showLst $ smthLetter Delete 2 5 "aaaaaaa" d)
            --  where --a = initList b Nothing
              --      b = length $ s
                   -- s =  concatList 1 test1
                   -- d = smthLetter Insert 1 5 "abcde" a
           -- putStrLn (show (getCenter (getCell 1)) )
           let initState = World 
           display window background (drawWorld (makeWorld test1))
           --play display bgColor stepsPerSecond initState drawWorld handleEvent update
        
        
        
        
        
