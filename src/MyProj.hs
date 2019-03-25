module MyProj
    ( runMyProj
    ) where
import Graphics.Gloss
import Prelude
sizeWin = 560 :: Int
centPos  = 100 :: Int


--data Maybe a = Nothing | Just a
data Action = Insert | Delete

cellDim =  8 :: Int
cellSize :: Int
cellSize = div sizeWin cellDim
heiOffset = 100 :: Int


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

--deleting the last letter in the word
delLast :: [a] -> [a]
delLast [x] = []
delLast (x:xs) = x:delLast xs

--doing the chein of chars for chainword
concatList :: [String] -> [Char]
concatList [x] = x
concatList (x:xs) =  delLast x ++ concatList xs

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

--initialize the chain
makeN :: Int -> a -> [a]
makeN 0 _ = []
makeN n x = x : makeN (n-1) x

--some functions for rendering
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
lineCoord n = fromIntegral( - div sizeWin 2 + n * cellSize)

lineCoordh :: Int -> Float
lineCoordh n = fromIntegral( - div (sizeWin - heiOffset) 2 + n * cellSize)


--something help, not need now
-----------------------------------------------
drawing :: Picture
drawing = pictures
  [ translate (-20) (-100) $ color ballColor $ circleSolid 30 
  , translate 30 50 $ color paddleColor $ rectangleSolid 10 50
  ]
  where
    ballColor = dark red
    paddleColor = light (light blue)
-----------------------------------------------


testList = ["anna", "apple", "elephant", "table"] :: [String]


runMyProj :: IO ()
runMyProj = do
        --  putStrLn (show $ concatList testList)
       -- putStrLn (show cellSize) 
        display window background ((makeVertical cellDim )<> (makeHorizontal cellDim) <> chainLines)
        
        
        
        
        
