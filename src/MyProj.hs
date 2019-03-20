module MyProj
    ( runMyProj
    ) where
import Graphics.Gloss

sizeWin = 560 :: Int
centPos  = 150 :: Int

cellDim =  8 :: Int
cellSize :: Int
cellSize = div sizeWin cellDim

window :: Display
window = InWindow "Chainword" (sizeWin, sizeWin) (centPos, centPos)

background :: Color
background = white

makeVertical :: Int -> Picture
makeVertical n
            | n < 0 = Blank
            | otherwise = (color green (line [(lineCoord 0, lineCoord n), (lineCoord cellDim, lineCoord n)])) <> (makeVertical (n-1))
            
makeHorizontal :: Int -> Picture
makeHorizontal n
            | n < 0 = Blank
            | otherwise = (color green (line [(lineCoord n, lineCoord 0), (lineCoord n, lineCoord cellDim)])) <> (makeHorizontal (n-1))
chainLines :: Picture
chainLines = mconcat [color (dark black) (line [(lineCoord n, lineCoord 0), (lineCoord n, lineCoord (cellDim -1))] ) | n <- [1,3..cellDim-1] ]<>
            mconcat [color (dark black) (line [(lineCoord n, lineCoord 1), (lineCoord n, lineCoord cellDim)]) | n <- [2,4..cellDim-2]]


--returns the coordinate of the line            
lineCoord :: Int -> Float
lineCoord n = fromIntegral( - div sizeWin 2 + n * cellSize)

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

runMyProj :: IO ()
runMyProj = do
       -- putStrLn (show cellSize) 
        display window background ((makeVertical cellDim )<> (makeHorizontal cellDim) <> chainLines)
        
        
        
        
        
