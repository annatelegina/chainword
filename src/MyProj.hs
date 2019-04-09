module MyProj
    ( runMyProj
    ) where

import Graphics.Gloss
import Prelude
import Graphics.Gloss.Interface.Pure.Game
import System.IO

import Config
import Type
import Graphics
import Utils
import Handler

---------initialize the game world-------------------------------------------
-----------------------------------------------------------------------------
makeWorld :: [String] -> World
makeWorld x = World (replicate (cellDim*cellDim) Nothing) (concatList 1 x) 1 green black red 0


-------------making the chain of chars for chainword--------------------------
------------------------------------------------------------------------------
concatList :: Int -> [String] -> [Letter]
concatList n [x] = makeChain (Just n) x ++ (getLastLetter x)
concatList n (x:xs) =  makeChain (Just n) x ++ (concatList (n+1) xs)

makeChain :: Maybe Int -> [Char] -> [Letter]
makeChain Nothing [x] = []
makeChain (Just n) [x] = [Letter x (Just n)]
makeChain Nothing (x:xs) = (Letter x Nothing) : makeChain Nothing xs
makeChain (Just n) (x:xs) = (Letter x (Just n)) : makeChain Nothing xs


checkRight :: (Eq a)=> [a]->[Maybe a] -> Int
checkRight [] _ = 0
checkRight (x:xs) (Nothing : ys) = checkRight xs ys
checkRight (x:xs) (y:ys) = if x ==  (fromJust y) then (1 + checkRight xs ys)  else checkRight xs ys


--------------initialize the chain---------------------------------
initList :: Int -> a -> [a]
initList 0 _ = []
initList n x = x : initList (n-1) x



update :: Float -> World -> World
update _ = id

parseInput:: String -> [String]
parseInput s
    | null s = [] 
    | otherwise= (take (find s '\n') s): (parseInput (drop ((find s '\n')+1) s))
    
runMyProj :: IO ()
runMyProj = do
           s <- readFile quest
           putStrLn s
           b <- readFile ans
           let initState = makeWorld $ parseInput b
           play window background stepsPerSecond initState drawWorld handleEvent update
        
        
        
        
        
