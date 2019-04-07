module Spec(main) where

list :: [Int]
list = [1, 2, 3, 4]

--getFromFile ::  String -> [String]
---getFromFile s = lines s

--printList :: [String] -> IO ()
--printList [] = putStrLn ""
--printList (x:xs) = do
    --putStrLn $ show x
   -- printList xs


lastList :: [a] -> a
lastList [x] = x
lastList (x:xs) = lastList xs



--frstList :: [a] -> a
--frstList (h:_) = h

--isCorrect :: [a] -> Bool
--isCorrect [] = False
--isCorrect (x:xs) = frstList x == frstList (lastList xs) && isCorrect xs
--isCorrect [x] = True




 
main :: IO ()
main = do
    --s <- readFile "/home/liza-notebook/Рабочий стол/chain/test/file.txt"
    --putStrLn s
    let b = lastList list
    --return ()
    putStrLn "Test completed"

