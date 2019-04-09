module Utils where

import Type

-------------functions for the chain of char symbols-----------
---------------------------------------------------------------

----last elem of the list
lastList :: [a] -> a
lastList [x] = x
lastList (x:xs) = lastList xs

--last letter
getLastLetter :: [Char] -> [Letter]
getLastLetter [x] = [Letter x Nothing]
getLastLetter (x:xs) = getLastLetter xs


--action for insert/delete symbols in the chain
smthLetter :: Action -> Int  -> Char -> [Maybe Char] -> [Maybe Char]
smthLetter Delete 1 _ (x:xs) = Nothing : xs
smthLetter Delete n s (x:xs) = x : smthLetter Delete (n-1) s xs
smthLetter Insert 1 s (x:xs) = Just s : xs
smthLetter Insert n s (x:xs) = x : smthLetter Insert (n-1) s xs


--checking the correct of the file data (answers)
isCorrect :: [String] -> Bool
isCorrect [] = False
isCorrect [x] = True
isCorrect (x:xs) = lastOne == firstTwo && isCorrect xs
    where
        lastOne = lastList x
        firstTwo = head (head xs)

-------utils for printing letters---------------------
------------------------------------------------------
showLetter :: [Letter] -> String
showLetter [] =  "done."
showLetter (x:xs) = "symbol is :" ++ (show (symb x)) ++ ", bool is: "++ (show (border x)) ++ "\n" ++ showLetter xs

showLst :: [Maybe Char] -> String
showLst [] = "The end."
showLst (Just x : xs) = "Char" ++(show x) ++ "\n" ++ showLst xs
showLst (Nothing : xs) = "Nothing" ++ "\n"++ showLst xs

showQuest :: [String] -> String
showQuest [] = "."
showQuest (x:xs) = (show x) ++ "\n" ++ showQuest xs

find:: (Eq a) => [a] -> a -> Int
find xs e 
    | elem e xs = ffind xs e 
    | otherwise = length xs 
    where 
        ffind (x:xs) e
                | x == e    = 0
                | otherwise     = 1 + ffind xs e

checkEnd :: (Eq a) => [Maybe a] -> Bool
checkEnd [] = True
checkEnd (Nothing : xs) = False
checkEnd (x : xs) = checkEnd xs

