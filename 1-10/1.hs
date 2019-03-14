import Data.List

-- last element
myLast = head . reverse

-- last but last
myAlmostLast = head . reverse . init 

-- k element
elementAt :: [a] -> Int -> a
elementAt list i = list !! (i-1)

-- k element match
elementAt' :: [a] -> Int -> a
elementAt' (x:_) 1 = x
elementAt' [] _ = error "out of bounds"
elementAt' (_:xs) k
  | k < 1       = error "out of bounds"
  | otherwise   = elementAt' xs (k -  1)


-- count elements
myLength :: [a] -> Int
myLength [] = 0   
myLength (_:xs) = 1 + myLength xs

-- silly length
myLength' :: [a] -> Int
myLength' = sum . map (\_->1)

-- reverse list
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- reverse list 2
reverse'' :: [a] -> [a]
reverse'' [] = []
reverse''(x:xs) = reverse'' xs ++ [x]

-- palindrome
isPali :: (Eq a) => [a] -> Bool
isPali xs = xs == (reverse xs)

-- palindrome with monad
--isPali' :: (Eq a) => [a] -> Bool
--isPali' = Control.Monad.liftM2 (==) id reverse

-- remove consequent dupes 
compress'' :: Eq a => [a] -> [a]
compress'' = map head . group

-- pack consequent into lists
pack [] = []
pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest

-- 10, legth encoding
-- (*) Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode xs = map (\x -> (length x, head x)) (group xs)