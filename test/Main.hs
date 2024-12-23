module Main where

main :: IO ()
main = do
    putStrLn "Module loaded"


-- 1 Custom map Implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []        -- Base case: if the list is empty, return an empty list
myMap fx (x:xs) = fx x : myMap fx xs -- Apply fx to x, then recurse on the rest

-- 2 Flatten a Nested List
flatten :: [[a]] -> [a]
flatten [] = []                       -- Base case: if the list is empty, return an empty list
flatten (xs:xy) = xs ++ flatten xy    -- Concatenate the first list with the flattened rest

-- 3 List Zipping Without zip
zipLists :: [a] -> [b] -> [(a, b)]
zipLists [] _ = []                     -- If the first list is empty, stop
zipLists _ [] = []                     -- If the second list is empty, stop
zipLists (x:xs) (y:ys) = (x, y) : zipLists xs ys -- Pair the heads, recurse on the tails

-- 4 Remove Duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []                -- Base case: an empty list has no duplicates
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs  -- If x is already in the rest, skip it
    | otherwise   = x : removeDuplicates xs  -- Otherwise, keep x and continue

-- 5 List Intersection
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []  -- If the first list is empty, return an empty list
intersect _ [] = []  -- If the second list is empty, return an empty list
intersect (x:xs) ys
    | x `elem` ys = x : intersect xs ys  -- If x is in ys, include it in the result
    | otherwise = intersect xs ys        -- Otherwise, skip x and recurse

-- 6
runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode [] = [] -- Base case: empty list
runLengthEncode (x:xs) = (x, length prefix) : runLengthEncode rest
  where
    (prefix, rest) = span (== x) (x:xs) -- Group consecutive occurrences of x

-- 7 Generate Squares Generate a list of squares of all numbers from 1 to n
squares :: Int -> [Int]
squares n = [x*x | x <- [1.. abs n]]

-- 8 Filter Even Numbers Create a list of all even numbers from 1 to n
evens :: Int -> [Int]
evens n = [x | x <- [1.. abs n], x `mod` 2 == 0]

-- 9 pythagorean Triples Generate all Pythagorean triples
-- (a, b, c) where a^2 + b^2 = c^2 and a, b, c are less than or equal to n.
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = [(a, b, c) | a <- [1..abs n], b <- [a+1..abs n], c <- [b+1..abs n], a*a + b*b == c*c]

-- 10 Double Each Element
doubleEach :: [Int] -> [Int]
doubleEach xs = map (\x -> x * 2) xs

-- 11 Filter even
filterEven :: [Int] -> [Int]
filterEven [] = []
filterEven xs = filter(\x -> x `mod` 2 == 0) xs

-- 12 Square of Each Element
squareEach :: [Int] -> [Int]
squareEach [] = []
squareEach xs = map(\x -> x^2) xs

-- 13 Sum od pairs
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs xs ys = customZipWith (\x y -> x+y) xs ys

-- 14 Custom Zip
customZip :: [Int] -> [Int] -> [(Int, Int)]
customZip _ [] = []
customZip [] _ = []
customZip (x:xs) (y:xy) = (x, y) : customZip xs xy

-- 15 Custom zipWith
customZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
customZipWith _ [] _ = []
customZipWith _ _ [] = []
customZipWith fx (x:xs) (y:ys) = fx x y : customZipWith fx xs ys
