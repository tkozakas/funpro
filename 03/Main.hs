module Main where

main :: IO ()
main = do
    putStrLn "Overlaps extended Shape:"
    -- Circle vs Circle
    print $ overlaps (Circle (0, 0) 5) (Circle (8, 0) 5)  -- True
    print $ overlaps (Circle (0, 0) 5) (Circle (11, 0) 5) -- False

    -- Rectangle vs Rectangle
    print $ overlaps (Rectangle (0, 0) 4 4) (Rectangle (3, 3) 4 4) -- True
    print $ overlaps (Rectangle (0, 0) 4 4) (Rectangle (5, 5) 4 4) -- False

    -- Circle vs Rectangle
    print $ overlaps (Circle (0, 0) 5) (Rectangle (-3, -3) 6 6) -- True
    print $ overlaps (Circle (10, 10) 5) (Rectangle (0, 0) 4 4) -- False

    putStrLn "\nmyAny, myAll (filter ir fold):"
    print $ myAnyFilter even [1, 3, 5, 6] -- True
    print $ myAnyFilter even [1, 3, 5]    -- False
    print $ myAllFilter (> 0) [1, 2, 3]   -- True
    print $ myAllFilter (> 0) [-1, 2, 3]  -- False
    print $ myAnyFold even [1, 3, 5, 6]   -- True
    print $ myAnyFold even [1, 3, 5]      -- False
    print $ myAllFold (> 0) [1, 2, 3]     -- True
    print $ myAllFold (> 0) [-1, 2, 3]    -- False

    putStrLn "\nmyUnzip:"
    print $ myUnzip [(1, 'a'), (2, 'b'), (3, 'c')] -- ([1,2,3], "abc")

    putStrLn "\nmyLength:"
    print $ myLengthMap [1, 2, 3, 4, 5]  -- 5
    print $ myLengthFold [1, 2, 3, 4, 5] -- 5

    putStrLn "\nff:"
    print $ ff 50 [1, -2, 3, 4, 5] -- 50 (10 + 30 + 40)
    print $ ff 100 [1, 2, -3, 4, 5] -- 100 (10 + 20 + 40 + 50)

    putStrLn "\ntotal:"
    print $ total (*2) 5 -- 0 + 2 + 4 + 6 + 8 + 10 = 30
    print $ totalFold (*2) 5 -- 0 + 2 + 4 + 6 + 8 + 10 = 30

    putStrLn "\niter:"
    let double = (*2)
    print $ (iter 3 double) 1 -- 8 (2^3 = 8)
    print $ (iterFold 3 double) 1 -- 8

    putStrLn "\nsplits:"
    print $ splits "Spy"
    print $ splits [1, 2, 3]

-- 1
data Shape = Circle (Float, Float) Float
           | Rectangle (Float, Float) Float Float
           deriving (Show, Eq)

overlaps :: Shape -> Shape -> Bool

-- Circle vs Circle
overlaps (Circle (x1, y1) r1) (Circle (x2, y2) r2) =
    let distance = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
    in distance < (r1 + r2)

-- Rectangle vs Rectangle
overlaps (Rectangle (x1, y1) w1 h1) (Rectangle (x2, y2) w2 h2) =
    not (x1 + w1 < x2 || x2 + w2 < x1 || y1 + h1 < y2 || y2 + h2 < y1)

-- Circle vs Rectangle
overlaps (Circle (cx, cy) r) (Rectangle (x, y) w h) =
    let nearestX = max x (min cx (x + w))
        nearestY = max y (min cy (y + h))
        distance = sqrt ((nearestX - cx)^2 + (nearestY - cy)^2)
    in distance < r

-- Rectangle vs Circle
overlaps rect@(Rectangle _ _ _) circ@(Circle _ _) = overlaps circ rect

-- 2
myAnyFilter :: (a -> Bool) -> [a] -> Bool
myAnyFilter p xs = not (null (filter p xs))

myAnyMap :: (a -> Bool) -> [a] -> Bool
myAnyMap p xs = or (map p xs)

myAnyFold :: (a -> Bool) -> [a] -> Bool
myAnyFold p = foldr (\x acc -> p x || acc) False


myAllFilter :: (a -> Bool) -> [a] -> Bool
myAllFilter p xs = null (filter (not . p) xs)

myAllFold :: (a -> Bool) -> [a] -> Bool
myAllFold p = foldr (\x acc -> p x && acc) True

myAllMap :: (a -> Bool) -> [a] -> Bool
myAllMap p xs = and (map p xs)

-- 3
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys)) ([], [])

-- 4
myLengthMap :: [a] -> Int
myLengthMap = sum . map (\_ -> 1)

myLengthFold :: [a] -> Int
myLengthFold = foldr (\_ acc -> acc + 1) 0

-- 5
ff :: Integer -> [Integer] -> Integer
ff maxNum = sum . takeWhile (<= maxNum) . scanl1 (+) . map (*10) . filter (>=0)

-- 6
total :: (Integer -> Integer) -> Integer -> Integer
total f n = sum . map f $ [0..n]

totalFold :: (Integer -> Integer) -> Integer -> Integer
totalFold f n = foldr (+) 0 (map f [0..n])

-- 7
iter :: Integer -> (a -> a) -> (a -> a)
iter n f
  | n <= 0    = id
  | otherwise = f . iter (n - 1) f

iterFold :: Integer -> (a -> a) -> (a -> a)
iterFold n f
  | n <= 0    = id
  | otherwise = foldr (.) id (replicate (fromInteger n) f)

-- 8
splits :: [a] -> [([a], [a])]
splits xs = [(take n xs, drop n xs) | n <- [0..length xs]]

