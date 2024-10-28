module Main where
import Data.Char (toUpper, isAlpha)

main :: IO ()
main = putStrLn "Module loaded"

-- 1
average :: [Float] -> Float
average xs = sum xs / fromIntegral (length xs)

-- 2
dividesRC :: Integer -> [Integer]
dividesRC n = dividesHelper n 1
  where
    dividesHelper abs n i
      | i > n = []
      | n `mod` i == 0 = i : dividesHelper n (i + 1)
      | otherwise = dividesHelper n (i + 1)

dividesLC :: Integer -> [Integer]
dividesLC n = [x | x <- [1.. abs n ], n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime n | n <= 0 = False
          | otherwise = dividesLC n == [1, n]

-- 3
prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

substring :: String -> String -> Bool
substring xs [] = False
substring xs ys@(y:ys')
  | prefix xs ys = True
  | otherwise = substring xs ys'

-- 4
permut :: [Integer] -> [Integer] -> Bool
permut [] [] = True
permut [] _ = False
permut (x:xs) ys
  | x `elem` ys = permut xs (remove x ys)
  | otherwise = False
  where
    remove _ [] = []
    remove z (y:ys)
      | z == y = ys
      | otherwise = y : remove z ys

-- 5
capitalise :: String -> String
capitalise str = [toUpper c | c <- str, isLetter c]

-- 6
itemTotal :: [(String, Float)] -> [(String, Float)]
itemTotal [] = []
itemTotal ((item, price):xs) = (item, price + sum [p | (i, p) <- xs, i == item]) : itemTotal [(i, p) | (i, p) <- xs, i /= item]

itemDiscount :: String -> Integer -> [(String, Float)] -> [(String, Float)]
itemDiscount _ _ [] = []
itemDiscount item discount ((i, price):xs)
  | i == item = (i, price * (1 - fromIntegral discount / 100)) : itemDiscount item discount xs
  | otherwise = (i, price) : itemDiscount item discount xs
