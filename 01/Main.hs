module Main where

import Test.QuickCheck

main :: IO ()
main = putStrLn "Module loaded"

-- 1
nAnd :: Bool -> Bool -> Bool
nAnd a b = not (a && b)

nAnd' :: Bool -> Bool -> Bool
nAnd' a b = if a && b then False else True

nAnd'' :: Bool -> Bool -> Bool
nAnd'' True True = False
nAnd'' True False = True
nAnd'' False True = True
nAnd'' False False = True

-- 2
prop_nAnd_eq_nAnd' :: Bool -> Bool -> Bool
prop_nAnd_eq_nAnd' a b = nAnd a b == nAnd' a b

prop_nAnd_eq_nAnd'' :: Bool -> Bool -> Bool
prop_nAnd_eq_nAnd'' a b = nAnd a b == nAnd'' a b

prop_nAnd'_eq_nAnd'' :: Bool -> Bool -> Bool
prop_nAnd'_eq_nAnd'' a b = nAnd' a b == nAnd'' a b

-- 3
nDigits :: Integer -> Int
nDigits n
  | n < 0 = length (show (abs n))
  | otherwise = length (show n)

-- 4
nRoots :: Float -> Float -> Float -> Int
nRoots a b c
  | a == 0.0 = error "the first argument should be non-zero!"
  | discriminant > 0.0 = 2
  | discriminant == 0.0 = 1
  | otherwise = 0
  where
    discriminant = b * b - 4.0 * a * c

-- 5
smallerRoot::Float -> Float -> Float -> Float
smallerRoot a b c
  | nRoots a b c == 0 = error "no roots"
  | nRoots a b c == 1 = -b / (2.0 * a)
  | otherwise = (-b - sqrt (b * b - 4.0 * a * c)) / (2.0 * a)

largerRoot::Float -> Float -> Float -> Float
largerRoot a b c
  | nRoots a b c == 0 = error "no roots"
  | nRoots a b c == 1 = -b / (2.0 * a)
  | otherwise = (-b + sqrt (b * b - 4.0 * a * c)) / (2.0 * a)