module Main where

main :: IO ()
main = putStrLn "Module loaded"

-- 1.1 Using Basic Boolean Logic
nAnd :: Bool -> Bool -> Bool
nAnd a b = not (a && b)

-- 1.2 Using Conditional Expressions
nAnd' :: Bool -> Bool -> Bool
nAnd' a b = if a && b then False else True

-- 1.3 Truth Tables
nAnd'' :: Bool -> Bool -> Bool
nAnd'' True  True  = False
nAnd'' True  False = True
nAnd'' False True  = True
nAnd'' False False = True

