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
nAnd'' True  True  = False
nAnd'' True  False = True
nAnd'' False True  = True
nAnd'' False False = True

-- 2
prop_nAnd_eq_nAnd' :: Bool -> Bool -> Bool
prop_nAnd_eq_nAnd' a b = nAnd a b == nAnd' a b

prop_nAnd_eq_nAnd'' :: Bool -> Bool -> Bool
prop_nAnd_eq_nAnd'' a b = nAnd a b == nAnd'' a b

prop_nAnd'_eq_nAnd'' :: Bool -> Bool -> Bool
prop_nAnd'_eq_nAnd'' a b = nAnd' a b == nAnd'' a b
