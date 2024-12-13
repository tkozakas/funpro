module Main where


main :: IO ()
main = do
    putStrLn "Module loaded"

    -- 1
    let exTree :: GTree Int
        exTree = Gnode [
                 Leaf 1,
                 Gnode [Leaf 2, Leaf 3],
                 Gnode [Gnode [Leaf 4]]
             ]

    print $ depth (Gnode [Leaf 1, Leaf 2, Leaf 3]) -- 2
    print $ depth exTree -- 4
    print $ elementOccurs 4 exTree -- True
    print $ elementOccurs 5 exTree -- False
    print $ mapGTree (*2) exTree -- Gnode [Leaf 2,Gnode [Leaf 4,Leaf 6],Gnode [Gnode [Leaf 8]]]

    -- 2
    let add :: Num a => Ops a
        add = sum

        mul :: Num a => Ops a
        mul = product

        valuation :: Valuation Int
        valuation 'x' = 10
        valuation 'y' = 5
        valuation _   = 0

        expression :: Expr Int
        expression = Op add [Lit 3, EVar 'x', Op mul [EVar 'y', Lit 2]]

    print $ eval valuation expression -- 3 + x + (y * 2) = 3 + 10 + (5 * 2) = 23

    -- 3
    let exoption :: RegExp
        exoption = option (Literal 'a') -- "" or "a"

        explus :: RegExp
        explus = plus (Literal 'b') -- "b" or "bb" or "bbb" or ...

    print exoption
    print explus

    -- 4
    let list1 :: NumList Double
        list1 = NList [1.0, 2.0, 3.0]

        list2 :: NumList Double
        list2 = NList [4.0, 5.0]

        list3 :: NumList Double
        list3 = NList []

    print (list1 == list2)  -- False
    print (list1 < list2)   -- True (2.0 < 4.5)
    print (list1 > list3)   -- True (2.0 > 0.0)

    -- 5
    let safeDiv :: Int -> Result Int
        safeDiv x = if x == 0 then Error "Division by zero" else OK (10 `div` x)

        increment :: Int -> Result Int
        increment x = OK (x + 1)

        composedFunction :: Int -> Result Int
        composedFunction = composeResult safeDiv increment

    print $ composedFunction 2    -- OK 6 (10 `div` 2 = 5, increment 5 -> 6)
    print $ composedFunction 0    -- Error "Division by zero"
    print $ composedFunction 5    -- OK 3 (10 `div` 5 = 2, increment 2 -> 3)

    -- 6
    print $ goldbach 10    -- True (checks 4, 6, 8, 10)
    print $ goldbach 50    -- True (checks even numbers from 4 to 50)
    print $ goldbach 100   -- True (checks even numbers from 4 to 100)

    -- 7
    let stream1 :: Stream Int
        stream1 = streamIterate (+1) 1  -- Stream of integers: 1, 2, 3, 4, ...

        stream2 :: Stream Int
        stream2 = streamIterate (*2) 1  -- Stream of powers of 2: 1, 2, 4, 8, ...

        -- Test streamToList
        testToList :: [Int]
        testToList = take 10 (streamToList stream1) -- Convert stream1 to a list and take the first 10 elements

        -- Test streamInterleave
        testInterleave :: [Int]
        testInterleave = take 10 (streamToList (streamInterleave stream1 stream2)) -- Interleave stream1 and stream2 and take the first 10 elements

    print testToList -- [1,2,3,4,5,6,7,8,9,10]
    print testInterleave -- [1,1,2,2,3,4,4,8,5,16]

-- 1
data GTree a = Leaf a | Gnode [GTree a]
    deriving (Show)
depth :: GTree a -> Int
depth (Leaf _) = 1
depth (Gnode subtrees) = 1 + maximum (map depth subtrees)

elementOccurs :: Eq a => a -> GTree a -> Bool
elementOccurs x (Leaf y) = x == y
elementOccurs x (Gnode subtrees) = any (elementOccurs x) subtrees

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf x) = Leaf (f x)
mapGTree f (Gnode subtrees) = Gnode (map (mapGTree f) subtrees)

-- 2
data Expr a = Lit a
            | EVar Var
            | Op (Ops a) [Expr a]

type Ops a = [a] -> a
type Var = Char

type Valuation a = Var -> a

eval :: Valuation a -> Expr a -> a
eval val (Lit x) = x
eval val (EVar v) = val v
eval val (Op op args) = op (map (eval val) args)

-- 3
data RegExp
    = Empty                 -- Matches the empty string
    | Literal Char          -- Matches a specific character
    | Concat RegExp RegExp  -- Concatenation of two regex patterns
    | Alt RegExp RegExp     -- Alternation (p | q)
    | Star RegExp           -- Zero or more occurrences of a pattern
    deriving (Show)

-- Matches zero or one occurrence of the pattern p
option :: RegExp -> RegExp
option p = Alt Empty p

-- Matches one or more occurrences of the pattern p
plus :: RegExp -> RegExp
plus p = Concat p (Star p)

-- 4
newtype NumList a = NList [a] deriving (Show)

average :: (Fractional a) => [a] -> a
average [] = 0
average xs = sum xs / fromIntegral (length xs)

instance (Fractional a, Eq a) => Eq (NumList a) where
    (NList xs) == (NList ys) = average xs == average ys

instance (Fractional a, Ord a) => Ord (NumList a) where
    compare (NList xs) (NList ys) = compare (average xs) (average ys)

-- 5
data Result a = OK a | Error String
    deriving (Show)

composeResult :: (a -> Result b) -> (b -> Result c) -> (a -> Result c)
composeResult f g x =
    case f x of
        Error msg -> Error msg
        OK y -> g y

-- 6
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []

canBeGoldbach :: Integer -> Bool
canBeGoldbach n = any (\p -> (n - p) `elem` takeWhile (<= n) primes) (takeWhile (<= n `div` 2) primes)

goldbach :: Integer -> Bool
goldbach n = all canBeGoldbach [x | x <- [4..n], even x]

-- 7
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f seed = Cons seed (streamIterate f (f seed))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)
