{-# OPTIONS_GHC -Wall #-}
module Homework4 where

import Data.List

-- multiplies together all even values in a given list of
-- `Integers`; if no even values appear in the list, 1 is
-- returned
fun1' :: [Integer] -> Integer
fun1' a = product $ filter even a

-- takes an `Integer` and returns a sum of all even values in the
-- corresponding "hailstone sequence"
fun2' :: Integer -> Integer
fun2' a = sum $ filter even $ takeWhile (/=1) $ iterate hailstone a
          where hailstone b = if even b
                              then b `div` 2
                              else 3*b+1

data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

-- creates a balanced binary tree from a list of values
foldTree :: [a] -> Tree a
foldTree b = foldr addToBalancedTree Leaf b

-- takes as input a balanced `Tree` and an element to add and returns
-- a balanced `Tree` with the new element included
addToBalancedTree :: a -> Tree a -> Tree a
addToBalancedTree a Leaf = Node 0 Leaf a Leaf
addToBalancedTree a (Node _ b c d) = Node height newB c newD
                                     where heightB = heightFunction b
                                           heightD = heightFunction d
                                           newB = if heightB >= heightD
                                                  then b
                                                  else addToBalancedTree a b
                                           newD = if heightB >= heightD
                                                  then addToBalancedTree a d
                                                  else d
                                           newHeightB = heightFunction newB
                                           newHeightD = heightFunction newD
                                           height = (max newHeightB newHeightD) + 1

-- takes as input a `Tree` and returns the `Tree`s height; a `Leaf`
-- is considered to have a height of -1
heightFunction :: Tree a -> Integer
heightFunction Leaf = -1
heightFunction (Node height _ _ _) = height

-- takes a `List` of `Bool`s and returns a `Bool` indicating whether or
-- not the given `List` contains an odd number of `True` values
xor :: [Bool] -> Bool
xor = foldr (/=) False

-- takes a function and a `List` and returns a `List` with the given
-- function applied to each element of the given `List`
map' :: (a->b) -> [a] -> [b]
map' f = foldr (\c resultSoFar -> f c : resultSoFar) []

-- takes two `List`s and returns the Cartesian product of them
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- takes an `Int` and returns a list of odd primes greater than or
-- equal to 3 and less than 2 more than twice the given `Int`
sieve :: Int -> [Int]
sieve n = map bump ([1..n] \\ map summation (filter (\b -> uncurry (<=) b && summation b <= n) (cartProd [1..n] [1..n])))
  where summation c = uncurry (+) c + 2 * uncurry (*) c
        bump d = 2*d + 1

{-
- Note: while the above solution uses the `cartProd` function
- mentioned in the assignment, Someone did suggest using the
- following instead:
-
- sundaram :: Integer -> [Integer]
- sundaram n = 2 : map bump remaining
-   where
-   bump x    = 1+2*x
-   remaining = [1..n] \\ crossout
-   crossout  = do i <- [1..n]
-                  takeWhile (<= n) [i+j+2*i*j | j <- [i..]]
-
- The idea being: naming more things and using less `uncurry`.
- Granted, this solution appears to use some features which have not
- been taught in the CIS194 at this point.
-}
