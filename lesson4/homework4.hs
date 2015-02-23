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

-- when adding an element to a balanced tree:
--  if the tree is a `Leaf`:
--    the tree is `Node 0 Leaf element Leaf`
--  else if the "left" branch is a `Leaf`:
--    add the element to the "left" branch
--    set the height of the current branch to 1
--  else if the "right" branch is a `Leaf`:
--    add the element to the "right" branch
--  else:
--    add the element to the branch with the lesser height, defaulting to the "left" branch
--    set the height of the current branch to 1 more than the the maximum height of either branch




{-
-- takes as input a balanced `Tree` and an element to add and returns
-- a balanced `Tree` with the new element included
addToBalancedTree :: a -> Tree a -> Tree a
addToBalancedTree entry tree = listToTree $ insert entry (treeToList tree)

-- takes as input a balanced `Tree` and returns a list representing
-- the non-`Leaf` nodes of that `Tree`
treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList Node _ node1 value node2 = (treeToList node1) ++ [value] ++ (treeToList node2)

-- takes as input a `List` and returns a balanced `Tree`
listToTree :: [a] -> Tree a
listToTree [] = Leaf
listToTree list = Node height left node right
                  where listLength = length list
                        (leftHalf, rightHalf) = splitAt (listLength `div` 2) list
                        leftList = initSafe leftHalf
                        node = tailSafe leftHalf
                        left = listToTree leftList
                        right = listToTree rightHalf
                        height = (max (heightOf left) (heightOf right)) + 1

-- takes as input a `Tree` and returns its height
heightOf :: Tree a -> Int
heightOf Leaf = -1
heightOf Node height _ _ _ = height
-}
