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
