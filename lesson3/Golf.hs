{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List
import Safe

-- produces a list of lists where the x-th list entry in the output
-- contains every x-th element from the input
skips :: [a] -> [[a]]
skips as = skips' 1 as

skips' :: Int -> [a] -> [[a]]
skips' b ds = if b > length ds
              then []
              else skipsEvery ds b : skips' (b+1) ds

skipsEvery :: [a] -> Int -> [a]
skipsEvery as b = skipsEvery' (b-1) b as

skipsEvery' :: Int -> Int -> [a] -> [a]
skipsEvery' b c ds = case ds `atMay` b of
                          Nothing -> []
                          Just e -> e : skipsEvery' (b+c) c ds

{-
Someone pointed out a simpler solution which is more recursion intensive, a point I think on which the lesson is focusing (My commentary is spliced between definitions):
> module Golf where
>
The following function returns the requires list of lists, starting
with the list which skips 0 elements.
> skips :: [a] -> [[a]]
> skips xs = skipsn 0 xs
>
The following function creates a list which skips `n` elements and
every `n` thereafter, as well as joining that list to lists which
skip `n+1` or more elements in likewise fashion until an attempt is
made to skip more elements than there are in the list.
> skipsn :: Int -> [a] -> [[a]]
> skipsn n xs = case drop n xs of
>   [] -> []
>   xs1 -> skip n xs1 : skipsn (n+1) xs
>
The following function creates a list which skips `n` elements and
every `n` thereafter.
> skip :: Int -> [a] -> [a]
> skip n (x:xs) = x : skip n (drop n xs)
> skip n [] = []
To elaborate on what is happening, the code makes a list of lists
which skips 0 elements each time before adding an element to the
list, then skips 1 element each time before adding an element to
another list, then skips 2 elements each time before adding another
element to yet another list, and so on until an attempt is made to
skip more elements than there are in the list. I would like to think
I would have come up with something closer to this level of
compactness had I bothered to write out commentary restating the
requirements of the exercise, a point I feel scores in favor of
literate Haskell.
-}

{-
Though I stated/implied in commentary for the previous exercise above
a desire, if not preference, for literate Haskell, for the duration
of this lesson, I will continue to use traditional Haskell and
reassess afterwards.
-}

-- produces a list of local maxima from a list of `Integer`s where
-- "local maxima" is defined as "an element of a list which is
-- strictly greater than both the elements immediately before and
-- after it"; therefore, the particular element must have elements
-- both before and after it and it must have a value greater than
-- both; elements at the beginning and end of a list are,
-- consequently, excluded
localMaxima :: [Integer] -> [Integer]
localMaxima as@(a:b:c:_) = if (b > a) && (b > c)
                           then b : localMaxima (tail as)
                           else localMaxima (tail as)
localMaxima _ = []

-- takes as input a list of `Int`s between 0 and 9 (inclusive)
-- and outputs a vertical histogram showing how many of each number
-- were in the input list
histogram :: [Int] -> String
histogram as = unlines $ (reverse $ transpose $ fillSpaces $ makeStarStrings $ countEach as ) ++ ["==========","0123456789"]

-- takes as input a list of `Int`s between 0 and 9 (inclusive) and
-- outputs a list of counts of each corresponding index value in the
-- given list
countEach :: [Int] -> [Int]
countEach as = [count b as | b<-[0..9]]

-- takes as input an `Int` and a list of `Int`s and outputs an `Int`
-- representing the number of times the given single `Int` appears in
-- the given list
count :: Int -> [Int] -> Int
count a bs = length [b | b <- bs, b == a]

-- takes as input a list of `Int`s and outputs a list of `String`s
-- where each string has as many '*' characters as the corresponding
-- entry in the list of `Int`s
makeStarStrings :: [Int] -> [String]
makeStarStrings as = map make1StarString as

-- takes as input an `Int` and outputs a `String` containing that
-- many '*' characters
make1StarString :: Int -> String
make1StarString a = replicate a '*'

-- takes as input a list of `String`s or '*' characters and returns a
-- list of the same `String`s with enough ' ' characters appended so
-- each `String` is the same length
fillSpaces :: [String] -> [String]
fillSpaces as = [a ++ replicate (stretch - length a) ' ' | a <- as]
                  where stretch = maximum (map length as)

{-
What follows is an earlier attempt, preserved for comparison.

-- takes as input a list of `Integer`s between 0 and 9 (inclusive)
-- and outputs a veritcal histogram showing how many of each number
-- were in the input list
histogram :: [Integer] -> String
histogram as = unlines (reverse (createLines (createReductions (data2counts as))) ++ ["==========", "0123456789"])

-- takes as input a list of `Integer`s between 0 and 9 (inclusive)
-- and outputs a list of `Integers` representing the number of times
-- the corresponding index value of each element appears in the input
-- list
data2counts :: [Integer] -> [Integer]
data2counts as = countData 0 as

-- takes as input an `Integer` and a list of `Integer`s between 0 and
-- 9 (inclusive) and outputs a list of `Integers` which represent
-- counts of occurences of the given single `Integer` and of all
-- `Integer`s greater than the given single `Integer` up to and
-- including 9
countData :: Integer -> [Integer] -> [Integer]
countData a bs = if a < 9
                    then count a bs : countData (a+1) bs
                    else [count a bs]

-- takes as input an `Integer` and a list of `Integer`s and output a
-- count of the number of times the given single `Integer` appears in
-- the given list
count :: Integer -> [Integer] -> Integer
count a bs = sum [1 | b <- bs, b == a]

-- takes as input a list of lists of `Integer`s and outputs a list of
-- `String`s representing each list of `Integer`s
createLines :: [[Integer]] -> [String]
createLines as = map create1Line as

--takes as input a list of `Integer`s and outputs a `String`
--representing that list
create1Line :: [Integer] -> String
create1Line as = map createChar as

-- takes as input a list of `Integer`s and outputs a list of lists of
-- `Integers` constituting a sequence of reductions (i.e.,
-- subtractions of 1 until values equal 0); a list of all zeroes is
-- not included in the output
createReductions :: [Integer] -> [[Integer]]
createReductions as = takeWhile atLeast1Positive (iterate reduceAll as)

-- takes as input a list of `Integer`s and outputs whether or not any
-- value is greater than 0
atLeast1Positive :: [Integer] -> Bool
atLeast1Positive as = maximum as > 0

-- takes as input an `Integer` and outputs a `Char`; if the `Integer`
-- is 0, a ' ' is output; otherwise, a '*' is output
createChar :: Integer -> Char
createChar a = if a > 0
                then '*'
                else ' '

-- takes as input a list of `Integer`s and outputs the same list with
-- the exception of all positive numbers being reduced by 1
reduceAll :: [Integer] -> [Integer]
reduceAll as = map reduce as

-- takes as input an `Integer` and outputs the greater of 0 or 1 less
-- than the original `Integer`
reduce :: Integer -> Integer
reduce a = max 0 (a-1)
-}
