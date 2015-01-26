{-# OPTIONS_GHC -Wall #-}
module Golf where

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
