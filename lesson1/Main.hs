-- convert positive `Integers` into a list of digits; for 0 or
-- negative inputs, `toDigits` should return an empty list
toDigits    :: Integer -> [Integer]
toDigits a
  | a <= 0  = []
  | otherwise = toDigits (a `div`10) ++ [a `mod` 10]

-- the same as `toDigits` but with the digits reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev a = reverse (toDigits a)

-- doubles every other number beginning from the right (e.g.,
-- the "second-to-last", "fourth-to-last", ... numbers are
-- doubled)
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther a = reverse (doubleEveryOtherHelper (reverse a))

-- doubles every other number beginning from the left (e.g., the
-- "second", "fourth", ... numbers are doubled)
doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper []         = []
doubleEveryOtherHelper (a:[])        = [a]
doubleEveryOtherHelper (a:(b:cs)) = (a) : (2*b) : (doubleEveryOtherHelper cs)

-- sums all the digits of each number in a given list (e.g.,
-- [16,7,12,5] => 1 + 6 + 7 + 1 + 2 + 5 => 22
sumDigits :: [Integer] -> Integer
sumDigits as = sum [ sum (toDigits a) | a <- as ]

-- indicates whether an `Integer` could be a valid credit card number
validate :: Integer -> Bool
validate a = (sumDigits (doubleEveryOther (toDigits a)) `mod` 10) == 0
