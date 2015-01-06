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
