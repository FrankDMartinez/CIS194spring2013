-- convert positive `Integers` into a list of digits; for 0 or
-- negative inputs, `toDigits` should return an empty list
toDigits    :: Integer -> [Integer]
toDigits n
  | n <= 0  = []
  | otherwise = toDigits (n `div`10) ++ [n `mod` 10]

-- the same as `toDigits` but with the digits reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)
