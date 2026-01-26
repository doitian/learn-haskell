-- | Converts positive Integers to a list of digits
--
-- ==== __Examples__
--
-- >>> toDigits 1234
-- [1,2,3,4]
--
-- >>> toDigits 0
-- []
--
-- >>> toDigits (-17)
-- []
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- | Converts positive Integers to a reverted list of digits
--
-- ==== __Examples__
--
-- >>> toDigitsRev 1234
-- [4,3,2,1]
--
-- >>> toDigitsRev 0
-- []
--
-- >>> toDigitsRev 10
-- [0,1]
--
-- >>> toDigitsRev (-17)
-- []
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = toDigitsRev' n

toDigitsRev' n
  | n < 10 = [n]
  | otherwise = r : toDigitsRev' q where (q, r) = n `quotRem` 10
