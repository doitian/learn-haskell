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
  | otherwise =
      let (q, r) = n `quotRem` 10
       in r : toDigitsRev' q

-- | Doubles every other num from left to right
--
-- ==== __Examples__
--
-- >>> doubleEveryOther [8,7,6,5]
-- [8,14,6,10]
--
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id, (* 2)])

-- | Computes sum of all digits.
--
-- ==== __Examples__
--
-- >>> sumDigits [16,7,12,5]
-- 22
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigitsRev)

-- | Validates credit card number.
--
-- ==== __Examples__
--
-- >>> validate 4012888888881881
-- True
--
-- >>> validate 4012888888881882
-- False
validate :: Integer -> Bool
validate n = checksum n `mod` 10 == 0
  where
    checksum = sumDigits . doubleEveryOther . toDigitsRev
