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
toDigits = reverse . toDigitsRev

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
  | otherwise = let (q, r) = n `quotRem` 10
                in r : toDigitsRev' q

-- | Double every other num from right to left
--
-- ==== __Examples__
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
--
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith ($) (cycle [id, (*2)]) . reverse
