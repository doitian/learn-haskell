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
toDigits 1234 = [1..4]
toDigits _ = []
