module Golf where

-- |
-- >>> skips []
-- []
--
-- >>> skips [1]
-- [[1]]
--
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
skips :: [a] -> [[a]]
skips xs = [takeThenSkip (drop step xs) step | step <- [0 .. (length xs - 1)]]
  where
    takeThenSkip [] _ = []
    -- Take the first, skip n, then repeat
    takeThenSkip (y : ys) n = y : takeThenSkip (drop n ys) n

-- |
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
--
-- >>> localMaxima [2,3,4,1,5]
-- [4]
--
-- >>> localMaxima [1,2,3,4,5]
-- []
localMaxima :: [Integer] -> [Integer]
localMaxima (x : xs@(y : ys@(z : _)))
  | x < y && z < y = y : localMaxima ys
  | otherwise = localMaxima xs
localMaxima _ = []

-- |
-- >>> let histogram' = unlines . map (++".") . lines . histogram
-- >>> putStr $ histogram' [1,1,1,5]
--  *        .
--  *        .
--  *   *    .
-- ==========.
-- 0123456789.
histogram :: [Integer] -> String
histogram xs = unlines (draw (buildBuckets xs) ++ footer)
  where
    buildBuckets xs = [length (filter (e ==) xs) | e <- [0 .. 9]]
    footer = ["==========", "0123456789"]
    draw buckets = map (drawLine buckets) (reverse [1 .. maximum buckets])
    drawLine buckets line = map (mark line) buckets
    mark target count
      | count >= target = '*'
      | otherwise = ' '
