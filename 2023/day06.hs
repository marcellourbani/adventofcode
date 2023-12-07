#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

parse :: String -> [(Int, Int)]
parse n = zip (read <$> t) (read <$> s) where [t, s] = tail . words <$> lines n

distance :: Int -> Int -> Int
distance press totTime = press * (totTime - press)

limits :: Float -> Float -> (Float, Float)
limits tottime goal = ((tottime - d) / 2, (tottime + d) / 2) where d = sqrt (tottime * tottime - 4 * goal)

beatRecord :: (Int, Int) -> Int
beatRecord (time, goal) = length [t | t <- [1 .. time - 1], distance t time > goal]

beatRecord2 :: (Int, Int) -> Int
beatRecord2 (time, goal) = b' - a' + 1
  where
    (a, b) = limits (fromIntegral time) (fromIntegral goal) -- got close with equation
    go d x
      | distance (x + d) time > goal = go d (x + d)
      | otherwise = x
    a' = go (-1) (floor a + 10) -- search neighbourhood for exact solution - sqrt approximation turned out to be about 3
    b' = go 1 (floor b - 10)

-- >>> solve $ parse "Time:      7  15   30\nDistance:  9  40  200"
-- (288,71503)
solve :: [(Int, Int)] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = product $ beatRecord <$> l
    t2 = concatMap (show . fst) l
    d2 = concatMap (show . snd) l
    p2 = beatRecord2 (read t2, read d2)

-- (a, b) = beatRecord2 (read t2, read d2)
-- p2 = (distance a (read d2), distance (b + 1) b)

main :: IO ()
main = readFile "input/day06.txt" >>= print . solve . parse
