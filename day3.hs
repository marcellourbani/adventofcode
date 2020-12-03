#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where
sampleMap = fmap cycle
    ["..##......."
    ,"#...#...#.."
    ,".#....#..#."
    ,"..#.#...#.#"
    ,".#...##..#."
    ,"..#.##....."
    ,".#.#.#....#"
    ,".#........#"
    ,"#.##...#..."
    ,"#...##....#"
    ,".#..#...#.#"]

data Trajectory = Trajectory Int Int deriving Show
data Position = Position Int Int deriving Show

startPos = Position 0 0
sartTrajectory = Trajectory 3 1
trajectories =
    [Trajectory 1 1
    ,Trajectory 3 1
    ,Trajectory 5 1
    ,Trajectory 7 1
    ,Trajectory 1 2]
-- >>> hasTree sampleMap $ Position 3 0
-- 1
hasTree :: [String] -> Position -> Int
hasTree wmap (Position x y ) =  fromEnum ( wmap !! y !! x == '#')

-- >>> nextPos startPos sartTrajectory
-- Position 3 1
nextPos :: Position -> Trajectory -> Position
nextPos  (Position x y)  (Trajectory vx vy) = Position  (x + vx)  (y + vy)

-- >>> sum $ trajectoryHits sampleMap startPos sartTrajectory
-- 7
trajectoryHits :: [String] -> Position -> Trajectory -> [Int]
trajectoryHits wmap (Position x y) v
  | y >= length wmap = []
  | otherwise = hasTree wmap pos : trajectoryHits wmap next v
  where pos = Position x y
        next = nextPos pos v

-- >>> product $ trajectoriesHits sampleMap startPos trajectories
-- 336
trajectoriesHits :: [String] -> Position -> [Trajectory] -> [Int]
trajectoriesHits world pos trajlist = sum . trajectoryHits world pos <$> trajlist

-- >>> solve sampleMap
-- (7,336)
solve:: [String] -> ( Int, Int)
solve world = (hits,multhits) where
    hits = sum $ trajectoryHits world startPos sartTrajectory
    multhits = product $ trajectoriesHits world startPos trajectories


main :: IO ()
main = interact $ show.solve.fmap cycle.lines
