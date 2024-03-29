-- stack --resolver lts-18.18 script

module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data Image = Image {minx :: Int, miny :: Int, maxx :: Int, maxy :: Int, bgc :: Int, image :: M.Map (Int, Int) Int}

type Algo = M.Map Int Char

type Input = (Algo, Image)

instance Show Image where
  show (Image minx miny maxx maxy _ i) = show (minx, miny, maxx, maxy) <> "\n" <> unlines (l <$> [miny .. maxy])
    where
      l y = [if M.lookup (x, y) i == Just 1 then '#' else '.' | x <- [minx .. maxx]]

parse :: String -> Input
parse s = (algo, Image 0 0 (length $ head ls) (length ls) 0 image)
  where
    (a, g) = case splitOn "\n\n" s of
      [a, g] -> (a, g)
      _ -> error "parse"
    algo = M.fromList $ zip [0 ..] a
    ls = lines g
    ptoi v = if v == '#' then 1 else 0
    image = M.fromList $ [((x, y), 1) | (y, l) <- zip [0 ..] ls, (x, p) <- zip [0 ..] l, ptoi p /= 0]

point :: Image -> (Int, Int) -> Int
point (Image lbx lby ubx uby bg img) idx@(x, y)
  | x >= lbx && x <= ubx && y >= lby && y <= uby = fromMaybe 0 $ M.lookup idx img
  | otherwise = bg

algIndex :: Image -> (Int, Int) -> Int
algIndex img (x, y) = sum values
  where
    neighbors = [(a, b) | b <- [y -1 .. y + 1], a <- [x -1 .. x + 1]]
    digits = point img <$> neighbors
    values = uncurry (*) <$> zip p2s digits
    p2s = (2 ^) <$> [8, 7 .. 0]

nextImage :: Algo -> Image -> Image
nextImage algo img = Image (minx -1) (miny -1) (maxx + 1) (maxy + 1) bc' image'
  where
    (Image minx miny maxx maxy bc image) = img
    bc' = if algo M.! (511 * bc) == '#' then 1 else 0
    points = [(x, y) | x <- [minx -1 .. maxx + 1], y <- [miny -1 .. maxy + 1]]
    image' = M.fromList [(p, 1) | p <- points, let v = M.lookup (algIndex img p) algo, v == Just '#']

numPoints :: Image -> Int
numPoints (Image _ _ _ _ _ i) = M.size i

-- >>> solve  $parse "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###"
-- (35,3351)

solve :: Input -> (Int, Int)
solve (algo, image) = (p1, p2)
  where
    p1 = numPoints (newimages !! 2)
    p2 = numPoints (newimages !! 50)
    newimages = iterate (nextImage algo) image

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve . parse
