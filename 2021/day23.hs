-- stack --resolver lts-18.18 script

module Main where

import Data.List (elemIndex)
import Data.Maybe (catMaybes)

data Input = Input
  { rStart :: Int,
    rStop :: Int,
    current :: [[Int]]
  }
  deriving (Show, Eq)

costs = [1, 10, 100, 1000]

parse :: String -> Input
parse s = case lines s of
  (_ : c : m : r : _) -> Input 1 (length c - 1) [pl m, pl r]
  _ -> error "bad input"
  where
    pl l = catMaybes $ elemIndex <$> "ABCD" <*> [l]

-- >>> solve  $parse "#############\n#...........#\n###C#B#A#D###\n  #C#D#A#B#\n  #########"
-- Input {rStart = 1, rStop = 12, current = [[7,5,3,9],[7,9,3,5]]}

-- solve :: Input -> (Int, Int)
solve i = i

main :: IO ()
main = readFile "input/day23.txt" >>= print . solve . parse
