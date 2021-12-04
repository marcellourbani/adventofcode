#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where

data Directions = North | East | South | West deriving (Show,Enum,Eq)
data Opcodes = Northoc | Eastoc | Southoc  | Westoc | Leftoc | Rightoc | Forwardoc
  deriving (Show, Eq,Enum)

data Ship = Ship {seast:: Int,snorth:: Int,sdirection:: Directions } deriving (Show)
data WayPoint = WayPoint {weast:: Int,wnorth:: Int } deriving (Show)

type Move = (Opcodes, Int)

parseOc :: String -> Move
parseOc (x:xs) = (c , read xs) where
  c = case x of
    'N' -> Northoc
    'S' -> Southoc
    'E' -> Eastoc
    'W' -> Westoc
    'L' -> Leftoc
    'R' -> Rightoc
    'F' -> Forwardoc
parseOc _ = (Forwardoc,0)


moveShip:: Ship -> Move -> Ship
moveShip s m = case m of
  (Northoc,x)   -> s {snorth= snorth s + x}
  (Southoc,x)   -> s {snorth= snorth s - x}
  (Eastoc,x)    -> s {seast= seast s + x}
  (Westoc,x)    -> s {seast= seast s - x}
  (Forwardoc,x) -> moveShip s (dir,x)
    where dir = toEnum $ fromEnum $ sdirection s
  (Leftoc,x)    -> s {sdirection = nextd}
    where nextd = toEnum $ mod (fromEnum (sdirection s) - div x 90) 4
  (Rightoc,x)   -> s {sdirection = nextd}
    where nextd = toEnum $ mod (fromEnum (sdirection s) + div x 90) 4


moveWp:: (Ship,WayPoint) -> Move -> (Ship,WayPoint)
moveWp (s,w) m = (ns,nw) where
  ns = case m of
    (Forwardoc,x) -> s {snorth= snorth s + (x * wnorth w),seast= seast s + (x*weast w)}
    _ -> s
  nw = case m of
    (Northoc,x) -> w {wnorth= wnorth w + x}
    (Southoc,x) -> w {wnorth= wnorth w - x}
    (Eastoc,x)  -> w {weast = weast  w + x}
    (Westoc,x)  -> w {weast = weast  w - x}
    (Leftoc,x)  -> rotateWP w $ mod (- (div x 90)) 4
    (Rightoc,x) -> rotateWP w $ mod (div x 90) 4
    _           -> w
  rotateWP wp n
    | n == 0 = wp
    | otherwise = rotateWP (wp {wnorth = - weast wp, weast = wnorth wp }) (n-1)

-- >>> solve "F10\nN3\nF7\nR90\nF11"
-- (25,286)

-- solve :: [Char] -> (Int, Int)
solve s = (first ,second)
  where manhattan sh = abs ( seast sh) + abs ( snorth sh)
        first = manhattan final1
        second = manhattan final2
        ship = Ship 0 0 East
        waypoint = WayPoint 10 1
        moves = parseOc <$> lines s
        final1 = foldl moveShip ship moves
        (final2,_) = foldl moveWp (ship,waypoint) moves

main :: IO ()
main = readFile "input/day12.txt" >>= print.solve
