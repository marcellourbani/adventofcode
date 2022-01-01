-- stack --resolver lts-18.18 script

module Main where

import Control.Monad.ST (ST, runST)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.HashTable.Class (HashTable (insert), toList)
import qualified Data.HashTable.ST.Cuckoo as H
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Vector as V
import Debug.Trace (trace)

type Grid = V.Vector (V.Vector Int)

data Input = Input {maxx :: Int, maxy :: Int, grid :: Grid} deriving (Show)

gridAt :: Grid -> Int -> Int -> Int
gridAt g x y = g V.! y V.! x

parse :: String -> Input
parse s = Input {maxx = mx, maxy = my, grid = V.fromList $ V.fromList <$> depthlist}
  where
    depthlist = map (read . (: [])) <$> lines s
    my = length depthlist - 1
    mx = length (head depthlist) - 1

extend :: Input -> Input
extend (Input mx my gr) = Input mx' my' gr''
  where
    mx' = 5 * (mx + 1) -1
    my' = 5 * (my + 1) -1
    w = mx + 1
    h = my + 1
    trim x = ((x - 1) `mod` 9) + 1
    updLine vec i = trim . (+ i) <$> vec
    extendLine vec = vec V.++ V.concat (updLine vec <$> [1 .. 4])
    gr' = extendLine <$> gr
    updRows g i = updLine <$> g <*> pure i
    gr'' = gr' V.++ V.concat (updRows gr' <$> [1 .. 4])

type DKV = ((Int, Int), (Int, Bool))

type DMap s = H.HashTable s (Int, Int) (Int, Bool)

mincandidate :: DMap s -> ST s (Maybe ((Int, Int), Int))
mincandidate = H.foldM go Nothing
  where
    go a (k, (val, visited)) = return $ case (visited, a) of
      (True, _) -> a
      (False, Nothing) -> Just (k, val)
      (False, Just (_, old))
        | old > val -> Just (k, val)
        | otherwise -> a

insValues :: [DKV] -> ST s (DMap s) -> ST s (DMap s)
insValues l m1 = case l of
  [] -> m1
  ((k, v) : xs) -> do
    m <- m1
    c <- H.lookup m k
    case c of
      Nothing -> do
        H.insert m k v
        insValues xs m1
      _ -> insValues xs m1

dijkstraMC :: Input -> M.Map (Int, Int) Int
dijkstraMC (Input mx my gr) = fst <$> M.fromList (runST $ go m)
  where
    m = do
      h <- H.newSized $ (mx + 1) * (my + 1)
      H.insert h (0, 0) (0, False)
      pure h
    go dmm = do
      dm <- dmm
      mc <- mincandidate dm
      case mc of
        Nothing -> toList dm
        Just ((cx, cy), vnv) -> do
          H.insert dm (cx, cy) (vnv, True)
          let xs = [max 0 $ cx -1 .. min mx $ cx + 1]
          let ys = [max 0 $cy -1 .. min my $cy + 1]
          let toUpd = [v | x <- xs, y <- ys, let v = (x, y), v /= (cx, cy), x == cx || y == cy]
          let values = (+ vnv) . uncurry (gridAt gr) <$> toUpd
          let kvs = zip toUpd $ zip values $ repeat False
          let _ = trace "foo" kvs
          insValues kvs $ pure dm
          go $ pure dm

-- >>> solve $parse "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"
-- (40,315)

solve :: Input -> (Int, Int)
solve (Input mx my g) = (distances M.! (mx, my), distances2 M.! (mx', my'))
  where
    distances = dijkstraMC (Input mx my g)
    (Input mx' my' g') = extend (Input mx my g)
    distances2 = dijkstraMC (Input mx' my' g')

main :: IO ()
main = readFile "input/day15.txt" >>= print . solve . parse
