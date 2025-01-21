#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.Bits
import Data.Foldable (find, for_)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

data OpCode = AND | OR | XOR deriving (Show, Read, Eq)

data Operation = Operation {opCode :: OpCode, opOperands :: (String, String), opRes :: String}

instance Show Operation where
  show (Operation o (a, b) r) = r <> " = " <> a <> " " <> show o <> " " <> b

data Device = Device {deState :: M.Map String Int, deOps :: [Operation]} deriving (Show)

data Expression = Wire String | Op OpCode Expression Expression

instance Show Expression where
  show (Wire s) = s
  show (Op o a b) = sc a <> " " <> show o <> " " <> sc b
    where
      sc (Wire s) = s
      sc x = '(' : show x <> ")"

parse :: String -> Device
parse s = Device vars $ parseo <$> lines ops
  where
    [vs, ops] = splitOn "\n\n" s
    vars = M.fromList $ parsev <$> lines vs
    parsev a = (take 3 a, read $ drop 4 a)
    parseo a = Operation (read op) (o1, o2) r where [o1, op, o2, _, r] = words a

applyOpCode :: OpCode -> Int -> Int -> Int
applyOpCode oc a b = case oc of
  AND -> a .&. b
  OR -> a .|. b
  XOR -> xor a b

evolve :: Device -> Device
evolve d@(Device st ops) = d {deState = M.union rs st}
  where
    runop (Operation oc (o1, o2) r) = (r,) <$> (applyOpCode oc <$> M.lookup o1 st <*> M.lookup o2 st)
    rs = M.fromList $ mapMaybe runop ops

normalize :: Expression -> Expression
normalize e = case e of
  Wire _ -> e
  Op oc e1 e2
    | depth e2 < depth e1 -> Op oc e2' e1'
    | depth e2 == depth e1 && mino e2 < mino e1 -> Op oc e2' e1'
    | otherwise -> Op oc e1' e2'
    where
      swapped = Op oc
      e2' = normalize e2
      e1' = normalize e1
      depth ex = case ex of
        Wire _ -> 1
        Op _ e1 e2 -> 1 + max (depth e1) (depth e2)
      mino ex = case ex of
        Wire w -> w
        Op _ e1 e2 -> min (mino e1) (mino e2)

part1 :: Device -> Int
part1 d@(Device st ops) = sum $ uncurry (*) <$> zip (go d) ((2 ^) <$> [0 ..])
  where
    targets = sort $ filter ((== 'z') . head) $ opRes <$> ops
    done dev = all (`M.member` deState dev) targets
    go dev
      | done dev = (deState dev M.!) <$> targets
      | otherwise = go $ evolve dev

toExp :: Device -> String
toExp d@(Device st ops) = unlines $ show <$> zip targets (toex <$> targets)
  where
    targets = sort $ filter ((== 'z') . head) $ opRes <$> ops
    origs = M.fromList $ zip (opRes <$> ops) ops
    toex op
      | head op `elem` "xy" = Wire op
      | otherwise = Op oc (toex a) (toex b)
      where
        o@(Operation oc (a, b) _) = origs M.! op

part2 :: Device -> (Device, String)
part2 d@(Device st ops) = (d {deOps = ops'}, sws)
  where
    (ops', sws) = go reltargs icarr origs []
    targets = sort $ filter ((== 'z') . head) $ opRes <$> ops
    -- assume first 2 and last are safe, as pattern differs going forward
    reltargs = init $ drop 2 targets
    origs = M.fromList $ zip (opRes <$> ops) ops
    icarr = fromJust $ byop origs XOR "x01" "y01" >>= byop2 origs XOR
    matchO o a b (Operation op (x, y) t) = op == o && (x == a && y == b || x == b && y == a)
    matchO1 o a (Operation op (x, y) t) = op == o && (x == a || y == a)
    byop opm o a b = opRes <$> find (matchO o a b) opm
    byop1 opm o a = opRes <$> find (matchO1 o a) opm
    byop2 opm o a = case find (matchO1 o a) opm of
      Nothing -> Nothing
      Just (Operation _ (x, y) _) -> Just $ if x == a then y else x
    swapops om a b = M.insert b ob $ M.insert a oa om
      where
        oa = (om M.! b) {opRes = a}
        ob = (om M.! a) {opRes = b}
    go targs carr opm swaps = case targs of
      [] -> (snd <$> M.toList opm, intercalate "," $ sort swaps)
      t : ts -> case (b2, carr', b1, c1) of
        (Just r, _, _, _) | head r /= 'z' -> go targs carr (swapops opm r t) (r : t : swaps)
        (_, Nothing, Just a, Just b) -> go ts carr (swapops opm a b) (a : b : swaps)
        (_, Just ca', _, _) -> go ts carr opm swaps
        where
          useOp v (Operation op (x, y) _) = op == XOR && (v == x || v == y)
          pf = tail t
          b1 = byop opm XOR ('x' : pf) ('y' : pf)
          carr' = b1 >>= byop2 opm XOR
          b2 = b1 >>= byop1 opm XOR
          c1 = byop opm AND ('x' : pf) ('y' : pf)

analyse :: Device -> [Expression]
analyse d@(Device st ops) = go srcs targets origs
  where
    targets = sort $ filter ((== 'z') . head) $ opRes <$> ops
    tos (Operation _ (a, b) _) = [a, b]
    srcs = S.fromList $ filter ((`elem` "xy") . head) $ ops >>= tos
    origs = M.fromList $ zip (opRes <$> ops) ops
    toex known op os
      | S.member op known = (Wire op, known)
      | otherwise = (normalize $ Op oc oa ob, known')
      where
        (Operation oc (a, b) _) = os M.! op
        (oa, ka) = toex known a os
        (ob, kb) = toex known b os
        known' = S.unions [S.singleton op, ka, kb]
    go known targs os = case targs of
      [] -> []
      t : ts -> op : go known' ts os
        where
          (op, known') = toex known t os

-- >>> filter ((`elem` "xy") . head) $ ["x01","aaa"]
-- ["x01"]

-- >>> solve $ parse "x00: 1\nx01: 1\nx02: 1\ny00: 0\ny01: 1\ny02: 0\n\nx00 AND y00 -> z00\nx01 XOR y01 -> z01\nx02 OR y02 -> z02"
-- >>> part1 $ parse "x00: 1\nx01: 0\nx02: 1\nx03: 1\nx04: 0\ny00: 1\ny01: 1\ny02: 1\ny03: 1\ny04: 1\n\nntg XOR fgs -> mjb\ny02 OR x01 -> tnw\nkwq OR kpj -> z05\nx00 OR x03 -> fst\ntgd XOR rvg -> z01\nvdt OR tnw -> bfw\nbfw AND frj -> z10\nffh OR nrd -> bqk\ny00 AND y03 -> djm\ny03 OR y00 -> psh\nbqk OR frj -> z08\ntnw OR fst -> frj\ngnj AND tgd -> z11\nbfw XOR mjb -> z00\nx03 OR x00 -> vdt\ngnj AND wpb -> z02\nx04 AND y00 -> kjc\ndjm OR pbm -> qhw\nnrd AND vdt -> hwm\nkjc AND fst -> rvg\ny04 OR y02 -> fgs\ny01 AND x02 -> pbm\nntg OR kjc -> kwq\npsh XOR fgs -> tgd\nqhw XOR tgd -> z09\npbm OR djm -> kpj\nx03 XOR y03 -> ffh\nx00 XOR y04 -> ntg\nbfw OR bqk -> z06\nnrd XOR fgs -> wpb\nfrj XOR qhw -> z04\nbqk OR frj -> z07\ny03 OR x01 -> nrd\nhwm AND bqk -> z03\ntgd XOR rvg -> z12\ntnw OR pbm -> gnj"
-- (4,(Device {deState = fromList [("x00",1),("x01",1),("x02",1),("y00",0),("y01",1),("y02",0)], deOps = [z00 = x00 AND y00,z01 = x01 XOR y01,z02 = x02 OR y02]},""),Device {deState = fromList [("x00",1),("x01",1),("x02",1),("y00",0),("y01",1),("y02",0)], deOps = [z00 = x00 AND y00,z01 = x01 XOR y01,z02 = x02 OR y02]})
-- 2024

solve :: Device -> (Int, (Device, String), Device)
solve l = (p1, p2, l)
  where
    p1 = part1 l
    p2 = part2 l

showResults :: (Int, (Device, String), Device) -> IO ()
showResults (p1, (l1, p2), l) = do
  for_ (analyse l) print
  print "---------------------------"
  for_ (analyse l1) print
  print p1
  putStrLn p2

main :: IO ()
main = readFile "input/day24.txt" >>= showResults . solve . parse
