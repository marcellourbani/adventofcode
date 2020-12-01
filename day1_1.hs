module Main where

parse:: String -> [Int]
parse= fmap read . words

solve:: [Int] -> Int
solve l = uncurry (*) $ head candidates where 
    candidates  = [(x,y)| k<- zip l [1..],let x=fst k ,y <- drop (snd k) l,x+y==2020]
    

main :: IO ()
main = interact $ show.solve.parse
