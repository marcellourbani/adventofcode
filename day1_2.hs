module Main where

parse:: String -> [Int]
parse= fmap read . words

solve:: [Int] -> Int
solve l = result where 
    candidates = [(x,y,z)| k<- zip l [1..],let x=fst k ,y <- drop (snd k) l,z<- drop (snd k + 1) l,x+y+z==2020]
    result = (\(a,b,c)->a * b * c) $ head candidates


main :: IO ()
main = interact $ show.solve.parse
