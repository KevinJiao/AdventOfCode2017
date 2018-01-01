import System.IO
import Data.Char
import Data.List.Split

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    (putStr . show ) test
    (putStr . show . checksum) contents
    putStr "\n"
    (putStr . show . checksum2) contents
    hClose handle

checksum :: String -> Int
checksum input = 
    sum (map rowsum (map getrow (splitOn "\n" input)))

checksum2 :: String -> Int
checksum2 input = 
    sum (map rowdiv (map getrow (splitOn "\n" input)))

getrow :: String -> [Int]
getrow input = 
    map (read:: String -> Int) (splitOn "\t" input)

rowsum :: [Int] -> Int
rowsum row = 
    maximum row - minimum row

rowdiv :: [Int] -> Int
rowdiv row = 
    sum [div x y | x <- row, y <- row, mod x y == 0, x /= y]


test :: Bool
test = and
    [
        getrow "5\t1\t9\t5" == [5, 1, 9, 5],
        rowsum [5, 1, 9, 5] ==8,
        rowdiv [5, 9, 2, 8] == 4,
        checksum "5\t1\t9\t5\n7\t5\t3\n2\t4\t6\t8" == 18
    ]
