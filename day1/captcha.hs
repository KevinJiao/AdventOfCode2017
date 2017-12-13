import System.IO
import Data.Char

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStr (show test)
    (putStr . show . captcha) (filter (/= '\n') contents)
    (putStr . show . captcha2) (filter (/= '\n') contents)    
    hClose handle
    
shift :: [a] -> Int -> [a]
shift list n = drop n list ++ take n list

shiftOne :: [a] -> [a]
shiftOne list = shift list 1

captcha :: String -> Int
captcha input =
    let
        digits = map digitToInt input
        shiftedDigits = shiftOne digits
        matches = getMatching digits shiftedDigits
    in
        sum matches

captcha2 :: String -> Int
captcha2 input = 
    let
        digits = map digitToInt input
        n = quot (length digits) 2
        shiftedDigits = shift digits n
        matches = getMatching digits shiftedDigits
    in
        sum matches


getMatching ::[Int] -> [Int] -> [Int]
getMatching xs ys = 
    [fst x | x <- zip xs ys, fst x == snd x]

test :: Bool
test = and
    [
        3 == captcha "1122",
        4 == captcha "1111",
        0 == captcha "1234",
        9 == captcha "91212129"
    ]