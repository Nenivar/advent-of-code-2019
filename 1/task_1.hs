import System.IO

fuel :: Int -> Int
fuel x = (x `div` 3) - 2

main = do
    text <- readFile "input.txt"
    let ans = foldr1 (+) (map (fuel . read) (lines text))
    putStr (show ans)