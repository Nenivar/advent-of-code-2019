import System.IO

fuel :: Int -> Int
fuel x = (x `div` 3) - 2

main = do
    text <- readFile "input.txt"
    let input = lines text

    let fuelvals x = until (\xs -> head xs <= 0) (\xs -> fuel (head xs) : xs) [fuel x]
    let ans2 = sum (map (sum . tail . fuelvals . read) input)
    putStr (show ans2)