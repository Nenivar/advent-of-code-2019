import Data.List

toDigits :: Int -> [Int]
toDigits x = map (\x -> read [x]) (show x)

-- test adj
isAdj :: [Int] -> Bool
isAdj ys = [] /= isAdj_ ys
    where
        isAdj_ [x] = []
        isAdj_ (x:xs) = if x == (head xs) then (x:xs) else isAdj_ xs

-- test increase
isSorted :: [Int] -> Bool
isSorted xs = (sort xs) == xs

isCorrect :: [Int] -> Bool
isCorrect xs = isAdj xs && isSorted xs

main :: IO ()
main = do
    let all = [123257..647015]
    let correct = map (isCorrect . toDigits) all
    let ans = length $ filter (== True) correct
    
    putStr ((show ans) ++ "\n")