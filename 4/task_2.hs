import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

toDigits :: Int -> [Int]
toDigits x = map (\x -> read [x]) (show x)

groupAdj :: [Int] -> Map Int Int
groupAdj xs = foldl (\acc x -> if Map.member x acc then Map.adjust (+ 1) x acc else Map.insert x 1 acc) Map.empty xs

isAdj :: [Int] -> Bool
isAdj xs = Map.size (Map.filter (== 2) (groupAdj xs)) > 0

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

    -- let test = groupAdj $ toDigits 123789
    -- let test1 = isAdj $ toDigits 123789

    -- putStr ((show test) ++ ", " ++ (show test1) ++ " test\n")
    putStr ((show ans) ++ "\n")