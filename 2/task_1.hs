import Data.Sequence
import Data.List.Split

-- takes intcode, position, operation -> intcode
operation :: (Int -> Int -> Int) -> Int -> Seq Int -> Seq Int
operation f n xs = update m3 vv xs
    where m1 = xs `index` (n + 1)
          m2 = xs `index` (n + 2)
          m3 = xs `index` (n + 3)
          v1 = xs `index` m1
          v2 = xs `index` m2
          vv = f v1 v2

-- takes intcode, position -> intcode
exec :: Int -> Seq Int -> Seq Int
exec n xs
    -- add
    | opcode == 1 = operation (+) n xs
    -- mult
    | opcode == 2 = operation (*) n xs
    -- halt
    | otherwise   = xs
    where opcode = xs `index` n

run :: Int -> Seq Int -> Seq Int
run n xs = if (v == 99) then r else (run (n + 4) r)
    where r = exec n xs
          v = xs `index` n

main = do
    text <- readFile "input.txt"
    let strcode = fromList (splitOn "," text)
    let strcode2 = update 2 "2" (update 1 "12" strcode)
    let execcode = run 0 (fmap read strcode2)
    let ans1 = execcode `index` 0
    putStr ((show ans1) ++ "\n")
    
    --let test = run 0 (fromList [2,4,4,5,99,0])
    --putStr ((show test) ++ "\n")