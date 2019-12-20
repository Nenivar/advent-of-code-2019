import Data.List
import Text.Parsec
import Data.Char
import Text.Parsec.String
import Control.Applicative ((<|>))
import Data.Set
import Data.Map (Map)
import qualified Data.Map as Map

data Dir n = L n | R n | U n | D n
    deriving (Show)

number :: Parser Int
number = read <$> many1 digit

dir :: Parser (Dir Int)
dir =                       L <$ (char 'L') <*> number
    Control.Applicative.<|> R <$ (char 'R') <*> number
    Control.Applicative.<|> U <$ (char 'U') <*> number
    Control.Applicative.<|> D <$ (char 'D') <*> number

allDir :: Parser [(Dir Int)]
allDir = sepEndBy dir (char ',')

genPath :: ((Int, Int), Int) -> Dir Int -> [((Int, Int), Int)]
genPath ((sx, sy), sn) (L n) = [((sx - x, sy),     sn + x) | x <- [1..n]]
genPath ((sx, sy), sn) (R n) = [((sx + x, sy),     sn + x) | x <- [1..n]]
genPath ((sx, sy), sn) (U n) = [((sx,     sy + y), sn + y) | y <- [1..n]]
genPath ((sx, sy), sn) (D n) = [((sx,     sy - y), sn + y) | y <- [1..n]]

genPaths :: [Dir Int] -> [((Int, Int), Int)]
genPaths xs = Data.List.foldl (\acc x -> acc ++ (genPath (last acc) x)) [((0,0),0)] xs

-- assume central point is (0,0)
--distance :: (Int, Int) -> Int
--distance (x,y) = (abs x) + (abs y)

parseLine :: String -> [Dir Int]
parseLine s = case (parse allDir "" s) of
                Left err  -> [(L 0)]
                Right dir -> dir

locLess :: ((Int, Int), Int) -> ((Int, Int), Int) -> ((Int, Int), Int)
locLess ((x1, y1), d1) ((x2, y2), d2) = if d1 < d2 then ((x1, y1), d1) else ((x2, y2), d2)

main = do
    -- take first dist
    -- let exPath1 = Map.fromListWith (\d1 d2 -> d2) $ genPaths [R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51]
    -- let exPath2 = Map.fromListWith (\d1 d2 -> d2) $ genPaths [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]

    -- let exInter = Map.intersectionWith (+) exPath1 exPath2

    -- let closest = Data.List.foldl (\acc x -> [locLess (head acc) x]) [((0, 0), 10000)] (tail $ Map.toList exInter)

    --putStr ((show closest) ++ "\n" ++ (show exInter) ++ "\n")

    text <- readFile "input.txt"
    let input = lines text

    let path1 = Map.fromListWith (\d1 d2 -> d2) $ genPaths (parseLine $ input !! 0)
    let path2 = Map.fromListWith (\d1 d2 -> d2) $ genPaths (parseLine $ input !! 1)

    let inter = Map.delete (0, 0) $ Map.intersectionWith (+) path1 path2

    let closest = Data.List.foldl (\acc x -> [locLess (head acc) x]) [((0, 0), 1000000)] (tail $ Map.toList inter)
    putStr ((show (closest)) ++ "\n")