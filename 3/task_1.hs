import Data.List
import Text.Parsec
import Data.Char
import Text.Parsec.String
import Control.Applicative ((<|>))
import Data.Set

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

genPath :: (Int, Int) -> Dir Int -> [(Int, Int)]
genPath (sx, sy) (L n) = [(sx - x, sy)     | x <- [1..n]]
genPath (sx, sy) (R n) = [(sx + x, sy)     | x <- [1..n]]
genPath (sx, sy) (U n) = [(sx,     sy + y) | y <- [1..n]]
genPath (sx, sy) (D n) = [(sx,     sy - y) | y <- [1..n]]

genPaths :: [Dir Int] -> [(Int, Int)]
genPaths xs = Data.List.foldl (\acc x -> acc ++ (genPath (last acc) x)) [(0,0)] xs

-- assume central point is (0,0)
distance :: (Int, Int) -> Int
distance (x,y) = (abs x) + (abs y)

parseLine :: String -> [Dir Int]
parseLine s = case (parse allDir "" s) of
                Left err  -> [(L 0)]
                Right dir -> dir

main = do
    -- let exPath1 = genPaths [R 8, U 5, L 5, D 3]
    -- let exPath2 = genPaths [U 7, R 6, D 4, L 4]
    -- let exInter = intersect exPath1 exPath2
    -- let dists = drop 1 (map distance exInter)
    -- let closest = head $ sort dists
    text <- readFile "input.txt"
    let input = lines text

    let path1 = fromList $ genPaths (parseLine $ input !! 0)
    let path2 = fromList $ genPaths (parseLine $ input !! 1)

    let inter = intersection path1 path2
    let dists = Data.Set.map distance inter
    let closest = sort (toList dists)
    putStr ((show (closest)) ++ "\n")