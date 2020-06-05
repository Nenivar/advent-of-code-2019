import Data.Tree

findNonEmpty :: [Maybe (Tree String)] -> Maybe (Tree String)
findNonEmpty []        = Nothing
findNonEmpty (Nothing) = Nothing
findNonEmpty (x:xs)    = findNonEmpty xs

findNode :: Tree String -> String -> Maybe (Tree String)
findNode (Node r []) v = if r == v then Just (Node r []) else Nothing
findNode (Node r s) v = if r == v then (Node r s) else (findNonEmpty (findNode s v))

addNode :: Tree String -> String -> String -> Tree String 
addNode (Node r s) k v = (Node r s)

main :: IO ()
main = do
    let d = (1)
    putStr "dog\n"