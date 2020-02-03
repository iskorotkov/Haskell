data Tree a = Branch { value :: a, left :: (Tree a), right :: (Tree a)} | Leaf a

parse s = parseExpr [] (reverse (words s))
 where
  parseExpr :: [a] -> [a] -> Tree a
  parseExpr l []      = parseTerm l
  parseExpr l (x : r) = if x == "+" || x == "-"
    then Branch x (parseExpr r) (parseTerm l)
    else parseExpr (l ++ [x]) r
   where
    parseTerm :: [a] -> Tree a
    parseTerm [x        ] = Leaf x
    parseTerm (l : x : r) = Branch x (parseTerm r) (Leaf l)

--instance (Read a) => Read (Tree a) where
    --read s = []

instance (Show a) => Show (Tree a) where
  show t = ""
