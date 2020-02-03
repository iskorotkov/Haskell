data Tree a =
  Branch {value :: a, left :: (Tree a), right :: (Tree a)}
  | Leaf {value :: a}

parseTerm :: [String] -> (Tree String)
parseTerm [x        ] = Leaf x
parseTerm (l : x : r) = Branch x (parseTerm r) (Leaf l)

parse :: String -> (Tree String)
parse s = parseExpr [] (reverse (words s))
 where
  parseExpr :: [String] -> [String] -> (Tree String)
  parseExpr l []      = parseTerm l
  parseExpr l (x : r) = if x == "+" || x == "-"
    then Branch x (parseExpr [] r) (parseTerm l)
    else parseExpr (l ++ [x]) r

--instance (Read a) => Read (Tree a) where
    --read s = []

instance (Show a) => Show (Tree a) where
  show (Leaf x      ) = show x
  show (Branch x l r) = show l ++ show x ++ show r
