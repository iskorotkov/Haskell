data Tree =
  Branch {value :: String, left :: Tree, right :: Tree}
  | Leaf {value :: String}

parseTerm :: [String] -> Tree
parseTerm [x        ] = Leaf x
parseTerm (l : x : r) = Branch x (parseTerm r) (Leaf l)

parse :: String -> Tree
parse s = parseExpr [] (reverse (words s))
 where
  parseExpr :: [String] -> [String] -> Tree
  parseExpr l []      = parseTerm l
  parseExpr l (x : r) = if x == "+" || x == "-"
    then Branch x (parseExpr [] r) (parseTerm l)
    else parseExpr (l ++ [x]) r

instance Show Tree where
  show (Leaf x      ) = x
  show (Branch x l r) = "<" ++ show l ++ ">" ++ x ++ "<" ++ show r ++ ">"

postfix (Leaf x      ) = x
postfix (Branch x l r) = postfix l ++ " " ++ postfix r ++ " " ++ x

evalTree :: Tree -> Integer
evalTree (Leaf x) = read x :: Integer
evalTree (Branch x l r) | x == "+" = evalTree l + evalTree r
                        | x == "-" = evalTree l - evalTree r
                        | x == "*" = evalTree l * evalTree r
                        | x == "/" = evalTree l `div` evalTree r

evalPostfix :: String -> Integer
evalPostfix [] = 0
evalPostfix s  = evalExpr [] (words s)
 where
  evalExpr :: [Integer] -> [String] -> Integer
  evalExpr [x         ] []          = x
  evalExpr (y : x : xs) ("+" : ops) = evalExpr ((x + y) : xs) ops
  evalExpr (y : x : xs) ("-" : ops) = evalExpr ((x - y) : xs) ops
  evalExpr (y : x : xs) ("*" : ops) = evalExpr ((x * y) : xs) ops
  evalExpr (y : x : xs) ("/" : ops) = evalExpr ((x `div` y) : xs) ops
  evalExpr stack        (x   : ops) = evalExpr ((read x :: Integer) : stack) ops
