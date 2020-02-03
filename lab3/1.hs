data Tree a =
  Branch {value :: a, left :: (Tree a), right :: (Tree a)}
  | Leaf {value :: a}

parseTerm :: [String] -> Tree String
parseTerm [x        ] = Leaf x
parseTerm (l : x : r) = Branch x (parseTerm r) (Leaf l)

parse :: String -> Tree String
parse s = parseExpr [] (reverse (words s))
 where
  parseExpr :: [String] -> [String] -> Tree String
  parseExpr l []      = parseTerm l
  parseExpr l (x : r) = if x == "+" || x == "-"
    then Branch x (parseExpr [] r) (parseTerm l)
    else parseExpr (l ++ [x]) r

instance (Show a) => Show (Tree a) where
  show (Leaf x      ) = show x
  show (Branch x l r) = "<" ++ show l ++ ">" ++ show x ++ "<" ++ show r ++ ">"

postfix (Leaf x      ) = show x
postfix (Branch x l r) = postfix l ++ " " ++ postfix r ++ " " ++ show x

evalTree :: Tree String -> Integer
evalTree (Leaf x) = read x :: Integer
evalTree (Branch x l r) | x == "+" = evalTree l + evalTree r
                        | x == "-" = evalTree l - evalTree r
                        | x == "*" = evalTree l * evalTree r
                        | x == "/" = evalTree l `div` evalTree r

evalPostfix :: String -> Integer
evalPostfix [] = 0
evalPostfix s  = evalExpr [] (words s)
 where
  val :: String -> Integer
  val x = read x :: Integer
  evalOp x y op | op == "+" = x + y
                | op == "-" = x - y
                | op == "*" = x * y
                | op == "/" = x `div` y
  evalExpr :: [Integer] -> [String] -> Integer
  evalExpr [x] [] = x
  -- + - * /
  evalExpr [p1] (x : y : op : t) =
    evalExpr (p1 : [evalOp (val x) (val y) op]) t
  --
  evalExpr [p1, p2] (op     : t) = evalExpr [evalOp (val x) (val y) op] t
  evalExpr [p1]     (x : op : t) = evalExpr [evalOp p1 (val x) op] t
