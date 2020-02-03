-- Part 1
data Tree =
  Branch String Tree Tree
  | Leaf String

parseTerm [x        ] = Leaf x
parseTerm (l : x : r) = Branch x (parseTerm r) (Leaf l)

parse s = parseExpr [] (reverse (words s))
 where
  parseExpr l []      = parseTerm l
  parseExpr l (x : r) = if x == "+" || x == "-"
    then Branch x (parseExpr [] r) (parseTerm l)
    else parseExpr (l ++ [x]) r

instance Show Tree where
  show (Leaf x      ) = x
  show (Branch x l r) = "<" ++ show l ++ ">" ++ x ++ "<" ++ show r ++ ">"

-- Part 2
postfix (Leaf x      ) = x
postfix (Branch x l r) = postfix l ++ " " ++ postfix r ++ " " ++ x

-- Part 3
evalPostfix s = evalExpr [] (words s)
 where
  evalExpr [x         ] []          = x
  evalExpr (y : x : xs) ("+" : ops) = evalExpr ((x + y) : xs) ops
  evalExpr (y : x : xs) ("-" : ops) = evalExpr ((x - y) : xs) ops
  evalExpr (y : x : xs) ("*" : ops) = evalExpr ((x * y) : xs) ops
  evalExpr (y : x : xs) ("/" : ops) = evalExpr ((x `div` y) : xs) ops
  evalExpr stack        (x   : ops) = evalExpr ((read x :: Integer) : stack) ops
