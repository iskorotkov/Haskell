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

postfix (Leaf x      ) = x
postfix (Branch x l r) = postfix l ++ " " ++ postfix r ++ " " ++ x

evalTree (Leaf x) = read x :: Integer
evalTree (Branch x l r) | x == "+" = evalTree l + evalTree r
                        | x == "-" = evalTree l - evalTree r
                        | x == "*" = evalTree l * evalTree r
                        | x == "/" = evalTree l `div` evalTree r

evalPostfix [] = 0
evalPostfix s  = evalExpr [] (words s)
 where
  evalExpr [x         ] []          = x
  evalExpr (y : x : xs) ("+" : ops) = evalExpr ((x + y) : xs) ops
  evalExpr (y : x : xs) ("-" : ops) = evalExpr ((x - y) : xs) ops
  evalExpr (y : x : xs) ("*" : ops) = evalExpr ((x * y) : xs) ops
  evalExpr (y : x : xs) ("/" : ops) = evalExpr ((x `div` y) : xs) ops
  evalExpr stack        (x   : ops) = evalExpr ((read x :: Integer) : stack) ops
