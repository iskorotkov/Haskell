-- Part 1.1
data Tree =
  Branch String Tree Tree
  | Leaf String

-- Parse list of strings that represents one term and create expression tree
parseTerm [x        ] = Leaf x
parseTerm (l : x : r) = Branch x (parseTerm r) (Leaf l)

-- Parse string that represents correct expression and create expression tree
parse s = parseExpr [] (reverse (words s))
 where
  -- Create expression tree out of given list of tokens
  parseExpr l [] = parseTerm l
  parseExpr l (x : r)
    | x == "+" || x == "-" = Branch x (parseExpr [] r) (parseTerm l)
    | otherwise            = parseExpr (l ++ [x]) r

-- Part 1.2
instance Show Tree where
  show (Leaf x      ) = x
  show (Branch x l r) = "<" ++ show l ++ "> " ++ x ++ " <" ++ show r ++ ">"

-- Part 2
-- Convert expression tree to string with postfix form of the same expression
postfix (Leaf x      ) = x
postfix (Branch x l r) = postfix l ++ " " ++ postfix r ++ " " ++ x

-- Get two values from stack, apply given operation and put the result back in the stack
shelve (y : x : st) op = op x y : st

-- Part 3
-- Evaluate expression in postfix form
eval s = evalExpr [] (words s)
 where
  -- Evaluate expression from list of tokens in postfix form
  evalExpr [x]   []         = x
  evalExpr stack (op : ops) = case op of
    "+" -> evalExpr (shelve stack (+)) ops
    "-" -> evalExpr (shelve stack (-)) ops
    "*" -> evalExpr (shelve stack (*)) ops
    "/" -> evalExpr (shelve stack quot) ops
    _   -> evalExpr ((read op :: Integer) : stack) ops
