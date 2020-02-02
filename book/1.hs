-- enum
data Bool2 = False
          | True

-- newtype declaration
newtype Mark a = Mark a

data Point a = Pt a a

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)

-- type synonims
type Persom = (Name, Address)

type Name = String

data Address = None
             | Addr String

quicksort [] = []
quicksort (x : xs) =
  quicksort [ y | y <- xs, y < x ] ++ [x] ++ quicksort [ y | y <- xs, y >= x ]

-- let
f x = let y = x + 1 in y

f2 x = y where y = x + 1

f3 x | cond1 x   = a
     | cond2 x   = g a
     | otherwise = f (h x a)
  where a = w x

f4 x =
  let a = w x
  in  case () of
        _ | cond1 x   -> a
          | cond2 x   -> g a
          | otherwise -> f (h x a)

fib =
  let fib' 0 = 0
      fib' 1 = 1
      fib' n = fib (n - 1) + fib (n - 2)
  in  (map fib' [0 ..] !!)

-- lambda abstractions
inc = \x -> x + 1

add = \x y -> x + y

-- function composition
add2 = inc . inc

-- sections
_inc = (+ 1)

_add = (+)

-- infinite data structure
ones = 1 : ones

foo = take 5 squares -- [0,1,4,9,16]
 where
  squares = map (^ 2) (numsFrom 0) where numsFrom n = n : numsFrom (n + 1)

fib = 1 : 1 : [ a + b | (a, b) <- zip fib (tail fib) ]

-- as-pattern
f (x : xs) = x : x : xs

f2 s@(x : xs) = x : s

-- boolean guard
sign x | x > 0  = 1
       | x == 0 = 0
       | x < 0  = -1

-- case expression
take2 m ys = case (m, ys) of
  (0, _     ) -> []
  (_, []    ) -> []
  (n, x : xs) -> x : take2 (n - 1) xs

-- pattern binding
fib2@(1 : tfib) = 1 : 1 : [ a + b | (a, b) <- zip fib2 tfib ]

-- type classes
class Eq2 a where
  (==) :: a -> a -> boolean

-- contexts
(==) :: (Eq2 a) => a -> a -> Bool2
elem :: (Eq2 a) => a -> [a] -> Bool2

-- instance declaration
instance Eq Integer where
  x == y = x `integerEq` y -- method

instance Eq Float where
  x == y = x `floatEq` y

instance (Eq a) => Eq (Tree a) where
  Leaf a         == Leaf b         = a == b
  (Branch l1 r1) == (Branch l2 r2) = (l1 == l2) && (r1 == r2)

-- class extension
class (Eq a) => Ord a where -- class Eq is a superclass or Ord
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min :: a -> a -> a

-- multiple inheritance
class (Eq a, Show a) => C a where -- ...

-- newtype declaration
newtype Natural = MakeNatural Integer

toNatural :: Integer -> Natural
toNatural x | x < 0     = error "Can't create negative naturals!"
            | otherwise = MakeNatural xs

fromNatural :: Natural -> Integer
fromNatural (MakeNatural i) = i

instance Num Natural where
  fromInteger = toNatural
  x + y = toNatural (fromNatural x + fromNatural y)
  x - y =
    let r = fromNatural x - fromNatural y
    in  if r < 0 then error "Unnatural substraction" else toNatural r
  x * y = toNatural (fromNatural x * fromNatural y)

-- field labels
data Point2 = Pt2 Float Float

pointx :: Point2 -> Float
pointx (Pt2 x _) = x

data Point3 = Pt3 { pointx3, pointy3 :: Float }

p = Pt { pointx3 = 1, pointy3 = 2 }
p2 = p { pointx3 = 3 }

absPoint (Pt3 { pointx3 = x, pointy3 = y }) = sqrt (x * x + y * y)

-- strict data constructors
data RealFloat a => Complex a = !a :+ !a

-- I/O
getChar :: IO Char
putChar :: Char -> IO ()

-- >>= or 'do'
main :: IO ()
main = do
  c <- getChar
  putChar c

-- return values
getLine2 :: IO String
getLine2 = do
  c <- getChar
  if c == '\n'
    then return ""
    else do
      l <- getLine
      return (c : l)

-- actions
todoList :: [IO ()]
todoList =
  [ putChar 'a'
  , do
    putChar 'b'
  , putChar 'c'
  , do
    c <- getChar
  , putChar c
  ]

sequence_ :: [IO ()] -> IO ()
sequence_ []       = return ()
sequence_ (a : as) = do
  a
  as

sequence_2 :: [IO ()] -> IO ()
sequence_2 = foldr (>>) (return ())

putStr :: String -> IO ()
putStr s = sequence_ (map putChar s)

-- error handling
getChar' = catch getChar (\e -> return '\n')

getChar'' = getChar `catch` eofHandler
  where eofHandler e = if isEofError e then return '\n' else ioError e

getLine' :: IO String
getLine' = catch getLine'' (\err -> return ("Error: " ++ show err))
  where
    getLine'' = do c <- getChar''
                if c == '\n' then return ""
                             else do l <- getLine'
                                     return (c:l)

-- files
type FilePath = String
openFile :: FilePath -> IOMode -> IOHandle
hClose :: Handle -> IO ()
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- copy one file to another
main = do
  fromHandle <- getAndOpenFile "Copy from: " ReadMode
  toHandle   <- getAndOpenFile "Copy to: " WriteMode
  contents   <- hGetContents fromHandle
  hPutStr toHandle contents
  hClose toHandle
  putStr "Done."

getAndOpenFile :: String -> IOMode -> IOHandle
getAndOpenFile prompt mode =
  do putStr prompt
     name <- getLine
     catch (openFile name mode)
        (\_ -> do putStrLn ("Cannot open " ++ name ++ "\n")
               getAndOpenFile prompt mode)

-- function composition
showsTree (Branch l r) =
  ('<' :) . showsTree l . ('|' :) . showsTree r . ('>' :)

-- to string: show x / shows x s

-- parse string: read / reads s

-- equality
data Tree = Leaf a | Branch (Tree a) (Tree a) deriving Eq
-- ... deriving (Class1, Class2, ...)

-- deriving Enum
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Enum)
