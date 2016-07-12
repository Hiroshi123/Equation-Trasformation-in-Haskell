{-# LANGUAGE MultiParamTypeClasses #-}


module Math1
  (
    
    EITHER(..),
    Item(..),
    Tree(..),
    MyMath(..)
    
  ) where


main :: IO ()
main = do
  
  {--
  print $ filter (\x -> x == Add) $ read_expr $ "(+ (+ 10 (+ 1 19)) X)"
  print $ toTree $ itemSort $ read_expr $ "(+ (+ X (+ 1 19)) 1)"
  
  let a = process'  "(+ (+ X (+ 1 19)) 1) = (+ 4 5)"
  
  print a
  let b = expr LEFT a
  print b
  let c = expr LEFT b
  print c
  let d = expr RIGHT c
  print d
  let e = expr RIGHT d
  print e
--}
  --print (N Sub (L (Num 21.0)) (L (Num 10.0)))
  
  
  print 3
  
data EITHER = LEFT | RIGHT deriving (Show)

calc :: EITHER -> Tree Item -> Tree Item
calc lr a = do
  case a of
    (N Equ a b) -> do
      case lr of
        LEFT  -> (N Equ (calc lr a) b)
        RIGHT -> (N Equ a (calc lr b))
    (N Add c d) -> do
      let (come,r) = opeAdd c d
      case r of
        Just 1  -> (calc lr come)
        Nothing -> (N Add (calc lr come) (calc lr d))
    (N Sub e f) -> do
      let (come,r) = opeSub e f
      case r of
        Just 1  -> (calc lr come)
        Nothing -> (N Sub (calc lr come) (calc lr f))
    (L g)       ->  L g
    
    
opeAdd :: Tree Item -> Tree Item -> (Tree Item,Maybe Int)
opeAdd (L (Num a)) (L (Num b)) = (L ( Num (a + b) ),Just 1)
opeAdd c d = (c,Nothing)


opeSub :: Tree Item -> Tree Item -> (Tree Item,Maybe Int)
opeSub (L (Num a)) (L (Num b)) = (L ( Num (a - b) ),Just 1)
opeSub c d = (c,Nothing)


{--
  where
    expr_sub (v, Add:xs) =
      let (v', ys) = term xs in expr_sub (v + v', ys)
    expr_sub (v, Sub:xs) =
      let (v', ys) = term xs in expr_sub (v - v', ys)
    expr_sub (v, xs) = (v, xs)
--}


data Item = Equ | Add | Sub | Mul | Div | Rpa | Lpa | Var String | Num Double | End  deriving (Eq, Show)

data Tree a = L a | N a (Tree a) (Tree a) deriving (Eq,Show)

class MyMath where

  --all process
  process' :: String -> Tree Item

--  toString :: [Item] -> String 
  toString':: [Item]  -> String
  
  
  --ngTopg :: Tree Item -> Tree Item
  
  --1st
  read_expr :: String -> [Item]
  --makeTree :: [Item] -> Tree Item
  
  --2nd
  itemSort   :: [Item] -> [Item]
  
  --sub function
  findEqu  :: [Item] -> Int
  getLeft  :: Int -> [Item] -> [Item]
  getRight :: Int -> [Item] -> [Item]
  
  --3rd
  lrDivide :: [Item] -> ([Item],[Item])
  
  --sub function
  collectNum :: [Item] -> [Item]
  collectAdd :: [Item] -> [Item]
  collectVar :: [Item] -> [Item]
  
  --4th
  toTree :: [Item] -> Tree Item
  
  --negativeToPositive  
  ngTopg :: Tree Item -> Tree Item
  ngTopg' :: Tree Item -> Tree Item

  --inverse signature for number
  invSig :: Double -> Double
  --inverse signature for variable
  invSigC :: String -> String
  
  --serialize
  serialize :: Tree Item -> [Item]

  lrSep :: Tree Item -> (Tree Item,Tree Item)
  
instance MyMath where
  --lexical analysis
  read_expr "" = []
  read_expr (' ':s) = read_expr s
  read_expr ('+':s) = Add : read_expr s
  read_expr ('-':s) = Sub : read_expr s
  read_expr ('*':s) = Mul : read_expr s
  read_expr ('/':s) = Div : read_expr s
  read_expr ('(':s) = read_expr s  --Lpa : read_expr s
  read_expr (')':s) = read_expr s  --Rpa : read_expr s
  read_expr ('=':s) = Equ : read_expr s
  read_expr ('X':s) = Var "X" :read_expr s
  read_expr xs =
    case reads xs of
      [] -> error "read_expr error"
      [(x, xs')] -> Num x : read_expr xs'
      
  toString' []      = ""
  toString' (Add:t) = (++) " ( + " $ toString' t
  toString' (Sub:t) = (++) " ( - " $ toString' t
  toString' (Mul:t) = (++) " ( * " $ toString' t
  toString' (Div:t) = (++) " ( / " $ toString' t
  toString' (Equ:t) = (++) " ( = " $ toString' t
  toString' ((Var a):t) = (++) " " $ (++) a $ (++) " " $ toString' t
  toString' ((Num a):t) = (++) " " $ (++) (show a) $ (++) " " $ toString' t
  toString' (End:t) = (++) " ) " $ toString' t
  
  --sub function
  collectNum s = iter s where
    iter []        = []
    iter (Num a:s) = (Num a) : iter s
    iter ( _   :s) = iter s
    
  collectVar s = iter s where
    iter []        = []
    iter (Var a:s) = (Var a) : iter s
    iter ( _   :s) = iter s
    
  collectAdd s = filter (\x -> x == Add) s
  
  --combination of above 3
  itemSort a = foldr (\x y -> x:y) ( foldr (\x y -> x:y) (collectVar a) (collectNum a) ) (collectAdd a)
  
  --sub function
  findEqu s = do
    let (h,t) = unzip $ filter (\(x,y) -> y == Equ ) $ zip [0..] s
    head h
    
  getLeft  i a = take i a
  getRight i a = drop (i+1) a
  --combination of above 3
  lrDivide a = ( (getLeft (findEqu a) a ) , (getRight (findEqu a) a ) )

  
  ngTopg h = do
    case h of
      N Sub fst sec -> N Add fst (ngTopg' sec)
      N a b c -> N a (ngTopg b) (ngTopg c)
      L d -> L d
    
  ngTopg' (L (Num a)) = L (Num (invSig  a))
  ngTopg' (L (Var a)) = L (Var (invSigC a))
  ngTopg' (N d a b)   = N d (ngTopg' a) (ngTopg' b)
  
  invSig a = - a
  invSigC a = (++) "-" a

  serialize (N b c d) = do
    let e = (++) (serialize c) $ (++) (serialize d) [End]
    b : e
  serialize (L b ) = [b]
  
  toTree s = fst $ iter s where
    iter (Num a:s) = (L (Num a), s)
    iter (Var b:s) = (L (Var b), s)
    iter (c:s)   = (N c l r, u)
      where (l, t) = iter s
            (r, u) = iter t
            
            
  --highest layer
  process' a = do
    let b = read_expr a
    let (c,d) = lrDivide b
    let e = serialize $ ngTopg $ toTree c
    let f = serialize $ ngTopg $ toTree d
    toTree (Equ : foldr (\x y -> x:y) (itemSort f) (itemSort e))

  lrSep (N Equ a b) = (a,b)
  
  
