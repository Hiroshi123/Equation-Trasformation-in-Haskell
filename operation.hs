{-# LANGUAGE MultiParamTypeClasses #-}

module Operation
  (
    OperationClass(..)
  ) where


import Math1

class OperationClass where
  transpose   :: Tree Item -> Tree Item
  transpose'  :: Tree Item -> (String,Double,Tree Item)
  transpose'' :: String    -> Double -> Tree Item -> Tree Item
  
  
  zeroRemove :: Tree Item -> Tree Item
  stopCalc :: Tree Item -> Bool
  
  calc :: Tree Item -> Tree Item
  
  opeAdd :: Tree Item -> Tree Item -> Tree Item
  opeSub :: Tree Item -> Tree Item -> (Tree Item,Maybe Int)  
  
instance OperationClass where
  transpose a = do
    let (left,right)  = lrSep a
    let (str,b,c) = transpose' left
    --let e     = zeroRemove $ calc c
    N Equ c (transpose'' str b right)
  
  transpose' (N Add (L (Num a)) b) = ("Add",invSig a, (N Add (N Add (L (Num a)) (L (Num (invSig a)))) b ) )
  transpose' (N Mul (L (Num a)) (L (Var b))) =  ("Div",a, (N Mul (N Div (L (Num a)) (L (Num a)) ) (L (Var b)) ) )
  
  transpose' a = ("Add",0.0, a)
  
  transpose'' "Add" s b = N Add (L (Num s)) b
  transpose'' "Div" s b = N Div (L (Num s)) b
  
  transpose'' "Add" s a = a
  
  zeroRemove (N Add (L (Num 0.0)) b) = b
  zeroRemove (N Mul (L (Num 1.0)) b) = b
  zeroRemove a = a
  
  stopCalc (N a (L (Num b)) (L (Var c))) = True
  stopCalc (N a (L (Num b)) (N Mul (L (Num c)) (L (Var d)) )) = True
  
  stopCalc (L (Var b)) = True
  stopCalc (L (Num c)) = True
  stopCalc a = False
  
  --basic operation <priority number1>
  calc (N Add (L (Num a)) (L (Num b))) = L (Num (a + b))
  calc (N Sub (L (Num a)) (L (Num b))) = L (Num (a - b))
  calc (N Mul (L (Num a)) (L (Num b))) = L (Num (a * b))
  calc (N Div (L (Num a)) (L (Num b))) = L (Num (a / b))
  
  --inversion ( sbbtraction -> addition ) <commutativity>
  --e.g. (x-5) -> (x+(-5))
  calc (N Sub a (L (Num n1)) ) = (N Add a (L (Num (invSig n1))) )
  
  --e.g. (5-x) -> (5+(-x))
  calc (N Sub a (L (Var v1)) ) = (N Add a (L (Var (invSigC v1))) )
  
  --addition <commutativity>
  --e.g. (x+5) -> (5+x)
  calc (N Add a (L (Num n1))) = (N Add (L (Num n1)) a )
  
  --multiplication <commutativity>
  --e.g. (x*5) -> (5*x)
  calc (N Mul a (L (Num n1))) = (N Mul (L (Num n1)) a )
  
  --(addition,addition) <associativity>
  --e.g. 2+(5+x) -> (2+5)+x
  calc (N Add (L (Num n1)) (N Add (L (Num n2)) a ) ) = N Add (N Add (L (Num n1)) (L (Num n2))) a
  
  --(multiplication,mulitplication) <associativity>
  --e.g. 3*(5*x) -> (3*5)*x
  calc (N Mul (L (Num n1)) (N Mul (L (Num n2)) a ) ) = N Mul (N Mul (L (Num n1)) (L (Num n2))) a
  
  --(multiplication,addition) <distribution>
  --e.g. 2*(2+x)-> 2*2 + 2*x
  calc (N Mul (L (Num n1)) (N Add (L (Num n2)) a ) ) = N Add (N Mul (L (Num n1)) (L (Num n2)) ) (N Mul (L (Num n1)) a )
  
  calc (N a b c) = (N a (calc b) (calc c))
  calc (L a) = L a
  
  opeAdd (L (Num a)) (L (Num b)) = L ( Num (a + b) )
  --opeAdd c d = (c,Nothing)
  
  opeSub (L (Num a)) (L (Num b)) = (L ( Num (a - b) ),Just 1)
  opeSub c d = (c,Nothing)
  
  
