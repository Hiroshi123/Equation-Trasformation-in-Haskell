{-# LANGUAGE MultiParamTypeClasses #-}


module Operation
  (
    OperationClass(..)
  ) where

import Data.Ratio
import Math1

class OperationClass where
  
  transpose   :: Tree Item -> Tree Item
  transpose'  :: Tree Item -> (String,(Int,Int),Tree Item)
  transpose'' :: String    -> (Int,Int) -> Tree Item -> Tree Item
  zeroRemove :: Tree Item -> Tree Item
  stopCalc :: Tree Item -> Bool
  
  calc :: Tree Item -> Tree Item
  
  add  :: (Int,Int) -> (Int,Int) -> (Int,Int)
  add' :: (Int,Int) -> (Int,Int) -> (Int,Int)
  sub  :: (Int,Int) -> (Int,Int) -> (Int,Int)
  sub' :: (Int,Int) -> (Int,Int) -> (Int,Int)
  
  mul  :: (Int,Int) -> (Int,Int) -> (Int,Int)
  div' :: (Int,Int) -> (Int,Int) -> (Int,Int)
  
  dividable :: Int -> Int -> (Int,Int)
  lcmFind :: (Int,Int) -> (Int,Int) -> ((Int,Int),(Int,Int))
  gcd' :: (Eq a,Integral a) => a -> a -> a
  lcm' :: (Eq a,Integral a) => a -> a -> a
  
  
instance OperationClass where
  transpose a = do
    let (left,right)  = lrSep a
    let (str,b,c) = transpose' left
    --let e     = zeroRemove $ calc c
    N Equ c (transpose'' str b right)
    
  transpose' (N Add (L (Num a)) b) = ("Add",invSig a, (N Add (N Add (L (Num a)) (L (Num (invSig a)))) b ) )
  transpose' (N Mul (L (Num a)) (L (Var b))) =  ("Div",a, (N Mul (N Div (L (Num a)) (L (Num a)) ) (L (Var b)) ) )
  
  transpose' a = ("Add",(0,1), a)
  
  transpose'' "Add" s b = N Add (L (Num s)) b
  transpose'' "Div" s b = N Div b (L (Num s))
  
  
  transpose'' "Add" s a = a
  
  zeroRemove (N Add (L (Num (0,1))) b) = b
  zeroRemove (N Mul (L (Num (1,1))) b) = b
  zeroRemove a = a
  
  stopCalc (N a (L (Num b)) (L (Var c))) = True
  stopCalc (N a (L (Num b)) (N Mul (L (Num c)) (L (Var d)) )) = True
  
  stopCalc (L (Var b)) = True
  stopCalc (L (Num c)) = True
  stopCalc a = False
  
  add x y = do
    let (x1,y1) = lcmFind x y
    add' x1 y1
    
  add' (x1,x2) (y1,y2)
    | x1 + y1 > 0 = dividable (x1+y1) x2
    | x1 + y1 < 0 = do
        let (p,q) = dividable (-x1-y1) x2
        (-p,q)
    | otherwise   = (0,1)
    
    
  sub x y = do
    let (x1,y1) = lcmFind x y
    sub' x1 y1
    
  sub' (x1,x2) (y1,y2)
    | x1 - y1 > 0 = dividable (x1-y1) x2
    | x1 - y1 < 0 = do
        let (p,q) = dividable (y1-x1) x2
        (-p,q)
    | otherwise   = (0,1)
    
    
  mul (x1,x2) (y1,y2)
    | x1 * y1 > 0 = dividable (x1*y1) (x2*y2)
    | x1 * y1 < 0 = do
        let (p,q) = dividable (-x1*y1) (x2*y2)
        (-p,q)
    | otherwise   = (0,1)
    
  div' (x1,x2) (y1,y2)
    | y1 < 0    = (-y2,-y1)
    | otherwise =  (y2,y1)
    
  dividable x y = do
    let l = gcd' x y
    ((x `div` l),(y `div` l))
    
  lcmFind (x1,y1) (x2,y2) = do
    let l = lcm' y1 y2
    ((x1 * (l `div` y1) ,l) , (x2 * (l `div` y2) ,l))
 
  gcd' a b = head [ x | let c = min a b, x <- [c,c-1..1], mod a x == 0 && mod b x ==0]
  lcm' a b = head [ x | let c = max a b, x <- [c,c+1..], mod x a == 0 && mod x b ==0]
  
  --lcm' a b = head [ x | let c = max a b, x <- [c,c+1..], mod x a == 0 && mod x b ==0]
    
  --basic operation <priority number1>
  
  calc (N Add (L (Num a)) (L (Num b))) = L (Num (add a b))
  calc (N Sub (L (Num a)) (L (Num b))) = L (Num (sub a b))
  calc (N Mul (L (Num a)) (L (Num b))) = L (Num (mul a b))
  calc (N Div (L (Num a)) (L (Num b))) = N Mul (L (Num a)) (L (Num (div' a b)))
  
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

  --(addition,multiplication) <distribution>
  --e.g. (2*x) + (3*x)-> (2+3)*(x)
  calc (N Add (N Mul (L (Num n1)) a ) (N Mul (L (Num n2)) b )  ) = N Mul (N Add (L (Num n1)) (L (Num n2)) ) a
  
  
  calc (N a b c) = (N a (calc b) (calc c))
  calc (L a) = L a
  
  
