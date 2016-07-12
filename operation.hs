{-# LANGUAGE MultiParamTypeClasses #-}

module Operation
  (
    OperationClass(..) 
  ) where

import Math1

class OperationClass where
  transpose :: Tree Item -> Tree Item
  transpose' :: Tree Item -> (Double,Tree Item)
  transpose'' :: Double -> Tree Item -> Tree Item

  zeroRemove :: Tree Item -> Tree Item
  stopCalc :: Tree Item -> Bool
  
  calc :: Tree Item -> Tree Item
  
  opeAdd :: Tree Item -> Tree Item -> Tree Item
  opeSub :: Tree Item -> Tree Item -> (Tree Item,Maybe Int)  
  
instance OperationClass where
  transpose a = do
    let (left,right)  = lrSep a
    let (b,c) = transpose' left
    --let e     = zeroRemove $ calc c
    N Equ c (transpose'' b right)
    
  transpose' (N c (L (Num a)) (L (Var b))) = (invSig a, (N c (N Add (L (Num a)) (L (Num (invSig a)))) (L (Var b)) ) )
  transpose' (N c (L (Var b)) (L (Num a))) = (invSig a, (N c (L (Var b)) (N Add (L (Num a)) (L (Num (invSig a))) ) ) )
  transpose' a = (0.0, a)

  transpose'' s (N c (L (Num a)) b) = (N c (N Add (L (Num a)) (L (Num s) ) )  b )
  transpose'' s a = a
  
  zeroRemove (N c (L (Num 0.0)) (L (Var b))) = (L (Var b))
  zeroRemove (N c (L (Var b)) (L (Num 0.0))) = (L (Var b))
  zeroRemove a = a


  stopCalc (N a (L (Num b)) (L (Var c))) = True
  stopCalc (N a (L (Var b)) (L (Num c))) = True
  stopCalc (L (Var b)) = True
  stopCalc (L (Num c)) = True
  stopCalc a = False
  
  calc (N Add (L (Num a)) (L (Num b))) = L (Num (a + b))
  calc (N Sub (L (Num a)) (L (Num b))) = L (Num (a - b))
  calc (N Mul (L (Num a)) (L (Num b))) = L (Num (a * b))
  calc (N Div (L (Num a)) (L (Num b))) = L (Num (a / b))

  --left 2 :  right : 1 (Add:Sub)
  
  calc (N Add (N Add (L (Var v1)) (L (Num n1))) (L (Num n2)) ) = N Add (L (Var v1)) (N Add (L (Num n1)) (L (Num n2)) )
  calc (N Add (N Add (L (Num n1)) (L (Var v1))) (L (Num n2)) ) = N Add (N Add (L (Num n1)) (L (Num n2))) (L (Var v1) )
  
  calc (N Add (N Sub (L (Var v1)) (L (Num n1))) (L (Num n2)) ) = N Add (L (Var v1)) (N Sub (L (Num n2)) (L (Num n1)) )
  calc (N Add (N Sub (L (Num n1)) (L (Var v1))) (L (Num n2)) ) = N Add (N Sub (L (Num n1)) (L (Num n2))) (L (Var v1) )
  
  calc (N Sub (N Add (L (Var v1)) (L (Num n1))) (L (Num n2)) ) = N Add (L (Var v1)) (N Sub (L (Num n1)) (L (Num n2)) )
  calc (N Sub (N Add (L (Num n1)) (L (Var v1))) (L (Num n2)) ) = N Add (N Sub (L (Num n1)) (L (Num n2))) (L (Var v1) )
  
  calc (N Sub (N Sub (L (Var v1)) (L (Num n1))) (L (Num n2)) ) = N Sub (L (Var v1)) (N Add (L (Num n1)) (L (Num n2)) )
  calc (N Sub (N Sub (L (Num n1)) (L (Var v1))) (L (Num n2)) ) = N Sub (N Sub (L (Num n1)) (L (Num n2))) (L (Var v1) )
  
  --left 1 :  right : 2 (Add:Sub)
  
  calc (N Add (L (Num n1)) (N Add (L (Num n2)) (L (Var v1)) ) ) = N Add (N Add (L (Num n1)) (L (Num n2)) ) (L (Var v1))
  calc (N Add (L (Num n1)) (N Add (L (Num n2)) (L (Var v1)) ) ) = N Add (L (Var v1)) (N Add (L (Num n1)) (L (Num n2)) )
  
  calc (N Add (N Sub (L (Num n1)) (L (Num n2))) (L (Var v1)) ) = N Sub (N Add (L (Num n1)) (L (Num n2))) (L (Var v1) )
  calc (N Add (N Sub (L (Num n1)) (L (Var v1))) (L (Num n2)) ) = N Add (L (Var v1)) (N Sub (L (Num n1)) (L (Num n2)))
  
  calc (N Sub (L (Num n1)) (N Add (L (Num n2)) (L (Var v1)) ) ) = N Sub (N Sub (L (Num n1)) (L (Num n2)) ) (L (Var v1))
  calc (N Sub (L (Num n1)) (N Add (L (Num n2)) (L (Var v1)) ) ) = N Sub (N Sub (L (Num n1)) (L (Num n2)) ) (L (Var v1))
  
  calc (N Sub (L (Num n1)) (N Sub (L (Num n2)) (L (Var v1)) ) ) = N Add (N Sub (L (Num n1)) (L (Num n2)) ) (L (Var v1))
  calc (N Sub (L (Num n1)) (N Sub (L (Num n2)) (L (Var v1)) ) ) = N Sub (N Add (L (Num n1)) (L (Num n2)) ) (L (Var v1))
  
  
  --left 2 :  right : 1 (Add:Mul)
  
  calc (N Mul (N Mul (L (Var v1)) (L (Num n1))) (L (Num n2)) ) = N Mul (L (Var v1)) (N Mul (L (Num n1)) (L (Num n2)) )
  calc (N Mul (N Mul (L (Num n1)) (L (Var v1))) (L (Num n2)) ) = N Mul (N Mul (L (Num n1)) (L (Num n2))) (L (Var v1) )
  
  calc (N Mul (N Add (L (Var v1)) (L (Num n1))) (L (Num n2)) ) = N Add (N Mul (L (Num n2)) (L (Var v1)) ) (N Mul (L (Num n1)) (L (Num n2)) )
  calc (N Mul (N Add (L (Num n1)) (L (Var v1))) (L (Num n2)) ) = N Add (N Mul (L (Num n1)) (L (Num n2)) ) (N Mul (L (Num n2)) (L (Var v1)) )
  
  
--  calc (N Add (N Mul (L (Var v1)) (L (Num n1))) (L (Num n2)) ) = N Mul (L (Add v1)) (N Mul (L (Num n1)) (L (Num n2)) )
--  calc (N Add (N Mul (L (Num n1)) (L (Var v1))) (L (Num n2)) ) = N Mul (N Add (L (Num n1)) (L (Num n2))) (L (Var v1) )

--  calc (N Mul (N Mul (L (Var v1)) (L (Num n1))) (L (Num n2)) ) = N Mul (L (Mul v1)) (N Mul (L (Num n1)) (L (Num n2)) )
--  calc (N Mul (N Mul (L (Num n1)) (L (Var v1))) (L (Num n2)) ) = N Mul (N Mul (L (Num n1)) (L (Num n2))) (L (Var v1) )
  
  calc (N a b c) = (N a (calc b) (calc c))
  calc (L a) = L a
  
  opeAdd (L (Num a)) (L (Num b)) = L ( Num (a + b) )

  
  --opeAdd c d = (c,Nothing)
  
  opeSub (L (Num a)) (L (Num b)) = (L ( Num (a - b) ),Just 1)
  opeSub c d = (c,Nothing)
  
  
