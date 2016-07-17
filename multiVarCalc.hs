{-# LANGUAGE MultiParamTypeClasses #-}

module MultiVarCalc
  (
    CalcTClass(..)
    
  ) where

import Math1
import Operation


class CalcTClass where
  --necessary to get target list(what kind of item you are going to operate)
  getAllTerms :: Tree Item -> [String]
  getTerm :: Tree Item -> String

  --necessary to get actual calculation
  getCoefficient :: Tree Item -> (Int,Int)

  --string contains a target variable. It returns maybe because so we can aware if nothing updated
  calc' :: Tree Item -> String -> Maybe (Tree Item)
  --actual calculation
  calc'' :: Tree Item -> Tree Item

  targetSelect :: Tree Item -> String
  
instance CalcTClass where

   getAllTerms (N Mul (L (Num b)) c) =  getAllTerms c
   getAllTerms (N Mul (L (Var b)) c) =  [ b ++ head (getAllTerms c) ]

   getAllTerms (N Add b c) = do
     --let s = filter (\x-> x == head cc) bb
     --case s 
     bb ++ cc
     where bb = getAllTerms b
           cc = getAllTerms c
           
           
   getAllTerms (L (Var a)) =  [a]

   --if a term is constant, I let it 0
   getAllTerms (L (Num a)) = ["0"]


   getTerm (N Mul (L (Num b)) c) =  getTerm c
   --term is going to be concatenated if it is joint with multiplication
   getTerm (N Mul (L (Var b)) c) =  b ++ getTerm c
   getTerm (L (Var a)) =  a
   getTerm (L (Num a)) = "0"

   getTerm _ = ""

   --if it is a characturer, coefficient is 1/1
   getCoefficient (L (Var a)) = (1,1)
   getCoefficient (L (Num a)) = a
   getCoefficient (N Mul (L (Num a)) (L (Var b))) = a


   --actual operation
   calc'' (N Add a b) = L (Num (add (getCoefficient a) (getCoefficient b)))
   calc'' (N Sub a b) = L (Num (sub (getCoefficient a) (getCoefficient b)))
   calc'' (N Mul a b) = L (Num (mul (getCoefficient a) (getCoefficient b)))
   calc'' (N Div (L (Num a)) (L (Num b))) = N Mul (L (Num a)) (L (Num (div' a b)))


   calc' (N ope a b) q
     -- letter <+|-|*|/> letter 
     | (not (aa == "")) && (not (aa == "0")) && (aa == bb) = Just (N Mul (calc'' (N ope a b)) (L (Var aa)) )
     --number  <+|-|*|/> number
     | (aa == "0") && (aa == bb) = Just (calc'' (N ope a b)) 
     where aa = getTerm a
           bb = getTerm b
           
           
   --comutative
   --e.g. (3+x) -> (x+3) 
   calc' (N Add ( L a ) b ) q
     | q == getTerm b = Just (N Add b (L a))
     
     
   --comutative
   --e.g. (3*y+x) -> (x+3*y)
   calc' (N Add (N Mul a b) c ) q
     | q == getTerm c = Just (N Add c (N Mul a b))
     

   --associative
   --e.g. (3+x)+y -> 3+(x+y)
   calc' (N Add (N Add a b) c ) q
     | q == getTerm c = Just (N Add a (N Add b c))
     

   --associative
   --e.g. 3+(x+y) -> (3+x)+y
   calc' (N Add a (N Add b c)) q
     | (q == getTerm b) && (not (q == getTerm c))  = Just (N Add (N Add a b) c)
     
   --if target is not found on all terms after coming rightest part of branch, Nothing will come back.
   calc' (L a) q = Nothing

   --which path are you going to select
   calc' (N a b c) q
     -- if left side of path contains a objective term
     | q `elem` d = do
         let (Just g) = calc' c q
         Just (N a b g)
         
     | otherwise  = do
         let e = calc' b q
         case e of
           Nothing -> Nothing  -- if no updates, you will not merge branch coming from the other side
           Just g  -> Just (N a g c)  --if the element is not on the right side of branch, you will switch to left branch.        
     where d = getAllTerms c
     
   
   targetSelect a = head $ foldr (\x y->if not (x `elem` y) then x:y else y) [] $ getAllTerms a
   

