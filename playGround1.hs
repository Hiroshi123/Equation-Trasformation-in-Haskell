import Mainloop
import Operation
import Math1
import MultiVarCalc

main :: IO ()
main = do
  
  loop
  
  --let b = toTree $ read_expr "(+ (+ (+ (+ (+ (+ 4 Y) (* 2 X)) (* 4 (* Y ( * X Y) ))) (* 3 a) ) X) 3)"
  
  --let d = toTree $ read_expr "(= (+ (+ 3 X) (+ X 4)) 5)"
  --let e = toTree $ read_expr "(+ 3 (+ X 4))"
  
  --print $ targetSelect d
  
  --print $ toString' $ serialize $ iter'' "(+ (+ 3 X) (+ 4 X))"
  
  --let g = toTree $ read_expr "(+ 3 (+ X 4))"
  --print $ filter (\x -> x == "X") c
  --print d
  
  --print $ com d "0"
  
  --print $ getAllItems b

  
  
  --print $ getAllItems (N Add (L (Num (12,1))) (N Add (L (Var "Y")) (N Mul (L (Num (2,1))) (L (Var "X")) )))
  --loop
  -- = (+ (* 2 X) (* 4 X)) (- (* 4 5) 32)"
  -- = (+ (+ 2 (* 2 X) (+ 4 (* 3 X)) (- (* 4 5) 32)"
  -- = (+ (* 2 X) (* 3 X)) (- (* 4 5) 32)"
  -- = (+ X X) (- (* 4 5) 32)"
  -- = (* (+ 2 X) (+ 4 X)) (- (* 4 5) 32)"
  -- = (+ 12 (- (+ (* 2 X) (/ 3 19)) 3)) (- (* 4 5) 32)"
  --let a = serialize $ toTree $ read_expr "= (- (+ (* 3 X) (* 3 19)) 1) (+ (* 4 5) 32)"
  --print a
  --print $ toString' a
  
--  let a = process' " = (+ (+ X (- 1 19)) 1) (- 4 5)"
--  print a 
--  let b = transpose (N Equ (N Add (L (Num 1.0)) (L (Var "X")) ) (N Add (L (Num 4.0)) (L (Num (-5.0)) )) )
--  print b

  print 10
      
        
--div'' :: Int -> Int -> Int
--div'' a b = a / b

--targetSelect :: Tree Item -> String
--targetSelect a = head $ foldr (\x y->if not (x `elem` y) then x:y else y) [] $ getAllTerms a

  
