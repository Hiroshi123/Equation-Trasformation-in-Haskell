import Mainloop


main :: IO ()
main = do
  
  loop

  
  --loop
  --let a = serialize $ toTree $ read_expr "= (+ (+ X (- 1 19)) 1) (+ (- 4 5) 32)"
  --print a
  --print $ toString' a

--  let a = process' " = (+ (+ X (- 1 19)) 1) (- 4 5)"
--  print a 
--  let b = transpose (N Equ (N Add (L (Num 1.0)) (L (Var "X")) ) (N Add (L (Num 4.0)) (L (Num (-5.0)) )) )
--  print b

{--
  let a = toTree $ read_expr "= (+ (+ X (- 1 19)) 1) (- 4 5)"
  print a
  let (b,s) = as' a
  print b
  let c = calc b
  print c
  let d = calc c
  print d
  let e = calc d
  print e
--}

  print 10
