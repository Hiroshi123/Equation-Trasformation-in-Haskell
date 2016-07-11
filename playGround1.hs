{-# LANGUAGE MultiParamTypeClasses #-}

import Math1

main :: IO ()
main = do
  print $ process' "(+ (+ X (- 1 19)) 1) = (- 4 5)"
  print 10



