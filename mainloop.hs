{-# LANGUAGE MultiParamTypeClasses #-}

module Mainloop
  (
    MainLoopClass(..)
    
  ) where

import Math1
import Operation


data PathStock a = Nil | Cell a (PathStock a) deriving (Show,Eq)

class MainLoopClass where
  loop  :: IO()
  loop' :: [String] -> Tree Item -> IO()
  
  
instance MainLoopClass where
  loop = do
    putStr ">> "
    input <- getLine
    case input of
      ""  -> return ()
      _   -> do
        let b = toTree $ read_expr input
        print $ toString' $ serialize b
        loop' []  b
        
        
  loop' str con = do
    putStr ">> "
    input <- getLine
    case input of
      ""  -> return ()
      "l" -> do
        let (l,r) = lrSep con
        print $ toString' $ serialize l
        loop' str con
      "r" -> do
        let (l,r) = lrSep con
        print $ toString' $ serialize r
        loop' str con
      "all" -> do
        
        mapM_ print str
        loop' str con
        
      "n" -> do
        let (l,r) = lrSep con
        let (b,c) = (stopCalc l,stopCalc r)
        case (b,c) of
          (True,True) -> do
              let a = transpose con
              let txt = toString' $ serialize a
              print txt
              print "done!"
              loop' (txt : str) a
          (True,False) -> do
              let a = N Equ l $ zeroRemove (calc r)
              let txt = toString' $ serialize a
              print txt
              loop' (txt : str) a
                
          (False,_) -> do
              let a = N Equ (zeroRemove (calc l)) r
              let txt = toString' $ serialize a
              print txt
              loop' (txt : str) a
            
      _   -> do
        print $ toString' $ serialize con
        loop' str con
        
        
-- = (+ (+ X (- 1 19)) 1) (- 4 5)

