{-# LANGUAGE MultiParamTypeClasses #-}

module Mainloop
  (
    MainLoopClass(..)
    
  ) where

import Math1
import Operation
import MultiVarCalc

data PathStock a = Nil | Cell a (PathStock a) deriving (Show,Eq)

class MainLoopClass where
  loop  :: IO()
  loop' :: [String] -> Tree Item -> String -> IO()  
  
instance MainLoopClass where
  loop = do
    putStr ">> "
    input <- getLine
    case input of
      ""  -> return ()
      _   -> do
        let b = toTree $ read_expr input
        print $ toString' $ serialize b
        loop' []  b "0" --(targetSelect b)
        
  loop' str con target = do
    putStr ">> "
    input <- getLine
    case input of
      ""  -> return ()
      "l" -> do
        let (l,r) = lrSep con
        print $ toString' $ serialize l
        loop' str con target
      "r" -> do
        let (l,r) = lrSep con
        print $ toString' $ serialize r
        loop' str con target
      "all" -> do
        mapM_ print str
        loop' str con target
        
      "n" -> do
        let (l,r) = lrSep con
        let (b,c) = (stopCalc l,stopCalc r)
        case (b,c) of
          (True,True) -> do
              let a = transpose con
              let newTarget  = targetSelect a
              let txt = toString' $ serialize a
              print txt
              print "done!"
              loop' (txt : str) a newTarget
              
          (True,False) -> do
              let (Just x) = (calc' r "0")
              let a = N Equ l $ zeroRemove x
              let txt = toString' $ serialize a
              print txt
              loop' (txt : str) a target
                
          (False,_) -> do
            --let d  = head $ reverse $ getAllTerms b
            --let a = N Equ (zeroRemove (calc l)) r
            let a = calc' l target
            case a of
              Nothing -> do
                let newTarget  = targetSelect l
                print "done!!!"
                loop' str con newTarget
                
              _     -> do
                let (Just e) = a
                let ee = N Equ (zeroRemove e) r
                let txt = toString' $ serialize ee
                print txt
                loop' (txt : str) ee target
                
      _   -> do
        print $ toString' $ serialize con
        loop' str con target
        
        
-- = (+ (+ X (- 1 19)) 1) (- 4 5)
  

