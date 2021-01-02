{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
module Main where

import Prelude hiding (Num, zipWith, Eq, (==), (!!))
import qualified Prelude as P
import Data.Array.Accelerate as A hiding ((++))
import qualified Data.Array.Accelerate.LLVM.Native as Native
import qualified Data.Array.Accelerate.LLVM.PTX as PTX

-- TODO: put this code on a branch, publish to github, and link as a testcase from
-- the GitHub issue

mat :: Acc (Matrix Int)
mat = use $ fromList (Z :. 4 :. 3) [1..]

s :: Acc (Scalar Int)
s = A.unit (A.constant 3)

testCase :: (forall a . Arrays a => Acc a -> a) -> Acc (Scalar Int) -> Acc (Matrix Int) -> IO ()
testCase run ix m = do
  putStrLn "given a matrix m ="
  print (run m)
  putStrLn $ "\n... and an index ix = " ++ show (run ix)
  let sliced = A.slice mat . lift $ Any :. the s :. All
  putStrLn $ "\nthe row at index " ++ show (run ix) ++ " is:\n" ++ show (run sliced)

main = do
  putStrLn "==============================="
  putStrLn "Native backend...\n"
  testCase Native.run s mat
  putStrLn "==============================="

  putStrLn "\n\n\n"

  putStrLn "==============================="
  putStrLn "PTX backend...\n"
  testCase PTX.run s mat
  putStrLn "==============================="
