{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding (Num, zipWith)
import Data.Array.Accelerate
import qualified Data.Array.Accelerate.LLVM.Native as LLVM
import qualified Data.Array.Accelerate.LLVM.PTX as PTX

-- | Run the dot product example code on the CPU and GPU.
main :: IO ()
main = do
  putStrLn "Make a vector"
  let v = fromList (Z :. 10) [0..] :: Vector Int
  print v
  putStrLn "-------------------------------"
  
  putStrLn "dot product program"
  let prog = dotp v v
  print prog
  putStrLn "-------------------------------"

  putStrLn "running on CPU..."
  print (LLVM.run prog)
  putStrLn "-------------------------------"

  putStrLn "running on GPU (CUDA)..."
  print (PTX.run prog)
  return ()

-- | This is the example Accelerate program as provided in the documentation of
-- @Data.Array.Accelerate@
dotp :: Num a => Vector a -> Vector a -> Acc (Scalar a)
dotp xs ys =
  let
      xs' = use xs
      ys' = use ys
  in
  fold (+) 0 ( zipWith (*) xs' ys' )
