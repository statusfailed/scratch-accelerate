{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
module BlasTest where

import Prelude hiding (Num, zipWith, Eq, (==))
import qualified Prelude as P
import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as LLVM
import qualified Data.Array.Accelerate.LLVM.PTX as PTX

import Data.Array.Accelerate.Numeric.LinearAlgebra ((#>))
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra as LinearAlgebra

--import Linear
import Data.Array.Accelerate.Linear

import Foreign.Ptr
import qualified Blas.Primitive.Types    as C
import qualified Blas.Primitive.Unsafe   as C

import qualified Data.Array.Accelerate.Type                 as A
import qualified Data.Array.Accelerate.Representation.Array as Repr
import qualified Data.Array.Accelerate.Representation.Shape as Repr
import qualified Data.Array.Accelerate.Representation.Type  as Repr
import qualified Data.Array.Accelerate.Array.Unique         as Unique

import Data.Array.Accelerate.Sugar.Array as Sugar
import Data.Array.Accelerate.Sugar.Elt   as Sugar
import Data.Array.Accelerate.LLVM.Native.Foreign as LLVM

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
  putStrLn "-------------------------------"

  putStrLn "running ffitest..."
  ffitest
  putStrLn "-------------------------------"


-- | This is the example Accelerate program as provided in the documentation of
-- @Data.Array.Accelerate@
dotp :: Num a => Vector a -> Vector a -> Acc (Scalar a)
dotp xs ys =
  let
      xs' = use xs
      ys' = use ys
  in
  fold (+) 0 ( zipWith (*) xs' ys' )

ixs :: (Elt e, A.FromIntegral Int e) => Int -> Acc (Vector e)
ixs n = generate (index1 $ lift n) (\i -> A.fromIntegral (unindex1 i))

test1 :: IO ()
test1 = do
  let xs = ixs 3 :: Acc (Vector Int)
  print xs
  print $ LLVM.run xs
  print $ PTX.run xs

identityN :: (P.Num e, Elt e, A.FromIntegral Int e) => Int -> Acc (Matrix e)
identityN n =
  generate (index2 n' n') (f . unindex2)
  where
    n' = lift n
    f ix =
      let (i, j) = (A.fst ix, A.snd ix)
      in  if i == j then constant 1 else constant 0

test2 :: IO ()
test2 = do
  let xs = identityN 3 :: Acc (Matrix Int)
  print xs
  print $ LLVM.run xs
  print $ PTX.run xs

multest :: IO ()
multest = do
  let m = identityN 3 :: Acc (Matrix Float)
      v = ixs 3 :: Acc (Vector Float)
  print . LLVM.run $ m #> v
  print . PTX.run $ m #> v

--idtest :: IO ()
--idtest = do
  --let v = fromList (Z :. 10) [0..] :: Vector Int
  --print $ LLVM.run $ identity !* v

-------------------------------
-- FFI BLAS example
-- Example of the FFI using matrix multiplication from the blas library.
-- NOTE: this uses

ffitest :: IO ()
ffitest = do
  let m = identityN 3 :: Acc (Matrix Float)
      v = ixs 3 :: Acc (Vector Float)
  print . LLVM.run $ foreignDotp m v

-- dot product, implemented in foreign code (cblas) via the FFI.
foreignDotp :: Acc (Matrix Float) -> Acc (Vector Float) -> Acc (Vector Float)
foreignDotp m v = foreignAcc sgemv (\(T2 m v) -> m #> v) (lift $ (m, v))

data Transpose
  = N
  | T
  | H
  deriving (P.Eq, P.Show)

encodeTranspose :: Transpose -> C.Transpose
encodeTranspose N = C.NoTrans
encodeTranspose T = C.Trans
encodeTranspose H = C.ConjTrans

floatTypeR :: Repr.TypeR Float
floatTypeR = Repr.TupRsingle floatType

floatType :: A.ScalarType Float
floatType = A.SingleScalarType $ A.NumSingleType $ A.FloatingNumType A.TypeFloat

-- | gemv is matrix-vector multiplication, sgemv means "single precision" gemv -
-- i.e., 32-bit floating point gemv.
-- read more here: https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms#Level_2
-- (actually, we omit the alpha and beta scalars and just set them to 1- so
-- technically this isn't even sgemv... just for illustration!)
--
-- gemv is the operation \\alpha A x + \\beta y with \\alpha, \\beta scalars, x, y
-- vectors, and A a matrix.
sgemv :: LLVM.ForeignAcc (Sugar.ArraysR (Matrix Float, Vector Float) -> Sugar.ArraysR (Vector Float))
sgemv = ForeignAcc "native.sgemv" go
  where
    go (((), matA :: Repr.Array Repr.DIM2 Float), vecx :: Repr.Array Repr.DIM1 Float) = do
      -- :: Par Native (Future (Repr.Array ((), Int) Float))
      let
        (((), rowsA), colsA) = Repr.shape matA
        sizeY = rowsA -- note: missed case
        opA' = encodeTranspose N
        alpha' = 1 :: Float -- gemv is the

      future <- new
      (vecy :: Repr.Array ((), Int) e) <- allocateRemote (Repr.ArrayR Repr.dim1 floatTypeR) ((), sizeY)
      () <- liftIO $ do
        withFloatArray matA     $ \ptr_A -> do
          withFloatArray vecx   $ \ptr_x -> do
            withFloatArray vecy $ \ptr_y -> do
              C.sgemv C.RowMajor opA' rowsA colsA alpha' ptr_A colsA ptr_x 1 0 ptr_y 1
      put future vecy
      return future

{-# INLINE withFloatArray #-}
withFloatArray
    :: forall sh b . ()
    => Repr.Array sh Float -- ^ e.g. Matrix Double
    -> (Ptr Float -> IO b) -- ^ ???
    -> IO b
withFloatArray (Repr.Array _ ad) k = Unique.withUniqueArrayPtr (ad :: Unique.UniqueArray Float) k
