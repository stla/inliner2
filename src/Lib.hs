{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
--import qualified Data.Vector.SEXP as VS
import           Foreign              (Ptr, newForeignPtr_, peekArray, mallocForeignPtrArray, 
                                      withForeignPtr, peek)
import           Foreign.C.Types
import           Foreign.R            (SEXP, SEXP0, sexp, unsexp)
import qualified Foreign.R            as R
--import qualified Foreign.R.Type       as R
--import System.IO.Unsafe (unsafePerformIO)


foreign import ccall unsafe "rangeSEXP" c_rangeSEXP :: CInt -> CInt -> SEXP0
rangeSEXP :: CInt -> CInt -> SEXP s 'R.Int
rangeSEXP a b = sexp $ c_rangeSEXP a b

foreign import ccall unsafe "myeval" c_myeval :: CDouble -> CDouble
myeval :: Double -> Double
myeval x = realToFrac (c_myeval (realToFrac x))

foreign import ccall unsafe "myeval2" c_myeval2 :: SEXP0 -> CDouble -> CDouble
myeval2 :: SEXP s 'R.Closure -> Double -> Double
myeval2 f x = realToFrac (c_myeval2 (unsexp f) (realToFrac x))


foreign import ccall unsafe "realToSEXP" c_realToSEXP :: CInt -> Ptr CDouble -> SEXP0

realToSEXP :: [Double] -> IO (SEXP s 'R.Real)
realToSEXP list =
  SV.unsafeWith
    (SV.fromList (map realToFrac list :: [CDouble]))
      (return . sexp . c_realToSEXP (fromIntegral (length list)))

realCVectorToSEXP :: SV.Vector CDouble -> IO (SEXP s 'R.Real)
realCVectorToSEXP vector =
  SV.unsafeWith
    vector
      (return . sexp . c_realToSEXP (fromIntegral (SV.length vector)))

realCVectorToSEXPN :: SV.Vector CDouble -> CInt -> IO (SEXP s 'R.Real)
realCVectorToSEXPN vector n =
  SV.unsafeWith
    vector
      (return . sexp . c_realToSEXP n)

foreign import ccall unsafe "SEXPtoReal" c_SEXPtoReal :: SEXP0 -> Ptr CDouble

sexpToReal :: SEXP s 'R.Real -> IO [Double]
sexpToReal vectorR = do
    let vectorPtr = c_SEXPtoReal (unsexp vectorR)
    n <- R.length vectorR
    vector <- peekArray n vectorPtr
    return $ map realToFrac vector

sexpToCRealVector :: SEXP s 'R.Real -> IO (SV.Vector CDouble)
sexpToCRealVector vectorR = do
    let vectorPtr = c_SEXPtoReal (unsexp vectorR)
    n <- R.length vectorR
    fptr <- newForeignPtr_ vectorPtr
    return $ SV.unsafeFromForeignPtr0 fptr n

sexpToRealVector :: SEXP s 'R.Real -> IO (SV.Vector Double)
sexpToRealVector vectorR = do
    let vectorPtr = c_SEXPtoReal (unsexp vectorR)
    n <- R.length vectorR
    fptr <- newForeignPtr_ vectorPtr
    return $ SV.map realToFrac (SV.unsafeFromForeignPtr0 fptr n)

-- sliceSEXP :: SEXP s 'R.Real -> CInt -> CInt -> IO (SEXP s 'R.Real)
-- sliceSEXP vectorR = do

foreign import ccall unsafe "intToSEXP" c_intToSEXP :: CInt -> Ptr CInt -> SEXP0

intVectorToSEXP :: SV.Vector Int -> IO (SEXP s 'R.Int)
intVectorToSEXP vector =
  SV.unsafeWith
    (SV.map fromIntegral vector)
      (return . sexp . c_intToSEXP (fromIntegral (SV.length vector)))


foreign import ccall unsafe "vectorAppend" c_vectorAppend :: SEXP0 -> SEXP0 -> SEXP0

vectorAppend :: SEXP s 'R.Vector -> SEXP s a -> SEXP s 'R.Vector
vectorAppend list x = sexp (c_vectorAppend (unsexp list) (unsexp x))

vectorAppendIO :: SEXP s 'R.Vector -> SEXP0 -> IO (SEXP s 'R.Vector)
vectorAppendIO list x = return $ sexp (c_vectorAppend (unsexp list) x)

foreign import ccall unsafe "null0" c_null0 :: SEXP0

foreign import ccall unsafe "mkVector" c_mkVector :: Ptr SEXP0 -> CInt -> IO SEXP0

mkVector :: SV.Vector SEXP0 -> IO (SEXP s 'R.Vector)
mkVector sv = do
    -- fptr <- mallocForeignPtrArray n
    -- sexp0 <- withForeignPtr fptr $ \x -> c_mkVector x (fromIntegral n)
    -- return $ sexp sexp0
    sexp0 <- SV.unsafeWith sv (\x -> c_mkVector x (fromIntegral n))
    --sexp0 <- peek ptr
    return $ sexp sexp0
  where
    n = SV.length sv

mkVector0 :: SV.Vector SEXP0 -> IO SEXP0
mkVector0 sv = 
    SV.unsafeWith sv (\x -> c_mkVector x (fromIntegral (SV.length sv)))

mkVector2 :: MSV.IOVector SEXP0 -> IO (SEXP s 'R.Vector)
mkVector2 sv = do
    sexp0 <- MSV.unsafeWith sv (\x -> c_mkVector x (fromIntegral n))
    return $ sexp sexp0
  where
    n = MSV.length sv

foreign import ccall unsafe "Rf_ScalarReal" c_ScalarReal :: CDouble -> SEXP0
realToSEXP0 :: Double -> SEXP0
realToSEXP0 x = c_ScalarReal (realToFrac x) 

foreign import ccall unsafe "allocProtectedVector" c_allocProtectedVector :: CInt -> IO SEXP0
allocProtectedVector :: Int -> IO SEXP0
allocProtectedVector i = c_allocProtectedVector (fromIntegral i)

foreign import ccall unsafe "writeInVector" c_writeInVector :: SEXP0 -> SEXP0 -> CInt -> IO ()
writeInVector :: SEXP0 -> SEXP0 -> Int -> IO ()
writeInVector list element index = c_writeInVector list element (fromIntegral index)
