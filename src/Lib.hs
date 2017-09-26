{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import qualified Data.Vector.Storable as SV
import           Foreign              (Ptr, newForeignPtr_, peekArray)
import           Foreign.C.Types
import           Foreign.R            (SEXP, SEXP0, sexp, unsexp)
import qualified Foreign.R            as R
import qualified Foreign.R.Type       as R


foreign import ccall "rangeSEXP" c_rangeSEXP :: CInt -> CInt -> SEXP0
rangeSEXP :: CInt -> CInt -> SEXP s 'R.Int
rangeSEXP a b = sexp $ c_rangeSEXP a b

foreign import ccall "myeval" c_myeval :: CDouble -> CDouble
myeval :: Double -> Double
myeval x = realToFrac (c_myeval (realToFrac x))

foreign import ccall "myeval2" c_myeval2 :: SEXP0 -> CDouble -> CDouble
myeval2 :: SEXP s 'R.Closure -> Double -> Double
myeval2 f x = realToFrac (c_myeval2 (unsexp f) (realToFrac x))

foreign import ccall "realToSEXP" c_realToSEXP :: CInt -> Ptr CDouble -> SEXP0

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

foreign import ccall "SEXPtoReal" c_SEXPtoReal :: SEXP0 -> Ptr CDouble

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
