{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}

module Lib where

import Foreign (Ptr, peekArray)
import Foreign.C.Types 
import Foreign.R (SEXP, SEXP0, sexp, unsexp)
import qualified Foreign.R as R
import qualified Foreign.R.Type as R
import qualified Data.Vector.Storable as SV


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
      (return . sexp . (c_realToSEXP (fromIntegral (length list))))

foreign import ccall "SEXPtoReal" c_SEXPtoReal :: SEXP0 -> Ptr CDouble
sexpToReal :: SEXP s 'R.Real -> IO ([Double])
sexpToReal vectorR = do
    let vectorPtr = c_SEXPtoReal (unsexp vectorR)
    n <- R.length vectorR
    vector <- peekArray n vectorPtr
    return $ map realToFrac vector