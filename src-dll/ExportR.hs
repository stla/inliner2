{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module ExportR
  where
import Lib 
import Control.Memory.Region
import Data.Singletons (sing)
import qualified Data.Vector.SEXP as VS
import Foreign
import Foreign.C
import Foreign.R
       (SEXP, SEXP0, SomeSEXP, SomeSEXP(..), allocList, allocVector,
        allocVectorProtected, cast, eval, globalEnv, lang2, 
        getAttribute, getAttributes, indexVector, install, integer, mkChar,
        mkString, nilValue, release, setAttributes,
        sexp, typeOf, unSomeSEXP, unprotect, unsexp, writeVector)
import qualified Foreign.R.Type as R
import Foreign.R.Type (SSEXPTYPE)
import Language.R.Literal (mkProtectedSEXPVectorIO, mkSEXPIO, fromSomeSEXP)
import System.IO.Unsafe (unsafePerformIO)

--import           Math.Polynomial
import           Math.Polynomial.Chebyshev

foreign export ccall rangeR :: Ptr CInt -> Ptr CInt -> Ptr (SEXP s 'R.Int) -> IO ()
rangeR :: Ptr CInt -> Ptr CInt -> Ptr (SEXP s 'R.Int) -> IO ()
rangeR a b result = do
    a <- peek a
    b <- peek b 
    poke result $ rangeSEXP a b

foreign export ccall test_myevalR :: Ptr CDouble -> Ptr CDouble -> IO ()
test_myevalR :: Ptr CDouble -> Ptr CDouble -> IO ()
test_myevalR x result = do
  x <- peek x
  poke result $ realToFrac $ myeval (realToFrac x :: Double)

foreign export ccall chebyshevFitR2 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR2 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR2 n result = do
  n <- peek n
  symbol <- (>>=) (newCString "f") install
  _genv <- peek globalEnv
  let genv = release _genv
  let {f x = do
      call <- lang2 symbol ((VS.toSEXP . VS.fromList) ([x :: Double]) :: SEXP V 'R.Real)
      ev <- eval call genv
      return (fromSomeSEXP ev :: Double)}
  let g = unsafePerformIO . f
  let fit = chebyshevFit (fromIntegral n :: Int) g
  poke result $ (VS.toSEXP . VS.fromList) fit

foreign export ccall chebyshevFitR3 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR3 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR3 n result = do
  n <- peek n
  let f = myeval 
  let fit = chebyshevFit (fromIntegral n :: Int) f
  poke result $ (VS.toSEXP . VS.fromList) fit

foreign export ccall chebyshevFitR4 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR4 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR4 n result = do
  n <- peek n
  let f = myeval  
  let fit = chebyshevFit (fromIntegral n :: Int) f
  (>>=) (realToSEXP fit) (poke result)

foreign export ccall chebyshevFitR5 :: Ptr (SEXP s 'R.Closure) -> Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR5 :: Ptr (SEXP s 'R.Closure) -> Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR5 f n result = do
  n <- peek n
  f <- peek f
  let g = myeval2 f 
  let fit = chebyshevFit (fromIntegral n :: Int) g
  (>>=) (realToSEXP fit) (poke result)

chebyshevCoefs :: CInt -> SEXP s 'R.Closure -> [Double]
chebyshevCoefs n f =
  chebyshevFit (fromIntegral n) (myeval2 f)
 
foreign export ccall chebApproxR :: Ptr CInt -> Ptr (SEXP s 'R.Closure) -> Ptr (SEXP s 'R.Real) -> Ptr (SEXP s 'R.Real) -> IO ()
chebApproxR :: Ptr CInt -> Ptr (SEXP s 'R.Closure) -> Ptr (SEXP s 'R.Real) -> Ptr (SEXP s 'R.Real) -> IO ()
chebApproxR n f x result = do
  n <- peek n
  f <- peek f
  x <- peek x
  let coefs = chebyshevCoefs n f
  xx <- sexpToReal x
  let y = map (\u -> sum (zipWith (*) coefs (evalTs u))) xx
  -- poke result $ (VS.toSEXP . VS.fromList) y
  (>>=) (realToSEXP y) (poke result) 

