{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module ExportR
  where
import           Control.Memory.Region
import           Data.Singletons           (sing)
-- import qualified Data.Vector.Generic       as GV
import qualified Data.Vector.SEXP          as VS
import qualified Data.Vector.Storable      as SV
import           Foreign
import           Foreign.C
import Foreign.C.String (peekCString)
import           Foreign.R                 (SEXP, SEXP0, SomeSEXP, SEXP (..),
                                            SomeSEXP (..), allocList,
                                            allocVector, allocVectorProtected,
                                            cast, eval, getAttribute,
                                            getAttributes, globalEnv,
                                            indexVector, install, integer,
                                            lang2, mkChar, mkString, nilValue,
                                            release, setAttributes, sexp,
                                            typeOf, unSomeSEXP, unprotect,
                                            unsexp, writeVector)
import           Foreign.R.Type            (SSEXPTYPE)
import qualified Foreign.R.Type            as R
import           Language.R.Literal        (fromSomeSEXP, fromSEXP, 
                                            mkProtectedSEXPVectorIO, mkSEXPIO)
import           Lib
import           System.IO.Unsafe          (unsafePerformIO)
import           Math.Polynomial.Chebyshev
import Codec.Xlsx.Types
import ReadXLSX 

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

foreign export ccall test_myeval2R :: Ptr (SEXP s 'R.Closure) -> Ptr CDouble -> Ptr CDouble -> IO ()
test_myeval2R :: Ptr (SEXP s 'R.Closure) -> Ptr CDouble -> Ptr CDouble -> IO ()
test_myeval2R f x result = do
  f <- peek f
  x <- peek x
  poke result $ realToFrac $ myeval2 f (realToFrac x :: Double)

foreign export ccall chebyshevFitR2 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR2 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR2 n result = do
  n <- peek n
  symbol <- (>>=) (newCString "f") install
  _genv <- peek globalEnv
  let genv = release _genv
  let {f x = do
      call <- lang2 symbol ((VS.toSEXP . VS.fromList) [x :: Double] :: SEXP V 'R.Real)
      ev <- eval call genv
      return (fromSomeSEXP ev :: Double)}
  let g = unsafePerformIO . f
  let fit = chebyshevFit (fromIntegral n :: Int) g
  poke result $ (VS.toSEXP . VS.fromList) fit

foreign export ccall chebyshevFitR3 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR3 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR3 n result = do
  n <- peek n
  let fit = chebyshevFit (fromIntegral n :: Int) myeval
  poke result $ (VS.toSEXP . VS.fromList) fit

foreign export ccall chebyshevFitR4 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR4 :: Ptr CInt -> Ptr (SEXP V 'R.Real) -> IO ()
chebyshevFitR4 n result = do
  n <- peek n
  let fit = chebyshevFit (fromIntegral n :: Int) myeval
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
  let y = map (sum . zipWith (*) coefs . evalTs) xx
  -- poke result $ (VS.toSEXP . VS.fromList) y
  (>>=) (realToSEXP y) (poke result)

foreign export ccall sliceR :: Ptr (SEXP s 'R.Real) -> Ptr CInt -> Ptr CInt -> Ptr (SEXP s 'R.Real) -> IO ()
sliceR :: Ptr (SEXP s 'R.Real) -> Ptr CInt -> Ptr CInt -> Ptr (SEXP s 'R.Real) -> IO ()
sliceR vectorR i n result = do
  vector <- (>>=) (peek vectorR) sexpToCRealVector
  i <- peek i
  n <- peek n
  let slicedVector = SV.slice (fromIntegral i) (fromIntegral n) vector
  -- let slicedVector = GV.basicUnsafeSlice (fromIntegral i) (fromIntegral n) vector
  (>>=) (realCVectorToSEXPN slicedVector n) (poke result)

foreign export ccall whichR :: Ptr (SEXP s 'R.Real) -> Ptr CDouble -> Ptr (SEXP s 'R.Int) -> IO ()
whichR :: Ptr (SEXP s 'R.Real) -> Ptr CDouble -> Ptr (SEXP s 'R.Int) -> IO ()
whichR vectorR a result = do
  vector <- (>>=) (peek vectorR) sexpToCRealVector
  a <- peek a
  let indices = SV.elemIndices a vector
  (>>=) (intVectorToSEXP indices) (poke result)

foreign export ccall vectorAppendR :: SEXP s 'R.Vector -> SEXP s a -> SEXP s 'R.Vector
vectorAppendR :: SEXP s 'R.Vector -> SEXP s a -> SEXP s 'R.Vector
vectorAppendR = vectorAppend

cells :: [Maybe CellValue]
cells = [Just (CellDouble 1), Just (CellDouble 2), Nothing, Just (CellBool True),
         Just (CellText "hello")]

foreign export ccall test00 :: Ptr (SEXP V 'R.Vector) -> IO ()
test00 :: Ptr (SEXP V 'R.Vector) -> IO ()
test00 result = (>>=) (cellValuesToRVector cells) (poke result)

foreign export ccall testmkVector :: IO (SEXP s 'R.Vector)
testmkVector :: IO (SEXP s 'R.Vector)
testmkVector = do
  sexp0s <- mapM cellValueToSEXP cells
  let sv = SV.fromList sexp0s
  mkVector sv

listOfSEXP0s :: IO [SEXP0]  
listOfSEXP0s = mapM cellValueToSEXP $ [Just (CellDouble i) | i <- [1..1000]]

foreign export ccall test01 :: Ptr CString -> Ptr CString -> Ptr CInt -> Ptr (SEXP V 'R.Vector) -> IO ()
test01 :: Ptr CString -> Ptr CString -> Ptr CInt -> Ptr (SEXP V 'R.Vector) -> IO ()
test01 file sheet index result = do
  file <- (>>=) (peek file) peekCString
  sheet <- (>>=) (peek sheet) peekCString
  index <- peek index
  fcellmap <- xlsxSheetToFormattedCellMap file sheet
  list <- extractColumn fcellmap fcellToCellValue 0 (fromIntegral index)
  poke result list

foreign export ccall test02 :: Ptr CString -> Ptr CString -> Ptr (SEXP V 'R.Vector) -> IO ()
test02 :: Ptr CString -> Ptr CString -> Ptr (SEXP V 'R.Vector) -> IO ()
test02 file sheet result = do
  file <- (>>=) (peek file) peekCString
  sheet <- (>>=) (peek sheet) peekCString
  fcellmap <- xlsxSheetToFormattedCellMap file sheet
  list <- formattedCellMapToRList fcellmap fcellToCellValue 0 
  --list2 <- formattedCellMapToRList fcellmap fcellToCellValue 0 
  poke result list

foreign export ccall test02bis :: Ptr (SEXP V 'R.Vector) -> IO ()
test02bis :: Ptr (SEXP V 'R.Vector) -> IO ()
test02bis result = do
  fcellmap <- xlsxSheetToFormattedCellMap "big.xlsx" "Sheet 1"
  list <- formattedCellMapToRList fcellmap fcellToCellValue 0 
  poke result list

foreign export ccall test03 :: SEXP V 'R.Vector
test03 :: SEXP V 'R.Vector
test03 = unsafePerformIO $ do 
  fcellmap <- xlsxSheetToFormattedCellMap "big.xlsx" "Sheet 1"
  formattedCellMapToRList fcellmap fcellToCellValue 0

foreign export ccall test04 :: SEXP s 'R.String -> SEXP s 'R.String -> SEXP V 'R.Vector
test04 :: SEXP s 'R.String -> SEXP s 'R.String -> SEXP V 'R.Vector
test04 file sheet = unsafePerformIO $ do 
  fcellmap <- xlsxSheetToFormattedCellMap (fromSEXP file) (fromSEXP sheet)
  formattedCellMapToRList fcellmap fcellToCellValue 0

foo :: String -> String -> SEXP V 'R.Vector
foo file sheet = unsafePerformIO $ do 
  fcellmap <- xlsxSheetToFormattedCellMap file sheet
  formattedCellMapToRList fcellmap fcellToCellValue 0

foreign export ccall test05 :: SEXP V 'R.String -> SEXP V 'R.String -> SEXP V 'R.Vector
test05 :: SEXP V 'R.String -> SEXP V 'R.String -> SEXP V 'R.Vector
test05 file sheet = foo (fromSEXP file) (fromSEXP sheet)

foreign export ccall test06 :: SEXP s 'R.String -> SEXP s 'R.String -> IO (SEXP V 'R.Vector)
test06 :: SEXP s 'R.String -> SEXP s 'R.String -> IO (SEXP V 'R.Vector)
test06 file sheet = do 
  fcellmap <- xlsxSheetToFormattedCellMap (fromSEXP file) (fromSEXP sheet)
  formattedCellMapToRList fcellmap fcellToCellValue 0

foreign export ccall test07 :: SEXP s 'R.String -> SEXP s 'R.String -> IO (SEXP V 'R.Vector)
test07 :: SEXP s 'R.String -> SEXP s 'R.String -> IO (SEXP V 'R.Vector)
test07 file sheet = do 
  fcellmap <- xlsxSheetToFormattedCellMap (fromSEXP file) (fromSEXP sheet)
  formattedCellMapToRList2 fcellmap fcellToCellValue 0

foreign export ccall test08 :: SEXP s 'R.String -> SEXP s 'R.String -> IO (SEXP V 'R.Vector)
test08 :: SEXP s 'R.String -> SEXP s 'R.String -> IO (SEXP V 'R.Vector)
test08 file sheet = do 
  fcellmap <- xlsxSheetToFormattedCellMap (fromSEXP file) (fromSEXP sheet)
  formattedCellMapToRList3 fcellmap fcellToCellValue 0

foreign export ccall testWriteVector :: IO SEXP0
testWriteVector :: IO SEXP0
testWriteVector = do
  list <- allocProtectedVector 1
  writeInVector list (realToSEXP0 1.5) 0
  unprotect 1
  return list

foreign export ccall test09 :: SEXP s 'R.String -> SEXP s 'R.String -> IO SEXP0
test09 :: SEXP s 'R.String -> SEXP s 'R.String -> IO SEXP0
test09 file sheet = do 
  fcellmap <- xlsxSheetToFormattedCellMap (fromSEXP file) (fromSEXP sheet)
  formattedCellMapToRList4 fcellmap fcellToCellValue 0

foreign export ccall bigListR :: Ptr CInt -> Ptr CInt -> Ptr (SEXP V 'R.Vector) -> IO ()
bigListR :: Ptr CInt -> Ptr CInt -> Ptr (SEXP V 'R.Vector) -> IO ()
bigListR m n result = do
  m <- peek m
  n <- peek n
  (>>=) (bigList (fromIntegral m) (fromIntegral n)) (poke result)