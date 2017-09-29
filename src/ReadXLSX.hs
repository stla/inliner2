{-# LANGUAGE BangPatterns                #-}
module ReadXLSX
  where
import Lib
import           Data.Either.Extra    (fromRight')
import qualified Data.Map.Strict                   as DM
import           Data.Map.Strict             (Map)
import Codec.Xlsx
import Codec.Xlsx.Formatted
import qualified Data.Text as T
import Data.Text (Text)
import           Foreign.R            (SEXP, SEXP0, unsexp, sexp)
import qualified Foreign.R            as R
-- import qualified Foreign.R.Type       as R
import qualified Data.Vector.SEXP as VS
import Control.Memory.Region (V)
import           Foreign.C.String (newCString)
import           Data.Singletons                  (sing)
import Data.Maybe
import Control.Lens ((^?))
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable      as SV
import qualified Data.Vector.Storable.Mutable as MSV


type FormattedCellMap = Map (Int, Int) FormattedCell

emptyFormattedCell :: FormattedCell
emptyFormattedCell = def


cellValueToSEXP :: Maybe CellValue -> IO SEXP0
cellValueToSEXP cell =
  case cell of
    Just (CellDouble double) -> return $ realToSEXP0 double
    Just (CellBool bool) -> return $ unsexp ((VS.toSEXP . VS.fromList) [if bool then R.TRUE else R.FALSE] :: SEXP s 'R.Logical)
    Just (CellText text) -> do
                        string <- (>>=) (newCString (T.unpack text)) R.mkString
                        return $ unsexp string
    _ -> return c_null0

cellValuesToRVector :: [Maybe CellValue] -> IO (SEXP V 'R.Vector)
cellValuesToRVector cells = do
  let n = length cells
  list <- R.allocVectorProtected (sing :: R.SSEXPTYPE 'R.Vector) n
  let out i
       | i == n = do
           R.unprotect 1
           return list
       | otherwise = do
           sexp0 <- cellValueToSEXP $ cells !! i
           R.writeVector list i (sexp sexp0)
           out (i+1)
  out 0

extractColumn :: FormattedCellMap -> (FormattedCell -> IO SEXP0) -> Int -> Int -> IO (SEXP V 'R.Vector)
extractColumn fcells fcellToValue skip j = do
  let rowCoords = map fst $ DM.keys fcells
      maxRow = maximum rowCoords
      minRow = minimum rowCoords
--      rowRange = [skip + minRow .. maxRow]
      m = maxRow - minRow - skip + 1
  -- listOfSEXP0s <-
  --   mapM (\i ->
  --         fcellToValue $ fromMaybe emptyFormattedCell (DM.lookup (i,j) fcells))
  --           rowRange
  list <- R.allocVectorProtected (sing :: R.SSEXPTYPE 'R.Vector) m
  let out i
       | i == m = do
           R.unprotect 1
           return list
       | otherwise = do
           sexp0 <- fcellToValue $ fromMaybe emptyFormattedCell (DM.lookup (minRow + i,j) fcells)
           R.writeVector list i (sexp sexp0)
           out (i+1)
  out 0

formattedCellMapToRList :: FormattedCellMap -> (FormattedCell -> IO SEXP0) -> Int -> IO (SEXP V 'R.Vector)
formattedCellMapToRList fcells fcellToValue skip = do
  let colCoords = map snd $ DM.keys fcells
      minCol = minimum colCoords
      maxCol = maximum colCoords
      n = maxCol - minCol + 1
  let rowCoords = map fst $ DM.keys fcells
      maxRow = maximum rowCoords
      minRow = minimum rowCoords
      m = maxRow - minRow - skip + 1
  list <- R.allocVectorProtected (sing :: R.SSEXPTYPE 'R.Vector) n
  let out j
       | j == n = do
           R.unprotect (n+1)
           return list
       | otherwise = do
           column <- R.allocVectorProtected (sing :: R.SSEXPTYPE 'R.Vector) m
           !sexpr <- inner 0 j column
           R.writeVector list j sexpr
           out (j+1)
      inner i j !column
       | i == m = do
           -- R.unprotect 1
           return column
       | otherwise = do
           sexp0 <- fcellToValue $ fromMaybe emptyFormattedCell (DM.lookup (minRow + i, minCol +j) fcells)
           R.writeVector column i (sexp sexp0)
           inner (i+1) j column
  out 0

formattedCellMapToRList2 :: FormattedCellMap -> (FormattedCell -> IO SEXP0) -> Int -> IO (SEXP V 'R.Vector)
formattedCellMapToRList2 fcells fcellToValue skip = do
  let colCoords = map snd $ DM.keys fcells
      minCol = minimum colCoords
      maxCol = maximum colCoords
      n = maxCol - minCol + 1
  let rowCoords = map fst $ DM.keys fcells
      maxRow = maximum rowCoords
      minRow = minimum rowCoords
      m = maxRow - minRow - skip + 1
  list <- R.allocVectorProtected (sing :: R.SSEXPTYPE 'R.Vector) n
  let out j
       | j == n = do
           R.unprotect 1
           return list
       | otherwise = do
           let jshifted = minCol + j
           --let jfcells = DM.filterWithKey (\k _ -> snd k == jshifted) fcells 
           sexp0s <- SV.mapM (\i -> fcellToValue $ (DM.findWithDefault emptyFormattedCell (i, jshifted) fcells)) (SV.enumFromN minRow m)
           sexpr <- mkVector sexp0s
           R.writeVector list j sexpr
           out (j+1)
  out 0

formattedCellMapToRList3 :: FormattedCellMap -> (FormattedCell -> IO SEXP0) -> Int -> IO (SEXP V 'R.Vector)
formattedCellMapToRList3 fcells fcellToValue skip = do
  let colCoords = map snd $ DM.keys fcells
      minCol = minimum colCoords
      maxCol = maximum colCoords
      n = maxCol - minCol + 1
  let rowCoords = map fst $ DM.keys fcells
      maxRow = maximum rowCoords
      minRow = minimum rowCoords
      m = maxRow - minRow - skip + 1
  list <- R.allocVectorProtected (sing :: R.SSEXPTYPE 'R.Vector) n
  let out j
       | j == n = do
           R.unprotect 1
           return list
       | otherwise = do
           column <- MSV.new m :: IO (MSV.IOVector SEXP0)
           mvs <- inner 0 j column
           sexpr <- mkVector2 mvs
           R.writeVector list j sexpr
           out (j+1)
      inner i j !column
       | i == m = do
           return column
       | otherwise = do
           sexp0 <- fcellToValue $ fromMaybe emptyFormattedCell (DM.lookup (minRow + i, minCol +j) fcells)
           MSV.write column i sexp0
           inner (i+1) j column
  out 0
  
formattedCellMapToRList4 :: FormattedCellMap -> (FormattedCell -> IO SEXP0) -> Int -> IO SEXP0
formattedCellMapToRList4 fcells fcellToValue skip = do
  let colCoords = map snd $ DM.keys fcells
      minCol = minimum colCoords
      maxCol = maximum colCoords
      n = maxCol - minCol + 1
  let rowCoords = map fst $ DM.keys fcells
      maxRow = maximum rowCoords
      minRow = minimum rowCoords
      m = maxRow - minRow - skip + 1
  list <- allocProtectedVector n
  let out j
       | j == n = do
           R.unprotect 1
           return list
       | otherwise = do
           let jshifted = minCol + j
           sexp0s <- SV.mapM (\i -> fcellToValue $ (DM.findWithDefault emptyFormattedCell (i, jshifted) fcells)) (SV.enumFromN minRow m)
           sexp0 <- mkVector0 sexp0s
           writeInVector list sexp0 j
           out (j+1)
  out 0

fcellToCellValue :: FormattedCell -> IO SEXP0
fcellToCellValue fcell = cellValueToSEXP (_cellValue (_formattedCell fcell))

sheetToFormattedCellMap :: Xlsx -> StyleSheet -> Text -> FormattedCellMap
sheetToFormattedCellMap xlsx stylesheet sheetname =
  toFormattedCells (_wsCells ws) (_wsMerges ws) stylesheet
  where ws = fromJust $ xlsx ^? ixSheet sheetname

xlsxSheetToFormattedCellMap :: FilePath -> String -> IO FormattedCellMap
xlsxSheetToFormattedCellMap file sheetname = do
  (xlsx, stylesheet) <- getXlsxAndStyleSheet file
  return $ sheetToFormattedCellMap xlsx stylesheet (T.pack sheetname)

getXlsxAndStyleSheet :: FilePath -> IO (Xlsx, StyleSheet)
getXlsxAndStyleSheet file =
  do
    bs <- L.readFile file
    let xlsx = toXlsx bs
    let stylesheet = fromRight' $ parseStyleSheet $ _xlStyles xlsx
    return (xlsx, stylesheet)
--
bigList :: Int -> Int -> IO (SEXP V 'R.Vector)
bigList m n = do
  list <- R.allocVectorProtected (sing :: R.SSEXPTYPE 'R.Vector) n
  let out j
       | j == n = do
           R.unprotect (n+1)
           return list
       | otherwise = do
           column <- R.allocVectorProtected (sing :: R.SSEXPTYPE 'R.Vector) m
           !sexpr <- inner 0 j column
           R.writeVector list j sexpr
           out (j+1)
      inner i j !column
       | i == m = do
           -- R.unprotect 1
           return column
       | otherwise = do
           let sexp0 = realToSEXP0 $ fromIntegral (i+j)
           R.writeVector column i (sexp sexp0)
           inner (i+1) j column
  out 0
