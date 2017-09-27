module ReadXLSX
  where
import Lib
import           Data.Either.Extra    (fromRight')
import qualified Data.Map                   as DM
import           Data.Map             (Map)
import Codec.Xlsx
import Codec.Xlsx.Formatted
import qualified Data.Text as T
import Data.Text (Text)
import           Foreign.R            (SEXP, SEXP0, unsexp, sexp)
import qualified Foreign.R            as R
import qualified Foreign.R.Type       as R
import qualified Data.Vector.SEXP as VS
import Control.Memory.Region (V)
import           Foreign.C.String (newCString)
import           Data.Singletons                  (sing)
import Data.Maybe
import Control.Lens ((^?))
import qualified Data.ByteString.Lazy as L


type FormattedCellMap = Map (Int, Int) FormattedCell

emptyFormattedCell :: FormattedCell
emptyFormattedCell = def


cellValueToSEXP :: Maybe CellValue -> IO SEXP0
cellValueToSEXP cell =
  case cell of
    Just (CellDouble double) -> return $ unsexp ((VS.toSEXP . VS.fromList) [double] :: SEXP s 'R.Real)
    Just (CellBool bool) -> return $ unsexp ((VS.toSEXP . VS.fromList) [if bool then R.TRUE else R.FALSE] :: SEXP s 'R.Logical)
    Just (CellText text) -> do
                        string <- (>>=) (newCString (T.unpack text)) R.mkString
                        return $ unsexp string
    _ -> return c_null0

cellValuesToRVector :: [Maybe CellValue] -> IO (SEXP V 'R.Vector)
cellValuesToRVector cells = do
  let n = length cells
  list <- R.allocVectorProtected (sing :: R.SSEXPTYPE R.Vector) n
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
      rowRange = [skip + minRow .. maxRow]
      n = maxRow - minRow - skip
  listOfSEXP0s <-
    mapM (\i ->
          fcellToValue $ fromMaybe emptyFormattedCell (DM.lookup (i,j) fcells))
            rowRange
  list <- R.allocVectorProtected (sing :: R.SSEXPTYPE R.Vector) n
  let out i
       | i == n = do
           R.unprotect 1
           return list
       | otherwise = do
           R.writeVector list i (sexp (listOfSEXP0s !! i))
           out (i+1)
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
