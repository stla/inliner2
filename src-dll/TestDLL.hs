{-# LANGUAGE ForeignFunctionInterface #-}
module TestDLL where
import qualified Data.ByteString.Lazy as L
import Foreign
import Foreign.C
import Foreign.C.String (peekCString, newCString)

readAndWrite :: FilePath -> IO ()
readAndWrite file = do
  bs <- L.readFile file
  L.writeFile "tmp.tmp" bs
  return ()

foreign export ccall funexport :: Ptr CString -> Ptr CString -> IO ()
funexport :: Ptr CString -> Ptr CString -> IO ()
funexport file result = do
  file <- (>>=) (peek file) peekCString
  readAndWrite file
  (>>=) (newCString "done") (poke result)
