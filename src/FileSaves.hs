module FileSaves where

import           Control.Monad
import           Data.Binary
import           ProjectInfo

saveProject :: FilePath -> ProjectInfo -> IO ()
saveProject filepath project = do
    void $ encodeFile filepath $ project {info = (info project) {saveFile = filepath}}

loadProject :: FilePath -> IO ProjectInfo
loadProject filepath = do
    decoded <- decodeFileOrFail filepath
    case (decoded) of
        Left (ofset, mes) -> do
            print ofset
            putStrLn mes
            return empty
        (Right project) -> return project
