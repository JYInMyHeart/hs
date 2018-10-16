module Copy where
import           System.Directory
import           System.FilePath

main = do
    filename <- getLine
    case filename of
        "" -> print "can't find the file"
        n -> do
            file <- findFile ["."] n
            case file of
                Nothing -> print "can't find the file"
                Just x ->
                    copyFile x "D://2.txt"


