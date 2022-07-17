import System.Directory
import Data.List
import System.FilePath

main' = do 
    putStr "Substring: "    
    subString <- getLine -- "remo"
    if subString == "\n" then putStr "\n cancelled"
    else do 
        directory <- getCurrentDirectory -- "/home/dick/Templates/Haskell/monads/functions/remover"
        contents <- listDirectory directory -- ["remover.hs","something.hs"]
        check contents subString

type Substring = String

check :: [String] -> String -> IO () 
check [] _ = return ()
check (x:xs) subStr = checked where 
    checked = do 
        if any (== True) [pref x, suff x, inf x] 
        then do
            putStrLn $ "Removing file: " ++ x 
            removeFile x
        else check xs subStr
    pref a = subStr `isPrefixOf` a
    suff a = subStr `isSuffixOf` a
    inf a = subStr `isInfixOf` a