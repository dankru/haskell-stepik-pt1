import System.Directory
import Data.List
import System.FilePath

{-In this program you must enter a string when you
asked to and this will delete all files from
the current directory for whom the string you entered
is a substring-}

main' = do 
    putStr "Substring: "    
    subString <- getLine -- "hell"
    if subString == "" then putStrLn "Canceled"
    else do 
        --directory <- getCurrentDirectory -- "/home/dick/Templates/Haskell/monads/functions/remover"
        contents <- getDirectoryContents "." -- ["remover.hs","something.hs","hello.world"]
        check contents subString

check :: [String] -> String -> IO () 
check [] _ = return ()
check (x:xs) subStr = checked where 
    checked = do 
        if any (== True) [pref x, suff x, inf x] 
        then do
            putStrLn $ "Removing file: " ++ x 
            removeFile x
            check xs subStr
        else check xs subStr
    pref a = subStr `isPrefixOf` a
    suff a = subStr `isSuffixOf` a
    inf a = subStr `isInfixOf` a