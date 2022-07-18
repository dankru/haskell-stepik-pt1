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
        let cList = check contents subString
        print cList

type Substring = String

check (x:xs) subStr = if checked then 
    checked = [pref x, suff x inf x] 
    pref = subStr `isPrefixOf` 
    suff = subStr `isSuffixOf` 
    inf  = subStr `isInfixOf` 