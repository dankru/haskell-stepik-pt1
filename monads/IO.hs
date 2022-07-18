import Control.Monad

{-main :: IO()
main = do 
    putStr "What is your name?\nName: "
    name <- getLine 
    if null name then main
    else do
        putStrLn name
        putStr $ "Hi, " ++ name ++ "!"
-}
{-

getCharFromConsole :: Char

getCharFromConsole :: RealWolrd -> (RealWorld, Char)

-}

{-
type IO a = (RealWorld -> (RealWorld, a))

return :: a -> IO a 
(>>=) :: IO a -> (a -> IO b) ->  IO b

return :: a -> (RealWorld -> (RealWorld, a))

(>>=) :: (RealWorld -> (RealWorld, a))
-> (a -> RealWorld -> (RealWorld, b))
-> RealWorld -> (RealWorld, b)

kleisli :: a -> m b 

instance Monad IO where
    return a = \w -> (w,a)
    (>>=) m k = \w -> case m w of (w', a) -> k a w' 
-}

getLine' :: IO String
getLine' = do
    ch <- getChar
    if ch == '\n' 
        then return []
    else do
        cs <- getLine'
        return (ch:cs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

--import Control.Monad

sequence_' :: Monad m => [m a] -> m ()
sequence_' = foldr (>>) (return ())

putStr'' :: String -> IO ()
putStr'' = sequence_ . map putChar

mapM_' :: Monad m => (a -> m b) -> [a] -> m ()
mapM_' f = sequence_ . map f 

sequence' :: Monad m => [m a] -> m [a]
sequence' ms = foldr k (return []) ms
    where
        k :: Monad m => m a -> m [a] -> m [a]
        k m m' = do 
            x <- m
            xs <- m'
            return (x:xs)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = sequence . map f 