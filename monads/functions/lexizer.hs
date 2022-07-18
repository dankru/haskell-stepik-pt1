import Data.Char 
import Data.String

data Token = Plus | Minus | Number Int | LeftBrace | RightBrace deriving 
    (Show,Eq)

{-
do {e1 ; e2} = e1 >> e2

do {p <- e1 ; e2} = e1 >>= \p -> e2

do {let v = e1 ; e2 } = let v = e1 in do e2
-}

asToken :: String -> Maybe Token
asToken s | s == "+" = Just Plus
		  | s == "-" = Just Minus
		  | s == "(" = Just LeftBrace
		  | s == ")" = Just RightBrace
		  | all isDigit	s = Just $ Number $ read s
		  | otherwise = Nothing


tokenize''' :: String -> Maybe [Token]
tokenize''' = foldr (\x xs -> xs >>= \y -> fmap (:y) $ asToken x) (Just []) . words

tokenize'' :: String -> Maybe [Token]
tokenize'' input = foldr program (return []) (words input) where
	program word list = do 
						t <- asToken word
						tl <- list
						return (t:tl)
						
tokenize' :: String -> Maybe [Token]
tokenize' s = f $ words s where
	f :: [String] -> Maybe [Token]
	f [] = Just []
	f (x:xs) = do
		t <- asToken x --Token
		tl <- f xs -- [Token]
		return (t:tl) 

tokenize :: String -> Maybe [Token]
tokenize s = foldr f (return []) (words s) where
	f word list = do
					token <- asToken word
					tokens <- list
					return $ token:tokens

{-
--import Data.Char
--import Data.String

asToken :: String -> Maybe Token
asToken "" = Nothing
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken str = helper str >>= (\(res,_) -> Just $ Number res) where
	helper :: String -> Maybe (Int,Int)
	helper [] = return (0, 1)
	helper (c:cs) = toInt c >>= \i -> return i >> helper cs >>= \(is, pow) -> Just (is+i*pow, 10*pow) where
	toInt c = if not $ isDigit c then Nothing else Just $ digitToInt c

tokenize :: String -> Maybe [Token]
tokenize = helper . words where
	helper :: [String] -> Maybe [Token]
	helper [] = return []
	helper (line:lines) = asToken line >>= \token -> return token >> helper lines >>= (\tokens -> Just (token:tokens))
-}
{-
asToken :: String -> Maybe Token
asToken s | s == "+" = Just Plus
		  | s == "-" = Just Minus
		  | s == "(" = Just LeftBrace
		  | s == ")" = Just RightBrace
		  | all isDigit	s = Just $ Number $ read s
		  | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize s = foldr f (return []) (words s) where
	f word list = do
					token <- asToken word
					tokens <- list
					return $ token:tokens
-}