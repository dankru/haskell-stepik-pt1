{-import Prelude hiding (Maybe, Just, Nothing)
import qualified Control.Monad.Fail as Fail 

data Maybe a = Nothing | Just a deriving (Eq, Ord, Show)


Class Monad m where 
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    fail :: String -> m a
-}

{-
instance Monad Maybe where
    return = Just 

    Nothing >>= _   = Nothing
    Just x  >>= k   = k x 

    Nothing >> _    = Nothing
    Just _  >> m    = m
    
instance Fail.MonadFail Maybe where
    fail _ = Nothing

instance Functor Maybe where
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  Just f <*> Just x = Just (f x)

instance Monad [] where
  return x = [x]
  xs >>= k = concat (map k xs)
  fail _ = []
-}

type Name = String
type DataBase = [(Name, Name)]

fathers, mothers :: DataBase 
fathers = [ ("Bill", "John")
          , ("Ann", "John")
          , ("John", "Peter")] 
mothers = [ ("Bill", "Jane")
          , ("Ann", "Jane")
          , ("John", "Alice")
          , ("Jane", "Dorothy")
          , ("Alice", "Mary")]

getM, getF :: Name -> Maybe Name
getM person = lookup person mothers
getF person = lookup person fathers

granmas :: Name -> Maybe (Name, Name)
granmas person = do
    m    <- getM person
    gm   <- getM m
    f    <- getF person
    gfm  <- getM f
    return (gm,gfm)

