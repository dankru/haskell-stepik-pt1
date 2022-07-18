import Prelude hiding (Monad, (>>=), (>>), return)

{-Monad laws:
1)return a >>= k = k a
2)m >>= return = m
3)(m >>= k) >>= k' = m >>= (\x -> k x >>= k')
-}


newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Show)

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b

{-instance Monad Identity where
    return x = Identity x
    Identity x >>= k = k x
    Identity x >> k = k
-}

wrap'n'succ :: (Enum a) => a -> Identity a
wrap'n'succ x = Identity (succ x)
    
goWrap0 = 
    runIdentity $
    wrap'n'succ 3 >>= 
    wrap'n'succ >>=
    wrap'n'succ >>= 
    return

goWrap1 = 
    wrap'n'succ 3 >>= (\x ->
    wrap'n'succ x >>= (\y ->
    wrap'n'succ y >>= (\z -> 
    return z)))

goWrap2 =
    wrap'n'succ 3 >>= (\x -> -- x := succ 3
    wrap'n'succ x >>= (\y -> -- y := succ x
    wrap'n'succ y >>= (\z -> -- z := succ y
    return (x,y,z))))        -- return x , y , z

goWrap3 = 
    wrap'n'succ 3 >>= (\x -> 
    wrap'n'succ x >>= (\y ->
    --wrap'n'succ y >>
    return (x + y)))

{-
do { e1 ; e2 } = e1 >> e2

do { p <- e1; e2 } = e1 >>= \p -> e2

do { let v = e1 ; e2 } = let v = e1 in do e2
-}

goWrap4 = 
    let i = 3 in 
    wrap'n'succ i >>= \x ->
    wrap'n'succ x >>= \y -> 
    wrap'n'succ y >>
    return (i, x + y)

goWrap5 = do
    let i = 3
    x <- wrap'n'succ i
    y <- wrap'n'succ x
    wrap'n'succ y
    return (i, x + y)