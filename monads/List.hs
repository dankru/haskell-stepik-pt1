--list monads create a tree of expressions which result
--in a multitude of different values
--list comprehensions are actually using list monads

--fail = []
list = [(x,y) | x <- [1,2,3], y <- [1,2], x /= y]

list' = do 
    x <- [1,2,3]
    y <- [4,5,6]
    return (x,y) 
    
lst = do 
    x <- [1,2,3]
    y <- [1,2]
    True <- return (x /= y)
    return (x,y)


list'' = 
    [1,2,3] >>= (\x -> 
    [4,5,6] >>= (\y -> 
    return (x,y)))

lst' = 
    [1,2,3]         >>= (\x ->
    [1,2]           >>= (\y ->
    return (x /= y) >>= (\b -> 
    case b of True -> return (x,y)
              _    -> fail "...")))

lst'' = do 
    x <- [1,2,3]
    y <- [1,2]
    if x /= y then return (x,y) 
    else []

lst''' = do
  x <- [1,2,3]
  y <- [1,2]
  if x /= y then "z" else []
  return (x,y)

pythagoreanTriple :: Int -> [(Int,Int,Int)]
pythagoreanTriple x = do
    c <- [1..x]
    b <- [1..c]
    a <- [1..b] 
    if a ^ 2 + b ^ 2 == c ^ 2 then "z" else []
    return (a,b,c)