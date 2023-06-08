and' [] = True
and' (b:bs) = b && and' bs

and'' [] = True
and'' (False:_) = False
and'' (True:bs) = and'' bs

and''' xs = foldr (&&) True xs

concat' xss = foldr (++) [] xss 

concat'' [] = []
concat'' (xs:xss) = xs ++ concat'' xss


repeat' x = [x] ++ repeat' x

repeat'' x = x: repeat'' x

replicate' n x = take n (repeat x)

replicate'' 0 _ = []
replicate'' n x = x: replicate'' (n-1) x

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool   
elem' _ []     = False
elem' x (y:ys)                  -- guarded
    | x == y    = True
    | otherwise = elem' x ys

elem'' :: Eq a => a -> [a] -> Bool   
elem'' _ []     = False
elem'' x (y:ys) = if x == y then True else elem'' x ys -- conditional expression

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x <= y    = x: merge xs (y:ys)
    | otherwise = y: merge (x:xs) ys

msort []  = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where
        half = length xs `div` 2
        left = take half xs
        right = drop half xs
