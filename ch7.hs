

filtmap f p xs = [f x | x <- xs, p x] 

filtmap' f p xs = map f (filter p xs)

filtmap'' f p = (map f).(filter p)

-- foldr version of above

map' f xs = [ f x |  x<-xs ]

map'' f xs = foldr (\x rest -> f x : rest) [] xs

map''' f xs = foldr (\x rest -> [f x] ++ rest) [] xs

map'''' f xs = foldl (\prev x -> prev ++ [f x]) [] xs


filter' p xs = [ x | x<-xs, p x]

filter'' p xs = foldr (\x rest-> if p x then x:rest else rest ) [] xs


