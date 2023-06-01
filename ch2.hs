double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average xs = sum xs `div` length xs

n = a `div` length xs
    where
       a = 10
       xs = [1,2,3,4,5]

last' xs = drop (length xs -1 ) xs !! 0

last'' xs = head ( drop (length xs -1 ) xs  )

last''' xs = head (reverse xs)

last'''' = head.reverse

veryFirstItem = head

init' xs = reverse (tail (reverse xs))

init'' xs = reverse (drop 1 (reverse xs))

init''' xs = take (length xs -1) xs

palindrome xs = xs == reverse xs

palindrome' xs = and (map (\(x,y)-> x==y) (zip xs (reverse xs)) )

add x y = x+y


