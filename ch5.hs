
pyths n = [ (x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2 + y^2 == z^2 ]


pyths' n = [ (x,y,z) | x<-[1..n], y<-[1..n], z<-[ (max x y)+1..n], x^2 + y^2 == z^2 ]

pyths'' n = [ (x,y,z) | x<-[1..n], y<-[x..n], z<-[ y+1..n], x^2 + y^2 == z^2 ]

pyths''' n = concat [ (\(x,y,z)->[(x,y,z),(y,x,z)] ) t | t<-ps  ]
    where
        ps = pyths'' n

factors :: Int -> [Int]
factors n =
   [x | x <- [1..n], n `mod` x == 0]

allButLast xs = [ x | (x,i)<-(zip xs [1..]), i <len]
    where 
        len = length xs

perfect :: Int -> Bool
perfect n = sum( init (factors n) )==n 

perfects :: Int -> [Int]
perfects n = [ n | n<-[1..n], perfect n]

sp :: [Int] -> [Int] -> Int
sp xs ys = sum [  xs !! i * ys !! i  | i<-[0.. length xs -1]]

sp' :: [Int] -> [Int] -> Int
sp' xs ys = sum [ (\ab-> fst ab * snd ab) xy | xy <- zip xs ys ]

sp'' :: [Int] -> [Int] -> Int
sp'' xs ys = sum [ x*y | (x,y) <- zip xs ys ]

sp''' :: [Int] -> [Int] -> Int
sp''' [x] [y]       = x*y
sp''' (x:xs) (y:ys) = x*y+ sp''' xs ys

adjEquals :: Eq a => [a] -> Bool
adjEquals [] = False
adjEquals [_] = False
adjEquals (x0:x1:xs) = if x0 == x1 then True else adjEquals (x1:xs)