type Vector = [Double]

type Matrix = [Vector] -- row-major

sp xs ys = sum [  fst xy * snd xy | xy <- zip xs ys]

sp' xs ys = sum [ x * y | (x,y) <- zip xs ys]

sp'' [] [] = 0
sp'' (x:xs) (y:ys) = x*y + sp'' xs ys

row :: Matrix -> Int -> Vector
row = (!!)

col :: Matrix -> Int -> Vector
col dss i = [ ds !! i | ds<-dss]  

matA= [[1,2,3],[4,5,6.0]]
matB =[[7, 8],[9,10],[11,12.0]]

n_rows :: Matrix -> Int
n_rows = length

n_cols :: Matrix -> Int
n_cols rs = length (rs !! 0)

matmul a b = [ [ sp r c | c<-cols] | r<-a ]
    where 
        cols = [ col b i  | i<-[0.. n_cols b -1 ] ]

safe_mm :: Matrix -> Matrix -> Maybe Matrix
safe_mm a b 
    | n_cols a == n_rows b = Just (matmul a b)
    | otherwise            = Nothing

-- mm_or_error :: Matrix -> Matrix -> String
-- mm_or_error
