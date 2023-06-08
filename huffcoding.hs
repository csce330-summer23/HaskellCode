import Data.List

data HTree = 
    CNode Double Char |
    HNode Double HTree HTree deriving Show

freq :: HTree -> Double
freq (CNode f _) = f
freq (HNode f _ _ ) = f

merge :: HTree -> HTree -> HTree
merge l r = HNode (freq l + freq r) l r

slide21 = [ ('A',0.35), ('B',0.1),('C',0.2),('D',0.2),('_',0.15)]

cfs2trees :: [(Char,Double)] -> [HTree]
cfs2trees cfs = [ CNode f c  | (c,f)<-cfs] 

mergeAllTrees' :: [HTree] -> HTree
mergeAllTrees' [t] = t
mergeAllTrees' ts = mergeAllTrees' (t:rest)
    where
        (f:s:rest) = sortOn freq ts
        t = merge f s

-- stop cheating mergeTrees tomorrow

min' :: [HTree] -> HTree
min' [t] = t
min' (t:ts) = if freq t < freq minTs then t else minTs
    where
        minTs = min' ts 

mini :: [HTree] -> Int
mini ts = head [ i | (t,i)<-zip ts [0..], freq t == fm]
    where
        fm = freq (min' ts)

removei :: [a] -> Int -> [a]
removei xs i = l ++ tail r 
    where
        (l,r)= splitAt i xs

removei' (x:xs) n 
    | n == 0     = xs
    | otherwise  = x: removei' xs (n-1)

removei'' xs n = [ x | (x,i)<-zip xs [0..] , n /= i]

mergeAllTrees :: [HTree] -> HTree
mergeAllTrees [t] = t
mergeAllTrees ts = mergeAllTrees (t:ts3)
    where
        mini1 = mini ts
        t1 = ts !! mini1
        ts2 = removei ts mini1
        mini2 = mini ts2
        t2 = ts2 !! mini2
        ts3 = removei ts2 mini2
        t = merge t1 t2
        
decodeTree :: String -> HTree -> [(Char,String)]
decodeTree code (CNode _ c) = [(c,code)] 
decodeTree code (HNode _ l r) = lcodes ++ rcodes
    where 
        lcodes = decodeTree (code++['0']) l
        rcodes = decodeTree (code++['1']) r

getCodes :: [(Char,Double)] -> [(Char,String)]
getCodes cfs =decodeTree "" ( mergeAllTrees (cfs2trees cfs) )

getCodes' :: [(Char,Double)] -> [(Char,String)]
getCodes' = (decodeTree ""). mergeAllTrees.cfs2trees 

type HuffTable = [(Char,String)]

encodeText' :: HuffTable -> String -> String
encodeText' _ [] = []
encodeText' ht (c:cs) = encodeChar ht c ++ encodeText' ht cs

encodeText ht cs = foldr (\ch codes-> encodeChar ht ch ++ codes) [] cs

encodeText'' ht cs = concat $ map (encodeChar ht) cs

huffTable21 = getCodes slide21

encodeChar :: HuffTable -> Char -> String
encodeChar ht c = snd( head $ filter (\(ch,s)->ch==c) (getCodes' slide21) )

inTable :: HuffTable -> String -> (Bool,Char)
inTable ht s 
    | null code_tuple = (False,'X')
    | otherwise = (True,fst (head code_tuple )) 
    where
        code_tuple = filter (\(c,code)->s==code ) ht
