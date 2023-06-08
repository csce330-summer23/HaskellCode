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