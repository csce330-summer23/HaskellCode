safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' xs 
    | null xs   = []
    | otherwise = tail xs

safetail'' [] = []
safetail'' xs = tail xs

(||) ::  Bool -> Bool -> Bool
False || b = b
True  || _ = True

{-
False || False = False
_     || _     = True
-}

-- False || False = False
-- False || True = True
-- True || False = True
-- True || True = True

(&&) :: Bool-> Bool -> Bool
(&&) b1 b2 = if b1 then if b2 then True else False else False

(&&&) :: Bool-> Bool -> Bool
(&&&) b1 b2 = if b1 then b2 else False
