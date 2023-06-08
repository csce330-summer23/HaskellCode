nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))


data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n) 

multLazy :: Nat -> Nat -> Nat
multLazy m n = int2nat ( nat2int m *  nat2int n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

evenNat Zero = True
evenNat (Succ Zero) = False
evenNat (Succ (Succ x)) = evenNat x

oddNat n = not (evenNat n)

three = int2nat 3
two = int2nat 2

evenNat' Zero = True
evenNat' (Succ Zero) = False
evenNat' (Succ n) = oddNat' n

oddNat' Zero = False
oddNat' (Succ Zero) = True
oddNat' (Succ n) = evenNat' n

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr deriving Show

expr = Add (Val 1) (Mul (Val 2) (Val 3))

eval = folde id (+) (*)

folde valOp addOp mulOp (Val n ) = valOp n
folde valOp addOp mulOp (Add l r) = addOp resultL resultR
    where
        resultL = folde valOp addOp mulOp l
        resultR = folde valOp addOp mulOp r
folde valOp addOp mulOp (Mul l r) = mulOp (folde valOp addOp mulOp l) (folde valOp addOp mulOp r)





data Tree a = Nil 
            | Leaf a
            | Node (Tree a) a (Tree a) deriving Show


size :: Tree a -> Int
size Nil      = 0
size (Leaf _) = 1
size (Node l _ r) = size l + 1 + size r
 
complete :: Tree a -> Bool
complete Nil = True
complete (Leaf _) = True
complete (Node l _ r) = size l == size r && complete l && complete r

slide21 = Node (Node (Leaf 1)3(Leaf 4) ) 5 (Node (Leaf 6)7(Leaf 9))

slide21incomplete = Node (Node (Leaf 1)3 Nil ) 5 (Node (Leaf 6)7(Leaf 9))