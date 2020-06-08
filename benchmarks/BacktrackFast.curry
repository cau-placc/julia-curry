-- Benchmark demonstrating the advantage of backtracking:
-- fast if one ND branch immediately fails

data Nat = O | S Nat

add :: Nat -> Nat -> Nat
add O     n = n
add (S m) n = S (add m n)

double :: Nat -> Nat
double x = add x x

mult :: Nat -> Nat -> Nat
mult O     _ = O
mult (S m) n = add n (mult m n)

two = S (S O)
four = double two
nat16 = mult four four
nat256 = mult nat16 nat16
nat4096 = mult nat256 nat16
nat16384 = mult nat4096 four

head (x:xs) = x

-- With pull-tabbing: split both computations until the root
-- With backtracking: immediately fail one computations
-- when (head []) is reached
main = add nat16384 (head [] ? O)
{-
with curry-rts: 1.86
with pulltab  : 1.46
with backtrack: 1.36
-}
