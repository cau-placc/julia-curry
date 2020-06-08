-- Benchmark for duplicating non-deterministic computations
-- based on Peano numbers

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

-- arbitrary number between 0 and n:
ndnum O     = O
ndnum (S n) = S n ? ndnum n

-- lazy non-deterministic duplication:
addNum2 n = let x = ndnum n in add x x
addNum3 n = let x = ndnum n in add (add x x) x
addNum4 n = let x = ndnum n in add (add (add x x) x) x
addNum5 n = let x = ndnum n in add (add (add (add x x) x) x) x

isZero O = True

main2 = addNum2 nat256 -- pull-tab:  3.63s / taskns: 3.06
main3 = addNum3 nat256 -- pull-tab:  6.68s / taskns: 4.06
main4 = addNum4 nat256 -- pull-tab: 11.41s / taskns: 5.13
main5 = addNum5 nat256 -- pull-tab: 18.77s / taskns: 6.30

main = main5

{-
-- arbitrary tuples between 1 and n:
ndpair n   = if (n==1) then (1,1)   else ((n,n)   ? ndpair (n-1))
ndtriple n = if (n==1) then (1,1,1) else ((n,n,n) ? ndtriple (n-1))

-- lazy non-deterministic duplication:
addPair   n = x+y   where (x,y) = ndpair n
addTriple n = x+y+z where (x,y,z) = ndtriple n
-}
