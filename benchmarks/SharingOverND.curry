-- Benchmark: demonstrating the advantage of pull-tabbing:
-- sharing over non-deterministic computations

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

app []     ys = ys
app (x:xs) ys = x : app xs ys

rev []     = []
rev (x:xs) = app (rev xs) [x]

isList []     = True
isList (x:xs) = isList xs

natList O     = []
natList (S x) = (S x) : (natList x)

cond True x = x

nfList xs = cond (isList xs) xs

goal1 = nfList (rev (natList nat16))
goal2 = nfList (rev (natList nat256))
goal3 = isList (rev (natList nat4096))
goal4 = isList (rev (natList nat16384))

costly = goal3

-- with sharing over nd, both times should be the same
-- currently, only --pulltab has this property
main1 = let x = costly in cond True costly
main2 = let x = costly in cond (True?True) costly
{-
with curry-rts: 4.02 / 4.11
with pulltab  : 3.48 / 3.56
with backtrack: 7.98 / 15.65
-}
