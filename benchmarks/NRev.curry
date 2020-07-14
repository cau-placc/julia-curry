-- A purely functional benchmark: naive reverse

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
nrev4096  = isList (rev (natList nat4096))
nrev16384 = isList (rev (natList nat16384))

nrev = nrev4096

main = nrev4096

{-
Results (reported time / time shown by time command for complete execution):

C2J:
> jucs -x -t --hnf NRev -m nrev4096
> time julia NRev.jl
3.85 seconds / 4.58 seconds
> jucs -x -t --hnf NRev -m nrev4096 --pulltabonly
> time julia NRev.jl
3.26 seconds / 3.89 seconds
> jucs -x -t --hnf NRev -m nrev4096 --pulltab
> time julia NRev.jl
3.40 seconds / 4.02 seconds
> jucs -x -t --hnf NRev -m nrev4096 --backtrack
> time julia NRev.jl
8.19 seconds / 8.82 seconds

> ~/pakcs/bin/pakcs :l NRev :set +time :save nrev4096 :q
> time ./NRev
2.1 second / 3.8 seconds

> ~/pakcs_swi/bin/pakcs :l NRev :set +time :save nrev4096 :q
> time ./NRev
12.6 second / 20.5 seconds

> ~/kics2/bin/kics2 :l NRev :set +time :save nrev4096 :q
> time ./NRev
-- / 0.32 seconds

PyCS: 126 seconds

-}
