-- Permutation sort with Peano numbers and user-defined lists

data MyBool = MyTrue | MyFalse

ifThenElse :: MyBool -> a -> a -> a
ifThenElse MyTrue  x _ = x
ifThenElse MyFalse _ y = y

guard :: MyBool -> a -> a
guard MyTrue x = x


data Nat = O | S Nat

dec (S x) = x

leq O     _     = MyTrue
leq (S _) O     = MyFalse
leq (S x) (S y) = leq x y

isNat O = MyTrue
isNat (S x) = isNat x

add O n = n
add (S x) y = S (add x y)

double x = add x x

mult O _ = O
mult (S x) y = add y (mult x y)

two = S (S O)
three = S two
four = double two
nat13 = S (mult three four)
nat14 = add two (mult three four)
nat15 = S nat14

----------------------------------------------------------------
data List a = Nil | Cons a (List a)

app :: List a -> List a  -> List a
app Nil         ys = ys
app (Cons x xs) ys = Cons x (app xs ys)

len :: List a -> Nat
len Nil         = O
len (Cons _ xs) = S (len xs)

insert :: a -> List a  -> List a
insert x Nil         = Cons x Nil
insert x (Cons y ys) = (Cons x (Cons y ys)) ? (Cons y (insert x ys))

perm :: List a  -> List a
perm Nil         = Nil
perm (Cons x xs) = insert x (perm xs)

sorted :: List Nat  -> List Nat
sorted Nil       = Nil
sorted (Cons x Nil) = Cons x Nil
sorted (Cons x (Cons y ys)) = guard (leq x y) (Cons x (sorted (Cons y ys)))

psort :: List Nat  -> List Nat
psort xs = sorted (perm xs)

descList up low =
  ifThenElse (leq low up) (Cons up (descList (dec up) low)) Nil

-- psort (2:[n,n-1 .. 3]++[1])
sortDescList n = psort (Cons two (app (descList n three) (Cons (S O) Nil)))

permsort = sortDescList nat13

psort14 = sortDescList nat14

psort14_conc = let x = psort14 in app (app x x) x

psort14_length = let x = len psort14 in add (add x x) x

main = psort14
