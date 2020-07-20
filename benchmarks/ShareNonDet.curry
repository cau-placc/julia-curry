-- Benchmark to measure sharing across non-determinism

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = myfilter (isdivs n) ns

primes :: [Int]
primes = mymap myhead (myiterate the_filter (myiterate suCC 2))

myfilter :: (Int -> Bool) -> [Int] -> [Int]
myfilter _ []     = []
myfilter p (x:xs) = if p x then x : myfilter p xs
                           else myfilter p xs

myiterate :: (a -> a) -> a -> [a]
myiterate f x = x : myiterate f (f x)

mymap :: (a -> b) -> [a] -> [b]
mymap _ []     = []
mymap f (x:xs) = f x : mymap f xs


myhead :: [Int] -> Int
myhead (x : _) = x

at :: [Int] -> Int -> Int
at (x:xs) n = if n==0  then x 
                       else at xs (n - 1)

ndinsert :: a -> [a] -> [a]
ndinsert x xs     = (x : xs) ? ndinsert2 x xs

ndinsert2 :: a -> [a] -> [a]
ndinsert2 x (y:ys) = y : ndinsert x ys

perm :: [a] -> [a]
perm []     = []
perm (x:xs) = ndinsert x (perm xs)

sorted :: [Int] -> [Int]
sorted []       = []
sorted [x]      = [x]
sorted (x:y:ys) = guard (x <= y) (x:sorted (y:ys))

psort :: [Int] -> [Int]
psort xs = sorted (perm xs)

guard :: Bool -> a -> a
guard True x = x

myand :: Bool -> Bool -> Bool
myand True y  = y
myand False _ = False

primeList :: [Int]
primeList = [primes!!303, primes!!302, primes!!301, primes!!300]

goal1 :: [Int]
goal1 = primeList                      -- curryrts: 9.84 / backtrack:  15.05

goal2 :: [Int]
goal2 = psort [2003, 1999, 1997, 1993] -- curryrts: 0.00 / backtrack:   0.00

goal3 :: [Int]
goal3 = psort primeList                -- curryrts: 9.92 / backtrack: 157.12
