-- Examples for duplicating non-deterministic computations:

-- arbitrary number between 1 and n:
ndnum :: Int -> Int
ndnum n = if (n<=1) then 1 else (n ? ndnum (n-1))

-- lazy non-deterministic duplication:
addNum1 n = let x = ndnum n in x
addNum2 n = let x = ndnum n in x+x
addNum3 n = let x = ndnum n in x+x+x
addNum4 n = let x = ndnum n in x+x+x+x
addNum5 n = let x = ndnum n in x+x+x+x+x

main1 = addNum1 2000 -- pull-tab:  0.75s / taskns: 1.14
main2 = addNum2 2000 -- pull-tab:  2.91s / taskns: 1.36
main3 = addNum3 2000 -- pull-tab:  6.14s / taskns: 1.54
main4 = addNum4 2000 -- pull-tab:  9.87s / taskns: 1.77
main5 = addNum5 2000 -- pull-tab: 14.88s / taskns: 2.00

main = main5

-- arbitrary tuples between 1 and n:
ndpair n   = if (n==1) then (1,1)   else ((n,n)   ? ndpair (n-1))
ndtriple n = if (n==1) then (1,1,1) else ((n,n,n) ? ndtriple (n-1))

-- lazy non-deterministic duplication:
addPair :: Int -> Int
addPair   n = x+y   where (x,y) = ndpair n

addTriple :: Int -> Int
addTriple n = x+y+z where (x,y,z) = ndtriple n

main10 = addPair   2000
main11 = addTriple 2000
