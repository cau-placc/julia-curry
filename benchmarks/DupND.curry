-- Examples for duplicating non-deterministic computations:

-- arbitrary number between 1 and n:
someNum :: Int -> Int
someNum n = if (n<=0) then 0 else (n ? someNum (n-1))

isZero :: Int -> Bool
isZero 0 = True

-- lazy non-deterministic duplication:
addSomeNum1 n = let x = someNum n in x
addSomeNum2 n = let x = someNum n in x+x
addSomeNum3 n = let x = someNum n in x+x+x
addSomeNum4 n = let x = someNum n in x+x+x+x
addSomeNum5 n = let x = someNum n in x+x+x+x+x

main1 = addSomeNum1 2000 -- pull-tab:  0.75s / taskns: 1.14
main2 = addSomeNum2 2000 -- pull-tab:  2.91s / taskns: 1.36
main3 = addSomeNum3 2000 -- pull-tab:  6.14s / taskns: 1.54
main4 = addSomeNum4 2000 -- pull-tab:  9.87s / taskns: 1.77
main5 = addSomeNum5 2000 -- pull-tab: 14.88s / taskns: 2.00

main = main5

addNum1 = isZero (addSomeNum1 2000)
addNum2 = isZero (addSomeNum2 2000)
addNum3 = isZero (addSomeNum3 2000)
addNum4 = isZero (addSomeNum4 2000)
addNum5 = isZero (addSomeNum5 2000)

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
