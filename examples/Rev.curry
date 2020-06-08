app []     ys = ys
app (x:xs) ys = x : app xs ys

rev [] = []
rev (x:xs) = app (rev xs) [x]

main :: [Int]
main = rev [0,1,2,3,4,5,6,7,8,9]

dlist :: [Int]
dlist = [1?2, 3?4]
