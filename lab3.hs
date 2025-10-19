import   Data.List

factori :: Int -> [Int]
factori x = [d | d <- [1 .. x], x `rem` d == 0 ]

prim :: Int -> Bool
prim 1 = False
prim x = length (factori x) ==2

numerePrime :: Int -> [Int]
numerePrime x = [d | d <- [2 .. x], prim d]

unpack ((a, b),c)=(a,b,c)
myzip3 a b c= map unpack(zip(zip a b) c)


f x y=x+y
