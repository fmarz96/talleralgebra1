--1
menorLex :: (Float, Float, Float)->(Float, Float, Float)->Bool
menorLex (x0, y0, z0) (x1, y1, z1) | x0 < x1 = True
                                   | x0 == x1 && y0 < y1 = True
                                   | x0 == x1 && y0 == y1 && z0 < z1 = True
                                   | otherwise = False

--2
fibonacci :: Integer->Integer
fibonacci n | n == 0 = 1
            | n == 1 = 1
            | n > 1 = fibonacci (n-1) + fibonacci (n-2)

sumaFibonacci :: Integer->Integer
sumaFibonacci i | i == 0 = 1
                | i > 0 = fibonacci i + sumaFibonacci (i-1)
--3
sumaDivisoresHasta :: Integer->Integer->Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | k > 1 && mod n k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)

esDefectivo :: Integer -> Bool
esDefectivo n | (sumaDivisoresHasta n (n - 1)) < n = True
              | otherwise = False
--4
mD :: [Integer]->Integer
mD [a, b] = abs(a-b)
mD (x:xs) = max(abs(x - head xs)) (mD xs)

--5
comprimir :: [Integer]->[(Integer, Integer)]
comprimir [] = []
comprimir list = compAux (format list)

format :: [Integer]->[(Integer, Integer)]
format (x:xs) | xs == [] = [(x,1)]
              | xs /= [] = (x,1) : format xs

compAux :: [(Integer, Integer)]->[(Integer, Integer)]
compAux ((a,b):xs) | a == fst(head xs) = compAux ((a, b+snd (head xs)) : tail xs)
                   | a /= fst (head xs) = (a,b) : compAux xs
                   | xs == [] = [(a,b)]

--comprimir::[Integer]->(Integer,Integer)
--comprimir l = juntar (reemplazar l)

--reemplazar::[Integer]->[(Integer)]
--reemplazar [] = []
--reemplazar (x:xs) = (x,1):reemplazar xs

--juntar::[(Integer,Integer)]->[(Integer,Integer)]
--juntar [] = []
--juntar [(a,b)] = [(a,b)]
--juntar ((a,b):(c,d):lps) | a==c = juntar ((a,b+d):lps)
--                         | otherwise = (a,b):juntar ((c,d):lps)