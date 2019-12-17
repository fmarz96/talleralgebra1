factorial :: Integer->Integer
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)

eAprox :: Integer->Float
eAprox n | n == 0 = 1
         | otherwise = (fromInteger(1)/(fromInteger(factorial (n)))) + eAprox (n-1)

e :: Float
e = eAprox 100

-- parteEntera : calcula parte entera de un numero
parteEntera :: Float->Integer
parteEntera n | n > -1 && n < 1 = 0
              | n <= -1 = 1 + parteEntera (n+1)
              | n >= 1 = 1 + parteEntera (n-1)

parteEnteraNeg :: Float->Integer 
parteEnteraNeg n | n < 0 = (-1)*(parteEntera n)
                 | n > 0 = parteEntera n

-- fst es el cociente(q), snd el resto. a = d*q + r devuelve (q,r)
division :: Integer->Integer->(Integer, Integer)
division a d | a < 0 = (fst (division (a+d) d) - 1, snd (division (a+d) d))
             | a < d && a > (-1)*d = (0, a)
             | a >= d = (1+fst qr', snd qr')
             where qr' = (division (a-d) d)

sumaDivisoresHasta :: Integer->Integer->Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | k > 1 && (mod n k) == 0 = k + sumaDivisoresHasta n (k-1)
                       | k > 1 && (mod n k) /= 0 = sumaDivisoresHasta n (k-1)

sumaDivisores :: Integer->Integer
sumaDivisores n = sumaDivisoresHasta n n

menorDivisorDesde :: Integer->Integer->Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

menorDivisor :: Integer->Integer
menorDivisor n | n == 1 = 1
               | otherwise = menorDivisorDesde n 2

esPrimo :: Integer->Bool
esPrimo n | sumaDivisores n == (n + 1) = True
          | otherwise = False

-- falta sumatoriasDobles, sumaPotencias, sumaRacionales

sumaInterior :: Integer -> Integer -> Integer
sumaInterior m 0 = 0
sumaInterior m n = (m*n) + sumaInterior m (n -1)

sumaDoble :: Integer -> Integer -> Integer
sumaDoble 0 n = 0
sumaDoble m n = sumaInterior m n + sumaDoble (m -1) n


-- ejercicio del parcial. 
zipPrimos :: [Integer]->[Integer]->[(Integer, Integer)]
zipPrimos [] [] = []
zipPrimos (x:xs) (y:ys) | (esPrimo x) && (esPrimo y) = (x, y):zipPrimos xs ys
                        | otherwise = zipPrimos xs ys

suma :: Integer -> Integer
suma 1 = 3
suma n = ((2*n)^2 + 2*(2*n)) + ((2*n-1)^2 + 2*(2*n -1)) + suma (n-1)