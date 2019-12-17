-- 1
comparar :: Integer->Integer->Integer
comparar a b | a < 10 && b < 10 && a == b || ((mod (div a 10) 10) + (mod a 10)) == ((mod (div b 10) 10) + (mod b 10)) = 0
             | ((mod (div a 10) 10) + (mod a 10)) < ((mod (div b 10) 10) + (mod b 10)) = 1
             | ((mod (div a 10) 10) + (mod a 10)) > ((mod (div b 10) 10) + (mod b 10)) = (-1)

-- 2
maximoExponente2 :: Integer->Integer
maximoExponente2 n | mod n 2 /= 0 = 0
                   | otherwise = 1 + maximoExponente2 (div n 2)

-- 3
sumaDivisoresHasta :: Integer->Integer->Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | k > 1 && (mod n k) == 0 = k + sumaDivisoresHasta n (k-1)
                       | k > 1 && (mod n k) /= 0 = sumaDivisoresHasta n (k-1)

sumaDivisores :: Integer->Integer
sumaDivisores n = sumaDivisoresHasta n (n-1)

sonAmigos :: Integer->Integer->Bool
sonAmigos n k = sumaDivisores n == k && sumaDivisores k == n

-- 4
-- regular/mal. cuando lo hice en el papel, me faltó el caso base para la recursion
cambiosDeParidad :: [Integer]->Integer
cambiosDeParidad [x] = 0
cambiosDeParidad (x:xs) | (mod x 2) /= (mod (head xs) 2) = 1 + cambiosDeParidad xs
                        | otherwise = cambiosDeParidad xs