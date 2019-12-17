listar :: a -> a -> a -> [a]
listar a b c = [a, b, c]

pertenece :: Eq a => a -> [a] -> Bool
pertenece c l | length l == 0 = False
              | c == head l = True
              | otherwise = pertenece c (tail l)

pertenecePM :: Integer -> [Integer] -> Bool
pertenecePM _ [] = False
pertenecePM n (x:xs) = n == x || pertenecePM n xs

primerMultiploDe45345 :: [Integer]->Integer
primerMultiploDe45345 l | mod (head l) 45345 == 0 = head l
                        | otherwise = primerMultiploDe45345 (tail l)

sumatoria :: [Integer]->Integer
sumatoria [] = 0
sumatoria (x:xs) = sumatoria xs + x

productoria :: [Integer]->Integer
productoria [] = 1
productoria (x:xs) = productoria xs * x

longitud :: [a] -> Integer
longitud [] = 0
longitud(_:xs) = 1 + longitud xs

sumarN :: Integer->[Integer]->[Integer]
sumarN n [] = []
sumarN n (x:xs) = (x + n) : sumarN n xs

sumarElPrimero :: [Integer]->[Integer]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarN x (x:xs)

ultimo :: [Integer]->Integer
ultimo l | length l /= 1 = ultimo (tail l)
         | otherwise = head l

sumarElUltimo :: [Integer]->[Integer]
sumarElUltimo list = sumarN (ultimo list) list

pares :: [Integer]->[Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x:(pares xs)
             | otherwise = pares xs

multiplosDeN :: Integer->[Integer]->[Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x:(multiplosDeN n xs)
                      | otherwise = multiplosDeN n xs

multiplosDe7 :: [ Integer ] -> [ Integer ]
multiplosDe7 [] = []
multiplosDe7 (x: xs ) | mod x 7 == 0 = x :( multiplosDe7 xs )
                      | otherwise = multiplosDe7 xs

quitar :: Integer->[Integer]->[Integer]
quitar _ [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = x : quitar n xs

hayRepetidos :: [Integer]->Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs

eliminarRepetidos :: [Integer]->[Integer]
eliminarRepetidos x | x == [] = []
                    | hayRepetidos x == False = x
                    | otherwise = (head x) : eliminarRepetidos ( (quitar (head x) (tail x)) ++ [(head x)])

maximo :: [Integer]->Integer
maximo [n] = n
maximo (x:xs) | x > maximo xs = x
              | otherwise = maximo xs

minimo :: [Integer]->Integer
minimo [n] = n
minimo (x:xs) | x < minimo xs = x
              | otherwise = minimo xs

--ordenar :: [Integer]->[Integer]
--ordenar [] = []
--ordenar (x:xs) = (minimo (x:xs)):(ordenar (quitar (minimo (x:xs)) xs))

ordenar :: [Integer]->[Integer]
ordenar [] = []
ordenar x = minimo x : ordenar (quitar (minimo x) x)

reverso :: [Integer]->[Integer]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- Ejercicio de practica para rec

cantComunes :: [Integer]->[Integer]->Integer
cantComunes (x:xs) [] = 0
cantComunes [] (y:ys) = 0
cantComunes (x:xs) (y:ys) | x == y || pertenecePM x ys = 1 + cantComunes xs (y:ys)
                          | otherwise = cantComunes xs (y:ys)