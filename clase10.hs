module Clase10
(grado, evaluar, derivada, derivadaNesima, suma, limpiar, productoPorEscalar, productoPorMonomio, agregarCeros, producto, sumaComplejos, productoComplejos, potenciaComplejos)
where

type Polinomio = [Float]
type Complejo = (Float, Float)

grado :: Polinomio -> Integer
grado [] = undefined
grado [x] = 0
grado pol = 1 + grado (tail pol)

-- last: ultimo elemento de la lista
-- init: lista sin el last

evaluar :: Polinomio -> Float -> Float
evaluar [] n = 0
evaluar [x] n = x*n
evaluar (x:xs) n = x*(n^grado (x:xs)) + evaluar xs n

derivada :: Polinomio -> Polinomio
derivada [x] = []
derivada (x:xs) = x*(fromInteger(grado (x:xs))) : derivada xs

derivadaNesima :: Integer -> Polinomio -> Polinomio
derivadaNesima 0 p = p
derivadaNesima n p = derivadaNesima (n-1) (derivada p)

suma :: Polinomio -> Polinomio -> Polinomio
suma [] pol = pol
suma pol [] = pol
suma a b = limpiar ((suma (init a) (init b)) ++ [last a + last b])

limpiar :: [Float] -> Polinomio
limpiar (0:xs) = limpiar xs
limpiar pol = pol

productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar n [x] = [n*x]
productoPorEscalar n (x:xs) = (n*x) : productoPorEscalar n xs

productoPorMonomio :: (Float, Integer) -> Polinomio -> Polinomio
productoPorMonomio (a, n) [] = []
productoPorMonomio (a,n) pol = limpiar (agregarCeros (productoPorEscalar a pol) n)

agregarCeros :: Polinomio -> Integer -> Polinomio
agregarCeros pol n | n == 0 = pol
               	| otherwise = agregarCeros (pol ++ [0]) (n-1)

producto :: Polinomio -> Polinomio -> Polinomio
producto [] p = []
producto p [] = []
producto [a] p = productoPorEscalar a p
producto (x:xs) (y:ys) = suma (productoPorMonomio (x, grado (x:xs)) (y:ys)) (producto xs (y:ys))

sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos (a, b) (c, d) = (a+c, b+d)

productoComplejos :: Complejo -> Complejo -> Complejo
productoComplejos (a, b) (c, d) = (a*c - b*d, a*d + b*c)

potenciaComplejos :: Complejo -> Integer -> Complejo
potenciaComplejos z 1 = z
potenciaComplejos z n = productoComplejos (potenciaComplejos z (n-1)) z

--evaluarComplejo :: Polinomio -> Complejo -> Complejo
--evaluarComplejo