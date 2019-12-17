module Clase11
where
	import Clase10

resta :: Polinomio -> Polinomio -> Polinomio
resta p q = suma p (productoPorEscalar (-1) q)

primerCociente :: Polinomio -> Polinomio -> (Float, Integer)
primerCociente p q = (head p / head q, grado p - grado q)

primerResto :: Polinomio -> Polinomio -> Polinomio
primerResto p q = resta p (productoPorMonomio (primerCociente p q ) q)

sumarMonomio :: (Float, Integer) -> Polinomio -> Polinomio
sumarMonomio (x, n) p = suma (hacerPolinomio (x, n)) p

hacerPolinomio :: (Float, Integer) -> Polinomio -> Polinomio
hacerPolinomio (x, n) = x:(ceros n)

division :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
division [] _ = ([], [])
division p q | grado p < grado q = ([], p)
             | otherwise = (sumarMonomio (primerCociente p q) (cp, rp))
             where (cp, rp) = division (primerResto p q ) q

mcdP :: Polinomio -> Polinomio -> Polinomio
mcdP p [] = p
mcdP p q = mcdP q (snd (division p q))

hacerMonico :: Polinomio -> Polinomio
hacerMonico [] = []
hacerMonico p = productoPorEscalar (1 / head p) p

multiplicidad :: Float -> Polinomio -> Integer
multiplicidad x p | evaluar p x /= 0 = 0
                  | otherwise = 1 + multiplicidad x (derivada p)
                  -- usando la division : otherwise = 1 + multiplicidad x (fst (division p [1, (-x)]))

raicesMultiples :: Polinomio -> Bool
raicesMultiples [] = True
raicesMultiples p = grado (mcdP p (derivada p)) > 0