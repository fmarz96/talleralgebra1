esPar ::  Integer->Bool
esPar x | (mod x 2) == 0 = True
    	| otherwise = False

esMultiploDe :: Integer->Integer->Bool
esMultiploDe x y | (mod x y) == 0 = True
             	 | otherwise = False

crearPar :: a->b->(a,b)
crearPar x y = (x,y)

invertir :: (a,b)->(b,a)
invertir par = (snd par, fst par)

distanciaPuntos :: (Float, Float)->(Float,Float)->Float
distanciaPuntos primero segundo = sqrt( (fst segundo - fst primero )^2 + (snd segundo - snd primero)^2 )

f1 :: Integer->(Integer, Integer, Integer)
f1 x = (2*x, x^2, x-7)

f2 :: Integer->Integer
f2 x | (mod x 2) == 0 = div x 2
     | otherwise = x + 1

f :: Integer->Integer
f n | (mod n 6) == 0 = div (n*n) 2
    | otherwise = 3*n + 1

g :: (Integer, Integer)->Integer
g x = f((fst x)*(snd x + 1))

h :: (Integer, Integer)->Integer
h x = g x