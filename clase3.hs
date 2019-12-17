inv :: Float->Float
inv x | x/=0 = 1/x
--Casos:

-- (inv 1 == 0) && (inv 0 == 1)
-- como la primera condicion da falso, no evalua la segunda. falso y cualquier cosa es falso. falso por lazy.

-- (inv 1 == 1) && (inv 0 == 1)
-- la funcion inversa no esta definida para el 0. la primera expresion es true, pero como para la segunda exp no esta definida
-- para el 0, tira non exhaustive patterns

-- (inv 0 == 1) && (inv 1 == 1)
-- como la funcion inverso no esta definida para el 0 directamente tira non exhaustive patterns. evalua solo la primera expresion,
-- la segunda es true, pero no se fija qué da.

--Non exhaustive patterns significa que faltan condiciones para cubrir con las guardas

--resto2 :: Integer->Integer->Integer->Bool
--resto2 x = mod x 2

--todosImpares2 :: Integer->Integer->Integer->Bool
-- todosImpares2 x y z = not(par(x*y*z))

--todosImpares :: Integer->Integer->Integer->Bool
-- todosImpares x y z = (resto2 x) + (resto2 y) + (resto2 z) == 3

par :: Integer -> Bool
par x | mod x 2 == 0 = True
  	  | otherwise = False

impar :: Integer -> Bool
impar x | mod x 2 /= 0 = True
  	    | otherwise = False

unidades :: Integer -> Integer
unidades x | x > 10 = mod x 10
       	   | otherwise = x

sumaUnidades3 :: Integer->Integer->Integer->Integer
sumaUnidades3 x y z = unidades x + unidades y + unidades z

todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares x y z | (impar x) && (impar y) && (impar z) = True
               	| otherwise = False

alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar x y z | (impar x) || (impar y) || (impar z) = True
                 	| otherwise = False

alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares x y z | ((impar x) && (impar y)) || ((impar y) && (impar z)) || ((impar x) && (impar z)) = True
                    	| otherwise = False

alMenosDosPares :: Integer->Integer->Integer->Bool
alMenosDosPares x y z | ((par x) && (par y)) || ((par y) && (par z)) || ((par x) && (par z)) = True
                  	| otherwise = False

relacion1 :: Integer->Integer->Bool
relacion1 a b | ((par a) && (par b)) || ((impar a) && (impar b)) = True
   	          | otherwise = False

relacion2 :: Integer->Integer->Bool
relacion2 a b | (mod (2*a + 3*b) 5) == 0 = True
    	         | otherwise = False

relacion3 :: Integer->Integer->Bool
relacion3 a b | ((unidades a) /= (unidades b)) && ((unidades a) /= (unidades (a*b))) && ((unidades b) /= (unidades (a*b))) = True
   	          | otherwise = False

relacion4 :: Integer->Integer->Bool
relacion4 a b | ((a < 3) && (b < 3)) || ((3 <= a) && (3 <= b)) = True
   	          | otherwise = False

relacion5 :: Integer->Integer->Bool
relacion5 a b | ((a < 3) && (b < 3)) || ((3 <= a && a < 7) && (3 <= b && b < 7)) || ((7 <= a) && (7 <= b)) = True
              | otherwise = False

relacion6 :: (Integer, Integer)->(Integer, Integer)->Bool
relacion6 (a, b) (p, q) | ((mod a p == 0 && mod b q == 0) && (div a p == div b q) && p /= 0 && q /= 0) = True
             	        | otherwise = False

relacion7 :: (Float, Float)->(Float, Float)->Bool
relacion7 (a, b) (p, q) | (((a/p) == (b/q)) && p /= 0 && q /= 0) = True
             	        | otherwise = False