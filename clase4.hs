factorial :: Integer->Integer
factorial n | n == 0 = 1
        	| n > 0 = n * factorial (n-1)

fib :: Integer->Integer
fib n | n == 0 = 0
  	  | n == 1 = 1
  	  | otherwise = fib(n-1)+fib(n-2)

a :: Integer->Integer
a n | n == 1 = 2
	  | n > 1 = 2*(n-1)*(a (n-1)) + (2^n)* factorial (n-1)

------ k = n+2
a2 :: Integer->Integer
a2 k | k == 1 = 1
 	   | k == 2 = 2
 	   | k > 2 = (k-2)*(a2(k-1)) + 2*(k-1)*(a2 (k-2))

esPar :: Integer->Bool
esPar n | mod n 2 == 0 = True
    	  | otherwise = False

esImpar :: Integer->Bool
esImpar n = not(esPar n)

----- k = n+2
a3 :: Integer->Integer
a3 k | k == 1 = (-3)
 	| k == 2 = 6
 	| not(esPar k) = (-1)*(a3 (k-1)) - 3
 	| esPar k = a3(k-1) + 2*(a3 (k-2)) + 9


sumatoria :: Integer->Integer
sumatoria n | n >= 1 = n + sumatoria(n-1)
        	| n == 0 = 0
        	| otherwise = 0

f1 :: Integer->Integer
f1 n | n == 0 = 1
 	| n > 0 = 2^n + f1(n-1)

f2 :: Integer->Float->Float
f2 n q | n == 1 = q
   	| (n > 1) = q^n + f2 (n-1) q

f3 :: Integer->Float->Float
f3 n q | n == 1 = q + q^2
   	| n > 1 = (f3 (n-1) q) + q^(2*n -1) + q^(2*n)

-- Falta f4

esMultiplode3 :: Integer->Bool
esMultiplode3 n | n == 0 = True
                | n == 1 = False
                | n == 2 = False
                | otherwise = esMultiplode3 (n - 3)

-- Verificar analiticamente
sumaImpares :: Integer->Integer
sumaImpares n | n <= 0 = 0
              | n >= 1 = (2*n - 1) + sumaImpares (n - 1)

medioFact :: Integer->Integer
medioFact n | n == 0 = 1
            | n == 1 = 1
            | n == 2 = 2
            | n > 2 = n * medioFact (n - 2)

-- falta ej24