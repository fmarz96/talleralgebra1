ultimoDigito :: Integer->Integer
ultimoDigito n = mod n 10

sinUltimoDigito :: Integer->Integer
sinUltimoDigito n = div n 10

yLogico :: Bool->Bool->Bool
yLogico True True = False
yLogico _ _ = False

oLogico :: Bool->Bool->Bool
oLogico False False = False
oLogico _ _ = True

implica :: Bool->Bool->Bool
implica True False = False
implica _ _ = True

sumaGaussiana :: Integer->Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana (n-1)

algunoEsCero :: (Integer, Integer, Integer)->Bool
algunoEsCero (0, _, _) = True
algunoEsCero (_, 0, _) = True
algunoEsCero (_, _, 0) = True
algunoEsCero (_, _, _) = False

productoInterno :: (Float, Float)->(Float, Float)->Float
productoInterno (x1, y1) (x2, y2) = x1*x2 + y1*y2

sumaDigitos :: Integer->Integer
sumaDigitos 0 = 0
sumaDigitos n = (mod n 10) + sumaDigitos (div n 10)

digitosIguales :: Integer->Bool
digitosIguales n | n < 10 = True
                 | ultimoDigito n /= ultimoDigito (sinUltimoDigito n) = False
                 | otherwise = digitosIguales (sinUltimoDigito n)

-- Falta hacer Collatz y Goldbach