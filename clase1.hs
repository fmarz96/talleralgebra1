doble x = 2*x
suma x y = x + y
normaVectorial x1 x2 = sqrt((x1**2) + (x2**2))
funcionConstante8 x = 8
respuestaATodo = 42

signo n | n > 0 = 1
        | n == 0 = 0
        | otherwise = -1

absoluto n | n > 0 = n
           | n == 0 = 0
           | otherwise = (-1)*n

maximo x y | x > y = x
           | otherwise = y

maximo3 x y z | x >= (maximo y z) = x
              | y >= (maximo x z) = y
              | z >= (maximo x y) = z