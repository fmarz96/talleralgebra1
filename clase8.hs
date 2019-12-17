type Set a = [a]

--vacio :: Set Integer
--vacio = []

incluido :: Set Integer -> Set Integer -> Bool
incluido [] cjto2 = True
incluido (x:xs) cjto2 = elem x cjto2 && incluido xs cjto2

iguales :: Set Integer -> Set Integer -> Bool
iguales cjto1 cjto2 = incluido cjto1 cjto2 && incluido cjto2 cjto1

agregar :: Integer -> Set Integer -> Set Integer
agregar n xs | elem n xs = xs
             | otherwise = n:xs

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos n [] = []
agregarATodos n (x:xs) = (agregar n x):(agregarATodos n xs)

-- partes 1 = [[], [1]]
partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = partes (n-1) ++ agregarATodos n (partes (n-1))

--productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
--productoCartesiano [] _ = []
--productoCartesiano _ [] = []
--productoCartesiano (x:xs) (y:ys) = (x,y) ++ (productoCartesiano (xs, ys)) ++ (productoCartesiano [x] ys)  ++ (productoCartesiano xs [y])

--variaciones :: Set Integer -> Integer -> Set [Integer]
--variaciones cjto 0 = [[]]
--variaciones cjto n = agregarAListas cjto (variaciones cjto (n-1))

bolitasNumEnCajas::Integer->Integer->Set [(Integer,Integer)]
bolitasNumEnCajas n 0 = []
bolitasNumEnCajas 0 n = []
bolitasNumEnCajas 1 k = productoCartesiano2 1 (listar2 k)
bolitasNumEnCajas n k = agregarAListas2 (productoCartesiano3 n (listar2 k)) (bolitasNumEnCajas (n-1) k)

productoCartesiano2::Integer->[Integer]->Set [(Integer,Integer)]
productoCartesiano2 _ [] = []
productoCartesiano2 n (l:ls) = [(n,l)]:productoCartesiano2 n ls

productoCartesiano3::Integer->[Integer]->Set (Integer,Integer)
productoCartesiano3 n [] = []
productoCartesiano3 n (x:xs) = (n,x) : productoCartesiano3 n xs

agregarAListas2::Set (Integer,Integer)->Set [(Integer,Integer)]->Set [(Integer,Integer)]
agregarAListas2 [] clps = []
agregarAListas2 ((a,b):lps) clps = agregarATodos3 (a,b) clps ++ agregarAListas2 lps clps

agregarATodos3::(Integer,Integer)->Set [(Integer,Integer)]->Set [(Integer,Integer)]
agregarATodos3 p [] = []
agregarATodos3 p (lps:clps) = (p:lps) : (agregarATodos3 p clps)

listar2::Integer->[Integer]
listar2 0 = []
listar2 n = n:listar2(n-1)