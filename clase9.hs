mcd :: Integer->Integer->Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)

--ejemplo del pizarron
euc a b | a < b = euc b a
    	| mod a b == 0 = b
    	| otherwise = euc (mod a b) b

--mcd usando menorDivisor
--mcd2 :: Integer->Integer->Integer
--mcd2 1 n = 1
--mcd2 n 1 = 1
--mcd2 a b | mod b c /= 0 = mcd2 (div a c) b
--     	| otherwise = c* (mcd2 (div a c) (div b c))
--     	where c = menorDivisor a

emcd1 :: Integer->Integer->(Integer, Integer, Integer)
emcd1 n m = (mcd n m, fst (emcd2 n m), snd (emcd2 n m))

emcd2 :: Integer->Integer->(Integer, Integer)
emcd2 n 0 = (1,0)
emcd2 n m = (snd (emcd2 m (mod n m)), fst (emcd2 m (mod n m)) - (snd ((emcd2 m (mod n m))) * (div n m)))

tieneSolucion :: Integer->Integer->Integer->Bool
tieneSolucion a b m = mod b (mcd a m) == 0

solucionParticular :: Integer->Integer->Integer->Integer
solucionParticular a b m | tieneSolucion a b m = fst (emcd2 a m)*(div b (mcd a m))

solucionGeneral :: Integer->Integer->Integer->(Integer, Integer)
solucionGeneral a b m | tieneSolucion a b m = (solucionParticular ag bg mg, mg)
                  	where ag = div a (mcd a m)
                        	bg = div b (mcd a m)
                        	mg = div m (mcd a m)
