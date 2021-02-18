--1 Calculo de costos

--COSTO: constante
head' :: [a] -> a
head' (x:xs) = x

--COSTO: Constante
sumar :: Int -> Int 
sumar x = x +1+1+1+1+1+1+1

--COSTO: Lineal 
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)


--COSTO: Lineal
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False 
pertenece n (x:xs) = n == x || pertenece n xs 

--COSTO: Cuadratica
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs 
    then sinRepetidos xs 
    else x : sinRepetidos xs 

--COSTO: Lineal
--equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys 
append (x:xs) ys = x : append xs ys

--COSTO: Lineal 
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) =x ++ concatenar xs 

--COSTO: Constante
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs 

--COSTO: Constante
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

--COSTO: Constante
partir:: Int -> [a]-> ([a],[a])
partir n xs =(takeN n xs, dropN n xs )

--COSTO: Lineal
minimo :: Ord a => [a] -> a 
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

--COSTO: Lineal
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x then xs else x : sacar n xs 

--COSTO: Lineal
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs = let m = minimo xs in m : ordenar (sacar m xs) 
-----------------------------------------------------------------------------
