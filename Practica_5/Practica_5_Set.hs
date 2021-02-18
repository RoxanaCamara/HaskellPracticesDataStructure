--PRACTICA 5 

--SET CONJUNTO

module Practica_5 ( 
Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

data Set a = S [a] deriving Show

--constante
emptyS :: Set a 
emptyS = (S [])


-----------Dos versiones
--constante
addS :: Eq a => a -> Set a -> Set a 
addS a (S xs) = (S (a:xs))

--Lineal
addS2 :: Eq a => a -> Set a -> Set a 
addS2 a (S xs) =
    if pertenece a xs then (S xs) else (S (a:xs))


--lineal
belongs :: Eq a => a -> Set a -> Bool 
belongs  a (S xs) = pertenece a xs 

-----------Dos versiones
--cuadratica
sizeS :: Eq a => Set a -> Int 
sizeS (S a) = length (sinRepetidos a)

---Lineal
sizeS2 :: Eq a => Set a -> Int 
sizeS2 (S a) = length a


--Lineal
removeS :: Eq a => a -> Set a -> Set a 
removeS  a (S xs) = (S (remover a xs))  

remover :: Eq a => a -> [a] -> [a]
remover a [] = []
remover a (x:xs) = if a == x 
    then remover a xs
    else x : remover a xs 

--Constante 
unionS :: Eq a => Set a -> Set a -> Set a 
unionS (S a) (S b) = (S (a ++ b))

--Cuadratica
setToList :: Eq a => Set a -> [a] 
setToList (S a) = sinRepetidos a
----------------------------------------------------------------

--BIBLIOTECA
--Lineal
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False 
pertenece n (x:xs) = n == x || pertenece n xs 


--Cuadratica
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs 
    then sinRepetidos xs 
    else x : sinRepetidos xs 
