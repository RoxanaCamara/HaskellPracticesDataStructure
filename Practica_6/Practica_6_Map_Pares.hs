--module Practica_6_Map_Pares ( Map, emptyM, assocM, lookupM,deleteM, keys) where

data Map k v = MKV [(k,v)] deriving Show 
--Sin repetidos

--Constante
emptyM :: Map k v
emptyM = (MKV [] )

--Cuadratica
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (MKV xs) = (MKV (actualizarValor k v xs) ) 


actualizarValor :: Eq k => k -> v -> [(k,v)]-> [(k,v)] 
actualizarValor k v []     = [(k,v)]
actualizarValor k v (x:xs) = if k == (fst x) then (k, v) : xs else x : actualizarValor k v xs 

--lookupM ::Eq k => k -> Map k v -> MaybeN v
--lookupM k (MKV ls) =  

--deleteM :: Eq k => k -> Map k v -> Map k v
--deleteM k (MKV xs) =  

--keys :: Map k v -> [k]
--keys (MKV xs) = 


----------------------------------------------------------------

--BIBLIOTECA
--Lineal
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False 
pertenece n (x:xs) = n == x || pertenece n xs 

data MaybeN a = NothingN | JustN a deriving Show

