----------------------------------------------------------------

module Practica_5_Queue ( 
Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where


--Queue (Cola)
--El primero que entra es el primero en salir
data Queue a = Q [a] deriving Show

--Constante 
emptyQ :: Queue a
emptyQ = (Q [])

--Constante
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

--Constante --Agrega al principio
enqueue :: a -> Queue a -> Queue a 
enqueue x (Q xs) = (Q (x:xs))

--Lineal --Agrega al final
enqueue2 :: a -> Queue a -> Queue a 
enqueue2 x (Q xs) = (Q (xs ++ [x]))

--Lineal
firstQ :: Queue a -> a 
firstQ (Q xs) = last xs

--Lineal
dequeue :: Queue a -> Queue a
dequeue (Q xs) = (Q (init xs))

--Constante
dequeue2 :: Queue a -> Queue a
dequeue2 (Q (x:xs)) = (Q xs)
