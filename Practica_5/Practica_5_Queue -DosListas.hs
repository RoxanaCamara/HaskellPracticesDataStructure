module Practica_5_Queue_DosListas ( 
Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

--INVARIANTE DE REPRESENTACION:
--Si fs se encuentra vacia la cola esta vacia
--Todas las opeciones son constantes

--fs front stack    --bs Back stack
--El primero que entra es el primero en salir
data Queue a = Q [a] [a] deriving Show

--Constante 
emptyQ :: Queue a
emptyQ = (Q [] [])

--Constante
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q fs bs) = null fs && null bs

--Amortizado 
enqueue :: a -> Queue a -> Queue a 
enqueue x (Q fs bs) = (Q fs (x:bs))

--Amortizado 
firstQ :: Queue a -> a 
firstQ (Q fs bs) = if null fs then last bs else head fs

--Amortizado 
dequeue :: Queue a -> Queue a
dequeue (Q fs bs) = if null fs then (Q (tail (reverse bs)) [] ) else (Q (tail fs) bs) 

--dequeue $ dequeue (enqueue 56 $ enqueue 46 $enqueue 36 $enqueue 26 $enqueue 16 $emptyQ)