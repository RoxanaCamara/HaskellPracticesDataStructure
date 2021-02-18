--PRACTICA 5 

--Stack --platos
--ultimo en venir , primero en salir 

module Practica_5_Stack ( 
Stack, emptyS, isEmptyS, push, top, pop, lenS) where

data Stack a = S [a] deriving Show

--constante
emptyS :: Stack a
emptyS = (S [])

--Constante
isEmptyS :: Stack a -> Bool 
isEmptyS (S xs) =  null xs 

--Constante
push :: a -> Stack a -> Stack a
push x (S xs) = (S (x:xs) )

--Constante
top :: Stack a -> a 
top  (S (x:xs)) = x 

--Lineal
pop :: Stack a -> Stack a 
pop (S (x:xs)) = (S xs)

--Lineal
lenS :: Stack a -> Int 
lenS (S xs) = length xs

