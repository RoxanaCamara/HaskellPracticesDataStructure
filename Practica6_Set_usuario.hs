import Practica6

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

-- O( n * n)
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s = 
    if belongs x s 
      then  x : losQuePertenecen xs s
      else losQuePertenecen xs s

-- (n * n )  /pero queda asi
--Realmente es asi (n * n) por el add * n (recursividad)
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos ls = setToList (sinRepetidosSet ls)

sinRepetidosSet :: Eq a => [a] -> Set a
sinRepetidosSet [] = emptyS 
sinRepetidosSet (x:xs) = addS x (sinRepetidosSet xs)

--O(n * n * n) 
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s t1 t2 ) = 
    unionS (unirTodos t2) (unionS (unirTodos t1)  s) 


---------------------------------------------------
