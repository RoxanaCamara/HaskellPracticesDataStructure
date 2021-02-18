--Priority Queue

data PriorityQueue a = PQ [a] deriving Show
--Invariantes de Representacion:
--Es prioridad los de mayor valor

--Constante 
emptyPQ :: PriorityQueue a
emptyPQ = (PQ [])

--Constante
isEmprtyPQ :: PriorityQueue a -> Bool 
isEmprtyPQ (PQ xs) = null xs

--Lineal
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a 
insertPQ e (PQ xs) = (PQ (ordenar e xs ))


ordenar ::  Ord a => a -> [a] -> [a]
ordenar e [] = [e]
ordenar e (x:xs) = if e > x then e : xs else x : ordenar e xs

--Constante
findMaxPQ :: Ord a => PriorityQueue a -> a 
findMaxPQ (PQ xs) = head xs 

--Constante
deleteMinPQ :: Ord a => PriorityQueue a  -> PriorityQueue a 
deleteMinPQ (PQ xs) = (PQ (tail xs))

heapSort :: Ord a => [a] -> [a] 
heapSort xs =  pqToList (listToPQ xs)

--Lineal
listToPQ:: Ord a => [a] -> PriorityQueue a  
listToPQ [] = emptyPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs)

--Lineal
pqToList ::  Ord a =>  PriorityQueue a -> [a]
pqToList pq = 
    if isEmprtyPQ pq 
    then [] 
    else (findMinPQ pq) : pqToList (deleteMinPQ pq)