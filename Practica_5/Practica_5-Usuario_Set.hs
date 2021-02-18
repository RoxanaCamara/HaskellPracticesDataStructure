import Practica_5_Set

import Practica_5_Queue

----------------------------------------------------------------------
--SET 
-- emptyS, addS, belongs, sizeS, removeS, unionS, setToList

--
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s = if belongs x s  
    then x : losQuePertenecen xs s  
    else losQuePertenecen xs s

--
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList ( listToSet xs)

---
listToSet :: Eq a => [a] -> Set a 
listToSet [] = emptyS
listToSet (x:xs) = addS x (listToSet xs )

----------------------------------------------------------------------

