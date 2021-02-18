import Practica_5_Queue

--Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue

--Lineal
lengthQ :: Queue a -> Int 
lengthQ q = if isEmptyQ q then 0 else 1 + lengthQ (dequeue q)

--Lineal
queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q then [] else (firstQ q) : queueToList (dequeue q )

--Lineal
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = 
    if isEmptyQ q2 then q1 else  enqueue (firstQ q2) (unionQ q1 (dequeue q2) )

--(enqueue 9 $ enqueue 9 $enqueue 97 $ enqueue 87 $ enqueue 7 emptyQ)

--(enqueue 4 $ enqueue 3 $enqueue 5 $ enqueue 1 $ enqueue 7 emptyQ)