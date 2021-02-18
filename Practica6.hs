--PRACTICA 6

--La eficiencia toma el peor caso
-- El ++ tiene un costo de O(n) 
--head' O(1)  
--sumar O(1)  
--factorial O(n)
--longitud  O(n)
--factoriales O(n * n)
--pertenece O(n) 
--sinRepetidos O(n * n)
--append O(n) 
--concatenar O(n * n)
--takeN O(n)
--dropN O(n)
--partir O(n)    --No usa takeN y dropN mas de una vez
--minimo O(n)
--sacar O(n)
--ordenar O(n)

---SET
module Practica6 ( 
Set, emptyS, addS, belongs, sizeS, removeS, unionS, intersectionS, setToList) where

data Set a = S [a] deriving Show

sinRepetidos:: Eq a => [a] -> [a]
sinRepetidos [] =  []
sinRepetidos (x:xs) =  if elem x xs then sinRepetidos xs else x: sinRepetidos xs

losRepetidos:: Eq a => [a] -> [a]
losRepetidos [] =  []
losRepetidos (x:xs) =  if elem x xs then x:losRepetidos xs else losRepetidos xs

tuVieja  = S [ 1, 2, 3 ]
tuViejo  = S [ 7, 6, 3 ]

--O(1)
emptyS :: Set a
emptyS = S []

--O(n * n) por culpa de sinRepetidos
addS :: Eq a => a -> Set a -> Set a 
addS elemnto (S ls) = S (sinRepetidos (elemnto : ls))

--O(n)
belongs :: Eq a => a -> Set a -> Bool
belongs elemnto (S ls) = elem elemnto ls  
 
--O(n)
sizeS :: Eq a => Set a -> Int 
sizeS (S ls) = length ls

--O(n)
removeS:: Eq a => a -> Set a -> Set a
removeS  elemnto (S ls) = S (remover elemnto ls)

remover:: Eq a => a -> [a] -> [a]
remover elemnto [] = []
remover elemnto (x:xs) = if elemnto == x
    then remover elemnto xs
    else x : remover elemnto xs

--O(n * n)
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S ls) (S xs) = S (sinRepetidos (ls ++ xs))

--O(n * n)
intersectionS :: Eq a => Set a -> Set a -> Set a 
intersectionS (S ls) (S xs) =  S (losRepetidos (ls ++ xs)) 

--O(1)
setToList :: Eq a => Set a -> [a]
setToList (S ls) = ls

--------USUARIO-----------------

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (l:ls) s =
    if belongs l s 
    then l : losQuePertenecen ls s
    else losQuePertenecen ls s

sinRepetidosU :: Eq a => [a] -> [a]
sinRepetidosU ls1 =  setToList (sinRepetidosConSet ls1)

sinRepetidosConSet :: Eq a => [a] -> Set a
sinRepetidosConSet [] = emptyS
sinRepetidosConSet (l:ls) = addS l (sinRepetidosConSet ls)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT a t1 t2) =
    unionS a (unionS (unirTodos t1) (unirTodos t2))

nodoS = NodeT (S [1,76]) 
        (NodeT (S [88,76]) 
            (NodeT (S [1,3])  
                EmptyT 
                (NodeT (S [88,12]) EmptyT  EmptyT)) 
            EmptyT)
        (NodeT (S [36,3])  
            EmptyT 
            (NodeT (S [77,1])  
                EmptyT 
                EmptyT))

---------------------------------

data Queue a = Q [a] Int deriving Show

-- Funcion Rancia :(
--isEmptyQ :: Queue a -> Bool 
--isEmptyQ (Q []) = True
--isEmptyQ (Q a) = False

--O(1)
emptyQ :: Queue a 
emptyQ = Q [] 0

--O(1)
isEmptyQ :: Queue a -> Bool 
isEmptyQ (Q ls nro) = isEmpty ls 

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty ls = False

-- O(n)
queue :: a -> Queue a -> Queue a
queue elemnto (Q ls nro) = Q (ls++[elemnto]) (nro+1)

--O(1)
firstQ :: Queue a -> a
firstQ (Q ls nro) = head ls 

--O(n)
dequeue :: Queue a -> Queue a
dequeue (Q ls nro) =Q (tail ls) (nro-1) 

-------------USUARIO-------------------------------------
-- O(n)
lengthQ :: Queue a -> Int 
lengthQ q = if isEmptyQ q 
    then 0
        else 1 + lengthQ q

-- O(n)
-- Por la recursion usa O(n) y por el dequeue usa O(n)
queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q 
    then []
    else firstQ q : queueToList (dequeue q)

--
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q2
    then q1
    else unionQ (queue (firstQ q2) q1) (dequeue q2)

---------------------------------------------------------------------

--STACK (Pila)
data Stack a = St [a] [a]  deriving Show

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

--O(1)
emptySt :: Stack a
emptySt = St [] [] 

--O(1)
isEmptySt :: Stack a -> Bool
isEmptySt (St ls lsM) = isEmpty ls


--O(1)
pushSt :: Ord a => a -> Stack a -> Stack a
pushSt elemnto (St ls lsM) = St (elemnto : ls ) (agregarLsMax elemnto lsM)

agregarLsMax:: Ord a => a -> [a] -> [a] 
agregarLsMax elemnto [] = [elemnto]
agregarLsMax  elemnto lsM = 
    if (head lsM) > elemnto
        then  (head lsM) : lsM
        else elemnto : lsM


topSt:: Stack a -> a
topSt (St ls lsM) = head (ls) 

--O(1)
popSt:: Stack a -> Stack a
popSt (St ls lsM) = St (tail ls) (tail lsM) 

--------USUARIO-----------------
apilar :: Ord a => [a] -> Stack a
apilar [] = emptySt  
apilar (x:xs) = pushSt x (apilar xs) 

desapilar :: Stack a -> [a]
desapilar st =  if isEmptySt st
    then []
    else  (topSt st) : desapilar ( popSt st)

{--non
treeToStack :: Tree a -> Stack a
treeToStack EmptyT = emptySt
treeToStack nodo =  transformacionStack nodo emptySt

transformacionStack :: Tree a -> Stack a -> Stack a
transformacionStack (NodeT a t1 t2) st =
--}


--balanceado :: String -> Bool 


{--nodoS = NodeT 1 
        (NodeT 2 
            (NodeT 3  
                EmptyT 
                (NodeT 4 EmptyT  EmptyT)) 
            EmptyT)
        (NodeT 5  
            EmptyT 
            (NodeT 6  
                EmptyT 
                EmptyT))
--}
--------------------------------------------------------
--QUE CON LOGITUD CONSTANTE

lenQ :: Queue a -> Int --O(1)
lenQ (Q ls cant) = cant

----------------------------------------
--STACK CON MAXIMO EN TIEMPO CONSTANTE

--INVARIANTES: 
--El Stack no debe estar vacio

maxS :: Ord a => Stack a -> a
maxS (St ls lsM) = head lsM

--------------------------------------------------------------------------

data MaybeN a = NothingN | JustN a deriving Show

--O(1)
headM :: [a] ->MaybeN a 
headM []     = NothingN
headM (l:ls) = JustN l

--O(n)
lastM :: [a] ->MaybeN a 
lastM []     = NothingN
lastM (l:ls) = 
    if isEmpty ls 
        then JustN l 
        else lastM ls

--O(n)
{--maximumM :: Ord a => [a] -> MaybeN a
maximumM [] = NothingN
maximumM ls = JustN ( maximo ls)--}


--O(n)
--Si tiene un elemento, queda vacio
initM :: [a] -> MaybeN [a]
initM []     = NothingN
initM ls     = case init' ls  of 
    []-> NothingN 
    a -> JustN a 

init' :: [a] -> [a]
init' []     = []     
init' (l:ls) =  
    if isEmpty ls 
    then init' ls 
    else l : init' ls

--O(n)
get :: Int -> [a] -> MaybeN a
get nro [] = NothingN
get nro (l:ls) = 
    if (nro-1) == 0 --si quiero que cuando pida la posicion 1 [a,b,c] = a (no cuenta el cero)
    --if nro == 0 --si quiero que cuando pida la posicion 1 [a,b,c] = b (cuenta el cero)
    then JustN l
    else get (nro-1) ls

--O(n)
indiceDe :: Eq a => a ->[a] -> MaybeN Int
indiceDe elemento ls =  case ubicacion elemento ls  of 
    0 -> NothingN 
    a -> JustN a

ubicacion :: Eq a => a -> [a] -> Int
ubicacion elemento [] = 0
ubicacion elemento ls = 
    if  (fromJust (get (length ls) ls )) == elemento
    then length ls
    else ubicacion elemento (init' ls)

fromJust :: MaybeN a -> a
fromJust (JustN a) = a
  
--lookupM :: Eq k => [(k,v)] -> k -> Maybe v

--etc