data Tree a  = EmptyT 
             | NodeT a (Tree a) (Tree a) deriving Show

--INVARIANTE:
-- Precondicion de la estructura
-- Los valores mas chicos se encuentran a la izquierda 
        --de la raiz y los mas grandes a la derecha
--Sin repetidos
-- Ela rbol en splitMinBST no debe estar vacio


-- O log(n) -----------------------------------------------------------  
--Dos funciones que hacen lo mismo:

--Admite repetidos 
insertBST :: Ord a => a -> Tree a -> Tree a 
insertBST elemento EmptyT = ( NodeT elemento EmptyT EmptyT)
insertBST elemento (NodeT a n1 n2) =
    if elemento < a
        then (NodeT a (insertBST elemento n1) n2  )
            else  (NodeT a n1 (insertBST elemento n2) )


-- O log(n)   
--No admite repetido -En este caso no lo copia /lo sobreescribe
insertBSTsinRepe :: Ord a => a -> Tree a -> Tree a 
insertBSTsinRepe elemento EmptyT = ( NodeT elemento EmptyT EmptyT)
insertBSTsinRepe elemento (NodeT a n1 n2) =
    if elemento == a 
        then (NodeT elemento n1 n2)
            else if elemento < a
                    then (NodeT a (insertBSTsinRepe elemento n1) n2  )
                        else  (NodeT a n1 (insertBSTsinRepe elemento n2) )


deleteBST :: Ord a => a -> Tree a -> Tree a 
deleteBST elemento EmptyT = EmptyT --El arbol no debe estar vacio asi no me haces gastar memoria al pedo ahr
deleteBST elemento (NodeT a EmptyT EmptyT) =  -- 5 (NodeT 5 EmptyT EmptyT) -> EmptyT
    if a == elemento
        then EmptyT 
        else (NodeT a EmptyT EmptyT)
deleteBST elemento (NodeT x EmptyT t2)=    -- 5 (NodeT 5 EmptyT (NodeT 6 EmptyT EmptyT)) -> (NodeT 6 EmptyT EmptyT)
    if x==elemento 
    then t2
    else (NodeT x EmptyT (deleteBST elemento t2))
deleteBST elemento (NodeT a t1 t2) = -- 5 (NodeT 5 (NodeT 4 EmptyT EmptyT) (NodeT 6 EmptyT EmptyT)) -> 
    if elemento == a
    then  NodeT (maxBST t1) (deleteBST (maxBST t1) t1) t2
        else if elemento < a 
         then (NodeT a (deleteBST elemento t1) t2  )
        else  (NodeT a t1 (deleteBST elemento t2) )



--O log (n)-----------------------------------------------------------
perteneceBST :: Ord a => a -> Tree a -> Bool
perteneceBST elemento EmptyT = False
perteneceBST elemento (NodeT a n1 n2) = 
    if elemento == a 
        then True
        else if elemento < a
            then perteneceBST elemento n1
                else perteneceBST elemento n2


--
--splitMinBST :: Ord a => Tree a -> (a, Tree a)
--splitMinBST elemento node = 
--    ( (minBST node ) , (deleteBST (minBST node) node ) )




--------------------------------------------
-- O log(n)  
--No tiene que estar vacio el arbol
minBST:: Ord a => Tree a -> a
--Caso Base
minBST (NodeT a EmptyT n2) = a
minBST (NodeT a n1 n2 ) = minBST n1 

--No tiene que estar vacio el arbol
maxBST:: Ord a => Tree a -> a
--Caso Base
maxBST (NodeT a n1 EmptyT ) = a
maxBST (NodeT a n1 n2 ) = maxBST n2 

--------------------------------------------
bst = NodeT 5 
            --Izq
            (NodeT 3 (NodeT 2 EmptyT EmptyT) EmptyT) 
            --Der
           ( NodeT 8 (NodeT 7 EmptyT EmptyT) EmptyT) 

