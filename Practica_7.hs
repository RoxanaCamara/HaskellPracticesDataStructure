--BST --Sin elementos repetidos

data Tree a = EmptyT 
            | NodeT a (Tree a) (Tree a) deriving Show

--TODAS DEBEN SER IMPLEMENTADAS EN O(log n)

----------------------------------------------------------------------------------
balanceado:: Tree a -> Bool 
balanceado  EmptyT = False 
balanceado (NodeT a t1 t2) = alturaIZQ t1 >= alturaDER t2 

alturaIZQ ::   Tree a -> Int 
alturaIZQ EmptyT = 0 
alturaIZQ (NodeT x t1 t2) = 1 + alturaIZQ t1

alturaDER ::   Tree a -> Int 
alturaDER EmptyT = 0 
alturaDER (NodeT x t1 t2) = 1 + alturaDER t2

----------------------------------------------------------------------------------
insertBST :: Ord a => a -> Tree a -> Tree a 
insertBST e EmptyT = (NodeT e EmptyT EmptyT)
insertBST e (NodeT x t1 t2 ) = 
    if e == x 
    then (NodeT x t1 t2 )
    else if e < x 
        then balancearIZQ (NodeT x (insertBST e t1) t2 )
        else if e > x  
            then balancearDER (NodeT x t1 (insertBST e t2) )
            else error "NO SE QUE PUTA HACER"

balancearIZQ :: Ord a => Tree a -> Tree a
balancearIZQ (NodeT a t1 (NodeT a2 t21 t22 ) ) = 
    (NodeT a2 (NodeT a t1 t21) t22) 

balancearDER:: Ord a => Tree a -> Tree a
balancearDER (NodeT a (NodeT a1 t11 t12 ) t2 ) =
   (NodeT a1 (NodeT a t11 t12) t2 )
  
-- O log(n)  
--No tiene que estar vacio el arbol
minBST:: Ord a => Tree a -> a
minBST (NodeT a EmptyT n2) = a
minBST (NodeT a n1 n2 ) = minBST n1 

maxBST:: Ord a => Tree a -> a
maxBST (NodeT a n1 EmptyT ) = a
maxBST (NodeT a n1 n2 ) = maxBST n2 


perfeccion = (NodeT 19  
    (NodeT 17 
        (NodeT 4 
            (NodeT 3 EmptyT EmptyT) 
            (NodeT 5 EmptyT EmptyT)) 
        (NodeT 7 
            (NodeT 6 EmptyT EmptyT) 
            (NodeT 8 EmptyT EmptyT)))
    (NodeT 28 
        (NodeT 25
            (NodeT 23 EmptyT EmptyT) 
            (NodeT 26 EmptyT EmptyT)) 
        (NodeT 31 
            (NodeT 30 EmptyT EmptyT) 
            (NodeT 34 EmptyT EmptyT))) )  





{--

balancear (NodeT a t1 (NodeT a2 t21 t22)) = 
    if balanceado (NodeT a t1 (NodeT a2 t21 t22))
        then (NodeT a t1 (NodeT a2 t21 t22))
            else (NodeT a2 (NodeT a  t1 t21 ) t22)

--}









--No balanceado--------------------------------------------------------------------------------


{--
(NodeT 1  
    (NodeT 1 
        (NodeT 1 EmptyT EmptyT) 
        (NodeT 1 EmptyT EmptyT)) 
    (NodeT 7 
        (NodeT 3 
            (NodeT 1 EmptyT EmptyT) 
            (NodeT 2 EmptyT EmptyT)) 
        (NodeT 5 
            (NodeT 4 EmptyT EmptyT) 
           (NodeT 6 EmptyT EmptyT))))  

--Si balanceado----------------------------------------------------------------------------------       

(NodeT 1  
    (NodeT 1 
        (NodeT 3 
            (NodeT 1 EmptyT EmptyT) 
            (NodeT 2 EmptyT EmptyT)) 
        (NodeT 6 
            (NodeT 4 EmptyT EmptyT) 
            (NodeT 5 EmptyT EmptyT)))
    (NodeT 1 
        (NodeT 1 EmptyT EmptyT) 
        (NodeT 1 EmptyT EmptyT)) )  

--Si balanceado perfectamente----------------------------------------------------------------------------------


--}     

----------------------------------------------------------------------------------

