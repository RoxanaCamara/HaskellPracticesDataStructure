module AVL (
	  AVL,
	  emptyAVL,
	  isEmptyAVL,
	  findAVL,
	  insertAVL,
	  deleteAVL,
	  minAVL,
	  deleteMinAVL,
	  inorderAVL
	)
where

data AVL a =
    EmptyAVL
  | NodeAVL a Int (AVL a) (AVL a)
-- Invariantes de representación:
-- Sea (NodeAVL x h ti td) un árbol AVL no vacío
-- 1) El árbol es BST
-- 2) h es la altura del árbol
-- 3) abs (heightAVL ti - heightAVL td) <= 1 (condicion de AVL)
-- 4) ti y td también son AVL
-- 5) los elementos no están repetidos

-- Auxiliares Simples -----------------------------------
---------------------------------------------------------
-- aprovecha los invariantes para devolver la altura
-- del arbol
-- Eficiencia: O(1)
heightAVL EmptyAVL          = 0
heightAVL (NodeAVL _ h _ _) = h

-- construye una hoja que cumple con los invariantes
-- Eficiencia: O(1)
leafAVL x = NodeAVL x 1 EmptyAVL EmptyAVL

-- dados dos árboles con invariantes de AVL
-- arma un nuevo nodo con invariantes de AVL
nodeAVL x ti td =
	NodeAVL x (1 + max (heightAVL ti) (heightAVL td)) ti td
---------------------------------------------------------

-- Propósito: denota un árbol vacío
-- Eficiencia: O(1)
emptyAVL :: AVL a
emptyAVL = EmptyAVL

-- Propósito: indica si el arbol está vacío
-- Eficiencia: O(1)
isEmptyAVL :: AVL a -> Bool
isEmptyAVL EmptyAVL = True
isEmptyAVL _        = False

-- Propósito: encuentra un valor dado dentro del arbol
-- Eficiencia: O(log n)
findAVL :: Ord a => a -> AVL a -> Maybe a
findAVL _  EmptyAVL = Nothing
findAVL x (NodeAVL r h ti td) =
   if x == r
    then Just r
    else if x < r
           then findAVL x ti
           else findAVL x td

-- Propósito: inserta un elemento en el árbol
-- Eficiencia: O(log n)
insertAVL :: Ord a => a -> AVL a -> AVL a
insertAVL x EmptyAVL = leafAVL x
insertAVL x (NodeAVL r h ti td) =
    if x == r
       then NodeAVL x h ti td
       else if x < r
               then joinAVL r (insertAVL x ti) td
               else joinAVL r ti (insertAVL x td)

-- Eficiencia: O(log n)
deleteAVL :: Ord a => a -> AVL a -> AVL a
deleteAVL e EmptyAVL = EmptyAVL
deleteAVL e (NodeAVL r h ti td) =
    if e == r
       then mergeTrees ti td
       else if e < r
               then joinAVL r (deleteAVL e ti) td
               else joinAVL r ti (deleteAVL e td)

mergeTrees ti EmptyAVL = ti
mergeTrees ti td       = joinAVL (minAVL td) ti (deleteMinAVL td)

-- Prec.: el árbol no es EmptyAVL y es BST
-- Eficiencia: O(log n)
minAVL :: AVL a -> a
minAVL (NodeAVL x h EmptyAVL td) = x
minAVL (NodeAVL x h ti td)       = minAVL ti

-- Prec.: el árbol no es EmptyAVL y es BST
deleteMinAVL :: AVL a -> AVL a
deleteMinAVL (NodeAVL x h EmptyAVL td) = td
deleteMinAVL (NodeAVL x h ti td)       = joinAVL x (deleteMinAVL ti) td

-- Propósito: junta un elemento y dos subárboles devolviendo
-- un árbol BST y AVL
-- Prec.:
--  * ti y td son BSTs y AVLs
--  * las claves de ti son menores que r
--  * las claves de td son mayores que r
-- Eficiencia: O(1)
joinAVL r ti td =
	if abs (heightAVL ti - heightAVL td) <= 1 -- ¿está balanceado?
    then nodeAVL r ti td
    else if heightAVL ti > heightAVL td -- ¿para dónde está desbalanceado?
           then rotateFromLeft r ti td
           else rotateFromRight r ti td

-- Propósito: implementa una rotación de izquierda a derecha,
-- devolviendo un árbol BST y AVL
-- Prec.:
-- * los árboles son BST y AVL
-- * el arbol izquierdo tiene exactamente 2 más de altura que el lado derecho
-- Eficiencia: O(1)
rotateFromLeft r (NodeAVL ri _ tii tid) td =
    if heightAVL tii >= heightAVL tid        -- ¿se desbalanceó por su subarbol izq?
      then nodeAVL ri tii (nodeAVL r tid td) -- implemento rotación simple
      else case tid of                       -- implemento una rotación doble
    	       (NodeAVL rid _ tidi tidd) ->
     	          nodeAVL rid (nodeAVL ri tii tidi)
    	                      (nodeAVL r   tidd td)

{-
Explicación:

Rotación simple de izquierda a derecha

       (r)                    (ri)
			 / \                    /  \
			/   \                  /    \
		(ri)   td     ----->    tii   (r)
	  /  \                          / \
 	 /    \                        /   \
 tii    tid                     tid  td

Rotación doble de izquierda a derecha

        (r)                      (rid)
			  / \                     /     \
			 /   \                   /       \
		 (ri)   td     ----->    (ri)      (r)
	   /  \                    /  \      / \
	  /    \                  /    \    /   \
	 tii  (rid)              tii  tidi tidd td
        /   \
       /     \
	   tidi   tidd
-}

-- Propósito: implementa una rotación de derecha a izquierda,
-- devolviendo un árbol BST y AVL
-- Prec.:
-- * los árboles son BST y AVL
-- * el arbol derecho tiene exactamente 2 más de altura que el arbol izquierdo
-- Eficiencia: O(1)
rotateFromRight r ti (NodeAVL rd _ tdi tdd) =
    if heightAVL tdi <= heightAVL tdd        -- ¿se desbalanceó por su subarbol izq?
      then nodeAVL rd (nodeAVL r ti tdi) tdd -- implemento rotación simple
      else case tdi of                       -- implemento una rotación doble
      	     (NodeAVL rdi _ tdii tdid) ->
		            nodeAVL rdi
                        (nodeAVL r ti tdii)
                        (nodeAVL rd tdid tdd)

-- Eficiencia: O(n)
inorderAVL :: AVL a -> [a]
inorderAVL EmptyAVL            = []
inorderAVL (NodeAVL x h ti td) =
  inorderAVL ti ++ [x] ++ inorderAVL td

-------------------------------------------
-- Para testing
listToAVL :: Ord a => [a] -> AVL a
listToAVL xs = insertMany' (reverse xs)

insertMany' [] = emptyAVL
insertMany' (x:xs) = insertAVL x (insertMany' xs)

-------------------------------------------
-- Operaciones de impresion de arboles
-------------------------------------------
-- Cortesía de Fidel
instance (Show a) => Show (AVL a) where
  show t = showsTree "\n" "\n" t ""
    where showsTree ctxtI ctxt EmptyAVL            = showString ctxtI . showString "E" 
          showsTree ctxtI ctxt (NodeAVL x _ ti td) = showString ctxtI . showString "N " . shows x 
                                                                      . showsTree (ctxt ++ "+ ") (ctxt ++ "| ") ti 
                                                                      . showsTree (ctxt ++ "+ ") (ctxt ++ "  ") td

renderT t = foldr (\line screen -> line ++ "\n" ++ screen) "" (renderT' [] t)
  where renderT' xs EmptyAVL              = renderSpine 0 (ocupaDeAnchoXs xs) (reverse xs) 0
        renderT' xs (NodeAVL x _ ti EmptyAVL) = renderT' (x:xs) ti
        renderT' xs (NodeAVL x _ EmptyAVL td) = renderT' (x:xs) td
        renderT' xs (NodeAVL x _ ti td)     = renderSpine (ocupaDeAnchoT ti) (ocupaDeAnchoXs (x:xs))
                                                      (reverse (x:xs)) (ocupaDeAnchoT td)
                                          ++ combinar (ocupaDeAnchoT ti) (renderT' [] ti) 
                                                      (ocupaDeAnchoXs (x:xs)) 
                                                      (renderT' [] td) (ocupaDeAnchoT td)

        combinar li []     lx ys     ld = map (\y -> nBlancos li ++ nBlancos lx ++ y) ys
        combinar li xs     lx []     ld = map (\x -> x ++ nBlancos lx ++ nBlancos ld) xs
        combinar li (x:xs) lx (y:ys) ld = (x ++ nBlancos lx ++ y) : combinar li xs lx ys ld

        renderSpine li lx xs ld = map (\x -> renderLine li lx x ld) xs

        renderLine li lx x ld = nBlancos li ++ nBlancos (lx - ocupaDeAnchoX x) ++ show x ++ nBlancos ld

        nBlancos n = [' ' | i <- [1..n]]

        ocupaDeAnchoT EmptyAVL                  = 0
        ocupaDeAnchoT (NodeAVL x _ EmptyAVL EmptyAVL) = ocupaDeAnchoX x
        ocupaDeAnchoT (NodeAVL x _ ti     EmptyAVL) = max (ocupaDeAnchoX x) (ocupaDeAnchoT ti)
        ocupaDeAnchoT (NodeAVL x _ EmptyAVL td)     = max (ocupaDeAnchoX x) (ocupaDeAnchoT td)
        ocupaDeAnchoT (NodeAVL x _ ti     td)     = ocupaDeAnchoT ti + ocupaDeAnchoX x + ocupaDeAnchoT td

        ocupaDeAnchoXs xs = maximum (map ocupaDeAnchoX xs)
        ocupaDeAnchoX x = length (show x)