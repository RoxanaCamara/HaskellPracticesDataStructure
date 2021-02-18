import Practica6

-----------PRACTICA 7
data Map k v = M [k] [v] deriving Show

--INVARIANTE
--Los valores de Key y Value pueden ser de diferene tipo 
--No pueden haber una keys de tipos diferentes en un mismo Map
--No pueden haber una values de tipos diferentes en un mismo Map
--Debe haber una key que exista en ese Map (DeleteM)

-- O(1)
emptyM :: Map k v
emptyM =  M [] []

-- = O( n * n ) --ESTA RANCIO
{--assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM  clave valor  (M lsk lsv) =
    if elem clave lsk 
    then(assocM clave valor ( deleteM clave (M lsk lsv)))
    else M (clave : lsk ) (valor : lsv) --}


assocOrAdd::Eq k=> k-> v-> [k]->[v]->([k],[v])
assocOrAdd k v [] _ = ([k],[v])
assocOrAdd k' v' (k:ks)(v:vs)= if k==k'
    then ((k:ks),(v':vs))
    else ((k:fst(assocOrAdd k' v' ks vs)),(v:snd(assocOrAdd k' v' ks vs)))

assocM (M ks vs) k v = M (fst(assocOrAdd k v ks vs)) (snd(assocOrAdd k v ks vs))


-- O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM clave ( M [] _ ) = Nothing
lookupM clave (M (k:lsk) (v:lsv)) = if k == clave
    then (Just v)
    else lookupM clave (M lsk lsv)

--O(n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM clave (M ls lv) = M (deleteMClave clave ls ) (deleteMValor clave ls lv )

deleteMClave :: Eq k => k -> [k] -> [k]
deleteMClave clave [] = []
deleteMClave clave (l:ls) =
    if l == clave
    then  ls
    else  l : deleteMClave clave ls

deleteMValor :: Eq k => k -> [k] -> [b] -> [b]
deleteMValor clave (l:ls) (v:lv) =
    if l == clave
    then lv
    else  v : deleteMValor clave ls lv

--O(1)
domM :: Map k v -> [k]
domM (M lsk lsv) = lsk

--  (assocM 6 "Nahu" $  assocM 1 "Roci" $ assocM 3 "Meli" $ assocM 5 "Dani" emptyM)

------------USUARIO------------------------------------

---O(n * n)
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m =  values (domM m) m

values :: Eq k => [k] -> Map k v -> [Maybe v]
values [] m =  []
values  (x:xs) m =  (lookupM x m) : values xs m

---O(n * n)
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] m = True
todasAsociadas (l:ls) m  =
    (notIsNothing (lookupM l m)) && todasAsociadas ls m
 
notIsNothing:: Maybe a -> Bool
notIsNothing Nothing = False
notIsNothing _ = True

---O(n)
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap (par: pares) = assocM (fst par) (snd par) (listToMap pares)


---O(n * n)
mapToList:: Eq k => Map k v -> [(k, v)]
mapToList m  = mapeadorM (domM m) (valuesM m )

mapeadorM :: Eq k =>[k] -> [Maybe v] -> [(k, v)]
mapeadorM [] _ = []
mapeadorM (x:xs) (l:ls) = ( x, (fromJust l)) : mapeadorM xs ls

fromJust :: Maybe v -> v
fromJust (Just v) = v

---
unirDoms :: Eq k => [Map k v] -> Set k 
unirDoms [] = emptyS
unirDoms (m : mps) = unionS ( transformarASet (domM m) ) (unirDoms mps)


--O(n * n) por culpa de sinRepetidos
transformarASet:: Eq k => [k] -> Set k
transformarASet [] = emptyS 
transformarASet (k:ks) = addS k (transformarASet ks)

--------
incrementar :: Eq k => [k] -> Map k Int -> Map k Int 
incrementar ks m = listToMap (incrementacion ks (mapToList m))
    
incrementacion:: Eq k => [k] -> [(k, Int)]-> [(k, Int)]
incrementacion ks [] = []  
incrementacion ks (p:ps) =
    if elem (fst p) ks
        then incrementarPar( p ) : incrementacion ks ps
        else p :  incrementacion ks ps

incrementarPar :: ( k, Int) -> (k , Int )
incrementarPar (a, b) = ( a , (b+1) ) 

---------
mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = 
   mergear  m1 (domM m1) (mapToList m2)

mergear :: Eq k => Map k v -> [k] -> [(k, v)] ->  Map k v 
mergear m1 lsKm1 [] = m1
mergear  m1 lsKm1 (par: pares) =
    if elem (fst par) lsKm1
        then mergear m1 lsKm1 pares
            else assocM (fst par) (snd par) (mergear m1 lsKm1 pares)  


---------EJERCICIO 3

indexar ::  Eq k => [k] -> Map k Int
indexar [] = emptyM
indexar (l:ls) = assocM l (length ls) (indexar ls)

--Debe ser el mismo Char siempre 
ocurrencias :: String -> Map Char Int 
ocurrencias "" = emptyM
ocurrencias (l:ls) =
     assocM l ((cantVecesQueAparece l (l:ls)))
              (ocurrencias ls) 

cantVecesQueAparece:: Char -> String -> Int
cantVecesQueAparece elemento "" = 0
cantVecesQueAparece  elemento (x:xs) =  
    if  elemento == x 
    then 1 + cantVecesQueAparece elemento xs 
    else  cantVecesQueAparece elemento xs

--}
----------------------------------------------------------
--MULTISET
data MultiSet k = MS (Map k Int) deriving Show

emptyMS :: MultiSet a
emptyMS = MS emptyM

addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS elemento (MS map) =
        MS (assocM elemento 
               (aumentar ( lookupM elemento map )) 
                map )

aumentar :: Maybe Int -> Int
aumentar Nothing = 1
aumentar (Just a) = a + 1

--------------------
ocurrenciasMS :: Ord a => a -> MultiSet a -> Int
ocurrenciasMS elemento (MS map) = 
    ocurrenciasM elemento map

ocurrenciasM :: Ord k => k -> Map k Int  -> Int
ocurrenciasM elemento map =
   case (lookupM elemento map) of 
    Nothing-> 0 
    Just a -> a 

-----------------------
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MS map1) (MS map2) =
    MS (unicion  (domM map1 ) map1 map2)

unicion ::  Ord k => [k] -> Map k Int -> Map k Int  -> Map k Int
unicion [] map1 map2 = map2
unicion (k:ks) map1 map2 =
    assocM 
    k 
    ((ocurrenciasM k map1)+ (ocurrenciasM k map2))
    (unicion ks map1 map2)
    
----------------------------------------

--interseccionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
    




   


--(MS (assocM "a" 10 $  assocM "Roci" 3 $ emptyM) )



---------------------------------------------------------



--HEAP (Kola de prioridad)

{--data Heap a = H [a]


emptyH :: Heap a
emptyH = H []

isEmptyH :: Heap a -> Bool
isEmptyH (H a) = isEmptyH a



--}





