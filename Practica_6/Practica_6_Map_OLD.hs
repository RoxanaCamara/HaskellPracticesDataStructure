
data Map k v = M [k] [v] deriving Show

-- O(1)
emptyM :: Map k v
emptyM =  M [] []

-- = O( n * n ) --ESTA RANCIO
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM  clave valor  (M lsk lsv) =
    if elem clave lsk 
    then(assocM clave valor ( deleteM clave (M lsk lsv)))
    else M (clave : lsk ) (valor : lsv) 

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
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m =  values (domM m) m

values :: Eq k => [k] -> Map k v -> [Maybe v]
values [] m =  []
values  (x:xs) m =  (lookupM x m) : values xs m
