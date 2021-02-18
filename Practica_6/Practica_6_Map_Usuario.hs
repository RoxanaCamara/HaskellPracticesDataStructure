import Practica_6_Map

-- emptyM, assocM, lookupM, keys, deleteM


--  (assocM 6 "Nahu" $  assocM 1 "Roci" $ assocM 3 "Meli" $ assocM 5 "Dani" emptyM)
valuesM :: Eq k => Map k v -> [MaybeN v]
valuesM m =  values (keys m) m

values :: Eq k => [k] -> Map k v -> [MaybeN v]
values [] m =  []
values  (x:xs) m =  (lookupM x m) : values xs m

