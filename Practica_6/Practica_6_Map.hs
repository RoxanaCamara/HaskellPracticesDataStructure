module Practica_6_Map ( 
        Map, emptyM, assocM, lookupM, keys, deleteM) where

data Map k v = MKV [k] [v] deriving Show 

--INVARIANTE
--Los valores de Key y Value pueden ser de diferene tipo 
--No pueden haber una keys de tipos diferentes en un mismo Map
--No pueden haber una values de tipos diferentes en un mismo Map
--Debe haber una key que exista en ese Map (DeleteM)

------------------------------------------------------------------------------
--Constante --O(1)
emptyM :: Map k v
emptyM = (MKV [] [])

------------------------------------------------------------------------------
--Lineal   --O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (MKV ks vs)  = (MKV (fst (assocOrAdd ks vs k v) ) (snd (assocOrAdd ks vs k v )))

assocOrAdd :: Eq k => [k] -> [v] -> k -> v -> ([k], [v])
assocOrAdd [] _ k v = ([k], [v])
assocOrAdd (k :ks) (v :vs) kn vn = if k == kn then ((k:ks), (v:vs)) else  ((k : (fst (assocOrAdd ks vs kn vn)) ), ( v: (snd (assocOrAdd ks vs kn vn) )))

------------------------------------------------------------------------------
--Lineal

lookupM :: Eq k => k -> Map k v -> MaybeN v
lookupM kn (MKV [] vs)     = NothingN
lookupM kn (MKV (k:ks) (v:vs)) = if k == kn then (JustN v) else lookupM kn (MKV ks vs) 



------------------------------------------------------------------------------

--Lineal
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (MKV ks vs) = (MKV (fst (deleteOrd ks vs k) ) (snd (deleteOrd ks vs k)))

deleteOrd :: Eq k => [k] -> [v] -> k -> ([k], [v])
deleteOrd [] _ k  = ([], [])
deleteOrd (k:ks) (v:vs) kn = if k == kn then (ks, vs) else  ((k : (fst (deleteOrd ks vs kn)) ), ( v: (snd (deleteOrd ks vs kn) )))

------------------------------------------------------------------------------
--Constante
keys :: Map k v -> [k]
keys (MKV ks vs) = ks

domM :: Map k v -> [k]
domM (MKV lsk lsv) = lsk

------------------------------------------------------------------------------

data MaybeN a = NothingN | JustN a deriving Show