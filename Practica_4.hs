--      PARCIAL 1
--EJERCICIO 2

{--darVuelta :: Texto -> Texto
darVuelta text = reversa (palabrasT text)

reversa :: [String] -> Texto
reversa [] = nuevoT
reversa (x:xs) = 
    agregarPalabraRevertida x (reversa xs)


agregarPalabraRevertida :: [Char] -> Texto -> Texto
agregarPalabraRevertida [] nT = espacioT nT
agregarPalabraRevertida (x:xs) nT =
    agregarPalabraRevertida xs (letraT nT x) --}

----PRACTICAS 4 -EJERCICIOS INTEGRADORES
data Pizza = Prepizza 
           | Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa 
                 | Queso
                 | Jamon
                 | Morron
                 | Aceitunas Int deriving Show
                 --No va eq si porque uno de los enum tiene un dato extra Eq)

---
cantidadDeCapas:: Pizza -> Int 
cantidadDeCapas (Prepizza) =  0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

--
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) =  
    (Capa x (armarPizza xs) ) 

--
sacarJamon :: Pizza -> Pizza 
sacarJamon (Prepizza) = Prepizza
sacarJamon (Capa i p) = if mismoIngrediente i Jamon 
    then  p
        else (Capa i (sacarJamon p) )

--
tieneSoloSalsaYQueso :: Pizza -> Bool 
tieneSoloSalsaYQueso p =
    (tieneIngrediente Salsa p ) && (tieneIngrediente Queso p )

tieneIngrediente :: Ingrediente -> Pizza ->  Bool 
tieneIngrediente ingrediente (Prepizza)  = False 
tieneIngrediente ingrediente (Capa i p)  = 
    mismoIngrediente i ingrediente || tieneIngrediente ingrediente p  


mismoIngrediente :: Ingrediente -> Ingrediente -> Bool 
mismoIngrediente Salsa Salsa = True 
mismoIngrediente Queso Queso = True
mismoIngrediente Morron Morron = True 
mismoIngrediente (Aceitunas n) (Aceitunas m) = True
mismoIngrediente m n = False
--
duplicarAceitunas :: Pizza -> Pizza 
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) =  
    (Capa (masAceitunas i ) (duplicarAceitunas p )  )


masAceitunas :: Ingrediente -> Ingrediente
masAceitunas (Aceitunas n) = Aceitunas (n*2)
masAceitunas i = i

---
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = 
    (cantidadDeCapas x , x) : cantCapasPorPizza xs

------------------------------
---MAPA DE TESOROS 

data Dir = Izq | Der deriving (Show, Eq)

data Objeto = Tesoro | Chatarra deriving (Show, Eq)

data Cofre = Cofre [Objeto] deriving Show

data Mapa = Fin Cofre 
          | Bifurcacion Cofre Mapa Mapa  deriving Show

--------------------
hayTesoro :: Mapa -> Bool 
hayTesoro (Fin c) = existeTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2 ) = 
    existeTesoroEnCofre c  || hayTesoro m1 || hayTesoro m2 

existeTesoroEnCofre :: Cofre -> Bool 
existeTesoroEnCofre (Cofre ls) = existeTesoro ls

existeTesoro:: [Objeto] -> Bool 
existeTesoro [] = False
existeTesoro (x:xs) = x == Tesoro || existeTesoro xs 

----------------------
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c)               = existeTesoroEnCofre c
hayTesoroEn [] (Bifurcacion c m1 m2) = existeTesoroEnCofre c
hayTesoroEn (x:xs) (Fin c) = False
hayTesoroEn (x:xs) (Bifurcacion c m1 m2) =
    if x == Der 
        then hayTesoroEn xs m1
        else hayTesoroEn xs m2

---------------M1 -> Izq 
---------------M2 -> Der

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) = 
    if existeTesoroEnCofre c 
        then []
        else if hayTesoro m1 
                then Izq : caminoAlTesoro m1 
                else Der : caminoAlTesoro m2 

----
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = 
    if largoDelMapa m1 > largoDelMapa m2 
        then Izq : caminoDeLaRamaMasLarga m1 
        else Der : caminoDeLaRamaMasLarga m2

largoDelMapa :: Mapa -> Int 
largoDelMapa (Fin c) = 0
largoDelMapa (Bifurcacion c m1 m2) = 
     1 + max ( largoDelMapa m1 ) (largoDelMapa m2)


----No es lo pedido-------------------------------------------------------------1
{--(Bifurcacion (Cofre [Tesoro])   
    (Bifurcacion (Cofre [Tesoro]) 
        (Bifurcacion (Cofre [Tesoro]) 
            (Fin (Cofre [Tesoro])) 
            (Fin (Cofre [Tesoro]))) 
        (Fin (Cofre [Tesoro])))
    (Fin (Cofre [Tesoro])))  --}
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = sacarTesoro c 
tesorosPorNivel (Bifurcacion c m1 m2) = 
    


sacarTesoroPorMapa:: Mapa -> [Objeto]
sacarTesoroPorMapa (Fin c) = sacarTesoro c
sacarTesoroPorMapa (Bifurcacion c m1 m2) = sacarTesoro c

sacarTesoro :: Cofre -> [Objeto]
sacarTesoro (Cofre ls) = sacarOro ls

sacarOro :: [Objeto] -> [Objeto]
sacarOro [] = []
sacarOro (x:xs) = if x == Tesoro 
    then x : sacarOro xs
    else sacarOro xs

---------------------------------------------------------------------------------2
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c) = [[]]
todosLosCaminos (Bifurcacion c mi md) = 
   (prepend Izq (todosLosCaminos mi)) ++ 
   (prepend Der (todosLosCaminos md))

prepend :: Dir -> [[Dir]] -> [[Dir]]
prepend d [] = []
prepend d (ds:dss) =
   (d:ds):(prepend d dss)

----------------
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show

data Sector = S SectorId [Componente] [Tripulante] deriving Show 

type SectorId   = String 
type Tripulante = String 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave =  N (Tree Sector) deriving Show

------
sectores :: Nave -> [SectorId]
sectores (N tree ) = sectoresDelArbol tree 


sectoresDelArbol :: Tree Sector -> [SectorId]
sectoresDelArbol EmptyT =  []
sectoresDelArbol (NodeT s t1 t2 ) =
    obtenerId s : sectoresDelArbol t1 ++ sectoresDelArbol t2

obtenerId :: Sector -> SectorId
obtenerId (S id c t )= id

------
poderDePropulsion :: Nave -> Int 
poderDePropulsion (N t) = poderPropulsionArbol t

poderPropulsionArbol :: Tree Sector -> Int 
poderPropulsionArbol EmptyT = 0
poderPropulsionArbol (NodeT s t1 t2) = 
    poderPropulsionSector s + poderPropulsionArbol t1 +
    poderPropulsionArbol t2 


poderPropulsionSector :: Sector -> Int 
poderPropulsionSector (S id c t) = sumaPoderMotores c 


sumaPoderMotores :: [Componente] -> Int 
sumaPoderMotores [] = 0
sumaPoderMotores (x:xs) =
    poderMotor x + sumaPoderMotores xs 


poderMotor :: Componente -> Int 
poderMotor (Motor n) = n 
poderMotor _         = 0

----------------------------------------------------------------------------------3
barriles :: Nave -> [Barril] 
barriles (N t) = barrilesSectores t 

barrilesSectores :: Tree Sector -> [Barril] 
barrilesSectores EmptyT = []
barrilesSectores (NodeT s t1 t2) = barrilesSector s ++ 
    barrilesSectores t1 ++ barrilesSectores t2

barrilesSector :: Sector -> [Barril] 
barrilesSector (S id c t) = todosLosBarriles c 

todosLosBarriles :: [Componente] -> [Barril]
todosLosBarriles []     = []
todosLosBarriles (x:xs) = 
    if esAlmacen x 
        then deVolverBarril x  ++ todosLosBarriles xs
            else todosLosBarriles xs

esAlmacen :: Componente -> Bool 
esAlmacen (Almacen l)= True 
esAlmacen _ = False 

deVolverBarril :: Componente -> [Barril]
deVolverBarril (Almacen ls) = ls


--[]++ [] = []  
--[] : [] = [[]] 


agregarASector :: [Componente] -> SectorId -> Nave -> Nave 
agregarASector lsC id (N n) =(N (aniadirASector n id lsC) ) 


aniadirASector :: Tree Sector -> SectorId -> [Componente] -> Tree Sector
aniadirASector (EmptyT) id lsC = EmptyT
aniadirASector (NodeT s t1 t2 ) id lsC = 
    if obtenerId s == id
        then (NodeT (aniadirComponente s lsC) t1 t2 )
            else (NodeT s (aniadirASector t1 id lsC) (aniadirASector t2 id lsC) )


aniadirComponente:: Sector -> [Componente] -> Sector
aniadirComponente (S id c t) lsC = (S id (lsC ++ c) t) 


--(N (NodeT (S "N 456" [] []) (NodeT (S "N 458" [] []) (NodeT (S "N 986" [] []) EmptyT EmptyT )EmptyT )(NodeT (S "N 555" [] []) EmptyT EmptyT )) )
--[LanzaTorpedos , Motor 10]
--"N 555"

-------------------
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave 
asignarTripulanteA  tripulante lsId (N t)= 
    (N (asignarEnSectores t tripulante lsId  ))

asignarEnSectores :: Tree Sector -> Tripulante -> [SectorId] -> Tree Sector 
asignarEnSectores EmptyT t lsId = EmptyT
asignarEnSectores (NodeT s t1 t2) t lsId =
    if  pertenece (obtenerId s) lsId
        then (NodeT (agregarTripulante s t ) (asignarEnSectores t1  t lsId) (asignarEnSectores t2 t lsId) )
            else (NodeT s (asignarEnSectores t1  t lsId) (asignarEnSectores t2  t lsId) )


pertenece:: Eq a => a -> [a] -> Bool
pertenece elem []     = False
pertenece elem (x:xs) = elem == x || pertenece elem xs

agregarTripulante:: Sector -> Tripulante -> Sector
agregarTripulante (S id c t) tripulante = (S id c (tripulante : t )) 

------------
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tripulante (N t) = 
    sectoresDe tripulante t

sectoresDe:: Tripulante -> Tree Sector -> [SectorId]
sectoresDe t (EmptyT) = []
sectoresDe t (NodeT s t1 t2) = 
    if perteneceA t s 
    then obtenerId s : sectoresDe t t1 ++ sectoresDe t t2
    else sectoresDe t t1 ++ sectoresDe t t2

perteneceA:: Tripulante -> Sector -> Bool 
perteneceA t (S id c ts) = pertenece t ts

-------------------------------

tripulantes :: Nave -> [Tripulante]
tripulantes (N t ) =  sinRepetidos (tripulantesDeSectores t )

tripulantesDeSectores :: Tree Sector -> [Tripulante]
tripulantesDeSectores (EmptyT) = []
tripulantesDeSectores (NodeT s t1 t2) =
    (obtenerTripulantes s) ++ tripulantesDeSectores t1 ++ tripulantesDeSectores t2


obtenerTripulantes :: Sector -> [Tripulante]
obtenerTripulantes (S id c t) = t 


sinRepetidos ::  [Tripulante] ->  [Tripulante]
sinRepetidos [] = []
sinRepetidos (x:xs) = 
    if pertenece x xs 
        then sinRepetidos xs 
        else x : sinRepetidos xs
