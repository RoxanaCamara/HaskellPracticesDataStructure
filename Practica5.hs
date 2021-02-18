--PRACTICA 5 
--ARBOLES BINARIOS

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show 

--1)
sumarT :: Tree Int -> Int
sumarT EmptyT           = 0 --tiene que ser cero porque sino en la suma falla el programa
sumarT (NodeT nro t1 t2) = nro + sumarT t1 + sumarT t2

--2)
sizeT :: Tree a -> Int 
sizeT EmptyT              = 0 
sizeT (NodeT elem t1 t2 ) = 1 + sizeT t1 + sizeT t2

--3)
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT nro t1 t2) = NodeT (nro * 2) (mapDobleT t1) (mapDobleT t2)

--4)
mapLongitudT :: Tree String -> Tree Int 
mapLongitudT EmptyT = EmptyT
mapLongitudT (NodeT word t1 t2) = NodeT ( length word ) (mapLongitudT t1) (mapLongitudT t2)

--5)
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT elem EmptyT          = False
perteneceT elem (NodeT a t1 t2) = (elem == a) || (perteneceT elem t1) || (perteneceT elem t2)

--6)
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT elem EmptyT          = 0
aparicionesT elem (NodeT a t1 t2) = 
    if (elem == a ) 
        then 1 + aparicionesT elem t1 + aparicionesT elem t2 
        else aparicionesT elem t1 + aparicionesT elem t2 

--7)
esUnEmptyT :: Tree a -> Bool
esUnEmptyT EmptyT = True
esUnEmptyT arbol  = False

countLeaves :: Tree a -> Int
countLeaves EmptyT                 =  0
countLeaves (NodeT a t1 t2)         = 
    if( esUnEmptyT t1 && esUnEmptyT t2 ) 
        then 1 + countLeaves t1 + countLeaves t2 
        else countLeaves t1 + countLeaves t2

--8)
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT a t1 t2) = 
    if ( esUnEmptyT t1 && esUnEmptyT t2 ) 
        then  a : leaves t1 ++ leaves t2
        else  leaves t1 ++ leaves t2

--9)
height :: Tree a -> Int
height EmptyT = 0
height (NodeT a t1 t2) = 
    1+
    if height t1 > height t2
    then height t1 
    else height t2

--10)
esUnNodeT :: Tree a -> Bool
esUnNodeT EmptyT = False
esUnNodeT arbol  = True

countNotLeaves :: Tree a -> Int
countNotLeaves EmptyT                 =  0
countNotLeaves (NodeT a t1 t2)         = 
    if( esUnNodeT t1 || esUnNodeT t2 ) 
        then 1 + countNotLeaves t1 + countNotLeaves t2 
        else countNotLeaves t1 + countNotLeaves t2

--11)
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT a t1 t2) = (NodeT a (mirrorT t2) (mirrorT t1))

--12)
--los hijos izq, raiz y derecho 
listInOrder :: Tree a -> [a]
listInOrder EmptyT = []
listInOrder (NodeT a t1 t2) = listInOrder t1 ++ [a] ++ listInOrder t2

--13) Se procesa la raiz,  elementos del hijo izq y despues del derecho
listPreOrder :: Tree a -> [a]
listPreOrder EmptyT = []
listPreOrder (NodeT a t1 t2) = a : listPreOrder t1 ++ listPreOrder t2

--14) 
-- hijo izq , derechos y raiz
listPostOrder :: Tree a -> [a]
listPostOrder EmptyT = []
listPostOrder (NodeT a t1 t2) = listPostOrder t1 ++ listPostOrder t2 ++ [a]

--15)
concatenarListasT :: Tree  [a] -> [a]
concatenarListasT EmptyT =  []
concatenarListasT (NodeT a t1 t2) = concatenarListasT t1 ++ a ++ concatenarListasT t2

--16)
levelN :: Int -> Tree a -> [a]
levelN nro EmptyT = []
levelN nro (NodeT a t1 t2) =  if nro == 0 then [a] else levelN (nro-1) t1 ++ if nro == 0 then [a] else levelN (nro-1) t2

--17) no lo hice
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
--listPerLevel (NodeT a t1 t2) =    a  listPerLevel t1      listPerLevel t2 

--18)
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
--ramaMasLarga (NodeT a t1 t2) =    if( largoDeRama t1 > largoDeRama t2 )  then t1    else t2

--19)
--todosLosCaminos :: Tree a -> [a]
--todosLosCaminos EmptyT = []
--todosLosCaminos (NodeT a t1 t2)

--20)
--balanceado :: Tree a -> Bool
--balanceado EmptyT = False
--balanceado (Node a t1 t2) =        a    balanceado t1    balanceado t2

--Ejemplos 
hoja :: a -> Tree a
hoja x = NodeT x EmptyT EmptyT

--arbol1 :: Tree Int
--arbol1 = NodeT 8                           --tronco
--                ( NodeT 1                   --rama
--                        (hoja 2) (NodeT 3  
--                            (hoja 4)  (NodeT 5
--                                        (hoja 6) (hoja 7) ) ) ) --hoja
--                (NodeT 9                    --rama
--                        (hoja 10) (hoja 11) ) --hoja

arbolWord :: Tree String
arbolWord = NodeT "Roxana Narela: "                                               --12
            (NodeT "Los de mi . "                      --
                (hoja "Vamos p. ")             --
                (hoja "Voy a p ") )  --
            (NodeT "vas a hablarlo. "                --
                (hoja "Vamos de viajes. ")             --
                (hoja "Vamos a ") )                --
-----------------------------------------------------------------

---MANADA DE LOBOS 
type Presa = String --Nombre de la presa

type Territorio = String --Nombre del Territorio

type Nombre = String --Nombre del lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cria Nombre deriving Show

data Manada = M Lobo deriving Show

manada1 :: Manada 
manada1 = M  (Explorador "Nerg" ["Pradera", "Montania"]
    (Cazador "Ogar" [ "Endht", "Gurt","Berg", "Endht", "Gurt"] 
            (Explorador "Emirg" ["Pradera", "Montania"] 
                    (Cria "Rezil")
                    (Cria "Lijuel")  )
            (Explorador "Zamir1" ["desierto", "bosque"] 
                    (Cria "Paluel")
                    (Cria "Birner")  ) 
            (Cria "Irnial") )
    (Cazador "Oga" ["Berg", "Endht", "Gurt","Berg", "Endht", "Gurt"] 
            (Explorador "Emirg" ["Pradera", "Montania"] 
                    (Cria "Rezil")
                    (Cria "Lijuel")  )
            (Explorador "Zamir" ["desierto", "bosque"] 
                    (Cria "Paluel")
                    (Cria "Birner")  ) 
            (Cria "Irnial") ) )


----------------------------------------
--La cantidad de alimento cazado debe ser mayor al de crias

buenaCaza :: Manada -> Bool
buenaCaza (M manadita) =  cantidadAlimento manadita > cantidadCrias manadita  

cantidadAlimento :: Lobo -> Int
cantidadAlimento (Cria nombre) = 0
cantidadAlimento (Explorador nombre territorios lobo1 lobo2) =
    cantidadAlimento lobo1 + cantidadAlimento lobo2
cantidadAlimento (Cazador nombre alimento lobo1 lobo2 lobo3) = 
    length alimento + cantidadAlimento lobo1 + cantidadAlimento lobo2 + cantidadAlimento lobo3


cantidadCrias :: Lobo -> Int
cantidadCrias (Cria nombre) = 1
cantidadCrias (Explorador nombre territorios lobo1 lobo2) = 
    cantidadCrias lobo1 + cantidadCrias lobo2
cantidadCrias (Cazador nombre alimento lobo1 lobo2 lobo3) = 
    cantidadCrias lobo1 + 
    cantidadCrias lobo2 + 
    cantidadCrias lobo3

---------------------------------
{--En el caso de que no hayan cazadores o
no hayan cazado ninguno el alfa va a ser el 
primer lobo que se cruce--}

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobitos) = maximoCazador lobitos 

maximoCazador :: Lobo -> (Nombre, Int) 
maximoCazador (Cria nombre) = (nombre, 0)
maximoCazador (Cazador nombre presa lobo1 lobo2 lobo3) =
    maximo
    (maximo (nombre, length presa) (maximoCazador lobo1))
    (maximo (maximoCazador lobo2 ) (maximoCazador lobo3))

maximoCazador (Explorador nombre territorios lobo1 lobo2) =
    maximo
    (maximo (nombre, 0) (maximoCazador lobo1))
    (maximoCazador lobo2) 

maximo :: (Nombre, Int ) -> (Nombre , Int )  -> (Nombre , Int ) 
maximo  (name1, nro1) (name2, nro2) = if  nro1 > nro2 then (name1, nro1) else (name2, nro2)  

--------------------
loQueExploraron :: Territorio -> Manada -> [Nombre]
loQueExploraron terri (M lobos) =  loQueExploraronLobos terri lobos 

loQueExploraronLobos :: Territorio -> Lobo -> [Nombre]
loQueExploraronLobos terri (Cria nombre) =   []
loQueExploraronLobos terri (Explorador nombre territorios lobo1 lobo2) =
   if elem  terri territorios 
       then nombre : loQueExploraronLobos terri lobo1 ++ loQueExploraronLobos terri lobo2
           else loQueExploraronLobos terri lobo1 ++ loQueExploraronLobos terri lobo2

loQueExploraronLobos terri (Cazador nombre presa lobo1 lobo2 lobo3) = 
    [] ++  loQueExploraronLobos terri lobo1 ++ loQueExploraronLobos terri lobo2 ++ loQueExploraronLobos terri lobo3


--exploro:: Territorio -> [Territorio] -> Bool
--exploro terri [] = False
--exploro terri (x:xs) =  x == terri || exploro terri xs
--elem  es lo mismo que exploro

-----------------------------------------
exploradoresPorTerritorios:: Manada -> [(Territorio, [Nombre]) ] 
exploradoresPorTerritorios (M lobos) =
    exploradoresPorTerritoriosLobos (sinRepetidos (todosLosTerritorios lobos) ) lobos

exploradoresPorTerritoriosLobos :: [Territorio] -> Lobo -> [(Territorio, [Nombre]) ] 
exploradoresPorTerritoriosLobos [] lobos = []
exploradoresPorTerritoriosLobos (x:xs) lobos = ( x, loQueExploraronLobos x lobos ) : exploradoresPorTerritoriosLobos xs lobos

todosLosTerritorios :: Lobo -> [Territorio]
todosLosTerritorios (Cria nombre) =  []
todosLosTerritorios (Explorador nombre territorios lobo1 lobo2) =
    territorios ++
    todosLosTerritorios lobo1 ++
    todosLosTerritorios lobo2
todosLosTerritorios (Cazador nombre presa lobo1 lobo2 lobo3) = 
    todosLosTerritorios lobo1 ++
    todosLosTerritorios lobo2 ++
    todosLosTerritorios lobo3

sinRepetidos:: Eq a => [a] -> [a]
sinRepetidos [] =  []
sinRepetidos (x:xs) =  if elem x xs then sinRepetidos xs else x: sinRepetidos xs

-----------------------------------------
superioresDelCazador :: Nombre -> Manada ->[Nombre]
superioresDelCazador nombre (M lobos) = superioresDelCazadorLobos nombre lobos []

superioresDelCazadorLobos :: Nombre -> Lobo ->[Nombre] ->[Nombre]
superioresDelCazadorLobos nombreI (Cria nombre) ls =  
    if nombre == nombreI 
        then ls 
        else []

superioresDelCazadorLobos nombreI (Explorador nombre territorios lobo1 lobo2) ls =
    if nombre == nombreI 
        then ls 
        else superioresDelCazadorLobos nombreI lobo1 (nombre:ls) ++ superioresDelCazadorLobos nombreI lobo2 (nombre:ls)

superioresDelCazadorLobos nombreI (Cazador nombre presa lobo1 lobo2 lobo3) ls =
    if nombre == nombreI 
        then ls 
        else superioresDelCazadorLobos nombreI lobo1 (nombre:ls) 
        ++ superioresDelCazadorLobos nombreI lobo2 (nombre:ls)
        ++ superioresDelCazadorLobos nombreI lobo3 (nombre:ls)

