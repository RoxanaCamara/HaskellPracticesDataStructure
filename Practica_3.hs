--TIPOS RECURSIVOS SIMPLES
--(Show,Eq)

data Color = Azul | Rojo deriving (Show, Eq)

data Celda = Bolita Color Celda | CeldaVacia deriving Show

--
nroBolitas :: Color -> Celda -> Int 
nroBolitas color CeldaVacia       = 0
nroBolitas color (Bolita c celda) =  if color == c 
    then 1 + nroBolitas color celda 
    else nroBolitas color celda 

--Pone en la ultima celda
poner :: Color -> Celda -> Celda
poner c CeldaVacia = (Bolita c CeldaVacia)
poner c (Bolita c1 celda) = (Bolita c1  (poner c celda ))


esCeldaVacia :: Celda -> Bool
esCeldaVacia CeldaVacia = True
esCeldaVacia _          = False

--Debe haber una Bolita del color indicado
sacar :: Color -> Celda -> Celda 
sacar c (Bolita c1 celda) =
    if c == c1
    then celda   
    else (Bolita c1 (sacar c celda))


ponerN :: Int -> Color -> Celda -> Celda
ponerN n c celda = 
    if n == 0 
    then celda 
    else ponerN (n-1) c (poner c celda)

---CAMINO HACIA EL TESORO
data Objeto  = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

--
hayTesoro :: Camino -> Bool
hayTesoro Fin             = False 
hayTesoro (Nada c)        = hayTesoro c
hayTesoro (Cofre objs c ) = poseeTesoro objs || hayTesoro c


poseeTesoro :: [Objeto] -> Bool
poseeTesoro [] = False 
poseeTesoro (x:xs) = esTesoro x || poseeTesoro xs 

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True
esTesoro _      = False 

---Agarra el tesoro mas lejano
pasosHastaElTesoro :: Camino -> Int 
pasosHastaElTesoro      Fin = 0
pasosHastaElTesoro (Nada c) = 1 + pasosHastaElTesoro c
pasosHastaElTesoro (Cofre objs c) =
    if hayTesoro c
    then 1+ pasosHastaElTesoro c
    else pasosHastaElTesoro c


--
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin = False
hayTesoroEn n (Nada c) = False 
hayTesoroEn n (Cofre objs c) = 
    if n == 0
    then poseeTesoro objs
    else hayTesoroEn (n-1) c

---Indica si la cantidad de tesoros es mayor o igual que N
alMenosNTesoros :: Int -> Camino ->  Bool
alMenosNTesoros n c = n <= cantidadDeTesoros c

cantidadDeTesoros :: Camino -> Int 
cantidadDeTesoros Fin = 0 
cantidadDeTesoros (Nada c) = cantidadDeTesoros c 
cantidadDeTesoros (Cofre objs c ) = 
    if poseeTesoro objs
    then 1+ cantidadDeTesoros c
    else cantidadDeTesoros c 

---
cantTesorosEntre :: Int -> Int -> Camino -> Int 
cantTesorosEntre n1 n2 Fin  = 0 
cantTesorosEntre n1 n2 (Nada c)= 
    cantTesorosEntre (n1+1) n2 c 
cantTesorosEntre n1 n2 (Cofre objs c) =
    if poseeTesoro objs &&  n2 >= n1
        then 1 + cantTesorosEntre (n1+1) n2 c 
        else cantTesorosEntre (n1+1) n2 c  

--TIPOS ARBOREOS
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

--
sumarT :: Tree Int -> Int 
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

--no entendi bien, todos los elementos que tiene el arbol,
-- sin contar los EmptyT
sizeT :: Tree a -> Int 
sizeT EmptyT = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2

--
mapDobleT :: Tree Int -> Tree Int 
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) =
    (NodeT (n * 2) (mapDobleT t1) (mapDobleT t2) )
    
--
perteneceT :: Eq a => a -> Tree a -> Bool 
perteneceT elem EmptyT = False
perteneceT elem (NodeT a t1 t2) = 
    elem  == a  || perteneceT elem t1 || perteneceT elem t2

---
aparicionesT :: Eq a => a -> Tree a -> Int 
aparicionesT elem EmptyT = 0
aparicionesT elem (NodeT a t1 t2) = 
    if elem == a 
    then 1 + aparicionesT elem t1 +  aparicionesT elem t2
    else aparicionesT elem t1 +  aparicionesT elem t2

---
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT a EmptyT EmptyT) = [a]
leaves (NodeT a t1 t2) = leaves t1 ++ leaves t2 

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT a t1 t2) = 
    1+
    if heightT t1 > heightT t2
    then heightT t1 
    else heightT t2
   
--
mirrorT :: Tree a -> Tree a
mirrorT (NodeT a t1 t2) = (NodeT a t2 t1)

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT a t1 t2) =
    toList t1 ++ [a] ++ toList t2

levelN :: Int -> Tree a -> [a]
levelN n  EmptyT         = []
levelN 0 (NodeT a t1 t2) = [a]
levelN n (NodeT a t1 t2) = 
    levelN (n-1) t1 ++ levelN (n-1) t2

{----
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT a t1 t2) = 
    [a] : listPerLevel t1 : listPerLevel t2 --}

ramaMasLarga ::Tree a ->[a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT a t1 t2) = 
    a : ( if heightT t1 >= heightT t2
    then toList t1 
    else toList t2)

---todosLosCaminos :: Tree a -> [[a]]
-- todosLosCaminos 

data ExpL = Valor Bool 
          | And ExpB ExpB 
          | Or ExpB ExpB 
          | Not ExpB deriving Show

--evalL :: ExpL -> Bool
--evalL exp = 


--simplificarL :: ExpL -> ExpL

