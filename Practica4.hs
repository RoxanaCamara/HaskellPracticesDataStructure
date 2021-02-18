--PRACTICA 4

--1)--Constructor

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa | Queso | Jamon | Morron | Cebolla | Ajo | Tomate | Aceitunas Int deriving (Show, Eq)

---
capas :: Pizza -> [Ingrediente]
capas Prepizza = []
capas (Capa i p ) = i : capas(p)

---
tieneJamon :: Pizza -> Bool
tieneJamon Prepizza = False
tieneJamon pizzaX = ingredienteEstaEn Jamon (capas pizzaX)

ingredienteEstaEn :: Ingrediente -> [Ingrediente] -> Bool
ingredienteEstaEn c []     = False
ingredienteEstaEn c (x:xs) = ( c == x )|| ingredienteEstaEn c xs

--
sacarJamon :: Pizza -> Pizza
sacarJamon pizzaX = 
    armarPizza (sacarIngrediente Jamon (capas pizzaX) )

sacarIngrediente :: Ingrediente -> [Ingrediente] -> [Ingrediente]
sacarIngrediente ing [] = []
sacarIngrediente ing (x:xs) =
     if x == ing 
        then sacarIngrediente ing xs
        else x: sacarIngrediente ing xs

--
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = (Capa x (armarPizza xs))

--
duplicarAceitunas :: Pizza -> Pizza 
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p ) = 
    ( Capa (masAceitunas ing) (duplicarAceitunas p) )

masAceitunas :: Ingrediente ->  Ingrediente
masAceitunas (Aceitunas nro) =( Aceitunas (nro * 2))
masAceitunas ing = ing

--
cantIngredientesPorPizza :: [Pizza] -> [(Int, Pizza)]
cantIngredientesPorPizza [] = [] 
cantIngredientesPorPizza (x:xs) = 
    (cantIngredientes x , x ) : cantIngredientesPorPizza xs 

cantIngredientes :: Pizza -> Int
cantIngredientes pizzaX = length (capas pizzaX)

--------------------------------
--4
napolitana :: Pizza
napolitana = Capa Ajo napolitanaSinAjo

--3
napolitanaSinAjo :: Pizza
napolitanaSinAjo = Capa Tomate pizzaConSalsaYQueso

--3
pizzaConJamon :: Pizza
pizzaConJamon = Capa Jamon pizzaConSalsaYQueso

--4
pizzaConJamonYMorron :: Pizza
pizzaConJamonYMorron = Capa Morron pizzaConJamon

--4
pizzaAceitunaYSalsa :: Pizza
pizzaAceitunaYSalsa = Capa (Aceitunas 5) pizzaConSalsaYQueso

--2
pizzaConSalsaYQueso :: Pizza
pizzaConSalsaYQueso = Capa Salsa soloQueso

--1
soloQueso :: Pizza
soloQueso = Capa Queso Prepizza

------------------------------------------------------------------------------------------------
--2)
--Constructores
data Objeto = Cacharro | Tesoro deriving (Show, Eq)

data Camino = Fin 
            | Cofre [Objeto] Camino 
            | Nada Camino 
            deriving (Show, Eq)

--Funciones 

-----------------------------------
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada camino)= False
hayTesoro (Cofre objs camino ) = 
    tesorosEncontrados objs || hayTesoro camino

tesorosEncontrados ::  [Objeto] -> Bool
tesorosEncontrados [] = False 
tesorosEncontrados (x:xs) = 
    esTesoro x || tesorosEncontrados xs 

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro Cacharro = False

-------------------------------------
--Precondicion: tiene que haber un tesoro en el camino
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin                = error "No hay tesoros"
pasosHastaTesoro (Nada camino)      = 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre obs camino) = 
    if hayTesoro (Cofre obs camino) 
        then 0 else 1 + pasosHastaTesoro camino 

--------------------------------------
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn nro Fin = False
hayTesoroEn nro (Nada camino) =
    if nro == 0
    then False
    else  hayTesoroEn (nro-1) camino
hayTesoroEn nro (Cofre objs camino) = 
    if 0 == nro 
    then  tesorosEncontrados objs
    else hayTesoroEn (nro-1) camino

-----------------------------------------
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros nro Fin = nro == 0
alMenosNTesoros nro (Nada camino) = alMenosNTesoros nro camino    
alMenosNTesoros nro (Cofre objs camino) =
    if tesorosEncontrados objs
    then alMenosNTesoros (nro-1) camino
    else alMenosNTesoros (nro) camino

-----------------------------------------
--Parcial:
--Nro2 tiene que ser mas grande que Nro1
--
--cantTesorosEntre :: Int -> Int -> Camino -> Int
--cantTesorosEntre nro1 nro2 camino =  cantTesorosHasta nro2 camino -cantTesorosHasta nro1 camino

--cantTesorosHasta :: Int -> Camino -> Int
--cantTesorosHasta nro Fin = 0
--cantTesorosHasta nro (Cofre objs camino) = 
  --  if tesorosEncontrados objs
   -- then 1 + cantTesorosHasta (nro-1) camino
    --else  cantTesorosHasta (nro-1) camino

-----------------------
caminoLargo1 :: Camino
caminoLargo1 = Cofre  [Cacharro, Cacharro, Tesoro] caminoLargo2

caminoLargo2 :: Camino
caminoLargo2 = Nada caminoLargo3

caminoLargo3 :: Camino
caminoLargo3 = Nada caminoLargo4

caminoLargo4 :: Camino
caminoLargo4 = Nada caminoAmarillo

caminoAmarillo :: Camino
caminoAmarillo = Cofre [Cacharro, Cacharro, Tesoro] Fin

-------------------------------------------------------------------------------------------------------------------

data ListaNoVacia a = Unit a | Cons a (ListaNoVacia a) deriving Show

--No me deja hacer intancia LPM
--    Cons 3 (Cons 4 (Cons 5 (Unit 1)))
length' :: ListaNoVacia a -> Int
length' (Unit a) = 1
length' (Cons a ls) = 1 + length' ls


--DETALLE:
--Si Unit de tipo String y los demas Cons deben ser de String
--en el mismo arbol
head' :: ListaNoVacia a -> a
head' (Unit a) = a
head' (Cons a ls) = a

----Es una funcion parcial
--Se necesita si o si mas de 2 valores en ese arbol
tail' :: ListaNoVacia a -> ListaNoVacia a
tail' (Unit nro) = error "Se necesita mas de 2 valores en ese arbol"
tail' (Cons nro cons2) = cons2 

---Es una funcion parcial
minimo :: ListaNoVacia Int -> Int
minimo (Unit nro) = nro
minimo (Cons nro cons2) =  minimoDeNros nro (minimo cons2)

minimoDeNros :: Int -> Int -> Int
minimoDeNros nro1 nro2 = if nro1 > nro2 then nro2 else nro1

-------------------------------------------------------------------------------------------------------------------
data T a = A | B a | C a a | D (T a) | E a (T a) deriving Show

--Los caso base ?
    --A , B a y C a a 
--Los recursivos?
    -- D (T a) y E (T a)

size :: T a -> Int 
