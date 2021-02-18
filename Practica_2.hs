
--1 Recursion sobrelistas 

--1
sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--2
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--3
sucesores :: [Int] -> [Int] 
sucesores [] = []
sucesores (x:xs) = (x+1) : sucesores xs

--4
conjuncion :: [Bool] -> Bool 
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

--5
disyuncion :: [Bool] -> Bool 
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

--6
--aplanar :: [[a]] -> [a]
--aplanar [a] = a + [] 
--aplanar (x:xs) =  x : aplanar xs

--7
pertenece :: Eq a => a -> [a] -> Bool
pertenece elem [] = False
pertenece elem (x:xs) = elem == x || pertenece elem xs

--8 
apariciones :: Eq a  => a -> [a] -> Int 
apariciones elem [] = 0
apariciones elem (x:xs) = if elem == x 
    then 1 + apariciones elem xs 
    else apariciones elem xs 

--9
losMenoresA:: Int -> [Int] -> [Int]
losMenoresA nro [] = []
losMenoresA nro (x:xs)= if nro < x 
    then x : losMenoresA nro xs 
    else losMenoresA nro xs

--10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA nro [] = []
lasDeLongitudMayorA nro (x:xs) = if nro < longitud x 
    then x : lasDeLongitudMayorA nro xs
    else  lasDeLongitudMayorA nro xs

--11
agergarAlFinal :: [a] -> a -> [a]
agergarAlFinal list elem = list ++ [elem]

--12
concatenar :: [a] -> [a] -> [a]
concatenar list1 list2 = list1++list2

--13
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

--14
--zipMaximos :: [Int] -> [Int] -> [Int]
--zipMaximos (x:xs) (c:cs) = 

--15 no puede estar vacia 
elMinimo :: Ord a => [a] -> a
elMinimo [x] = x
elMinimo (x:xs) = if x < elMinimo xs
    then  x
    else elMinimo xs

--RECURCION SOBRE NUMEROS

--1
factorial :: Int -> Int 
factorial  0  = 1
factorial nro = nro * factorial (nro-1) 

--2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva nro = nro : cuentaRegresiva (nro-1)

--3
repetir :: Int -> a -> [a]
repetir 0 elem   = []
repetir nro elem = elem : repetir (nro-1) elem

--4
losPrimeros :: Int -> [a] -> [a]
losPrimeros nro [] = []
losPrimeros 0 list = []
losPrimeros nro (x:xs) = x : losPrimeros (nro-1) xs

--5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros nro [] = []
sinLosPrimeros  0 list = list 
sinLosPrimeros nro (x:xs) = sinLosPrimeros (nro-1) xs 

--REGISTROS
--1
data Persona = Prsn String Int deriving Show

--
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA eedad [] = []
mayoresA edad (x:xs) = if edad < getEdad x 
    then x : mayoresA edad xs
    else mayoresA edad xs

getEdad :: Persona -> Int
getEdad (Prsn nombre edad) = edad

--
promedioDeEdad:: [Persona] -> Int 
promedioDeEdad listP  = sumatoriaDeEdades listP  `div`  longitud listP

sumatoriaDeEdades:: [Persona] -> Int 
sumatoriaDeEdades [] = 0
sumatoriaDeEdades (x:xs) = (getEdad x) + sumatoriaDeEdades xs

--
elMasViejo :: [Persona] -> Persona 
elMasViejo [x] =  x 
elMasViejo (x:xs) =if (getEdad x) > getEdad (elMasViejo xs)
    then x 
    else elMasViejo xs

--2
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = ConstPokemon TipoDePokemon Int deriving Show
data Entrenador = ConstEntrenador String [Pokemon] deriving Show

cantPokemones :: Entrenador -> Int 
cantPokemones (ConstEntrenador nombre lsPok ) = longitud lsPok

--
cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int 
cantPokemonesDe tipoB (ConstEntrenador nombre lsPok ) =
    cantPokemonesTipo tipoB lsPok

cantPokemonesTipo :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonesTipo tipoB [] = 0
cantPokemonesTipo tipoB (x:xs) =
    if pokemonEs tipoB x
        then 1 + cantPokemonesTipo tipoB xs
        else cantPokemonesTipo tipoB xs

pokemonEs :: TipoDePokemon -> Pokemon -> Bool
pokemonEs Agua (ConstPokemon Agua nro ) = True
pokemonEs Planta (ConstPokemon Planta nro ) = True
pokemonEs Fuego (ConstPokemon Fuego nro ) = True
pokemonEs tipoB (ConstPokemon _ nro ) = False

---losQueGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int 
--losQueGanan tipoB (ConstEntrenador nombre1 lsPok1) (ConstEntrenador nombre2 lsPok2 ) =


--
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConstEntrenador nombre lsPok ) = 
    hayPokemonTipo Agua lsPok &&  hayPokemonTipo Fuego lsPok &&  hayPokemonTipo Planta lsPok  

hayPokemonTipo :: TipoDePokemon -> [Pokemon]  -> Bool
hayPokemonTipo tipo [] = False
hayPokemonTipo tipo (x:xs) = pokemonEs tipo x || hayPokemonTipo tipo xs

--3
data Seniority = Junior | SemiSenior | Senior deriving Show

data Proyecto  = ConsProyecto String deriving (Show,Eq)

data Rol       = Developer Seniority Proyecto |
 Magnament Seniority Proyecto deriving Show

data Empresa = ConsEmpresa [Rol] deriving Show


--
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa lsRols ) = proyectosRoles lsRols

proyectosRoles :: [Rol] -> [Proyecto]
proyectosRoles [] =  [] 
proyectosRoles (x:xs) = if existeProyecto (obtenerProyecto x) ( proyectosRoles xs) 
    then proyectosRoles xs
    else obtenerProyecto x : proyectosRoles xs

obtenerProyecto :: Rol -> Proyecto
obtenerProyecto (Developer s p ) = p
obtenerProyecto (Magnament s p ) = p

existeProyecto:: Proyecto -> [Proyecto] -> Bool
existeProyecto p [] = False
existeProyecto p (x:xs) = 
    if x == p then True else existeProyecto p xs

---
losDevSenior :: Empresa -> Int 
losDevSenior (ConsEmpresa ls) = cantDevSeniors ls

cantDevSeniors:: [Rol] -> Int 
cantDevSeniors []     = 0
cantDevSeniors (x:xs) = if esSenior x 
        then 1 + cantDevSeniors xs
        else cantDevSeniors xs

esSenior :: Rol -> Bool
esSenior (Developer Senior p ) = True
esSenior (Magnament Senior p ) = True
esSenior cualRol = False

---
cantQuetrabajaEn :: [Proyecto] -> Empresa -> Int
cantQuetrabajaEn lsP (ConsEmpresa lsR) =
    cantTrabajanEn lsP lsR

cantTrabajanEn :: [Proyecto] -> [Rol] ->Int
cantTrabajanEn ps []     = 0
cantTrabajanEn ps (x:xs) = if existeProyecto (obtenerProyecto x) ps
    then 1 + cantTrabajanEn ps xs
    else cantTrabajanEn ps xs

---
asignadosPorProyectos :: Empresa -> [(Proyecto, Int)]
asignadosPorProyectos (ConsEmpresa ls) =
    proyectoYCantEmpleados (todosLosProyectos ls) ls

proyectoYCantEmpleados :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
proyectoYCantEmpleados [] lsR     = []
proyectoYCantEmpleados (p:ps) lsR = 
    (p , (cantEmpleadosProyecto p lsR ) ) : proyectoYCantEmpleados ps lsR


todosLosProyectos ::  [Rol] -> [Proyecto]
todosLosProyectos []     = []
todosLosProyectos (r:rs) =
    if existeProyecto (obtenerProyecto r) (todosLosProyectos rs)
        then todosLosProyectos rs
        else (obtenerProyecto r) : todosLosProyectos rs

cantEmpleadosProyecto :: Proyecto -> [Rol] -> Int 
cantEmpleadosProyecto p []     = 0
cantEmpleadosProyecto p (r:rs) = 
    if  (obtenerProyecto r) == p 
        then 1 + cantEmpleadosProyecto p rs
            else cantEmpleadosProyecto p rs








--asignadosPorProyectos (ConsEmpresa [ ( Developer Senior (ConsProyecto "Comafi") ), ( Developer Senior (ConsProyecto "Comafi") ) ] )






