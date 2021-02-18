--PRACTICA 3
--TIPOS ALGEBRAICOS

--Constructores
data Persona = ConstP String Int  deriving Show

--1) Funciones

nombre :: Persona -> String 
nombre (ConstP n e) = n

--
edad :: Persona -> Int
edad (ConstP n e) = e

--
crecer :: Persona -> Persona
crecer (ConstP n e) = ConstP n (e+1)

--
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre newName (ConstP n e) = (ConstP newName e)

--
esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra (ConstP n1 e1) (ConstP n2 e2) = e1 < e2

--
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA nro  [] =  []
mayoresA nro (x : xs) = if (edad x) > nro then x: mayoresA nro xs else mayoresA nro xs

--
promedioEdad :: [Persona] -> Int
promedioEdad listPersonas = ((sumaDeEdades listPersonas) `div` (length listPersonas))

sumaDeEdades :: [Persona] -> Int
sumaDeEdades [] = 0
sumaDeEdades (x:xs) = edad x + sumaDeEdades xs

cantidadDePersonas :: [Persona] -> Int
cantidadDePersonas [] = 0
cantidadDePersonas (x:xs) = 1 + cantidadDePersonas xs

--
elMasViejo :: [Persona] -> Persona
elMasViejo [x] = x
elMasViejo (x:xs) =  maximoEntrePersonas x  (elMasViejo xs)

maximoEntrePersonas :: Persona -> Persona -> Persona
maximoEntrePersonas (ConstP n1 e1) (ConstP n2 e2) = if e1 > e2 then (ConstP n1 e1) else (ConstP n2 e2) 

--Registro Celda
roxanne :: Persona
roxanne = ConstP "Roxanne Ricci" 27

orquito :: Persona
orquito = ConstP "Jorge" 23

grupoA = [orquito, orquito, orquito, roxanne]

------------------------------------------------------------------------------------------------------------
data Pokemon = Pk TipoPokemon Int deriving (Show, Eq)

data TipoPokemon = Agua | Fuego | Planta  deriving (Show, Eq)

data Entrenador = Ent String [Pokemon] deriving (Show, Eq)

--
superaA :: Pokemon -> Pokemon -> Bool
superaA (Pk Agua n1)    (Pk Fuego n2)   = True
superaA (Pk Fuego n1)   (Pk Planta n2)  = True
superaA (Pk Planta n1)  (Pk Agua n2)    = True
superaA (Pk tipo1 n1)   (Pk tipo2 n2)   = False

--
cantPokemones :: Entrenador -> Int
cantPokemones (Ent name [] ) = 0
cantPokemones (Ent name pokemones ) = cantidad pokemones

cantidad :: [Pokemon] -> Int
cantidad [] = 0
cantidad (x:xs) = 1 + cantidad xs

--
cantPokemonesDe :: TipoPokemon -> Entrenador -> Int
cantPokemonesDe tipoPk (Ent name pokemones ) = 
    cantidadPokemonesTipo tipoPk pokemones

cantidadPokemonesTipo :: TipoPokemon -> [Pokemon] -> Int
cantidadPokemonesTipo tipo [] = 0
cantidadPokemonesTipo tipo (x:xs) = 
    if pokemonEsDe tipo x 
        then 1 + cantidadPokemonesTipo tipo xs 
        else cantidadPokemonesTipo tipo xs

pokemonEsDe :: TipoPokemon -> Pokemon -> Bool
pokemonEsDe tipo (Pk tipoPk int) = tipo == tipoPk

---
concatPokemon :: [Entrenador] -> [Pokemon]
concatPokemon [] = []
concatPokemon  (x:xs) = pokemonesDe x ++ concatPokemon xs

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (Ent name pokemones ) = pokemones

---
esMaestro :: Entrenador -> Bool
esMaestro (Ent name pokemones) = cantidadPokemonesTipo Agua pokemones >= 1 && cantidadPokemonesTipo Fuego pokemones >= 1 && cantidadPokemonesTipo Planta pokemones >= 1 

lizardon :: Pokemon
lizardon = Pk Fuego 89

magikarp :: Pokemon 
magikarp = Pk Agua 1

squirtle:: Pokemon 
squirtle = Pk Agua 87

vulvasaur :: Pokemon 
vulvasaur = Pk Planta 86

ash :: Entrenador
ash = Ent "Ash el pierde ligas" [squirtle,lizardon,vulvasaur ]

don :: Entrenador
don = Ent "don " [squirtle,lizardon,vulvasaur,magikarp ]

nare :: Entrenador
nare = Ent "nare el pierde ligas" [squirtle,magikarp ]