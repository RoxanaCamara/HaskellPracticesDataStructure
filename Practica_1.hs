--Practica 1


--1 Numeros Enteros
--a)
sucesor :: Int -> Int
sucesor nro = nro + 1

--b)
sumar :: Int -> Int -> Int 
sumar nro1 nro2 = nro1 + nro2 

--c)
divisionYResto :: Int -> Int -> (Int , Int)
divisionYResto nro1 nro2 = (nro1 `div` nro2  , mod nro1 nro2)

--d)
maxDelPar:: (Int, Int) -> Int 
maxDelPar (nro1, nro2) = if nro1 > nro2 then nro1 else nro2

--e) 2)
--1 (sumar ( sucesor 4 ) ( sucesor 4 ))
--2  maxDelPar ((sumar  2 (sucesor 7) ), 4)
--3  maxDelPar (divisionYResto (sumar 30 10) 4)
--4  sucesor (maxDelPar (3, (sumar 5 4)) )

--2 Tipo Enumerativos
--1
data Dir = Norte | Sur | Este | Oeste deriving Show

--a)
opuesto ::Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

--b)
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

--c)
vieneDespuesDir :: Dir -> Dir 
vieneDespuesDir Norte = Este
vieneDespuesDir Este = Sur
vieneDespuesDir Sur = Oeste
vieneDespuesDir Oeste = Norte

--Precondicion que los valores que recibe por parametro 
--sean validos 
--Entonces es parcial

--2
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

--a)
primeroyUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroyUltimoDia = (Lunes, Domingo)

--b)
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

--c)
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes  Martes = True
vieneDespues Martes   Miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves   Viernes = True
vieneDespues Viernes  Sabado = True
vieneDespues Sabado    Domingo  = True
vieneDespues Domingo     Lunes  = True
vieneDespues _ _ = False

--d)
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes =  False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

--3
--a
negar :: Bool -> Bool
negar True = False
negar False = True

--b 
implica :: Bool -> Bool -> Bool 
implica True False = False
implica _ _ = True

--c
and :: Bool -> Bool -> Bool
and  True True  = True
and  _    _     = False

--d)
or:: Bool -> Bool -> Bool 
or False False = False
or _ _ = True

--3 Registros

--1
data Persona = ConstP String Int  deriving Show

nombre :: Persona -> String 
nombre (ConstP name age ) = name

edad:: Persona -> Int 
edad (ConstP name age ) = age

crecer :: Persona -> Persona
crecer (ConstP name age ) = (ConstP name (age+1) )  

cambioDeNombre :: String -> Persona -> Persona 
cambioDeNombre newName (ConstP name age) = (ConstP newName age)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (ConstP name1 age1) (ConstP name2 age2) =  age1 > age2 

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (ConstP name1 age1) (ConstP name2 age2) = 
    if age1 > age2 
    then (ConstP name1 age1) 
    else (ConstP name2 age2)

--2

data Pokemon = Poke TipoDePokemon Int deriving Show
data TipoDePokemon = Agua | Fuego | Planta deriving Show

data Entrenador = Ent String Pokemon Pokemon deriving Show

----
superaA :: Pokemon -> Pokemon -> Bool
superaA (Poke tipo1 vida1 ) (Poke tipo2 vida2 ) = tipoEsMasFuerteQue tipo1 tipo2 

tipoEsMasFuerteQue :: TipoDePokemon -> TipoDePokemon -> Bool
tipoEsMasFuerteQue Agua Fuego   = True 
tipoEsMasFuerteQue Fuego Planta = True
tipoEsMasFuerteQue Planta Agua  = True 
tipoEsMasFuerteQue _      _     = False 

---
cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe tipo (Ent name poke1 poke2) =
    if esDeTipo tipo poke1  && esDeTipo tipo poke2
        then 2
        else if esDeTipo tipo poke1 || esDeTipo tipo poke2
            then 1
            else 0

esDeTipo :: TipoDePokemon -> Pokemon -> Bool
esDeTipo Agua   (Poke Agua vida2 ) = True
esDeTipo Fuego  (Poke Fuego vida2 )= True
esDeTipo Planta (Poke Planta vida2)= True
esDeTipo _      (Poke _ vida2 )    = False

--
juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemones ( entrenador1, entrenador2) = juntarPokemonsEntrenadores entrenador1 entrenador2

juntarPokemonsEntrenadores :: Entrenador -> Entrenador -> [Pokemon]
juntarPokemonsEntrenadores (Ent nameA p3 p4) (Ent name1 p1 p2) =  p1 : p2 : p3 : p4 : []

--

--4 Funciones Polimorficas

--a
loMismo :: a -> a 
loMismo unaCosa = unaCosa

siempreSiete :: a -> Int 
siempreSiete elem = 7

swap :: (a,b) -> (b,a)
swap (a,b) = (b, a)

--Estas son funciones polimorficas porque estas trabajan
-- con elementos con un tipo y pueden llegar a devolver de otro tipo totalmente diferente
 
--Patter Matching 

--2
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False 

--
elPrimero :: [a] -> a
elPrimero (x:xs) = x
--4
sinElPrimero :: [a] -> [a]
sinElPrimero (x:xs) = xs 

--5
splitHead ::  [a] -> (a, [a])
splitHead  (x:xs) = (x , xs )