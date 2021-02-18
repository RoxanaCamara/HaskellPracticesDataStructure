{-
    DATO:
        Si no corre en el winhugs es que hay jun error en el codigo y no de winghugs. Mira aca primero
-}

--1) NUMEROS ENTEROS
--A) 
sucesor :: Int -> Int
sucesor num = num + 1

--B) 
suma :: Int -> Int -> Int
suma num1 num2 = num1 + num2

--C) 
maximo :: Int -> Int -> Int
maximo numA numB = if numA > numB then numA else numB 

--2) maximo (sucesor(suma 2 3 )) (maximo 4 3 )

--3) TUPLAS 
--A) 
primera :: (Int, Int) -> Int
primera (compF, compS) = compF

--B) 
segunda :: (Int, Int) -> Int
segunda (compF, compS) = compS

--C) 
sumaPar :: (Int, Int) -> Int
sumaPar (compF, compS) = compF + compS

--D)
maxDelPar :: (Int, Int) -> Int
maxDelPar (compF, compS) = if compF > compS then compF else compS 

--4) TIPO ENUMERATIVOS Y PATTER MATCHING
--A)
data Adresses = Norte | Sur | Este | Oeste deriving Show


opuesto:: Adresses -> Adresses
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Este  = Oeste
opuesto Oeste = Este

--B)
siguiente:: Adresses -> Adresses
siguiente Norte = Este
siguiente Sur   = Oeste
siguiente Este  = Sur
siguiente Oeste = Norte

--5)
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

--A)
primerDia :: DiaDeSemana
primerDia = Lunes

--B)
ultimoDia :: DiaDeSemana
ultimoDia = Domingo

--C)
nroDeDia :: DiaDeSemana -> Int
nroDeDia Lunes     = 1
nroDeDia Martes    = 2
nroDeDia Miercoles = 3
nroDeDia Jueves    = 4
nroDeDia Viernes   = 5
nroDeDia Sabado    = 6
nroDeDia Domingo   = 7

--D)
empiezaConM :: DiaDeSemana -> Bool 
empiezaConM Martes          = True
empiezaConM Miercoles       = True
empiezaConM paraTodoLoDemas = False

--E)
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio diasSobrantes = True

--F)
diaSiguiente :: DiaDeSemana -> DiaDeSemana 
diaSiguiente Lunes = Martes
diaSiguiente Martes = Miercoles
diaSiguiente Miercoles = Jueves
diaSiguiente Jueves = Viernes
diaSiguiente Viernes = Sabado
diaSiguiente Sabado = Domingo

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = nroDeDia dia1== nroDeDia(diaSiguiente  dia2)

--6) 
--A)
negar :: Bool -> Bool
negar True = False
negar False = True

--B) 
and :: Bool -> Bool -> Bool
and True True = True
and x y       = False

--C) falla
or :: Bool -> Bool -> Bool
or False False = False
or bool1 bool2  = True

--7) FUNCIONES POLIMORFICAS 
--A)
loMismo :: cosa -> cosa
loMismo cosa = cosa

--B)
siempreSiete :: cosa -> Int
siempreSiete cosa = 7

--C)
duplicar :: cosa -> (cosa,cosa)
duplicar cosa = (cosa,cosa) 

--D)
loPrimero :: cosa -> cosab -> cosa
loPrimero cosa cosab = cosa

--E)
loSegundo :: cosa -> cosab -> cosab
loSegundo cosa cosab = cosab

--8)RESPONDEER: 
    -- PORQUE ESTAS FUNCIONES SON POLIMORFICAS?
--Porque obtienen valores de un tipo y pueden manejarlos para devovler otros de otro tipo
--Ejemplo esMayorACinco 8 y devuelve un booleano recibe un int y devuelve un bool


