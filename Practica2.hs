--Auxiliares
sucesor :: Int -> Int
sucesor num = num + 1

negar :: Bool -> Bool
negar True = False
negar False = True

------------------------------------------------------------------------------------------------------------
--PRACTICA 2
--PATTER MATCHING 

--1)
isEmpty :: [a] -> Bool
isEmpty  []  = True
isEmpty  [a]  = False

--2)
head' :: [a] -> a
head' (x: xs) = x
head' xs = error "head no aplica para una lista vacia"

--3)
tail' ::  [xs]  -> [xs] 
tail' (x : xs) =  xs

--Recursion sobre listas
--1)
sumatoria :: [Int] -> Int
sumatoria []        = 0
sumatoria  (x : xs ) = x + sumatoria xs

--2)
longitud :: [a] -> Int
longitud []       = 0
longitud (x : xs) = 1 + longitud xs

--3)
sucesores :: [Int] -> [Int]
sucesores []     = [] 
sucesores (x:xs) = sucesor x : sucesores xs

--4)
sumaDePares :: [(Int, Int)] -> [Int]
sumaDePares []             = []
sumaDePares [(nro1, nro2)] = [ nro1 + nro2 ] 

--5)
conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (x:xs) =  x && conjuncion xs

--6)
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

--7)
pertenece :: Eq a => a -> [a] -> Bool
pertenece elemento [] = False
pertenece elemento (x : xs) = if elemento == x then True else pertenece elemento xs

--8) 
apariciones :: Eq a => a -> [a] -> Int
apariciones elemento [] = 0
apariciones elemento (x:xs) = if elemento == x then 1 + apariciones elemento xs else apariciones elemento xs

--9)
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA nro [] = []
losMenoresA nro (x:xs) = if nro > x then x : losMenoresA nro xs else losMenoresA nro xs

--10)
losDistintosA :: Eq a => a -> [a] -> [a]
losDistintosA elemento [] = []
losDistintosA elemento (x:xs) = if  negar(elemento == x) then [x]++losDistintosA elemento xs else losDistintosA elemento xs

--11)
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = longitud x : longitudes xs 

--12)
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA nro  [] = []
lasDeLongitudMayorA nro (x:xs) = if longitud x > nro then x: lasDeLongitudMayorA nro xs else lasDeLongitudMayorA nro xs

--13)
intercalar :: a -> [a] -> [a]
intercalar a [] = []
intercalar a (x:xs) = [x] ++ [a] ++ intercalar a xs

--14) 
snoc :: [a] -> a -> [a]
snoc [] y = [y]
snoc (x:xs) y = x : snoc xs y

--15)
append :: [a] -> [a] -> [a]
append [] [] = []
append [] (x:xs) = (x:xs)
---append (x:xs) [] = (x:xs)
append (x:xs) lista2 = x : append xs lista2

--16)
aplanar :: [[a]] -> [a]
aplanar [[]] = [] 
aplanar [(x:xs)] = (x:xs) 

--17)
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = snoc (reversa xs) x

--18)
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos  []  [] =  []
zipMaximos [] (x:xs) = (x:xs)
zipMaximos (x:xs) [] = (x:xs)
zipMaximos (x1:xs1) (x2:xs2) = if x1 > x2 then x1: zipMaximos xs1 xs2  else x2: zipMaximos xs1 xs2

--19)
zipSort :: [Int] -> [Int] -> [(Int, Int)]
zipSort  []  [] =  []
zipSort  (x:xs)  [] =  error "ZipSort requiere que las dos listas posean un valor y con la misma cantidad"
zipSort  [] (x:xs)  =  error "ZipSort requiere que las dos listas posean un valor y con la misma cantidad"
zipSort (x1:xs1) (x2:xs2) = if x1 > x2  then (x2, x1) : zipSort xs1 xs2  else (x1, x2) : zipSort xs1 xs2 

--20)
promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = (sumatoria (x:xs) ) `div` longitud (x:xs)

--21)
minimun :: Ord a => [a] -> a
minimun (x:[]) = x
minimun [] = error "ZipSort requiere que las dos listas posean un valor y con la misma cantidad"
minimun (x:xs) = if x > (head(xs) ) && null xs then minimun (x :(tail xs)) else minimun xs  


--RECURSION SOBRE NUMEROS

--1)factorial :: Int -> [Int]

--2)
--cuentaRegresiva ::

--3)

--4)

--5)

--6)

--7)

--8)

--ANEXO CON EJERCICIOS ADICIONALES


