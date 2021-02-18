import Practica_5_Stack

-- emptyS, isEmptyS, push, top, lenS

--Constante
apilar :: [a] -> Stack a 
apilar []     = emptyS
apilar (x:xs) = push x ( apilar xs )

--Cuadratica
desapilar :: Stack a -> [a]
desapilar s = if isEmptyS s then [] else (top s) :  desapilar (pop s) 

--Constante porque depende de un numero fijo
insertarEnPos :: Int -> a -> Stack a -> Stack a 
insertarEnPos nro e s = 
    if (nro-1) == 0 
        then push e s 
        else push (top s) (insertarEnPos (nro-1) e (pop s) )


--  push 8 $ push 55 $ push 4 emptyS
