-----------------------------------------------
--               Ejemplo.hs                  --
-----------------------------------------------
answer :: Int -- una constante entera
answer = 42

newline :: Char
newline = '\n'

yes :: Bool -- La respuesta yes es representada
yes = True -- por el valor Booleano True

greater :: Bool
greater = (answer > 71)

-----------------------------------------------
--         El cuadrado de un número          --
-----------------------------------------------
square :: Int -> Int
square x = x * x

-------------------------------------------------
--             Son todos iguales               --
-------------------------------------------------
allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

-------------------------------------------------
--          El máximo de dos enteros           --
-------------------------------------------------
maxi :: Int -> Int -> Int
maxi n m
  | n > m = n
  | otherwise = m