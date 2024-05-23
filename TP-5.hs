import Data.Kind (Type)

--------------------------------
--          TP-5.hs           --
--------------------------------
-- Punto 2 --
losCuatroIguales :: Int -> Int -> Int -> Int -> Bool
losCuatroIguales n m o p = (n == m) && (m == o) && (o == p)

-- Punto 3 --
allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

losCuatroIguales2 :: Int -> Int -> Int -> Int -> Bool
losCuatroIguales2 n m o p = allEqual n m o && (o == p)

-- Punto 4 --
cuantosIguales :: Int -> Int -> Int -> Int
cuantosIguales n m o
  | (n == m) && (m == o) = 3
  | (n == m) || (m == o) || (n == o) = 2
  | otherwise = 0

cuantosIgualesDeDos :: Int -> Int -> Int
cuantosIgualesDeDos n m
  | n == m = 2
  | otherwise = 0

-- Punto 5 --
-- inciso a--
fibonacci :: Int -> Int
fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | n > 1 = fibonacci (n - 2) + fibonacci (n - 1)
  | otherwise = 0

numeroEnecimoDeFibonacci :: Int
numeroEnecimoDeFibonacci = fibonacci 11

-- inciso b--
fac :: Integer -> Integer
fac a
  | a < 2 = 1
  | otherwise = a * fac (a - 1)

-- inciso c--
sumatoriaPotencia :: Integer -> Integer -> Integer
sumatoriaPotencia i n
  | i == 1 = 1 ^ n
  | otherwise = i ^ n + sumatoriaPotencia (i - 1) n

funcionF :: Integer -> Integer -> Double
funcionF x n = fromIntegral (sumatoriaPotencia n n) / fromIntegral (fac x)

-- Punto 6 --
nAnd :: Bool -> Bool -> Bool
nAnd arg arg2
  | arg && arg = False
  | otherwise = True

-- Punto 7 --
funny :: Int -> Int -> Int -> Bool
funny x y z
  | x >= y = False
  | otherwise = True

-- Punto 8 --
allDiferent :: Int -> Int -> Int -> Bool
allDiferent n m o = (n /= m) && (n /= o) && (m /= o)

-- Punto 9--
-- allDiferent n m p = ((n /= m) && (m /= p))

-- Punto 10--
alCuadrado :: Int -> Int
alCuadrado x = x * x

cuartaPotencia :: Int -> Int
cuartaPotencia x = alCuadrado x * alCuadrado x

-- Punto 11 --
allFourEqual :: Int -> Int -> Int -> Int -> Bool
allFourEqual n m o p = (n == m) && (n == o) && (n == p) && (m == o) && (m == p) && (o == p)

-- Punto 12 --
valorNumerico :: Char -> String
valorNumerico n = case n of
  '0' -> "cero"
  '1' -> "uno"
  '2' -> "dos"
  '3' -> "tres"
  '4' -> "cuatro"
  '5' -> "cinco"
  '6' -> "seis"
  '7' -> "siete"
  '8' -> "ocho"
  '9' -> "nueve"

-- Punto 13 --
digitoRomano :: Char -> String
digitoRomano d = case d of
  '0' -> ""
  '1' -> "I"
  '2' -> "II"
  '3' -> "III"
  '4' -> "IV"
  '5' -> "V"
  '6' -> "VI"
  '7' -> "VII"
  '8' -> "VIII"
  '9' -> "IV"

-- Hacer el ejercicio de de digitoRomano :: Int -> String (usar recursividad y usar fotos)

-- Punto 14 --
entreLineas :: String -> String -> String -> String
entreLineas a b c = a ++ "\n" ++ b ++ "\n" ++ c

imprimirEntreLineas :: String -> String -> String -> IO ()
imprimirEntreLineas a b c = putStrLn (entreLineas a b c)

-- Punto 15 --
duplicar :: String -> Int -> String
duplicar cadena n
  | n == 0 = ""
  | otherwise = cadena ++ duplicar cadena (n - 1)

-- Punto 16 --
hacerEspacios :: Int -> String
hacerEspacios n
  | n == 0 = ""
  | otherwise = " " ++ hacerEspacios (n - 1)

-- Punto 17 --
factorialTable :: Int -> Int -> String
factorialTable m n
  | m == n = show n ++ "\t" ++ show (fac (toInteger n))
  | otherwise = factorialTable m (n - 1) ++ "\n" ++ show n ++ "\t" ++ show (fac (toInteger n))

imprimirFactorialTable :: Int -> Int -> IO ()
imprimirFactorialTable m n = putStrLn (factorialTable m n)

-- Punto 18 --
justificarCentro :: Int -> String -> String
justificarCentro n cad = hacerEspacios ((n - 1) `div` 2) ++ cad ++ hacerEspacios ((n - 1) `div` 2)

imprimirJustificarCentro :: Int -> String -> IO ()
imprimirJustificarCentro n cad = putStrLn (justificarCentro n cad)

justificarCentro2 :: Int -> String -> String
justificarCentro2 n cad = espacios ++ cad ++ espacios
  where
    espacios = hacerEspacios tam
    tam = div (n - 1) 2

imprimirJustificarCentro2 :: Int -> String -> IO ()
imprimirJustificarCentro2 n cad = putStrLn (justificarCentro2 n cad)

-- Punto 19 --
minMax :: (Int, Int) -> (Int, Int)
minMax (n, m)
  | n > m = (m, n)
  | otherwise = (n, m)

-- Punto 20 --
maxi :: Int -> Int -> Int
maxi n m
  | n > m = n
  | otherwise = m

maxOcurr :: Int -> Int -> (Int, Int)
maxOcurr n m = (max, cant)
  where
    max = maxi n m
    cant = cuentaIguales max n m
    cuentaIguales val n m = esval n + esval m
      where
        esval :: Int -> Int
        esval x = if x == val then 1 else 0

maxOcurr2 :: Int -> Int -> Int -> (Int, Int)
maxOcurr2 n m p = (max, cant)
  where
    max = maxi (maxi n m) p
    cant = cuentaIguales max n m p
    cuentaIguales val n m p = esval n + esval m + esval p
      where
        esval :: Int -> Int
        esval x = if x == val then 1 else 0

-- Punto 21 --
ordenTriple :: (Int, Int, Int) -> (Int, Int, Int)
ordenTriple (a, b, c)
  | min < a && a < max = (min, a, max)
  | min < c && c < max = (min, c, max)
  | otherwise = (min, b, max)
  where
    max = maxi (maxi a b) c
    min = mini (mini a b) c
    mini :: Int -> Int -> Int
    mini x y
      | x < y = x
      | otherwise = y

-- Punto 22 --
-- inciso a --
type Persona = (String, String, Int)

nombre :: Persona -> String
nombre (a, b, x) = a

direccion :: Persona -> String
direccion (a, b, x) = b

telefono :: Persona -> Int
telefono (a, b, x) = x