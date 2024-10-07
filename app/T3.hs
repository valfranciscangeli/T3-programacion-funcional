{-# LANGUAGE InstanceSigs #-}
module T3 where

import Control.Monad.State
import Data.List (nub) -- para eliminar duplicados de listas
import Prelude hiding (cycle)

{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

-- First component: # moves to the right
-- Second component: # moves up
type Pos = (Int, Int)

-- Parte (a)
possibleMoves :: Pos -> [Pos]
possibleMoves (x, y) = do
  pos <- [(x + 1, y), (x, y + 1)] -- posiciones posibles
  if abs (uncurry (-) pos) <= 3 -- aplicar condicion a cada Pos
    then return pos
    else []

-- Parte (b)
nSteps :: Int -> Pos -> [Pos]
nSteps 0 pos = [pos] -- caso base
nSteps n pos = nub $ do
  -- se aplica nub al final para eliminar repetidos
  newPos <- possibleMoves pos
  nub $ nSteps (n - 1) newPos -- se aplica nub en cada recursion para evitar iterar en elementos repetidos

{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

data Result e g = Fail e | Good g

instance Functor (Result e) where
  -- fmap :: (a->b) -> Result e a -> Result e b
  fmap _ (Fail err) = Fail err -- f no se usa
  fmap f (Good res) = Good (f res)

-- instancia para poder mostrar el resultado:
instance (Show e, Show g) => Show (Result e g) where
  show :: (Show e, Show g) => Result e g -> String
  show (Fail err) = "Fail " ++ show err
  show (Good res) = "Good " ++ show res

-- instancia para comparar resultados:
instance (Eq e, Eq g) => Eq (Result e g) where
  (==) :: (Eq e, Eq g) => Result e g -> Result e g -> Bool
  (Fail err1) == (Fail err2) = err1 == err2
  (Good res1) == (Good res2) = res1 == res2
  _ == _ = False

-- Parte (a)
-- diapo 22 clase 13
instance Applicative (Result e) where
  pure :: a -> Result e a
  pure = Good
  (<*>) :: Result e (a -> b) -> Result e a -> Result e b
  (Fail err) <*> _ = Fail err
  (Good f) <*> res = fmap f res

-- diapo 10 clase 14
-- ya es Applicative, asi que agregamos los op de monada solamente
instance Monad (Result e) where
  (>>=) :: Result e a -> (a -> Result e b) -> Result e b
  (Fail err) >>= _ = Fail err
  (Good res) >>= f = f res

-- Parte (b)
type Shop = String

cycle :: Shop -> Shop -> Result String ()
cycle = error "cycle undef"

shop :: Shop -> Result String ()
shop = error "shop undef"

-- original
{-
doGroceries :: [Shop] -> Result String ()
doGroceries []         = Fail "No Shops to visit!"
doGroceries [x]        = shop x
doGroceries (x1:x2:xs) = case shop x1 of
    Fail e1 -> Fail e1
    Good _  -> case cycle x1 x2 of
        Fail e2 -> Fail e2
        Good _  -> doGroceries (x2:xs)
-}

-- base en diapo 19 clase 14
doGroceries :: [Shop] -> Result String ()
doGroceries [] = Fail "No Shops to visit!" -- se mantiene
doGroceries [x] = shop x -- tambien se mantiene
doGroceries (x1 : x2 : xs) = do
  -- se aplica do-notation para recorrer la lista
  shop x1 -- entra a comprar a la tienda 1
  cycle x1 x2 -- si se pudo comprar, sigue a la tienda 2
  doGroceries (x2 : xs) -- si llega a la tienda 2, se repite

{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

data Command = Inc | Dec | Mult | Store

-- Primer componente: numero en la hoja
-- Segundo componente: numero en la mano de Bob
type GameState = (Int, Int)

playGame :: [Command] -> State GameState Int
------------
playGame [] = do
  -- no quedan comandos
  (_, mano) <- get
  return mano -- termina, entrega el num en mano
playGame (Inc : cmds) = do
  -- comando inicial pide +1
  modify (\(hoja, mano) -> (hoja, mano + 1))
  playGame cmds -- sigue la recursion
playGame (Dec : cmds) = do
  -- comando inicial pide -1
  modify (\(hoja, mano) -> (hoja, mano - 1))
  playGame cmds -- sigue la recursion
playGame (Mult : cmds) = do
  -- comando inicial pide multiplicar por hoja
  (_, _) <- get
  modify (\(hoja, mano) -> (hoja, mano * hoja))
  playGame cmds -- sigue la recursion
playGame (Store : cmds) = do
  -- comando inicial pide sobreescribir hoja
  (_, _) <- get
  modify (\(_, mano) -> (mano, mano))
  playGame cmds -- sigue la recursion

{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}

type Probability = Rational

--type Dist a = T Probability a  -- se comenta para poder crear una monada, no pude usar la libreria
-- fuente: https://dennybritz.com/posts/probability-monads-from-scratch/

newtype Dist a = Dist [(a, Probability)]

unpackDist :: Dist a -> [(a, Probability)]
unpackDist (Dist xs) = xs

squishD :: (Ord a) => Dist a -> Dist a
squishD (Dist xs) = Dist $ M.toList $ M.fromListWith (+) xs

-- | Sum all probilities in the given list
sumP :: [(a, Probability)] -> Probability
sumP = sum . Prelude.map snd

-- | Normalize the probabilities to 1.0
normP :: [(a, Probability)] -> [(a, Probability)]
normP xs = [(x, p / q) | let q = sumP xs, (x, p) <- xs]

instance Functor Dist where
  fmap f (Dist xs) = Dist $ [(f x, p) | (x, p) <- xs]

instance Applicative Dist where
  -- pure :: a -> Dist a
  pure x = Dist [(x, 1.0)]
  -- (<*>) :: Dist (a -> b) -> Dist a -> Dist b
  (Dist fs) <*> (Dist xs) = Dist $ do
    (x, px) <- xs
    (f, pf) <- fs
    return (f x, px * pf)

instance Monad Dist where
  -- (>>=) :: Dist a -> (a -> Dist b) -> Dist b
  (Dist xs) >>= f = Dist $ do
    (x, p) <- xs
    (y, p') <- unpackDist (f x)
    return (y, p * p')



-- Genera una distribución uniforme de los valores en el rango dado
uniform :: [a] -> Dist a
uniform xs = Dist [(x, 1 / fromIntegral (length xs)) | x <- xs]

-- Parte (a.I)
pointDist :: Int -> Dist (Int, Int)
-----------
pointDist r = do
  x <- uniform [-r .. r] -- x toma un valor del rango del cuadrado
  y <- uniform [-r .. r] -- y toma un valor del rango del cuadrado
  return (x, y)

-- Parte (a.II)
-- ejercicio tipo Monte Carlo
resultE3a :: Int -> Probability
resultE3a r =
    let cantidadPuntos = 10000
    in runDist $ do
        puntos <- replicateM cantidadPuntos (pointDist r) -- Genera los puntos
        dentroCirculo <- cantidadDentroCirculo r (return puntos) -- Cuenta los puntos dentro del círculo
        return (4 * (fromIntegral dentroCirculo / fromIntegral cantidadPuntos)) -- Realiza el cálculo

-- Asegúrate de que `cantidadDentroCirculo` tenga este tipo
cantidadDentroCirculo :: Int -> Dist [(Int, Int)] -> Dist Int
cantidadDentroCirculo r pointsDist = do
    points <- pointsDist
    let dentroCirculo = length [() | (x, y) <- points, fromIntegral x^2 + fromIntegral y^2 <= fromIntegral (r^2)]
    return dentroCirculo

-- Parte (b)
data Uni = Chile | Cato deriving (Eq)

type Urn = (Int, Int)

-- 1er componente: #jugadores Chile, 2do componente: #jugadores Cato


-- Función que representa el proceso de sacar una bolilla
pickPlayer :: Urn -> Dist (Uni, Urn)
pickPlayer (uch, cato)
  | uch > 0 && cato > 0 = uniform [(Chile, (uch - 1, cato)), (Cato, (uch, cato - 1))]
  | uch > 0 = return (Chile, (uch - 1, cato))
  | otherwise = return (Cato, (uch, cato - 1))

-- Función para armar los singles
pairs :: Urn -> Dist [(Uni, Uni)]
pairs urn = go urn []
  where
    go :: Urn -> [(Uni, Uni)] -> Dist [(Uni, Uni)]
    go (0, 0) acc = return (reverse acc)
    go urn acc = do
      (p1, urn1) <- pickPlayer urn
      (p2, urn2) <- pickPlayer urn1
      go urn2 ((p1, p2) : acc)

-- Función para calcular la probabilidad deseada
resultE3b :: Probability
resultE3b = do
  let totalPairs = 10 
      initialUrn = (8, 2) 
  singles <- pairs initialUrn
  let sameUni = filter (uncurry (==)) singles
      probability = 1 - (fromIntegral (length sameUni) / fromIntegral totalPairs)
  return probability




-- 